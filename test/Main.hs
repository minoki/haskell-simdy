{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where
import qualified Data.Bits
import           Data.Int
import           Data.Kind
import           Data.List (foldl1')
import           Data.Proxy
import           Data.Simdy (SIMD, SIMDNum, X16, X2, X32, X4, X64, X8)
import           Data.Simdy.Bits
import           Data.Simdy.Horizontal
import           Data.Simdy.MinMax
import           Data.Simdy.Internal.Class as S
import           Data.Word
import           GHC.Exts (IsList (Item, fromList, toList))
import           Prelude hiding (max, min, (/=), (<), (<=), (==), (>), (>=))
import qualified Prelude
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Providers (IsTest (..))
import           Test.Tasty.Providers.ConsoleFormat (noResultDetails)
import qualified Test.Tasty.QuickCheck as QC
import           Test.Tasty.Runners (Outcome (Success), Result (..),
                                     TestTree (SingleTest))

newtype Report = Report String

instance IsTest Report where
  run _ (Report message) _callback = pure $ Result Success message "OK" 0.0 noResultDetails
  testOptions = pure []

type SizedList :: (Type -> Type) -> Type -> Type
newtype SizedList x a = MkSizedList { unSized :: [a] } deriving Show

instance (KnownSIMDLength x, QC.Arbitrary a) => QC.Arbitrary (SizedList x a) where
  arbitrary = MkSizedList <$> QC.vectorOf (simdLength @x) QC.arbitrary

fromSizedList :: (IsList (x a), Item (x a) ~ a) => SizedList x a -> x a
fromSizedList (MkSizedList xs) = fromList xs

class (IsList (x a), Item (x a) ~ a) => ListLike x a
instance PackX2 X2 a => ListLike X2 a
instance PackX4 X4 a => ListLike X4 a
instance PackX8 X8 a => ListLike X8 a
instance PackX16 X16 a => ListLike X16 a
instance PackX32 X32 a => ListLike X32 a
instance PackX64 X64 a => ListLike X64 a

infix 4 ===

class SameValue a where
  sameValue :: a -> a -> Bool
  (===) :: a -> a -> QC.Property

newtype StdEq a = MkStdEq a

instance (Eq a, Show a) => SameValue (StdEq a) where
  sameValue (MkStdEq x) (MkStdEq y) = x Prelude.== y
  MkStdEq x === MkStdEq y = x QC.=== y

newtype FloatEq a = MkFloatEq a

instance (Eq a, RealFloat a, Show a) => SameValue (FloatEq a) where
  sameValue (MkFloatEq x) (MkFloatEq y) = x Prelude.== y Prelude.|| (isNaN x Prelude.&& isNaN y)
  MkFloatEq x === MkFloatEq y = QC.counterexample (show x ++ interpret res ++ show y) res
    where
      res = sameValue (MkFloatEq x) (MkFloatEq y)
      interpret True  = " == "
      interpret False = " /= "

deriving via StdEq Bool instance SameValue Bool
deriving via StdEq Int8 instance SameValue Int8
deriving via StdEq Int16 instance SameValue Int16
deriving via StdEq Int32 instance SameValue Int32
deriving via StdEq Int64 instance SameValue Int64
deriving via StdEq Word8 instance SameValue Word8
deriving via StdEq Word16 instance SameValue Word16
deriving via StdEq Word32 instance SameValue Word32
deriving via StdEq Word64 instance SameValue Word64
deriving via FloatEq Float instance SameValue Float
deriving via FloatEq Double instance SameValue Double

instance (SameValue a, Show a) => SameValue [a] where
  sameValue xs ys = length xs Prelude.== length ys Prelude.&& and (zipWith sameValue xs ys)
  x === y = QC.counterexample (show x ++ interpret res ++ show y) res
    where
      res = sameValue x y
      interpret True  = " == "
      interpret False = " /= "

simdLen :: forall x a. KnownSIMDLength x => Proxy (x a) -> Int
simdLen _ = simdLength @x

testPackUnpack :: (KnownSIMDLength x, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testPackUnpack proxy = QC.testProperty "pack/unpack" $
  \xs -> toList (fromSizedList xs `asProxyTypeOf` proxy) === unSized xs

testBroadcast :: (Broadcast x a, KnownSIMDLength x, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testBroadcast proxy = QC.testProperty "broadcast" $
  \a -> toList (broadcast a `asProxyTypeOf` proxy) === replicate (simdLen proxy) a

testSelect :: forall x a. (Selectable (x a), KnownSIMDLength x, MaskIsLiftedBool x a, ListLike x a, ListLike x Bool, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testSelect proxy = QC.testProperty "select" $
  \cond t f ->
    toList (select (fromSizedList cond) (fromSizedList t) (fromSizedList f) `asProxyTypeOf` proxy) === zipWith3 (\c a b -> if c then a else b) (unSized cond) (unSized t) (unSized f)

testEq :: (Eq a, Equatable (x a), KnownSIMDLength x, MaskIsLiftedBool x a, ListLike x a, ListLike x Bool, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testEq proxy = testGroup "equality"
  [ QC.testProperty "==" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) == fromSizedList b) === zipWith (Prelude.==) (unSized a) (unSized b)
  , QC.testProperty "/=" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) /= fromSizedList b) === zipWith (Prelude./=) (unSized a) (unSized b)
  ]

testOrd :: (Ord a, Ordered (x a), KnownSIMDLength x, MaskIsLiftedBool x a, ListLike x a, ListLike x Bool, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testOrd proxy = testGroup "order"
  [ QC.testProperty "<" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) < fromSizedList b) === zipWith (Prelude.<) (unSized a) (unSized b)
  , QC.testProperty "<=" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) <= fromSizedList b) === zipWith (Prelude.<=) (unSized a) (unSized b)
  , QC.testProperty ">" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) > fromSizedList b) === zipWith (Prelude.>) (unSized a) (unSized b)
  , QC.testProperty ">=" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) >= fromSizedList b) === zipWith (Prelude.>=) (unSized a) (unSized b)
  ]

testNum :: (Num a, Num (x a), KnownSIMDLength x, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testNum proxy = testGroup "Num"
  [ QC.testProperty "+" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) + fromSizedList b) === zipWith (+) (unSized a) (unSized b)
  , QC.testProperty "-" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) - fromSizedList b) === zipWith (-) (unSized a) (unSized b)
  , QC.testProperty "*" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) * fromSizedList b) === zipWith (*) (unSized a) (unSized b)
  , QC.testProperty "negate" $ \a ->
      toList (negate (fromSizedList a `asProxyTypeOf` proxy)) === map negate (unSized a)
  , QC.testProperty "abs" $ \a ->
      toList (abs (fromSizedList a `asProxyTypeOf` proxy)) === map abs (unSized a)
  , QC.testProperty "signum" $ \a ->
      toList (signum (fromSizedList a `asProxyTypeOf` proxy)) === map signum (unSized a)
  ]

testFractional :: (Fractional a, Fractional (x a), KnownSIMDLength x, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testFractional proxy = testGroup "Fractional"
  [ QC.testProperty "/" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) / fromSizedList b) === zipWith (/) (unSized a) (unSized b)
  , QC.testProperty "recip" $ \a ->
      toList (recip (fromSizedList a `asProxyTypeOf` proxy)) === map recip (unSized a)
  ]

testFloating :: (Floating a, Floating (x a), KnownSIMDLength x, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testFloating proxy = testGroup "Floating"
  [ QC.testProperty "sqrt" $ \a ->
      toList (sqrt (fromSizedList a `asProxyTypeOf` proxy)) === map sqrt (unSized a)
  ]

testEnum :: forall x a. (Enum a, EnumFromZero x a, KnownSIMDLength x, ListLike x a, QC.Arbitrary a, Eq a, Show a) => Proxy (x a) -> TestTree
testEnum proxy = testCase "enumFromZero" $ toList (enumFromZero :: x a) @?= [0..fromIntegral n - 1]
  where
    n = simdLen proxy

testBoolean :: forall x a. (Data.Bits.Bits a, SIMDBoolean a, SIMD x, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testBoolean proxy = testGroup "Boolean"
  [ QC.testProperty ".&." $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) .&. fromSizedList b) === zipWith (Data.Bits..&.) (unSized a) (unSized b)
  , QC.testProperty ".|." $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) .|. fromSizedList b) === zipWith (Data.Bits..|.) (unSized a) (unSized b)
  , QC.testProperty "xor" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) `xor` fromSizedList b) === zipWith Data.Bits.xor (unSized a) (unSized b)
  , QC.testProperty "complement" $ \a ->
      toList (complement (fromSizedList a `asProxyTypeOf` proxy)) === map Data.Bits.complement (unSized a)
  , QC.testProperty "horizontalAnd" $ \a ->
      horizontalAnd (fromSizedList a `asProxyTypeOf` proxy) === foldl1' (.&.) (unSized a)
  , QC.testProperty "horizontalOr" $ \a ->
      horizontalOr (fromSizedList a `asProxyTypeOf` proxy) === foldl1' (.|.) (unSized a)
  , QC.testProperty "horizontalXor" $ \a ->
      horizontalXor (fromSizedList a `asProxyTypeOf` proxy) === foldl1' xor (unSized a)
  ]

testBits :: forall x a. (Data.Bits.FiniteBits a, BitShift (x a), KnownSIMDLength x, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testBits proxy = testGroup "Bits"
  [ QC.testProperty "shiftL" $ \(QC.NonNegative i) a ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) `shiftL` i) === map (`Data.Bits.shiftL` i) (unSized a)
  , QC.testProperty "unsafeShiftL" $ QC.forAll (QC.chooseInt (0, bs - 1)) $ \i a ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) `unsafeShiftL` i) === map (`Data.Bits.shiftL` i) (unSized a)
  , QC.testProperty "shiftR" $ \(QC.NonNegative i) a ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) `shiftR` i) === map (`Data.Bits.shiftR` i) (unSized a)
  , QC.testProperty "unsafeShiftL" $ QC.forAll (QC.chooseInt (0, bs - 1)) $ \i a ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) `unsafeShiftR` i) === map (`Data.Bits.shiftR` i) (unSized a)
  ]
  where
    bs = Data.Bits.finiteBitSize (undefined :: a)

testMinMax :: forall x a. (MinMax a, MinMax (x a), KnownSIMDLength x, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testMinMax proxy = testGroup "MinMax"
  [ QC.testProperty "min" $ \a b ->
      toList (min (fromSizedList a `asProxyTypeOf` proxy) (fromSizedList b)) === zipWith min (unSized a) (unSized b)
  , QC.testProperty "max" $ \a b ->
      toList (max (fromSizedList a `asProxyTypeOf` proxy) (fromSizedList b)) === zipWith max (unSized a) (unSized b)
  , QC.testProperty "minimumNumber" $ \a b ->
      toList (minimumNumber (fromSizedList a `asProxyTypeOf` proxy) (fromSizedList b)) === zipWith minimumNumber (unSized a) (unSized b)
  , QC.testProperty "maximumNumber" $ \a b ->
      toList (maximumNumber (fromSizedList a `asProxyTypeOf` proxy) (fromSizedList b)) === zipWith maximumNumber (unSized a) (unSized b)
  ]

testFMA :: forall x a. (FusedMultiplyAdd a, HasFMA => FusedMultiplyAdd (x a), KnownSIMDLength x, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testFMA proxy = case isFMAAvailable of
  Just MkFMAWitness -> QC.testProperty "FMA" $ \a b c ->
    toList (fusedMultiplyAdd (fromSizedList a `asProxyTypeOf` proxy) (fromSizedList b) (fromSizedList c)) === zipWith3 fusedMultiplyAdd (unSized a) (unSized b) (unSized c)
  Nothing -> testGroup "FMA" []

testHorizontalIntegral :: forall x a. (SIMD x, SIMDNum a, SIMDMinMax a, SIMDBoolean a, Boolean a, Ord a, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testHorizontalIntegral proxy = testGroup "Horizontal(Integral)"
  [ QC.testProperty "sum" $ \a ->
      horizontalSum (fromSizedList a `asProxyTypeOf` proxy) === sum (unSized a)
  , QC.testProperty "product" $ \a ->
      horizontalProduct (fromSizedList a `asProxyTypeOf` proxy) === product (unSized a)
  , QC.testProperty "min" $ \a ->
      horizontalMin (fromSizedList a `asProxyTypeOf` proxy) === minimum (unSized a)
  , QC.testProperty "max" $ \a ->
      horizontalMax (fromSizedList a `asProxyTypeOf` proxy) === maximum (unSized a)
  , QC.testProperty "minimumNumber" $ \a ->
      horizontalMinimumNumber (fromSizedList a `asProxyTypeOf` proxy) === minimum (unSized a)
  , QC.testProperty "maximumNumber" $ \a ->
      horizontalMaximumNumber (fromSizedList a `asProxyTypeOf` proxy) === maximum (unSized a)
  , QC.testProperty "and" $ \a ->
      horizontalAnd (fromSizedList a `asProxyTypeOf` proxy) === foldl1' (.&.) (unSized a)
  , QC.testProperty "or" $ \a ->
      horizontalOr (fromSizedList a `asProxyTypeOf` proxy) === foldl1' (.|.) (unSized a)
  , QC.testProperty "xor" $ \a ->
      horizontalXor (fromSizedList a `asProxyTypeOf` proxy) === foldl1' xor (unSized a)
  ]

testHorizontalFloating :: forall x a. (SIMD x, SIMDNum a, SIMDMinMax a, RealFloat a, Enum a, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testHorizontalFloating proxy = testGroup "Horizontal(Floating)"
  [ QC.testProperty "sum" $ QC.forAll (fmap MkSizedList . QC.vectorOf (simdLength @x) $ QC.chooseEnum (-10, 10)) $ \a ->
      horizontalSum (fromSizedList a `asProxyTypeOf` proxy) === sum (unSized a)
  , QC.testProperty "product" $ QC.forAll (fmap MkSizedList . QC.vectorOf (simdLength @x) $ QC.elements [-8,-4,-2,-1,1,2,4,8]) $ \a ->
      horizontalProduct (fromSizedList a `asProxyTypeOf` proxy) === product (unSized a)
  , QC.testProperty "min" $ \a ->
      horizontalMin (fromSizedList a `asProxyTypeOf` proxy) === foldl1' S.min (unSized a)
  , QC.testProperty "max" $ \a ->
      horizontalMax (fromSizedList a `asProxyTypeOf` proxy) === foldl1' S.max (unSized a)
  , QC.testProperty "minimumNumber" $ \a ->
      horizontalMinimumNumber (fromSizedList a `asProxyTypeOf` proxy) === foldl1' S.minimumNumber (unSized a)
  , QC.testProperty "maximumNumber" $ \a ->
      horizontalMaximumNumber (fromSizedList a `asProxyTypeOf` proxy) === foldl1' S.maximumNumber (unSized a)
  ]

properties :: forall x
            . ( SIMD x
              , ImplementationDescription x
              , ListLike x Bool
              , ListLike x Int8
              , ListLike x Int16
              , ListLike x Int32
              , ListLike x Int64
              , ListLike x Word8
              , ListLike x Word16
              , ListLike x Word32
              , ListLike x Word64
              , ListLike x Float
              , ListLike x Double
              , NumF x Int8
              , NumF x Int16
              , NumF x Int32
              , NumF x Int64
              , NumF x Word8
              , NumF x Word16
              , NumF x Word32
              , NumF x Word64
              , NumF x Float
              , NumF x Double
              , FractionalF x Float
              , FractionalF x Double
              , FloatingF x Float
              , FloatingF x Double
              ) => Proxy x -> [TestTree]
properties proxyX =
  [ SingleTest "implementation description" $ Report (implementationDescription proxyX)
  , let proxy :: Proxy (x Bool)
        proxy = Proxy
    in testGroup "Bool"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      -- , testEq proxy
      , testBoolean proxy
      ]
  , let proxy :: Proxy (x Int8)
        proxy = Proxy
    in testGroup "Int8"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      , testEq proxy
      , testOrd proxy
      , testNum proxy
      , testEnum proxy
      , testBoolean proxy
      , testBits proxy
      , testMinMax proxy
      , testHorizontalIntegral proxy
      ]
  , let proxy :: Proxy (x Int16)
        proxy = Proxy
    in testGroup "Int16"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      , testEq proxy
      , testOrd proxy
      , testNum proxy
      , testEnum proxy
      , testBoolean proxy
      , testBits proxy
      , testMinMax proxy
      , testHorizontalIntegral proxy
      ]
  , let proxy :: Proxy (x Int32)
        proxy = Proxy
    in testGroup "Int32"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      , testEq proxy
      , testOrd proxy
      , testNum proxy
      , testEnum proxy
      , testBoolean proxy
      , testBits proxy
      , testMinMax proxy
      , testHorizontalIntegral proxy
      ]
  , let proxy :: Proxy (x Int64)
        proxy = Proxy
    in testGroup "Int64"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      , testEq proxy
      , testOrd proxy
      , testNum proxy
      , testEnum proxy
      , testBoolean proxy
      , testBits proxy
      , testMinMax proxy
      , testHorizontalIntegral proxy
      ]
  , let proxy :: Proxy (x Word8)
        proxy = Proxy
    in testGroup "Word8"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      , testEq proxy
      , testOrd proxy
      , testNum proxy
      , testEnum proxy
      , testBoolean proxy
      , testBits proxy
      , testMinMax proxy
      , testHorizontalIntegral proxy
      ]
  , let proxy :: Proxy (x Word16)
        proxy = Proxy
    in testGroup "Word16"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      , testEq proxy
      , testOrd proxy
      , testNum proxy
      , testEnum proxy
      , testBoolean proxy
      , testBits proxy
      , testMinMax proxy
      , testHorizontalIntegral proxy
      ]
  , let proxy :: Proxy (x Word32)
        proxy = Proxy
    in testGroup "Word32"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      , testEq proxy
      , testOrd proxy
      , testNum proxy
      , testEnum proxy
      , testBoolean proxy
      , testBits proxy
      , testMinMax proxy
      , testHorizontalIntegral proxy
      ]
  , let proxy :: Proxy (x Word64)
        proxy = Proxy
    in testGroup "Word64"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      , testEq proxy
      , testOrd proxy
      , testNum proxy
      , testEnum proxy
      , testBoolean proxy
      , testBits proxy
      , testMinMax proxy
      , testHorizontalIntegral proxy
      ]
  , let proxy :: Proxy (x Float)
        proxy = Proxy
    in testGroup "Float"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      , testEq proxy
      , testOrd proxy
      , testNum proxy
      , testFractional proxy
      , testFloating proxy
      , testEnum proxy
      , testMinMax proxy
      , testFMA proxy
      , testHorizontalFloating proxy
      ]
  , let proxy :: Proxy (x Double)
        proxy = Proxy
    in testGroup "Double"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      , testEq proxy
      , testOrd proxy
      , testNum proxy
      , testFractional proxy
      , testFloating proxy
      , testEnum proxy
      , testMinMax proxy
      , testFMA proxy
      , testHorizontalFloating proxy
      ]
  ]

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "X2" (properties (Proxy @X2))
  , testGroup "X4" (properties (Proxy @X4))
  , testGroup "X8" (properties (Proxy @X8))
  , testGroup "X16" (properties (Proxy @X16))
  , testGroup "X32" (properties (Proxy @X32))
  , testGroup "X64" (properties (Proxy @X64))
  ]

main :: IO ()
main = defaultMain tests
