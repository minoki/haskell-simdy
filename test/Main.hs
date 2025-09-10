{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where
import qualified Data.Bits
import           Data.Int
import           Data.Kind
import           Data.Proxy
import           Data.Simdy (SIMD, X16, X2, X32, X4, X8)
import           Data.Simdy.Class.Bits
import           Data.Simdy.Internal.Class as S
import           Data.Word
import           GHC.Exts (IsList (Item, fromList, toList))
import           Prelude hiding ((/=), (<), (<=), (==), (>), (>=))
import qualified Prelude
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

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

testBits :: forall x a. (Data.Bits.FiniteBits a, MiniBits (x a), KnownSIMDLength x, ListLike x a, QC.Arbitrary a, SameValue a, Show a) => Proxy (x a) -> TestTree
testBits proxy = testGroup "Bits"
  [ QC.testProperty ".&." $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) .&. fromSizedList b) === zipWith (Data.Bits..&.) (unSized a) (unSized b)
  , QC.testProperty ".|." $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) .|. fromSizedList b) === zipWith (Data.Bits..|.) (unSized a) (unSized b)
  , QC.testProperty "xor" $ \a b ->
      toList ((fromSizedList a `asProxyTypeOf` proxy) `xor` fromSizedList b) === zipWith Data.Bits.xor (unSized a) (unSized b)
  , QC.testProperty "complement" $ \a ->
      toList (complement (fromSizedList a `asProxyTypeOf` proxy)) === map Data.Bits.complement (unSized a)
  , QC.testProperty "shiftL" $ \(QC.NonNegative i) a ->
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

properties :: forall x
            . ( SIMD x
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
properties _ =
  [ let proxy :: Proxy (x Bool)
        proxy = Proxy
    in testGroup "Bool"
      [ testPackUnpack proxy
      , testBroadcast proxy
      , testSelect proxy
      -- , testEq proxy
      -- , testBits proxy
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
      , testBits proxy
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
      , testBits proxy
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
      , testBits proxy
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
      , testBits proxy
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
      , testBits proxy
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
      , testBits proxy
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
      , testBits proxy
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
      , testBits proxy
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
      ]
  ]

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "X2" (properties (Proxy @X2))
  , testGroup "X4" (properties (Proxy @X4))
  , testGroup "X8" (properties (Proxy @X8))
  , testGroup "X16" (properties (Proxy @X16))
  , testGroup "X32" (properties (Proxy @X32))
  ]

main :: IO ()
main = defaultMain tests
