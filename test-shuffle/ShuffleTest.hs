{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Data.Simdy.Shuffle.Plugin #-}
module Main (main) where
import           Data.Int
import           Data.Kind
import           Data.Proxy
import           Data.Simdy (KnownSIMDLength (..), X16, X2, X32, X4, X64, X8)
import           Data.Simdy.Shuffle
import           Data.Simdy.Pack
import           Data.Word
import           GHC.Exts (IsList (Item, fromList, toList))
import           Prelude hiding (max, min, (/=), (<), (<=), (==), (>), (>=))
import qualified Prelude
import           Test.Tasty
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

-- simdLen :: forall x a. KnownSIMDLength x => Proxy (x a) -> Int
-- simdLen _ = simdLength @x

elemTypeAs :: x a -> Proxy a -> x a
elemTypeAs x _ = x

testX2 :: ( ListLike X2 a
          , QC.Arbitrary a
          , SameValue a
          , Show a
          , UnaryShuffle '[0, 0] X2 a
          , UnaryShuffle '[0, 1] X2 a
          , UnaryShuffle '[1, 0] X2 a
          , UnaryShuffle '[1, 1] X2 a
          , BinaryShuffle '[0, 0] X2 a
          , BinaryShuffle '[0, 1] X2 a
          , BinaryShuffle '[0, 2] X2 a
          , BinaryShuffle '[0, 3] X2 a
          , BinaryShuffle '[1, 0] X2 a
          , BinaryShuffle '[1, 1] X2 a
          , BinaryShuffle '[1, 2] X2 a
          , BinaryShuffle '[1, 3] X2 a
          , BinaryShuffle '[2, 0] X2 a
          , BinaryShuffle '[2, 1] X2 a
          , BinaryShuffle '[2, 2] X2 a
          , BinaryShuffle '[2, 3] X2 a
          , BinaryShuffle '[3, 0] X2 a
          , BinaryShuffle '[3, 1] X2 a
          , BinaryShuffle '[3, 2] X2 a
          , BinaryShuffle '[3, 3] X2 a
          ) => Proxy a -> TestTree
testX2 proxy = testGroup "X2"
  [ testGroup "unary"
    [ QC.testProperty "(0,0)" $ \a ->
        toList (unaryShuffleWithX2 (\(x0, _x1) -> (x0, x0)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 0]
    , QC.testProperty "(0,1)" $ \a ->
        toList (unaryShuffleWithX2 (\(x0, x1) -> (x0, x1)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 1]
    , QC.testProperty "(1,0)" $ \a ->
        toList (unaryShuffleWithX2 (\(x0, x1) -> (x1, x0)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [1, 0]
    , QC.testProperty "(1,1)" $ \a ->
        toList (unaryShuffleWithX2 (\(_x0, x1) -> (x1, x1)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [1, 1]
    ]
  , testGroup "binary"
    [ QC.testProperty "(0,0)" $ \a b ->
        toList (binaryShuffleWithX2 (\(x0, _x1) (_x2, _x3) -> (x0, x0)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 0]
    , QC.testProperty "(0,1)" $ \a b ->
        toList (binaryShuffleWithX2 (\(x0, x1) (_x2, _x3) -> (x0, x1)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 1]
    , QC.testProperty "(0,2)" $ \a b ->
        toList (binaryShuffleWithX2 (\(x0, _x1) (x2, _x3) -> (x0, x2)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 2]
    , QC.testProperty "(0,3)" $ \a b ->
        toList (binaryShuffleWithX2 (\(x0, _x1) (_x2, x3) -> (x0, x3)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 3]
    , QC.testProperty "(1,0)" $ \a b ->
        toList (binaryShuffleWithX2 (\(x0, x1) (_x2, _x3) -> (x1, x0)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [1, 0]
    , QC.testProperty "(1,1)" $ \a b ->
        toList (binaryShuffleWithX2 (\(_x0, x1) (_x2, _x3) -> (x1, x1)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [1, 1]
    , QC.testProperty "(1,2)" $ \a b ->
        toList (binaryShuffleWithX2 (\(_x0, x1) (x2, _x3) -> (x1, x2)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [1, 2]
    , QC.testProperty "(1,3)" $ \a b ->
        toList (binaryShuffleWithX2 (\(_x0, x1) (_x2, x3) -> (x1, x3)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [1, 3]
    , QC.testProperty "(2,0)" $ \a b ->
        toList (binaryShuffleWithX2 (\(x0, _x1) (x2, _x3) -> (x2, x0)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [2, 0]
    , QC.testProperty "(2,1)" $ \a b ->
        toList (binaryShuffleWithX2 (\(_x0, x1) (x2, _x3) -> (x2, x1)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [2, 1]
    , QC.testProperty "(2,2)" $ \a b ->
        toList (binaryShuffleWithX2 (\(_x0, _x1) (x2, _x3) -> (x2, x2)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [2, 2]
    , QC.testProperty "(2,3)" $ \a b ->
        toList (binaryShuffleWithX2 (\(_x0, _x1) (x2, x3) -> (x2, x3)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [2, 3]
    , QC.testProperty "(3,0)" $ \a b ->
        toList (binaryShuffleWithX2 (\(x0, _x1) (_x2, x3) -> (x3, x0)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [3, 0]
    , QC.testProperty "(3,1)" $ \a b ->
        toList (binaryShuffleWithX2 (\(_x0, x1) (_x2, x3) -> (x3, x1)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [3, 1]
    , QC.testProperty "(3,2)" $ \a b ->
        toList (binaryShuffleWithX2 (\(_x0, _x1) (x2, x3) -> (x3, x2)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [3, 2]
    , QC.testProperty "(3,3)" $ \a b ->
        toList (binaryShuffleWithX2 (\(_x0, _x1) (_x2, x3) -> (x3, x3)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [3, 3]
    ]
  ]

testX4 :: ( ListLike X4 a
          , QC.Arbitrary a
          , SameValue a
          , Show a
          , UnaryShuffle '[0, 0, 0, 0] X4 a
          , UnaryShuffle '[0, 1, 2, 3] X4 a
          , BinaryShuffle '[0, 0, 0, 0] X4 a
          , BinaryShuffle '[0, 1, 2, 3] X4 a
          , BinaryShuffle '[0, 2, 4, 6] X4 a
          , BinaryShuffle '[1, 3, 5, 7] X4 a
          ) => Proxy a -> TestTree
testX4 proxy = testGroup "X4"
  [ testGroup "unary"
    [ QC.testProperty "(0,0,0,0)" $ \a ->
        toList (unaryShuffleWithX4 (\(x0, _x1, _x2, _x3) -> (x0, x0, x0, x0)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 0, 0, 0]
    , QC.testProperty "(0,1,2,3)" $ \a ->
        toList (unaryShuffleWithX4 (\(x0, x1, x2, x3) -> (x0, x1, x2, x3)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 1, 2, 3]
    ]
  , testGroup "binary"
    [ QC.testProperty "(0,0,0,0)" $ \a b ->
        toList (binaryShuffleWithX4 (\(x0, _x1, _x2, _x3) (_x4, _x5, _x6, _x7) -> (x0, x0, x0, x0)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 0, 0, 0]
    , QC.testProperty "(0,1,2,3)" $ \a b ->
        toList (binaryShuffleWithX4 (\(x0, x1, x2, x3) (_x4, _x5, _x6, _x7) -> (x0, x1, x2, x3)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 1, 2, 3]
    , QC.testProperty "(0,2,4,6)" $ \a b ->
        toList (binaryShuffleWithX4 (\(x0, _x1, x2, _x3) (x4, _x5, x6, _x7) -> (x0, x2, x4, x6)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 2, 4, 6]
    , QC.testProperty "(1,3,5,7)" $ \a b ->
        toList (binaryShuffleWithX4 (\(_x0, x1, _x2, x3) (_x4, x5, _x6, x7) -> (x1, x3, x5, x7)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [1, 3, 5, 7]
    ]
  ]

tests :: TestTree
tests = testGroup "Tests"
  [ let proxy :: Proxy Int8
        proxy = Proxy
    in testGroup "Int8"
      [ testX2 proxy
      , testX4 proxy
      ]
  , let proxy :: Proxy Int16
        proxy = Proxy
    in testGroup "Int16"
      [ testX2 proxy
      , testX4 proxy
      ]
  , let proxy :: Proxy Int32
        proxy = Proxy
    in testGroup "Int32"
      [ testX2 proxy
      , testX4 proxy
      ]
  , let proxy :: Proxy Int64
        proxy = Proxy
    in testGroup "Int64"
      [ testX2 proxy
      , testX4 proxy
      ]
  , let proxy :: Proxy Word8
        proxy = Proxy
    in testGroup "Word8"
      [ testX2 proxy
      , testX4 proxy
      ]
  , let proxy :: Proxy Word16
        proxy = Proxy
    in testGroup "Word16"
      [ testX2 proxy
      , testX4 proxy
      ]
  , let proxy :: Proxy Word32
        proxy = Proxy
    in testGroup "Word32"
      [ testX2 proxy
      , testX4 proxy
      ]
  , let proxy :: Proxy Word64
        proxy = Proxy
    in testGroup "Word64"
      [ testX2 proxy
      , testX4 proxy
      ]
  , let proxy :: Proxy Float
        proxy = Proxy
    in testGroup "Float"
      [ testX2 proxy
      , testX4 proxy
      ]
  , let proxy :: Proxy Double
        proxy = Proxy
    in testGroup "Double"
      [ testX2 proxy
      , testX4 proxy
      ]
  ]

main :: IO ()
main = defaultMain tests
