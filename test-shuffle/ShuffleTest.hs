{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Data.Simdy.Shuffle.Plugin #-}
module Main (main) where
import           Data.Int
import           Data.Kind
import           Data.Proxy
import           Data.Simdy (KnownSIMDLength (..), SIMDElement, X16, X2, X32,
                             X4, X64, X8)
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
          , SIMDElement a
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
          , SIMDElement a
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

testX8 :: ( ListLike X8 a
          , QC.Arbitrary a
          , SameValue a
          , Show a
          , SIMDElement a
          ) => Proxy a -> TestTree
testX8 proxy = testGroup "X8"
  [ testGroup "unary"
    [ QC.testProperty "(0,0,0,0,0,0,0,0)" $ \a ->
        toList (unaryShuffleWithX8 (\(x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7) -> (x0, x0, x0, x0, x0, x0, x0, x0)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 0, 0, 0, 0, 0, 0, 0]
    , QC.testProperty "(0,1,2,3,4,5,6,7)" $ \a ->
        toList (unaryShuffleWithX8 (\(x0, x1, x2, x3, x4, x5, x6, x7) -> (x0, x1, x2, x3, x4, x5, x6, x7)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 1, 2, 3, 4, 5, 6, 7]
    ]
  , testGroup "binary"
    [ QC.testProperty "(0,0,0,0,0,0,0,0)" $ \a b ->
        toList (binaryShuffleWithX8 (\(x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7) (_x8, _x9, _x10, _x11, _x12, _x13, _x14, _x15) -> (x0, x0, x0, x0, x0, x0, x0, x0)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 0, 0, 0, 0, 0, 0, 0]
    , QC.testProperty "(0,1,2,3,4,5,6,7)" $ \a b ->
        toList (binaryShuffleWithX8 (\(x0, x1, x2, x3, x4, x5, x6, x7) (_x8, _x9, _x10, _x11, _x12, _x13, _x14, _x15) -> (x0, x1, x2, x3, x4, x5, x6, x7)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 1, 2, 3, 4, 5, 6, 7]
    , QC.testProperty "(0,2,4,6,8,10,12,14)" $ \a b ->
        toList (binaryShuffleWithX8 (\(x0, _x1, x2, _x3, x4, _x5, x6, _x7) (x8, _x9, x10, _x11, x12, _x13, x14, _x15) -> (x0, x2, x4, x6, x8, x10, x12, x14)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 2, 4, 6, 8, 10, 12, 14]
    , QC.testProperty "(1,3,5,7,9,11,13,15)" $ \a b ->
        toList (binaryShuffleWithX8 (\(_x0, x1, _x2, x3, _x4, x5, _x6, x7) (_x8, x9, _x10, x11, _x12, x13, _x14, x15) -> (x1, x3, x5, x7, x9, x11, x13, x15)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [1, 3, 5, 7, 9, 11, 13, 15]
    ]
  ]

testX16 :: ( ListLike X16 a
           , QC.Arbitrary a
           , SameValue a
           , Show a
           , SIMDElement a
           ) => Proxy a -> TestTree
testX16 proxy = testGroup "X16"
  [ testGroup "unary"
    [ QC.testProperty "(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)" $ \a ->
        toList (unaryShuffleWithX16 (\(x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7, _x8, _x9, _x10, _x11, _x12, _x13, _x14, _x15) -> (x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , QC.testProperty "(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)" $ \a ->
        toList (unaryShuffleWithX16 (\(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
    ]
  , testGroup "binary"
    [ QC.testProperty "(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)" $ \a b ->
        toList (binaryShuffleWithX16 (\(x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7, _x8, _x9, _x10, _x11, _x12, _x13, _x14, _x15) (_x16, _x17, _x18, _x19, _x20, _x21, _x22, _x23, _x24, _x25, _x26, _x27, _x28, _x29, _x30, _x31) -> (x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , QC.testProperty "(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)" $ \a b ->
        toList (binaryShuffleWithX16 (\(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) (_x16, _x17, _x18, _x19, _x20, _x21, _x22, _x23, _x24, _x25, _x26, _x27, _x28, _x29, _x30, _x31) -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
    , QC.testProperty "(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30)" $ \a b ->
        toList (binaryShuffleWithX16 (\(x0, _x1, x2, _x3, x4, _x5, x6, _x7, x8, _x9, x10, _x11, x12, _x13, x14, _x15) (x16, _x17, x18, _x19, x20, _x21, x22, _x23, x24, _x25, x26, _x27, x28, _x29, x30, _x31) -> (x0, x2, x4, x6, x8, x10, x12, x14, x16, x18, x20, x22, x24, x26, x28, x30)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30]
    , QC.testProperty "(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31)" $ \a b ->
        toList (binaryShuffleWithX16 (\(_x0, x1, _x2, x3, _x4, x5, _x6, x7, _x8, x9, _x10, x11, _x12, x13, _x14, x15) (_x16, x17, _x18, x19, _x20, x21, _x22, x23, _x24, x25, _x26, x27, _x28, x29, _x30, x31) -> (x1, x3, x5, x7, x9, x11, x13, x15, x17, x19, x21, x23, x25, x27, x29, x31)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31]
    ]
  ]

testX32 :: ( ListLike X32 a
           , QC.Arbitrary a
           , SameValue a
           , Show a
           , SIMDElement a
           ) => Proxy a -> TestTree
testX32 proxy = testGroup "X32"
  [ testGroup "unary"
    [ QC.testProperty "(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)" $ \a ->
        toList (unaryShuffleWithX32 (\(x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7, _x8, _x9, _x10, _x11, _x12, _x13, _x14, _x15, _x16, _x17, _x18, _x19, _x20, _x21, _x22, _x23, _x24, _x25, _x26, _x27, _x28, _x29, _x30, _x31) -> (x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , QC.testProperty "(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)" $ \a ->
        toList (unaryShuffleWithX32 (\(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]
    ]
  , testGroup "binary"
    [ QC.testProperty "(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)" $ \a b ->
        toList (binaryShuffleWithX32 (\(x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7, _x8, _x9, _x10, _x11, _x12, _x13, _x14, _x15, _x16, _x17, _x18, _x19, _x20, _x21, _x22, _x23, _x24, _x25, _x26, _x27, _x28, _x29, _x30, _x31) (_x32, _x33, _x34, _x35, _x36, _x37, _x38, _x39, _x40, _x41, _x42, _x43, _x44, _x45, _x46, _x47, _x48, _x49, _x50, _x51, _x52, _x53, _x54, _x55, _x56, _x57, _x58, _x59, _x60, _x61, _x62, _x63) -> (x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , QC.testProperty "(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31)" $ \a b ->
        toList (binaryShuffleWithX32 (\(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) (_x32, _x33, _x34, _x35, _x36, _x37, _x38, _x39, _x40, _x41, _x42, _x43, _x44, _x45, _x46, _x47, _x48, _x49, _x50, _x51, _x52, _x53, _x54, _x55, _x56, _x57, _x58, _x59, _x60, _x61, _x62, _x63) -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]
    , QC.testProperty "(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62)" $ \a b ->
        toList (binaryShuffleWithX32 (\(x0, _x1, x2, _x3, x4, _x5, x6, _x7, x8, _x9, x10, _x11, x12, _x13, x14, _x15, x16, _x17, x18, _x19, x20, _x21, x22, _x23, x24, _x25, x26, _x27, x28, _x29, x30, _x31) (x32, _x33, x34, _x35, x36, _x37, x38, _x39, x40, _x41, x42, _x43, x44, _x45, x46, _x47, x48, _x49, x50, _x51, x52, _x53, x54, _x55, x56, _x57, x58, _x59, x60, _x61, x62, _x63) -> (x0, x2, x4, x6, x8, x10, x12, x14, x16, x18, x20, x22, x24, x26, x28, x30, x32, x34, x36, x38, x40, x42, x44, x46, x48, x50, x52, x54, x56, x58, x60, x62)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62]
    , QC.testProperty "(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63)" $ \a b ->
        toList (binaryShuffleWithX32 (\(_x0, x1, _x2, x3, _x4, x5, _x6, x7, _x8, x9, _x10, x11, _x12, x13, _x14, x15, _x16, x17, _x18, x19, _x20, x21, _x22, x23, _x24, x25, _x26, x27, _x28, x29, _x30, x31) (_x32, x33, _x34, x35, _x36, x37, _x38, x39, _x40, x41, _x42, x43, _x44, x45, _x46, x47, _x48, x49, _x50, x51, _x52, x53, _x54, x55, _x56, x57, _x58, x59, _x60, x61, _x62, x63) -> (x1, x3, x5, x7, x9, x11, x13, x15, x17, x19, x21, x23, x25, x27, x29, x31, x33, x35, x37, x39, x41, x43, x45, x47, x49, x51, x53, x55, x57, x59, x61, x63)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63]
    ]
  ]

testX64 :: ( ListLike X64 a
           , QC.Arbitrary a
           , SameValue a
           , Show a
           , SIMDElement a
           ) => Proxy a -> TestTree
testX64 proxy = testGroup "X64"
  [ testGroup "unary"
    [ QC.testProperty "(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)" $ \a ->
        toList (unaryShuffleWithX64 (\(x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7, _x8, _x9, _x10, _x11, _x12, _x13, _x14, _x15, _x16, _x17, _x18, _x19, _x20, _x21, _x22, _x23, _x24, _x25, _x26, _x27, _x28, _x29, _x30, _x31, _x32, _x33, _x34, _x35, _x36, _x37, _x38, _x39, _x40, _x41, _x42, _x43, _x44, _x45, _x46, _x47, _x48, _x49, _x50, _x51, _x52, _x53, _x54, _x55, _x56, _x57, _x58, _x59, _x60, _x61, _x62, _x63) -> (x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , QC.testProperty "(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63)" $ \a ->
        toList (unaryShuffleWithX64 (\(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63)) (fromSizedList a) `elemTypeAs` proxy) === map (unSized a !!) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63]
    ]
  , testGroup "binary"
    [ QC.testProperty "(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)" $ \a b ->
        toList (binaryShuffleWithX64 (\(x0, _x1, _x2, _x3, _x4, _x5, _x6, _x7, _x8, _x9, _x10, _x11, _x12, _x13, _x14, _x15, _x16, _x17, _x18, _x19, _x20, _x21, _x22, _x23, _x24, _x25, _x26, _x27, _x28, _x29, _x30, _x31, _x32, _x33, _x34, _x35, _x36, _x37, _x38, _x39, _x40, _x41, _x42, _x43, _x44, _x45, _x46, _x47, _x48, _x49, _x50, _x51, _x52, _x53, _x54, _x55, _x56, _x57, _x58, _x59, _x60, _x61, _x62, _x63) (_x64, _x65, _x66, _x67, _x68, _x69, _x70, _x71, _x72, _x73, _x74, _x75, _x76, _x77, _x78, _x79, _x80, _x81, _x82, _x83, _x84, _x85, _x86, _x87, _x88, _x89, _x90, _x91, _x92, _x93, _x94, _x95, _x96, _x97, _x98, _x99, _x100, _x101, _x102, _x103, _x104, _x105, _x106, _x107, _x108, _x109, _x110, _x111, _x112, _x113, _x114, _x115, _x116, _x117, _x118, _x119, _x120, _x121, _x122, _x123, _x124, _x125, _x126, _x127) -> (x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0, x0)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    , QC.testProperty "(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63)" $ \a b ->
        toList (binaryShuffleWithX64 (\(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) (_x64, _x65, _x66, _x67, _x68, _x69, _x70, _x71, _x72, _x73, _x74, _x75, _x76, _x77, _x78, _x79, _x80, _x81, _x82, _x83, _x84, _x85, _x86, _x87, _x88, _x89, _x90, _x91, _x92, _x93, _x94, _x95, _x96, _x97, _x98, _x99, _x100, _x101, _x102, _x103, _x104, _x105, _x106, _x107, _x108, _x109, _x110, _x111, _x112, _x113, _x114, _x115, _x116, _x117, _x118, _x119, _x120, _x121, _x122, _x123, _x124, _x125, _x126, _x127) -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63]
    , QC.testProperty "(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,104,106,108,110,112,114,116,118,120,122,124,126)" $ \a b ->
        toList (binaryShuffleWithX64 (\(x0, _x1, x2, _x3, x4, _x5, x6, _x7, x8, _x9, x10, _x11, x12, _x13, x14, _x15, x16, _x17, x18, _x19, x20, _x21, x22, _x23, x24, _x25, x26, _x27, x28, _x29, x30, _x31, x32, _x33, x34, _x35, x36, _x37, x38, _x39, x40, _x41, x42, _x43, x44, _x45, x46, _x47, x48, _x49, x50, _x51, x52, _x53, x54, _x55, x56, _x57, x58, _x59, x60, _x61, x62, _x63) (x64, _x65, x66, _x67, x68, _x69, x70, _x71, x72, _x73, x74, _x75, x76, _x77, x78, _x79, x80, _x81, x82, _x83, x84, _x85, x86, _x87, x88, _x89, x90, _x91, x92, _x93, x94, _x95, x96, _x97, x98, _x99, x100, _x101, x102, _x103, x104, _x105, x106, _x107, x108, _x109, x110, _x111, x112, _x113, x114, _x115, x116, _x117, x118, _x119, x120, _x121, x122, _x123, x124, _x125, x126, _x127) -> (x0, x2, x4, x6, x8, x10, x12, x14, x16, x18, x20, x22, x24, x26, x28, x30, x32, x34, x36, x38, x40, x42, x44, x46, x48, x50, x52, x54, x56, x58, x60, x62, x64, x66, x68, x70, x72, x74, x76, x78, x80, x82, x84, x86, x88, x90, x92, x94, x96, x98, x100, x102, x104, x106, x108, x110, x112, x114, x116, x118, x120, x122, x124, x126)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100, 102, 104, 106, 108, 110, 112, 114, 116, 118, 120, 122, 124, 126]
    , QC.testProperty "(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125,127)" $ \a b ->
        toList (binaryShuffleWithX64 (\(_x0, x1, _x2, x3, _x4, x5, _x6, x7, _x8, x9, _x10, x11, _x12, x13, _x14, x15, _x16, x17, _x18, x19, _x20, x21, _x22, x23, _x24, x25, _x26, x27, _x28, x29, _x30, x31, _x32, x33, _x34, x35, _x36, x37, _x38, x39, _x40, x41, _x42, x43, _x44, x45, _x46, x47, _x48, x49, _x50, x51, _x52, x53, _x54, x55, _x56, x57, _x58, x59, _x60, x61, _x62, x63) (_x64, x65, _x66, x67, _x68, x69, _x70, x71, _x72, x73, _x74, x75, _x76, x77, _x78, x79, _x80, x81, _x82, x83, _x84, x85, _x86, x87, _x88, x89, _x90, x91, _x92, x93, _x94, x95, _x96, x97, _x98, x99, _x100, x101, _x102, x103, _x104, x105, _x106, x107, _x108, x109, _x110, x111, _x112, x113, _x114, x115, _x116, x117, _x118, x119, _x120, x121, _x122, x123, _x124, x125, _x126, x127) -> (x1, x3, x5, x7, x9, x11, x13, x15, x17, x19, x21, x23, x25, x27, x29, x31, x33, x35, x37, x39, x41, x43, x45, x47, x49, x51, x53, x55, x57, x59, x61, x63, x65, x67, x69, x71, x73, x75, x77, x79, x81, x83, x85, x87, x89, x91, x93, x95, x97, x99, x101, x103, x105, x107, x109, x111, x113, x115, x117, x119, x121, x123, x125, x127)) (fromSizedList a) (fromSizedList b) `elemTypeAs` proxy) === map ((unSized a ++ unSized b) !!) [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75, 77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99, 101, 103, 105, 107, 109, 111, 113, 115, 117, 119, 121, 123, 125, 127]
    ]
  ]

tests :: TestTree
tests = testGroup "Tests"
  [ let proxy :: Proxy Int8
        proxy = Proxy
    in testGroup "Int8"
      [ testX2 proxy
      , testX4 proxy
      , testX8 proxy
      , testX16 proxy
      , testX32 proxy
      , testX64 proxy
      ]
  , let proxy :: Proxy Int16
        proxy = Proxy
    in testGroup "Int16"
      [ testX2 proxy
      , testX4 proxy
      , testX8 proxy
      , testX16 proxy
      , testX32 proxy
      , testX64 proxy
      ]
  , let proxy :: Proxy Int32
        proxy = Proxy
    in testGroup "Int32"
      [ testX2 proxy
      , testX4 proxy
      , testX8 proxy
      , testX16 proxy
      , testX32 proxy
      , testX64 proxy
      ]
  , let proxy :: Proxy Int64
        proxy = Proxy
    in testGroup "Int64"
      [ testX2 proxy
      , testX4 proxy
      , testX8 proxy
      , testX16 proxy
      , testX32 proxy
      , testX64 proxy
      ]
  , let proxy :: Proxy Word8
        proxy = Proxy
    in testGroup "Word8"
      [ testX2 proxy
      , testX4 proxy
      , testX8 proxy
      , testX16 proxy
      , testX32 proxy
      , testX64 proxy
      ]
  , let proxy :: Proxy Word16
        proxy = Proxy
    in testGroup "Word16"
      [ testX2 proxy
      , testX4 proxy
      , testX8 proxy
      , testX16 proxy
      , testX32 proxy
      , testX64 proxy
      ]
  , let proxy :: Proxy Word32
        proxy = Proxy
    in testGroup "Word32"
      [ testX2 proxy
      , testX4 proxy
      , testX8 proxy
      , testX16 proxy
      , testX32 proxy
      , testX64 proxy
      ]
  , let proxy :: Proxy Word64
        proxy = Proxy
    in testGroup "Word64"
      [ testX2 proxy
      , testX4 proxy
      , testX8 proxy
      , testX16 proxy
      , testX32 proxy
      , testX64 proxy
      ]
  , let proxy :: Proxy Float
        proxy = Proxy
    in testGroup "Float"
      [ testX2 proxy
      , testX4 proxy
      , testX8 proxy
      , testX16 proxy
      , testX32 proxy
      , testX64 proxy
      ]
  , let proxy :: Proxy Double
        proxy = Proxy
    in testGroup "Double"
      [ testX2 proxy
      , testX4 proxy
      , testX8 proxy
      , testX16 proxy
      , testX32 proxy
      , testX64 proxy
      ]
  ]

main :: IO ()
main = defaultMain tests
