{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Shuffle operations for SIMD vectors.
--
-- Shuffles rearrange the lanes of one or two vectors according to compile-time index lists.
-- The indices are specified as type-level lists of 'GHC.TypeNats.Natural's.
--
-- The 'ShuffleMany' class has no hand-written instances; they are solved by
-- the compiler plugin ("Data.Simdy.Shuffle.Plugin").
module Data.Simdy.Internal.Shuffle where
import GHC.TypeNats
import GHC.Exts (TYPE)
import Data.Type.Ord
import Data.Kind (Type, Constraint)

-- | Low-level shuffle primitive.
--
-- Instances are solved by the compiler plugin ("Data.Simdy.Shuffle.Plugin"),
-- not defined by hand. The plugin reads the type-level @indices@ and emits
-- the appropriate GHC shuffle primop or pack\/unpack fallback.
type ShuffleMany :: forall rep. TYPE rep -> [Natural] -> Constraint
class ShuffleMany v indices where
  shuffleMany# :: (Natural -> v) -> v

-- Technically this needs no compiler magic, but it is solved by the plugin
-- anyway, for consistency with 'ShuffleMany'.
type Pick :: Type -> Natural -> Constraint
class Pick t index where
  pick :: (Natural -> t) -> t

type AllLessThan :: [Natural] -> Natural -> Constraint
type family AllLessThan indices n where
  AllLessThan '[] _ = ()
  AllLessThan (x : xs) y = (x < y, AllLessThan xs y)

-- | Rearrange lanes of a single vector according to compile-time @indices@.
type UnaryShuffle :: [Natural] -> (Type -> Type) -> Type -> Constraint
class UnaryShuffle indices x a where
  unaryShuffle :: x a -> x a

-- | Select lanes from two concatenated vectors according to compile-time @indices@.
-- Indices @0..n-1@ select from the first vector, @n..2n-1@ from the second.
type BinaryShuffle :: [Natural] -> (Type -> Type) -> Type -> Constraint
class BinaryShuffle indices x a where
  binaryShuffle :: x a -> x a -> x a

type Tuple2ToList :: (k, k) -> [k]
type family Tuple2ToList t where
  Tuple2ToList '(x0, x1) = '[x0, x1]

type Tuple4ToList :: (k, k, k, k) -> [k]
type family Tuple4ToList t where
  Tuple4ToList '(x0, x1, x2, x3) = '[x0, x1, x2, x3]

type Tuple8ToList :: (k, k, k, k, k, k, k, k) -> [k]
type family Tuple8ToList t where
  Tuple8ToList '(x0, x1, x2, x3, x4, x5, x6, x7) = '[x0, x1, x2, x3, x4, x5, x6, x7]

type Tuple16ToList :: (k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k) -> [k]
type family Tuple16ToList t where
  Tuple16ToList '(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) = '[x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15]

type Tuple32ToList :: (k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k) -> [k]
type family Tuple32ToList t where
  Tuple32ToList '(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) = '[x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31]

type Tuple64ToList :: (k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k, k) -> [k]
type family Tuple64ToList t where
  Tuple64ToList '(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) = '[x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63]

{-
type Take :: Natural -> [k] -> [k]
type family Take n xs where
  Take 0 xs = '[]
  Take n (_ : xs) = Take (n - 1) xs

type Drop :: Natural -> [k] -> [k]
type family Drop n xs where
  Drop 0 xs = xs
  Drop n (_ : xs) = Drop (n - 1) xs

instance (AllLessThan indices 2, ShuffleMany DoubleX2# indices) => UnaryShuffle indices X2 Double where
  unaryShuffle (MkDoubleX2 v) = MkDoubleX2 (shuffleMany# @_ @_ @indices (\_ -> v))

instance (AllLessThan indices 4, ShuffleMany DoubleX2# indices) => BinaryShuffle indices X2 Double where
  binaryShuffle (MkDoubleX2 v0) (MkDoubleX2 v1) = MkDoubleX2 (shuffleMany# @_ @_ @indices (\case { 0 -> v0; _ -> v1 }))

unaryShuffleX2 :: X2 a -> forall t -> UnaryShuffle (Tuple2ToList t) X2 a => X2 a
unaryShuffleX2 v t = unaryShuffle @(Tuple2ToList t) v

unaryShuffleByX2 :: UnaryShuffle '[i0, i1] X2 a => ((Proxy 0, Proxy 1) -> (Proxy i0, Proxy i1)) -> X2 a -> X2 a
unaryShuffleByX2 _ = unaryShuffle @'[i0, i1]

shuffleX2 :: X2 a -> X2 a -> forall t -> BinaryShuffle (Tuple2ToList t) X2 a => X2 a
shuffleX2 u v t = binaryShuffle @(Tuple2ToList t) u v

unaryShuffleX4 :: X4 a -> forall t -> UnaryShuffle (Tuple4ToList t) X4 a => X4 a
unaryShuffleX4 v t = unaryShuffle @(Tuple4ToList t) v

shuffleX4 :: X4 a -> X4 a -> forall t -> BinaryShuffle (Tuple4ToList t) X4 a => X4 a
shuffleX4 u v t = binaryShuffle @(Tuple4ToList t) u v
-}
