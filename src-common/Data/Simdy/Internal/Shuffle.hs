{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
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
import           Data.Functor.Identity (Identity)
import           Data.Kind (Constraint, Type)
import           Data.Type.Ord (type (<))
import           GHC.Exts (TYPE)
import           GHC.TypeNats (Natural)

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
type UnaryShuffleT :: [Natural] -> (Type -> Type) -> Type -> Constraint
class UnaryShuffleT indices x a where
  unaryShuffle :: x a -> x a

-- | Select lanes from two concatenated vectors according to compile-time @indices@.
-- Indices @0..n-1@ select from the first vector, @n..2n-1@ from the second.
type BinaryShuffleT :: [Natural] -> (Type -> Type) -> Type -> Constraint
class BinaryShuffleT indices x a where
  binaryShuffle :: x a -> x a -> x a

-- | Valid index lists for a binary shuffle on the one-lane vector type 'Identity':
-- @'[0]@ selects the first operand and @'[1]@ the second.
type SelectOne :: [Natural] -> Constraint
class SelectOne indices where
  selectOne :: a -> a -> a

instance SelectOne '[0] where
  selectOne x _ = x
  {-# INLINE selectOne #-}

instance SelectOne '[1] where
  selectOne _ y = y
  {-# INLINE selectOne #-}

-- | The vector-type-specific part of the @UnaryShuffle@ constraint synonym
-- generated for each backend.
--
-- 'Identity' is a one-lane vector, so the only valid index list is @'[0]@;
-- pinning it down here is what lets the single @UnaryShuffleT indices Identity a@
-- instance below satisfy the quantified superclass of @SIMD Identity@.
-- For the proper vector types @X2@ .. @X64@ no extra constraint is needed.
type UnaryShuffleFor :: (Type -> Type) -> [Natural] -> Constraint
type family UnaryShuffleFor x indices where
  UnaryShuffleFor Identity indices = indices ~ '[0]
  UnaryShuffleFor _        _       = ()

-- | The vector-type-specific part of the @BinaryShuffle@ constraint synonym
-- generated for each backend. See 'UnaryShuffleFor'.
type BinaryShuffleFor :: (Type -> Type) -> [Natural] -> Constraint
type family BinaryShuffleFor x indices where
  BinaryShuffleFor Identity indices = SelectOne indices
  BinaryShuffleFor _        _       = ()

instance indices ~ '[0] => UnaryShuffleT indices Identity a where
  unaryShuffle v = v
  {-# INLINE unaryShuffle #-}

instance SelectOne indices => BinaryShuffleT indices Identity a where
  binaryShuffle = selectOne @indices
  {-# INLINE binaryShuffle #-}

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
