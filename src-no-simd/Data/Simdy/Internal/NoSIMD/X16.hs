-- This file was created by script/Gen.hs. Do not edit by hand!
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Simdy.Internal.NoSIMD.X16 where
import           Data.Bits
import           Data.Coerce (coerce)
import           Data.Complex
import           Data.Monoid
import           Data.Primitive (Prim)
import           Data.Semigroup
import           Data.Simdy.Internal.Bits (Boolean, BitShift)
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Shuffle
import           Data.Type.Ord (type (<))
import           Data.Simdy.Internal.NoSIMD.X8
import           Foreign.Storable (Storable)
import qualified GHC.Exts
import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..))
import           GHC.Int
import           GHC.IO
import           GHC.Word
import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=), min, max)
import qualified Prelude
-- | @'X16' a@ is a fixed-length vector of length 16.
--
-- Conceptually, @data 'X16' a = MkX16 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a@.
--
-- You can access the elements by 'mkX16', 'packX16' and 'unpackX16'.
data X16 a = MkX16WithX8 !(X8 a) !(X8 a)
instance ImplementationDescription X16 where
  implementationDescription _ = "X16;maxBits=0"
instance KnownSIMDLength X16 where
  type SIMDLength X16 = 16
  simdLength = 16
  {-# INLINE simdLength #-}
type instance Mask (X16 a) = X16 Bool
instance MaskIsLiftedBool X16 a
deriving via WrappedMulti X16 a instance EquatableF X8 a => Equatable (X16 a)
deriving via WrappedMulti X16 a instance OrderedF X8 a => Ordered (X16 a)
deriving via WrappedMulti X16 a instance SelectableF X8 a => Selectable (X16 a)
deriving via WrappedMulti X16 a instance (Num a, NumF X8 a, Broadcast X8 a, PackX8 X8 a) => Num (X16 a)
deriving via WrappedMulti X16 a instance (Fractional a, FractionalF X8 a, Broadcast X8 a, PackX8 X8 a) => Fractional (X16 a)
deriving via WrappedMulti X16 a instance (Floating a, FloatingF X8 a, Broadcast X8 a, PackX8 X8 a) => Floating (X16 a)
deriving via WrappedMulti X16 a instance BooleanF X16 a => Boolean (X16 a)
deriving via WrappedMulti X16 a instance BitShiftF X16 a => BitShift (X16 a)
deriving via WrappedMulti X16 a instance MinMaxF X16 a => MinMax (X16 a)
deriving via WrappedMulti X16 a instance (Num a, FusedMultiplyAddF X8 a, Broadcast X8 a, PackX8 X8 a) => FusedMultiplyAdd (X16 a)
instance PackX16 X16 a => IsList (X16 a) where
  type Item (X16 a) = a
  toList = toListX16
  fromList = fromListX16
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX8 X8 a => PackX16 X16 a where
  mkX16 !x0 !x1 !x2 !x3 !x4 !x5 !x6 !x7 !x8 !x9 !x10 !x11 !x12 !x13 !x14 !x15 = MkX16WithX8 (mkX8 x0 x1 x2 x3 x4 x5 x6 x7) (mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  unpackX16 (MkX16WithX8 u0 u1) = case unpackX8 u0 of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 u1 of (x8, x9, x10, x11, x12, x13, x14, x15) -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance (PackX8 X8 a, PackX8 X8 b) => LiftSIMD X16 a b where
  liftSIMD f !v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9) (f x10) (f x11) (f x12) (f x13) (f x14) (f x15)
  {-# INLINE liftSIMD #-}
instance (PackX16 X16 a, PackX16 X16 b, PackX16 X16 c) => LiftSIMD2 X16 a b c where
  liftSIMD2 f !u !v = case unpackX16 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> case unpackX16 v of (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15) -> mkX16 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4) (f x5 y5) (f x6 y6) (f x7 y7) (f x8 y8) (f x9 y9) (f x10 y10) (f x11 y11) (f x12 y12) (f x13 y13) (f x14 y14) (f x15 y15)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X8 => LiftConstructor X16 where
  mkTuple2 (MkX16WithX8 u0_0 u0_1) (MkX16WithX8 u1_0 u1_1) = MkX16WithX8 (mkTuple2 u0_0 u1_0) (mkTuple2 u0_1 u1_1)
  mkTuple3 (MkX16WithX8 u0_0 u0_1) (MkX16WithX8 u1_0 u1_1) (MkX16WithX8 u2_0 u2_1) = MkX16WithX8 (mkTuple3 u0_0 u1_0 u2_0) (mkTuple3 u0_1 u1_1 u2_1)
  mkTuple4 (MkX16WithX8 u0_0 u0_1) (MkX16WithX8 u1_0 u1_1) (MkX16WithX8 u2_0 u2_1) (MkX16WithX8 u3_0 u3_1) = MkX16WithX8 (mkTuple4 u0_0 u1_0 u2_0 u3_0) (mkTuple4 u0_1 u1_1 u2_1 u3_1)
  mkTuple5 (MkX16WithX8 u0_0 u0_1) (MkX16WithX8 u1_0 u1_1) (MkX16WithX8 u2_0 u2_1) (MkX16WithX8 u3_0 u3_1) (MkX16WithX8 u4_0 u4_1) = MkX16WithX8 (mkTuple5 u0_0 u1_0 u2_0 u3_0 u4_0) (mkTuple5 u0_1 u1_1 u2_1 u3_1 u4_1)
  mkTuple6 (MkX16WithX8 u0_0 u0_1) (MkX16WithX8 u1_0 u1_1) (MkX16WithX8 u2_0 u2_1) (MkX16WithX8 u3_0 u3_1) (MkX16WithX8 u4_0 u4_1) (MkX16WithX8 u5_0 u5_1) = MkX16WithX8 (mkTuple6 u0_0 u1_0 u2_0 u3_0 u4_0 u5_0) (mkTuple6 u0_1 u1_1 u2_1 u3_1 u4_1 u5_1)
  deconstructTuple2 (MkX16WithX8 u0 u1) = case deconstructTuple2 u0 of (u0_0, u0_1) -> case deconstructTuple2 u1 of (u1_0, u1_1) -> (MkX16WithX8 u0_0 u1_0, MkX16WithX8 u0_1 u1_1)
  deconstructTuple3 (MkX16WithX8 u0 u1) = case deconstructTuple3 u0 of (u0_0, u0_1, u0_2) -> case deconstructTuple3 u1 of (u1_0, u1_1, u1_2) -> (MkX16WithX8 u0_0 u1_0, MkX16WithX8 u0_1 u1_1, MkX16WithX8 u0_2 u1_2)
  deconstructTuple4 (MkX16WithX8 u0 u1) = case deconstructTuple4 u0 of (u0_0, u0_1, u0_2, u0_3) -> case deconstructTuple4 u1 of (u1_0, u1_1, u1_2, u1_3) -> (MkX16WithX8 u0_0 u1_0, MkX16WithX8 u0_1 u1_1, MkX16WithX8 u0_2 u1_2, MkX16WithX8 u0_3 u1_3)
  deconstructTuple5 (MkX16WithX8 u0 u1) = case deconstructTuple5 u0 of (u0_0, u0_1, u0_2, u0_3, u0_4) -> case deconstructTuple5 u1 of (u1_0, u1_1, u1_2, u1_3, u1_4) -> (MkX16WithX8 u0_0 u1_0, MkX16WithX8 u0_1 u1_1, MkX16WithX8 u0_2 u1_2, MkX16WithX8 u0_3 u1_3, MkX16WithX8 u0_4 u1_4)
  deconstructTuple6 (MkX16WithX8 u0 u1) = case deconstructTuple6 u0 of (u0_0, u0_1, u0_2, u0_3, u0_4, u0_5) -> case deconstructTuple6 u1 of (u1_0, u1_1, u1_2, u1_3, u1_4, u1_5) -> (MkX16WithX8 u0_0 u1_0, MkX16WithX8 u0_1 u1_1, MkX16WithX8 u0_2 u1_2, MkX16WithX8 u0_3 u1_3, MkX16WithX8 u0_4 u1_4, MkX16WithX8 u0_5 u1_5)
  mkSum (MkX16WithX8 u0 u1) = MkX16WithX8 (mkSum u0) (mkSum u1)
  getSum' (MkX16WithX8 u0 u1) = MkX16WithX8 (getSum' u0) (getSum' u1)
  mkProduct (MkX16WithX8 u0 u1) = MkX16WithX8 (mkProduct u0) (mkProduct u1)
  getProduct' (MkX16WithX8 u0 u1) = MkX16WithX8 (getProduct' u0) (getProduct' u1)
  mkMin (MkX16WithX8 u0 u1) = MkX16WithX8 (mkMin u0) (mkMin u1)
  getMin' (MkX16WithX8 u0 u1) = MkX16WithX8 (getMin' u0) (getMin' u1)
  mkMax (MkX16WithX8 u0 u1) = MkX16WithX8 (mkMax u0) (mkMax u1)
  getMax' (MkX16WithX8 u0 u1) = MkX16WithX8 (getMax' u0) (getMax' u1)
  mkComplex (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (mkComplex u0 v0) (mkComplex u1 v1)
  deconstructComplex (MkX16WithX8 u0 u1) = case deconstructComplex u0 of (v0, w0) -> case deconstructComplex u1 of (v1, w1) -> (MkX16WithX8 v0 v1, MkX16WithX8 w0 w1)
  {-# INLINE mkTuple2 #-}
  {-# INLINE mkTuple3 #-}
  {-# INLINE mkTuple4 #-}
  {-# INLINE mkTuple5 #-}
  {-# INLINE mkTuple6 #-}
  {-# INLINE deconstructTuple2 #-}
  {-# INLINE deconstructTuple3 #-}
  {-# INLINE deconstructTuple4 #-}
  {-# INLINE deconstructTuple5 #-}
  {-# INLINE deconstructTuple6 #-}
  {-# INLINE mkSum #-}
  {-# INLINE getSum' #-}
  {-# INLINE mkProduct #-}
  {-# INLINE getProduct' #-}
  {-# INLINE mkMin #-}
  {-# INLINE getMin' #-}
  {-# INLINE mkMax #-}
  {-# INLINE getMax' #-}
  {-# INLINE mkComplex #-}
  {-# INLINE deconstructComplex #-}
instance Broadcast X8 a => Broadcast X16 a where
  broadcast !x = let !v = broadcast x in MkX16WithX8 v v
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X16 a where
  selectF (MkX16WithX8 cond0 cond1) (MkX16WithX8 x0 x1) (MkX16WithX8 y0 y1) = MkX16WithX8 (selectF cond0 x0 y0) (selectF cond1 x1 y1)
  {-# INLINE selectF #-}
instance BooleanReduction X16 where
  horizontalAndBool (MkX16WithX8 cond0 cond1) = horizontalAndBool cond0 Prelude.&& horizontalAndBool cond1
  horizontalOrBool (MkX16WithX8 cond0 cond1) = horizontalOrBool cond0 Prelude.|| horizontalOrBool cond1
  {-# INLINE horizontalAndBool #-}
  {-# INLINE horizontalOrBool #-}
instance UnaryShuffleT indices X16 Bool where
  unaryShuffle = error "not implemented yet"
  {-# NOINLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 Bool where
  binaryShuffle = error "not implemented yet"
  {-# NOINLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16) => UnaryShuffleT '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 () where
  unaryShuffle _ = MkX16WithX8 MkUnitX8 MkUnitX8
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32) => BinaryShuffleT '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 () where
  binaryShuffle _ _ = MkX16WithX8 MkUnitX8 MkUnitX8
  {-# INLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Float i0, Pick Float i1, Pick Float i2, Pick Float i3, Pick Float i4, Pick Float i5, Pick Float i6, Pick Float i7, Pick Float i8, Pick Float i9, Pick Float i10, Pick Float i11, Pick Float i12, Pick Float i13, Pick Float i14, Pick Float i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Float where
  unaryShuffle (MkX16WithX8 (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkX16WithX8 (MkFloatX8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkFloatX8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Float i0, Pick Float i1, Pick Float i2, Pick Float i3, Pick Float i4, Pick Float i5, Pick Float i6, Pick Float i7, Pick Float i8, Pick Float i9, Pick Float i10, Pick Float i11, Pick Float i12, Pick Float i13, Pick Float i14, Pick Float i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Float where
  binaryShuffle (MkX16WithX8 (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) (MkX16WithX8 (MkFloatX8WithElems x16 x17 x18 x19 x20 x21 x22 x23) (MkFloatX8WithElems x24 x25 x26 x27 x28 x29 x30 x31)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkX16WithX8 (MkFloatX8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkFloatX8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Double i0, Pick Double i1, Pick Double i2, Pick Double i3, Pick Double i4, Pick Double i5, Pick Double i6, Pick Double i7, Pick Double i8, Pick Double i9, Pick Double i10, Pick Double i11, Pick Double i12, Pick Double i13, Pick Double i14, Pick Double i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Double where
  unaryShuffle (MkX16WithX8 (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkX16WithX8 (MkDoubleX8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkDoubleX8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Double i0, Pick Double i1, Pick Double i2, Pick Double i3, Pick Double i4, Pick Double i5, Pick Double i6, Pick Double i7, Pick Double i8, Pick Double i9, Pick Double i10, Pick Double i11, Pick Double i12, Pick Double i13, Pick Double i14, Pick Double i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Double where
  binaryShuffle (MkX16WithX8 (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) (MkX16WithX8 (MkDoubleX8WithElems x16 x17 x18 x19 x20 x21 x22 x23) (MkDoubleX8WithElems x24 x25 x26 x27 x28 x29 x30 x31)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkX16WithX8 (MkDoubleX8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkDoubleX8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Int8 i0, Pick Int8 i1, Pick Int8 i2, Pick Int8 i3, Pick Int8 i4, Pick Int8 i5, Pick Int8 i6, Pick Int8 i7, Pick Int8 i8, Pick Int8 i9, Pick Int8 i10, Pick Int8 i11, Pick Int8 i12, Pick Int8 i13, Pick Int8 i14, Pick Int8 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int8 where
  unaryShuffle (MkX16WithX8 (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkX16WithX8 (MkInt8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkInt8X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Int8 i0, Pick Int8 i1, Pick Int8 i2, Pick Int8 i3, Pick Int8 i4, Pick Int8 i5, Pick Int8 i6, Pick Int8 i7, Pick Int8 i8, Pick Int8 i9, Pick Int8 i10, Pick Int8 i11, Pick Int8 i12, Pick Int8 i13, Pick Int8 i14, Pick Int8 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int8 where
  binaryShuffle (MkX16WithX8 (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) (MkX16WithX8 (MkInt8X8WithElems x16 x17 x18 x19 x20 x21 x22 x23) (MkInt8X8WithElems x24 x25 x26 x27 x28 x29 x30 x31)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkX16WithX8 (MkInt8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkInt8X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Int16 i0, Pick Int16 i1, Pick Int16 i2, Pick Int16 i3, Pick Int16 i4, Pick Int16 i5, Pick Int16 i6, Pick Int16 i7, Pick Int16 i8, Pick Int16 i9, Pick Int16 i10, Pick Int16 i11, Pick Int16 i12, Pick Int16 i13, Pick Int16 i14, Pick Int16 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int16 where
  unaryShuffle (MkX16WithX8 (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkX16WithX8 (MkInt16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkInt16X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Int16 i0, Pick Int16 i1, Pick Int16 i2, Pick Int16 i3, Pick Int16 i4, Pick Int16 i5, Pick Int16 i6, Pick Int16 i7, Pick Int16 i8, Pick Int16 i9, Pick Int16 i10, Pick Int16 i11, Pick Int16 i12, Pick Int16 i13, Pick Int16 i14, Pick Int16 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int16 where
  binaryShuffle (MkX16WithX8 (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) (MkX16WithX8 (MkInt16X8WithElems x16 x17 x18 x19 x20 x21 x22 x23) (MkInt16X8WithElems x24 x25 x26 x27 x28 x29 x30 x31)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkX16WithX8 (MkInt16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkInt16X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Int32 i0, Pick Int32 i1, Pick Int32 i2, Pick Int32 i3, Pick Int32 i4, Pick Int32 i5, Pick Int32 i6, Pick Int32 i7, Pick Int32 i8, Pick Int32 i9, Pick Int32 i10, Pick Int32 i11, Pick Int32 i12, Pick Int32 i13, Pick Int32 i14, Pick Int32 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int32 where
  unaryShuffle (MkX16WithX8 (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkX16WithX8 (MkInt32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkInt32X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Int32 i0, Pick Int32 i1, Pick Int32 i2, Pick Int32 i3, Pick Int32 i4, Pick Int32 i5, Pick Int32 i6, Pick Int32 i7, Pick Int32 i8, Pick Int32 i9, Pick Int32 i10, Pick Int32 i11, Pick Int32 i12, Pick Int32 i13, Pick Int32 i14, Pick Int32 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int32 where
  binaryShuffle (MkX16WithX8 (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) (MkX16WithX8 (MkInt32X8WithElems x16 x17 x18 x19 x20 x21 x22 x23) (MkInt32X8WithElems x24 x25 x26 x27 x28 x29 x30 x31)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkX16WithX8 (MkInt32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkInt32X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Int64 i0, Pick Int64 i1, Pick Int64 i2, Pick Int64 i3, Pick Int64 i4, Pick Int64 i5, Pick Int64 i6, Pick Int64 i7, Pick Int64 i8, Pick Int64 i9, Pick Int64 i10, Pick Int64 i11, Pick Int64 i12, Pick Int64 i13, Pick Int64 i14, Pick Int64 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int64 where
  unaryShuffle (MkX16WithX8 (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkX16WithX8 (MkInt64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkInt64X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Int64 i0, Pick Int64 i1, Pick Int64 i2, Pick Int64 i3, Pick Int64 i4, Pick Int64 i5, Pick Int64 i6, Pick Int64 i7, Pick Int64 i8, Pick Int64 i9, Pick Int64 i10, Pick Int64 i11, Pick Int64 i12, Pick Int64 i13, Pick Int64 i14, Pick Int64 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int64 where
  binaryShuffle (MkX16WithX8 (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) (MkX16WithX8 (MkInt64X8WithElems x16 x17 x18 x19 x20 x21 x22 x23) (MkInt64X8WithElems x24 x25 x26 x27 x28 x29 x30 x31)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkX16WithX8 (MkInt64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkInt64X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Word8 i0, Pick Word8 i1, Pick Word8 i2, Pick Word8 i3, Pick Word8 i4, Pick Word8 i5, Pick Word8 i6, Pick Word8 i7, Pick Word8 i8, Pick Word8 i9, Pick Word8 i10, Pick Word8 i11, Pick Word8 i12, Pick Word8 i13, Pick Word8 i14, Pick Word8 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word8 where
  unaryShuffle (MkX16WithX8 (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkX16WithX8 (MkWord8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkWord8X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Word8 i0, Pick Word8 i1, Pick Word8 i2, Pick Word8 i3, Pick Word8 i4, Pick Word8 i5, Pick Word8 i6, Pick Word8 i7, Pick Word8 i8, Pick Word8 i9, Pick Word8 i10, Pick Word8 i11, Pick Word8 i12, Pick Word8 i13, Pick Word8 i14, Pick Word8 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word8 where
  binaryShuffle (MkX16WithX8 (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) (MkX16WithX8 (MkWord8X8WithElems x16 x17 x18 x19 x20 x21 x22 x23) (MkWord8X8WithElems x24 x25 x26 x27 x28 x29 x30 x31)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkX16WithX8 (MkWord8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkWord8X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Word16 i0, Pick Word16 i1, Pick Word16 i2, Pick Word16 i3, Pick Word16 i4, Pick Word16 i5, Pick Word16 i6, Pick Word16 i7, Pick Word16 i8, Pick Word16 i9, Pick Word16 i10, Pick Word16 i11, Pick Word16 i12, Pick Word16 i13, Pick Word16 i14, Pick Word16 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word16 where
  unaryShuffle (MkX16WithX8 (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkX16WithX8 (MkWord16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkWord16X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Word16 i0, Pick Word16 i1, Pick Word16 i2, Pick Word16 i3, Pick Word16 i4, Pick Word16 i5, Pick Word16 i6, Pick Word16 i7, Pick Word16 i8, Pick Word16 i9, Pick Word16 i10, Pick Word16 i11, Pick Word16 i12, Pick Word16 i13, Pick Word16 i14, Pick Word16 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word16 where
  binaryShuffle (MkX16WithX8 (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) (MkX16WithX8 (MkWord16X8WithElems x16 x17 x18 x19 x20 x21 x22 x23) (MkWord16X8WithElems x24 x25 x26 x27 x28 x29 x30 x31)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkX16WithX8 (MkWord16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkWord16X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Word32 i0, Pick Word32 i1, Pick Word32 i2, Pick Word32 i3, Pick Word32 i4, Pick Word32 i5, Pick Word32 i6, Pick Word32 i7, Pick Word32 i8, Pick Word32 i9, Pick Word32 i10, Pick Word32 i11, Pick Word32 i12, Pick Word32 i13, Pick Word32 i14, Pick Word32 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word32 where
  unaryShuffle (MkX16WithX8 (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkX16WithX8 (MkWord32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkWord32X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Word32 i0, Pick Word32 i1, Pick Word32 i2, Pick Word32 i3, Pick Word32 i4, Pick Word32 i5, Pick Word32 i6, Pick Word32 i7, Pick Word32 i8, Pick Word32 i9, Pick Word32 i10, Pick Word32 i11, Pick Word32 i12, Pick Word32 i13, Pick Word32 i14, Pick Word32 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word32 where
  binaryShuffle (MkX16WithX8 (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) (MkX16WithX8 (MkWord32X8WithElems x16 x17 x18 x19 x20 x21 x22 x23) (MkWord32X8WithElems x24 x25 x26 x27 x28 x29 x30 x31)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkX16WithX8 (MkWord32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkWord32X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE binaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Word64 i0, Pick Word64 i1, Pick Word64 i2, Pick Word64 i3, Pick Word64 i4, Pick Word64 i5, Pick Word64 i6, Pick Word64 i7, Pick Word64 i8, Pick Word64 i9, Pick Word64 i10, Pick Word64 i11, Pick Word64 i12, Pick Word64 i13, Pick Word64 i14, Pick Word64 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word64 where
  unaryShuffle (MkX16WithX8 (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkX16WithX8 (MkWord64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkWord64X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Word64 i0, Pick Word64 i1, Pick Word64 i2, Pick Word64 i3, Pick Word64 i4, Pick Word64 i5, Pick Word64 i6, Pick Word64 i7, Pick Word64 i8, Pick Word64 i9, Pick Word64 i10, Pick Word64 i11, Pick Word64 i12, Pick Word64 i13, Pick Word64 i14, Pick Word64 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word64 where
  binaryShuffle (MkX16WithX8 (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)) (MkX16WithX8 (MkWord64X8WithElems x16 x17 x18 x19 x20 x21 x22 x23) (MkWord64X8WithElems x24 x25 x26 x27 x28 x29 x30 x31)) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkX16WithX8 (MkWord64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)) (MkWord64X8WithElems (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources))
  {-# INLINE binaryShuffle #-}
instance UnaryShuffleT indices X16 a => UnaryShuffleT indices X16 (Sum a) where
  unaryShuffle v = mkSum (unaryShuffle @indices (getSum' v))
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 a => BinaryShuffleT indices X16 (Sum a) where
  binaryShuffle u v = mkSum (binaryShuffle @indices (getSum' u) (getSum' v))
  {-# INLINE binaryShuffle #-}
instance UnaryShuffleT indices X16 a => UnaryShuffleT indices X16 (Product a) where
  unaryShuffle v = mkProduct (unaryShuffle @indices (getProduct' v))
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 a => BinaryShuffleT indices X16 (Product a) where
  binaryShuffle u v = mkProduct (binaryShuffle @indices (getProduct' u) (getProduct' v))
  {-# INLINE binaryShuffle #-}
instance UnaryShuffleT indices X16 a => UnaryShuffleT indices X16 (Min a) where
  unaryShuffle v = mkMin (unaryShuffle @indices (getMin' v))
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 a => BinaryShuffleT indices X16 (Min a) where
  binaryShuffle u v = mkMin (binaryShuffle @indices (getMin' u) (getMin' v))
  {-# INLINE binaryShuffle #-}
instance UnaryShuffleT indices X16 a => UnaryShuffleT indices X16 (Max a) where
  unaryShuffle v = mkMax (unaryShuffle @indices (getMax' v))
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 a => BinaryShuffleT indices X16 (Max a) where
  binaryShuffle u v = mkMax (binaryShuffle @indices (getMax' u) (getMax' v))
  {-# INLINE binaryShuffle #-}
instance UnaryShuffleT indices X16 a => UnaryShuffleT indices X16 (Complex a) where
  unaryShuffle v = case deconstructComplex v of (x0, x1) -> mkComplex (unaryShuffle @indices x0) (unaryShuffle @indices x1)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 a => BinaryShuffleT indices X16 (Complex a) where
  binaryShuffle u v = case deconstructComplex u of (x0, x1) -> case deconstructComplex v of (y0, y1) -> mkComplex (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1)
  {-# INLINE binaryShuffle #-}
instance (UnaryShuffleT indices X16 a0, UnaryShuffleT indices X16 a1) => UnaryShuffleT indices X16 (a0, a1) where
  unaryShuffle v = case deconstructTuple2 v of (x0, x1) -> mkTuple2 (unaryShuffle @indices x0) (unaryShuffle @indices x1)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X16 a0, BinaryShuffleT indices X16 a1) => BinaryShuffleT indices X16 (a0, a1) where
  binaryShuffle u v = case deconstructTuple2 u of (x0, x1) -> case deconstructTuple2 v of (y0, y1) -> mkTuple2 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1)
  {-# INLINE binaryShuffle #-}
instance (UnaryShuffleT indices X16 a0, UnaryShuffleT indices X16 a1, UnaryShuffleT indices X16 a2) => UnaryShuffleT indices X16 (a0, a1, a2) where
  unaryShuffle v = case deconstructTuple3 v of (x0, x1, x2) -> mkTuple3 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X16 a0, BinaryShuffleT indices X16 a1, BinaryShuffleT indices X16 a2) => BinaryShuffleT indices X16 (a0, a1, a2) where
  binaryShuffle u v = case deconstructTuple3 u of (x0, x1, x2) -> case deconstructTuple3 v of (y0, y1, y2) -> mkTuple3 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2)
  {-# INLINE binaryShuffle #-}
instance (UnaryShuffleT indices X16 a0, UnaryShuffleT indices X16 a1, UnaryShuffleT indices X16 a2, UnaryShuffleT indices X16 a3) => UnaryShuffleT indices X16 (a0, a1, a2, a3) where
  unaryShuffle v = case deconstructTuple4 v of (x0, x1, x2, x3) -> mkTuple4 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X16 a0, BinaryShuffleT indices X16 a1, BinaryShuffleT indices X16 a2, BinaryShuffleT indices X16 a3) => BinaryShuffleT indices X16 (a0, a1, a2, a3) where
  binaryShuffle u v = case deconstructTuple4 u of (x0, x1, x2, x3) -> case deconstructTuple4 v of (y0, y1, y2, y3) -> mkTuple4 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3)
  {-# INLINE binaryShuffle #-}
instance (UnaryShuffleT indices X16 a0, UnaryShuffleT indices X16 a1, UnaryShuffleT indices X16 a2, UnaryShuffleT indices X16 a3, UnaryShuffleT indices X16 a4) => UnaryShuffleT indices X16 (a0, a1, a2, a3, a4) where
  unaryShuffle v = case deconstructTuple5 v of (x0, x1, x2, x3, x4) -> mkTuple5 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X16 a0, BinaryShuffleT indices X16 a1, BinaryShuffleT indices X16 a2, BinaryShuffleT indices X16 a3, BinaryShuffleT indices X16 a4) => BinaryShuffleT indices X16 (a0, a1, a2, a3, a4) where
  binaryShuffle u v = case deconstructTuple5 u of (x0, x1, x2, x3, x4) -> case deconstructTuple5 v of (y0, y1, y2, y3, y4) -> mkTuple5 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4)
  {-# INLINE binaryShuffle #-}
instance (UnaryShuffleT indices X16 a0, UnaryShuffleT indices X16 a1, UnaryShuffleT indices X16 a2, UnaryShuffleT indices X16 a3, UnaryShuffleT indices X16 a4, UnaryShuffleT indices X16 a5) => UnaryShuffleT indices X16 (a0, a1, a2, a3, a4, a5) where
  unaryShuffle v = case deconstructTuple6 v of (x0, x1, x2, x3, x4, x5) -> mkTuple6 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4) (unaryShuffle @indices x5)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X16 a0, BinaryShuffleT indices X16 a1, BinaryShuffleT indices X16 a2, BinaryShuffleT indices X16 a3, BinaryShuffleT indices X16 a4, BinaryShuffleT indices X16 a5) => BinaryShuffleT indices X16 (a0, a1, a2, a3, a4, a5) where
  binaryShuffle u v = case deconstructTuple6 u of (x0, x1, x2, x3, x4, x5) -> case deconstructTuple6 v of (y0, y1, y2, y3, y4, y5) -> mkTuple6 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4) (binaryShuffle @indices x5 y5)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 a => EquatableF X16 a where
  eqF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (eqF u0 v0) (eqF u1 v1)
  {-# INLINE eqF #-}
instance OrderedF X8 a => OrderedF X16 a where
  ltF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (ltF u0 v0) (ltF u1 v1)
  leF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (leF u0 v0) (leF u1 v1)
  gtF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (gtF u0 v0) (gtF u1 v1)
  geF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (geF u0 v0) (geF u1 v1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 a => MinMaxF X16 a where
  minF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (minF u0 v0) (minF u1 v1)
  maxF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (maxF u0 v0) (maxF u1 v1)
  minimumNumberF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (minimumNumberF u0 v0) (minimumNumberF u1 v1)
  maximumNumberF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (maximumNumberF u0 v0) (maximumNumberF u1 v1)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance (Num a, NumF X8 a, Broadcast X8 a, PackX8 X8 a) => NumF X16 a where
  plusF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (plusF u0 v0) (plusF u1 v1)
  minusF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (minusF u0 v0) (minusF u1 v1)
  timesF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (timesF u0 v0) (timesF u1 v1)
  negateF (MkX16WithX8 u0 u1) = MkX16WithX8 (negateF u0) (negateF u1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance (Fractional a, FractionalF X8 a, Broadcast X8 a, PackX8 X8 a) => FractionalF X16 a where
  divideF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (divideF u0 v0) (divideF u1 v1)
  recipF (MkX16WithX8 u0 u1) = MkX16WithX8 (recipF u0) (recipF u1)
  {-# INLINE divideF #-}
  {-# INLINE recipF #-}
instance (Floating a, FloatingF X8 a, Broadcast X8 a, PackX8 X8 a) => FloatingF X16 a where
  sqrtF (MkX16WithX8 u0 u1) = MkX16WithX8 (sqrtF u0) (sqrtF u1)
  {-# INLINE sqrtF #-}
instance BooleanF X8 a => BooleanF X16 a where
  andF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (andF u0 v0) (andF u1 v1)
  orF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (orF u0 v0) (orF u1 v1)
  xorF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (xorF u0 v0) (xorF u1 v1)
  complementF (MkX16WithX8 u0 u1) = MkX16WithX8 (complementF u0) (complementF u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 a => BitShiftF X16 a where
  shiftLF (MkX16WithX8 u0 u1) !i = MkX16WithX8 (shiftLF u0 i) (shiftLF u1 i)
  unsafeShiftLF (MkX16WithX8 u0 u1) !i = MkX16WithX8 (unsafeShiftLF u0 i) (unsafeShiftLF u1 i)
  shiftRF (MkX16WithX8 u0 u1) !i = MkX16WithX8 (shiftRF u0 i) (shiftRF u1 i)
  unsafeShiftRF (MkX16WithX8 u0 u1) !i = MkX16WithX8 (unsafeShiftRF u0 i) (unsafeShiftRF u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance (Num a, FusedMultiplyAddF X8 a, Broadcast X8 a, PackX8 X8 a) => FusedMultiplyAddF X16 a where
  fusedMultiplyAddF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) (MkX16WithX8 w0 w1) = MkX16WithX8 (fusedMultiplyAddF u0 v0 w0) (fusedMultiplyAddF u1 v1 w1)
  {-# INLINE fusedMultiplyAddF #-}
instance (Num a, PackX8 X8 a) => EnumFromZero_ X16 a where
  enumFromZero = mkX16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  {-# INLINE enumFromZero #-}
instance (Prim a, MultiPrim X8 a) => MultiPrim X16 a where
  indexByteArraySIMD# ba i = MkX16WithX8 (indexByteArraySIMD# ba i) (indexByteArraySIMD# ba (i +# 8#))
  readByteArraySIMD# mba i s0 = case readByteArraySIMD# mba i s0 of (# s1, u0 #) -> case readByteArraySIMD# mba (i +# 8#) s1 of (# s2, u1 #) -> (# s2, MkX16WithX8 u0 u1 #)
  writeByteArraySIMD# mba i (MkX16WithX8 u0 u1) s0 = case writeByteArraySIMD# mba i u0 s0 of s1 -> writeByteArraySIMD# mba (i +# 1#) u1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance (Storable a, MultiStorable X8 a) => MultiStorable X16 a where
  peekElemOffSIMD !ptr !i = MkX16WithX8 <$> peekElemOffSIMD ptr i <*> peekElemOffSIMD ptr (i + 1)
  pokeElemOffSIMD !ptr !i (MkX16WithX8 u0 u1) = pokeElemOffSIMD ptr i u0 >> pokeElemOffSIMD ptr (i + 1) u1
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
