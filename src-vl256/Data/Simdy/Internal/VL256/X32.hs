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
module Data.Simdy.Internal.VL256.X32 where
import           Data.Bits
import           Data.Coerce (coerce)
import           Data.Complex
import           Data.Monoid
import           Data.Semigroup
import qualified Data.Simdy.Fusible as F
import           Data.Simdy.Internal.Bits (Boolean, BitShift)
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Shuffle
import           Data.Type.Ord (type (<))
import           Data.Simdy.Internal.VL256.Prim
import           Data.Simdy.Internal.VL128.PrimExtra
import           Data.Simdy.Internal.VL256.PrimExtra
import qualified GHC.Exts
import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..))
import           GHC.Int
import           GHC.IO
import           GHC.Word
import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=), min, max)
-- | @'X32' a@ is a fixed-length vector of length 32.
--
-- Conceptually, @data 'X32' a = MkX32 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a@.
--
-- You can access the elements by 'mkX32', 'packX32' and 'unpackX32'.
data family X32 a
instance KnownSIMDLength X32 where
  type SIMDLength X32 = 32
  simdLength = 32
  {-# INLINE simdLength #-}
newtype instance X32 Bool = MkBoolX32 Word32
type instance Mask (X32 a) = X32 Bool
instance MaskIsLiftedBool X32 a
instance BooleanF X32 Bool where
  andF (MkBoolX32 x) (MkBoolX32 y) = MkBoolX32 (x .&. y)
  orF (MkBoolX32 x) (MkBoolX32 y) = MkBoolX32 (x .|. y)
  xorF (MkBoolX32 x) (MkBoolX32 y) = MkBoolX32 (xor x y)
  complementF (MkBoolX32 x) = MkBoolX32 (0xffffffff - x)
deriving via WrappedMulti X32 a instance EquatableF X32 a => Equatable (X32 a)
deriving via WrappedMulti X32 a instance OrderedF X32 a => Ordered (X32 a)
instance PackX32 X32 a => IsList (X32 a) where
  type Item (X32 a) = a
  toList = toListX32
  fromList = fromListX32
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX32 X32 Bool where
  mkX32 !x0 !x1 !x2 !x3 !x4 !x5 !x6 !x7 !x8 !x9 !x10 !x11 !x12 !x13 !x14 !x15 !x16 !x17 !x18 !x19 !x20 !x21 !x22 !x23 !x24 !x25 !x26 !x27 !x28 !x29 !x30 !x31 = MkBoolX32 ((if x0 then 0x1 else 0) .|. (if x1 then 0x2 else 0) .|. (if x2 then 0x4 else 0) .|. (if x3 then 0x8 else 0) .|. (if x4 then 0x10 else 0) .|. (if x5 then 0x20 else 0) .|. (if x6 then 0x40 else 0) .|. (if x7 then 0x80 else 0) .|. (if x8 then 0x100 else 0) .|. (if x9 then 0x200 else 0) .|. (if x10 then 0x400 else 0) .|. (if x11 then 0x800 else 0) .|. (if x12 then 0x1000 else 0) .|. (if x13 then 0x2000 else 0) .|. (if x14 then 0x4000 else 0) .|. (if x15 then 0x8000 else 0) .|. (if x16 then 0x10000 else 0) .|. (if x17 then 0x20000 else 0) .|. (if x18 then 0x40000 else 0) .|. (if x19 then 0x80000 else 0) .|. (if x20 then 0x100000 else 0) .|. (if x21 then 0x200000 else 0) .|. (if x22 then 0x400000 else 0) .|. (if x23 then 0x800000 else 0) .|. (if x24 then 0x1000000 else 0) .|. (if x25 then 0x2000000 else 0) .|. (if x26 then 0x4000000 else 0) .|. (if x27 then 0x8000000 else 0) .|. (if x28 then 0x10000000 else 0) .|. (if x29 then 0x20000000 else 0) .|. (if x30 then 0x40000000 else 0) .|. (if x31 then 0x80000000 else 0))
  unpackX32 (MkBoolX32 !x) = (testBit x 0, testBit x 1, testBit x 2, testBit x 3, testBit x 4, testBit x 5, testBit x 6, testBit x 7, testBit x 8, testBit x 9, testBit x 10, testBit x 11, testBit x 12, testBit x 13, testBit x 14, testBit x 15, testBit x 16, testBit x 17, testBit x 18, testBit x 19, testBit x 20, testBit x 21, testBit x 22, testBit x 23, testBit x 24, testBit x 25, testBit x 26, testBit x 27, testBit x 28, testBit x 29, testBit x 30, testBit x 31)
instance Broadcast X32 Bool where
  broadcast False = MkBoolX32 0
  broadcast True = MkBoolX32 0xffffffff
  {-# INLINE broadcast #-}
instance SelectableF X32 Bool where
  selectF (MkBoolX32 !cond) (MkBoolX32 !x) (MkBoolX32 !y) = MkBoolX32 ((cond .&. x) .|. (complement cond .&. y))
  {-# INLINE selectF #-}
data instance X32 Float = MkFloatX32WithVec256 FloatX8# FloatX8# FloatX8# FloatX8#
instance PackX32 X32 Float where
  mkX32 (F# x0) (F# x1) (F# x2) (F# x3) (F# x4) (F# x5) (F# x6) (F# x7) (F# x8) (F# x9) (F# x10) (F# x11) (F# x12) (F# x13) (F# x14) (F# x15) (F# x16) (F# x17) (F# x18) (F# x19) (F# x20) (F# x21) (F# x22) (F# x23) (F# x24) (F# x25) (F# x26) (F# x27) (F# x28) (F# x29) (F# x30) (F# x31) = MkFloatX32WithVec256 (packFloatX8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packFloatX8# (# x8, x9, x10, x11, x12, x13, x14, x15 #)) (packFloatX8# (# x16, x17, x18, x19, x20, x21, x22, x23 #)) (packFloatX8# (# x24, x25, x26, x27, x28, x29, x30, x31 #))
  unpackX32 (MkFloatX32WithVec256 v0 v1 v2 v3) = case unpackFloatX8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackFloatX8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackFloatX8# v2 of (# x16, x17, x18, x19, x20, x21, x22, x23 #) -> case unpackFloatX8# v3 of (# x24, x25, x26, x27, x28, x29, x30, x31 #) -> (F# x0, F# x1, F# x2, F# x3, F# x4, F# x5, F# x6, F# x7, F# x8, F# x9, F# x10, F# x11, F# x12, F# x13, F# x14, F# x15, F# x16, F# x17, F# x18, F# x19, F# x20, F# x21, F# x22, F# x23, F# x24, F# x25, F# x26, F# x27, F# x28, F# x29, F# x30, F# x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 Float where
  broadcast (F# x) = let !v = broadcastFloatX8# x in MkFloatX32WithVec256 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X32 Float where
  selectF (MkBoolX32 !cond) (MkFloatX32WithVec256 x0 x1 x2 x3) (MkFloatX32WithVec256 y0 y1 y2 y3) = MkFloatX32WithVec256 (selectFloatX8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectFloatX8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1) (selectFloatX8# (fromIntegral $ cond `unsafeShiftR` 16) x2 y2) (selectFloatX8# (fromIntegral $ cond `unsafeShiftR` 24) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, i16 < 32, i17 < 32, i18 < 32, i19 < 32, i20 < 32, i21 < 32, i22 < 32, i23 < 32, i24 < 32, i25 < 32, i26 < 32, i27 < 32, i28 < 32, i29 < 32, i30 < 32, i31 < 32, ShuffleMany FloatX8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany FloatX8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany FloatX8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany FloatX8# [i24, i25, i26, i27, i28, i29, i30, i31]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Float where
  unaryShuffle (MkFloatX32WithVec256 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkFloatX32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, ShuffleMany FloatX8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany FloatX8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany FloatX8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany FloatX8# [i24, i25, i26, i27, i28, i29, i30, i31]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Float where
  binaryShuffle (MkFloatX32WithVec256 x0 x1 x2 x3) (MkFloatX32WithVec256 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkFloatX32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X32 Float where
  eqF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkBoolX32 $ (fromIntegral (eqFloatX8# u0 v0)) .|. (fromIntegral (eqFloatX8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (eqFloatX8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (eqFloatX8# u3 v3) `unsafeShiftL` 24)
  {-# INLINE eqF #-}
instance OrderedF X32 Float where
  ltF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkBoolX32 $ (fromIntegral (ltFloatX8# u0 v0)) .|. (fromIntegral (ltFloatX8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (ltFloatX8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (ltFloatX8# u3 v3) `unsafeShiftL` 24)
  leF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkBoolX32 $ (fromIntegral (leFloatX8# u0 v0)) .|. (fromIntegral (leFloatX8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (leFloatX8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (leFloatX8# u3 v3) `unsafeShiftL` 24)
  gtF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkBoolX32 $ (fromIntegral (gtFloatX8# u0 v0)) .|. (fromIntegral (gtFloatX8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (gtFloatX8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (gtFloatX8# u3 v3) `unsafeShiftL` 24)
  geF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkBoolX32 $ (fromIntegral (geFloatX8# u0 v0)) .|. (fromIntegral (geFloatX8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (geFloatX8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (geFloatX8# u3 v3) `unsafeShiftL` 24)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X32 Float where
  minF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkFloatX32WithVec256 (minimumFloatX8# u0 v0) (minimumFloatX8# u1 v1) (minimumFloatX8# u2 v2) (minimumFloatX8# u3 v3)
  maxF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkFloatX32WithVec256 (maximumFloatX8# u0 v0) (maximumFloatX8# u1 v1) (maximumFloatX8# u2 v2) (maximumFloatX8# u3 v3)
  minimumNumberF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkFloatX32WithVec256 (minimumNumberFloatX8# u0 v0) (minimumNumberFloatX8# u1 v1) (minimumNumberFloatX8# u2 v2) (minimumNumberFloatX8# u3 v3)
  maximumNumberF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkFloatX32WithVec256 (maximumNumberFloatX8# u0 v0) (maximumNumberFloatX8# u1 v1) (maximumNumberFloatX8# u2 v2) (maximumNumberFloatX8# u3 v3)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX32Float :: X32 Float -> X32 Float
negateX32Float (MkFloatX32WithVec256 u0 u1 u2 u3) = MkFloatX32WithVec256 (negateFloatX8# u0) (negateFloatX8# u1) (negateFloatX8# u2) (negateFloatX8# u3)
#if defined(USE_FMA)
{-# INLINE [0] negateX32Float #-}
#else
{-# INLINE negateX32Float #-}
#endif
instance NumF X32 Float where
  plusF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkFloatX32WithVec256 (plusFloatX8# u0 v0) (plusFloatX8# u1 v1) (plusFloatX8# u2 v2) (plusFloatX8# u3 v3)
  minusF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkFloatX32WithVec256 (minusFloatX8# u0 v0) (minusFloatX8# u1 v1) (minusFloatX8# u2 v2) (minusFloatX8# u3 v3)
  timesF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkFloatX32WithVec256 (timesFloatX8# u0 v0) (timesFloatX8# u1 v1) (timesFloatX8# u2 v2) (timesFloatX8# u3 v3)
  negateF = negateX32Float
  absF (MkFloatX32WithVec256 u0 u1 u2 u3) = MkFloatX32WithVec256 (absFloatX8# u0) (absFloatX8# u1) (absFloatX8# u2) (absFloatX8# u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X32 Float where
  divideF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) = MkFloatX32WithVec256 (divideFloatX8# u0 v0) (divideFloatX8# u1 v1) (divideFloatX8# u2 v2) (divideFloatX8# u3 v3)
  {-# INLINE divideF #-}
instance FloatingF X32 Float where
  sqrtF (MkFloatX32WithVec256 u0 u1 u2 u3) = MkFloatX32WithVec256 (sqrtFloatX8# u0) (sqrtFloatX8# u1) (sqrtFloatX8# u2) (sqrtFloatX8# u3)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X32 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX32WithVec256 u0 u1 u2 u3) (MkFloatX32WithVec256 v0 v1 v2 v3) (MkFloatX32WithVec256 w0 w1 w2 w3) = MkFloatX32WithVec256 (fmaddFloatX8# u0 v0 w0) (fmaddFloatX8# u1 v1 w1) (fmaddFloatX8# u2 v2 w2) (fmaddFloatX8# u3 v3 w3)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X32 Float" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X32 Float
"Fusible/*-/X32 Float" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX32Float c) :: X32 Float
"Fusible/-*+/X32 Float" forall a b c.
  negateX32Float (a F.* b) F.+ c = fusedMultiplyAdd (negateX32Float a) b c :: X32 Float
"Fusible/-*-/X32 Float" forall a b c.
  negateX32Float (a F.* b) F.- c = fusedMultiplyAdd (negateX32Float a) b (negateX32Float c) :: X32 Float
"Fusible/+*/X32 Float" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X32 Float
"Fusible/-*/X32 Float" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX32Float b) c a :: X32 Float
  #-}
#endif
instance EnumFromZero_ X32 Float where
  enumFromZero = MkFloatX32WithVec256 (packFloatX8# (# 0.0#, 1.0#, 2.0#, 3.0#, 4.0#, 5.0#, 6.0#, 7.0# #)) (packFloatX8# (# 8.0#, 9.0#, 10.0#, 11.0#, 12.0#, 13.0#, 14.0#, 15.0# #)) (packFloatX8# (# 16.0#, 17.0#, 18.0#, 19.0#, 20.0#, 21.0#, 22.0#, 23.0# #)) (packFloatX8# (# 24.0#, 25.0#, 26.0#, 27.0#, 28.0#, 29.0#, 30.0#, 31.0# #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X32 Float where
  indexByteArraySIMD# ba i = MkFloatX32WithVec256 (indexFloatArrayAsFloatX8# ba i) (indexFloatArrayAsFloatX8# ba (i +# 8#)) (indexFloatArrayAsFloatX8# ba (i +# 16#)) (indexFloatArrayAsFloatX8# ba (i +# 24#))
  readByteArraySIMD# mba i s0 = case readFloatArrayAsFloatX8# mba i s0 of (# s1, v0 #) -> case readFloatArrayAsFloatX8# mba (i +# 8#) s1 of (# s2, v1 #) -> case readFloatArrayAsFloatX8# mba (i +# 16#) s2 of (# s3, v2 #) -> case readFloatArrayAsFloatX8# mba (i +# 24#) s3 of (# s4, v3 #) -> (# s4, MkFloatX32WithVec256 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkFloatX32WithVec256 v0 v1 v2 v3) s0 = case writeFloatArrayAsFloatX8# mba i v0 s0 of s1 -> case writeFloatArrayAsFloatX8# mba (i +# 8#) v1 s1 of s2 -> case writeFloatArrayAsFloatX8# mba (i +# 16#) v2 s2 of s3 -> writeFloatArrayAsFloatX8# mba (i +# 24#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X32 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readFloatOffAddrAsFloatX8# addr i s0 of (# s1, v0 #) -> case readFloatOffAddrAsFloatX8# addr (i +# 8#) s1 of (# s2, v1 #) -> case readFloatOffAddrAsFloatX8# addr (i +# 16#) s2 of (# s3, v2 #) -> case readFloatOffAddrAsFloatX8# addr (i +# 24#) s3 of (# s4, v3 #) -> (# s4, MkFloatX32WithVec256 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX32WithVec256 v0 v1 v2 v3) = IO (\s0 -> case writeFloatOffAddrAsFloatX8# addr i v0 s0 of s1 -> case writeFloatOffAddrAsFloatX8# addr (i +# 8#) v1 s1 of s2 -> case writeFloatOffAddrAsFloatX8# addr (i +# 16#) v2 s2 of s3 -> case writeFloatOffAddrAsFloatX8# addr (i +# 24#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X32 Double = MkDoubleX32WithVec256 DoubleX4# DoubleX4# DoubleX4# DoubleX4# DoubleX4# DoubleX4# DoubleX4# DoubleX4#
instance PackX32 X32 Double where
  mkX32 (D# x0) (D# x1) (D# x2) (D# x3) (D# x4) (D# x5) (D# x6) (D# x7) (D# x8) (D# x9) (D# x10) (D# x11) (D# x12) (D# x13) (D# x14) (D# x15) (D# x16) (D# x17) (D# x18) (D# x19) (D# x20) (D# x21) (D# x22) (D# x23) (D# x24) (D# x25) (D# x26) (D# x27) (D# x28) (D# x29) (D# x30) (D# x31) = MkDoubleX32WithVec256 (packDoubleX4# (# x0, x1, x2, x3 #)) (packDoubleX4# (# x4, x5, x6, x7 #)) (packDoubleX4# (# x8, x9, x10, x11 #)) (packDoubleX4# (# x12, x13, x14, x15 #)) (packDoubleX4# (# x16, x17, x18, x19 #)) (packDoubleX4# (# x20, x21, x22, x23 #)) (packDoubleX4# (# x24, x25, x26, x27 #)) (packDoubleX4# (# x28, x29, x30, x31 #))
  unpackX32 (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = case unpackDoubleX4# v0 of (# x0, x1, x2, x3 #) -> case unpackDoubleX4# v1 of (# x4, x5, x6, x7 #) -> case unpackDoubleX4# v2 of (# x8, x9, x10, x11 #) -> case unpackDoubleX4# v3 of (# x12, x13, x14, x15 #) -> case unpackDoubleX4# v4 of (# x16, x17, x18, x19 #) -> case unpackDoubleX4# v5 of (# x20, x21, x22, x23 #) -> case unpackDoubleX4# v6 of (# x24, x25, x26, x27 #) -> case unpackDoubleX4# v7 of (# x28, x29, x30, x31 #) -> (D# x0, D# x1, D# x2, D# x3, D# x4, D# x5, D# x6, D# x7, D# x8, D# x9, D# x10, D# x11, D# x12, D# x13, D# x14, D# x15, D# x16, D# x17, D# x18, D# x19, D# x20, D# x21, D# x22, D# x23, D# x24, D# x25, D# x26, D# x27, D# x28, D# x29, D# x30, D# x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 Double where
  broadcast (D# x) = let !v = broadcastDoubleX4# x in MkDoubleX32WithVec256 v v v v v v v v
  {-# INLINE broadcast #-}
instance SelectableF X32 Double where
  selectF (MkBoolX32 !cond) (MkDoubleX32WithVec256 x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX32WithVec256 y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX32WithVec256 (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 4) x1 y1) (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 8) x2 y2) (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 12) x3 y3) (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 16) x4 y4) (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 20) x5 y5) (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 24) x6 y6) (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 28) x7 y7)
  {-# INLINE selectF #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, i16 < 32, i17 < 32, i18 < 32, i19 < 32, i20 < 32, i21 < 32, i22 < 32, i23 < 32, i24 < 32, i25 < 32, i26 < 32, i27 < 32, i28 < 32, i29 < 32, i30 < 32, i31 < 32, ShuffleMany DoubleX4# [i0, i1, i2, i3], ShuffleMany DoubleX4# [i4, i5, i6, i7], ShuffleMany DoubleX4# [i8, i9, i10, i11], ShuffleMany DoubleX4# [i12, i13, i14, i15], ShuffleMany DoubleX4# [i16, i17, i18, i19], ShuffleMany DoubleX4# [i20, i21, i22, i23], ShuffleMany DoubleX4# [i24, i25, i26, i27], ShuffleMany DoubleX4# [i28, i29, i30, i31]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Double where
  unaryShuffle (MkDoubleX32WithVec256 x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkDoubleX32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19] sources) (shuffleMany# @_ @_ @[i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27] sources) (shuffleMany# @_ @_ @[i28, i29, i30, i31] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, ShuffleMany DoubleX4# [i0, i1, i2, i3], ShuffleMany DoubleX4# [i4, i5, i6, i7], ShuffleMany DoubleX4# [i8, i9, i10, i11], ShuffleMany DoubleX4# [i12, i13, i14, i15], ShuffleMany DoubleX4# [i16, i17, i18, i19], ShuffleMany DoubleX4# [i20, i21, i22, i23], ShuffleMany DoubleX4# [i24, i25, i26, i27], ShuffleMany DoubleX4# [i28, i29, i30, i31]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Double where
  binaryShuffle (MkDoubleX32WithVec256 x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX32WithVec256 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkDoubleX32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19] sources) (shuffleMany# @_ @_ @[i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27] sources) (shuffleMany# @_ @_ @[i28, i29, i30, i31] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X32 Double where
  eqF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX32 $ (fromIntegral (eqDoubleX4# u0 v0)) .|. (fromIntegral (eqDoubleX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (eqDoubleX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (eqDoubleX4# u3 v3) `unsafeShiftL` 12) .|. (fromIntegral (eqDoubleX4# u4 v4) `unsafeShiftL` 16) .|. (fromIntegral (eqDoubleX4# u5 v5) `unsafeShiftL` 20) .|. (fromIntegral (eqDoubleX4# u6 v6) `unsafeShiftL` 24) .|. (fromIntegral (eqDoubleX4# u7 v7) `unsafeShiftL` 28)
  {-# INLINE eqF #-}
instance OrderedF X32 Double where
  ltF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX32 $ (fromIntegral (ltDoubleX4# u0 v0)) .|. (fromIntegral (ltDoubleX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (ltDoubleX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (ltDoubleX4# u3 v3) `unsafeShiftL` 12) .|. (fromIntegral (ltDoubleX4# u4 v4) `unsafeShiftL` 16) .|. (fromIntegral (ltDoubleX4# u5 v5) `unsafeShiftL` 20) .|. (fromIntegral (ltDoubleX4# u6 v6) `unsafeShiftL` 24) .|. (fromIntegral (ltDoubleX4# u7 v7) `unsafeShiftL` 28)
  leF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX32 $ (fromIntegral (leDoubleX4# u0 v0)) .|. (fromIntegral (leDoubleX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (leDoubleX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (leDoubleX4# u3 v3) `unsafeShiftL` 12) .|. (fromIntegral (leDoubleX4# u4 v4) `unsafeShiftL` 16) .|. (fromIntegral (leDoubleX4# u5 v5) `unsafeShiftL` 20) .|. (fromIntegral (leDoubleX4# u6 v6) `unsafeShiftL` 24) .|. (fromIntegral (leDoubleX4# u7 v7) `unsafeShiftL` 28)
  gtF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX32 $ (fromIntegral (gtDoubleX4# u0 v0)) .|. (fromIntegral (gtDoubleX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (gtDoubleX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (gtDoubleX4# u3 v3) `unsafeShiftL` 12) .|. (fromIntegral (gtDoubleX4# u4 v4) `unsafeShiftL` 16) .|. (fromIntegral (gtDoubleX4# u5 v5) `unsafeShiftL` 20) .|. (fromIntegral (gtDoubleX4# u6 v6) `unsafeShiftL` 24) .|. (fromIntegral (gtDoubleX4# u7 v7) `unsafeShiftL` 28)
  geF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX32 $ (fromIntegral (geDoubleX4# u0 v0)) .|. (fromIntegral (geDoubleX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (geDoubleX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (geDoubleX4# u3 v3) `unsafeShiftL` 12) .|. (fromIntegral (geDoubleX4# u4 v4) `unsafeShiftL` 16) .|. (fromIntegral (geDoubleX4# u5 v5) `unsafeShiftL` 20) .|. (fromIntegral (geDoubleX4# u6 v6) `unsafeShiftL` 24) .|. (fromIntegral (geDoubleX4# u7 v7) `unsafeShiftL` 28)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X32 Double where
  minF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX32WithVec256 (minimumDoubleX4# u0 v0) (minimumDoubleX4# u1 v1) (minimumDoubleX4# u2 v2) (minimumDoubleX4# u3 v3) (minimumDoubleX4# u4 v4) (minimumDoubleX4# u5 v5) (minimumDoubleX4# u6 v6) (minimumDoubleX4# u7 v7)
  maxF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX32WithVec256 (maximumDoubleX4# u0 v0) (maximumDoubleX4# u1 v1) (maximumDoubleX4# u2 v2) (maximumDoubleX4# u3 v3) (maximumDoubleX4# u4 v4) (maximumDoubleX4# u5 v5) (maximumDoubleX4# u6 v6) (maximumDoubleX4# u7 v7)
  minimumNumberF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX32WithVec256 (minimumNumberDoubleX4# u0 v0) (minimumNumberDoubleX4# u1 v1) (minimumNumberDoubleX4# u2 v2) (minimumNumberDoubleX4# u3 v3) (minimumNumberDoubleX4# u4 v4) (minimumNumberDoubleX4# u5 v5) (minimumNumberDoubleX4# u6 v6) (minimumNumberDoubleX4# u7 v7)
  maximumNumberF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX32WithVec256 (maximumNumberDoubleX4# u0 v0) (maximumNumberDoubleX4# u1 v1) (maximumNumberDoubleX4# u2 v2) (maximumNumberDoubleX4# u3 v3) (maximumNumberDoubleX4# u4 v4) (maximumNumberDoubleX4# u5 v5) (maximumNumberDoubleX4# u6 v6) (maximumNumberDoubleX4# u7 v7)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX32Double :: X32 Double -> X32 Double
negateX32Double (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) = MkDoubleX32WithVec256 (negateDoubleX4# u0) (negateDoubleX4# u1) (negateDoubleX4# u2) (negateDoubleX4# u3) (negateDoubleX4# u4) (negateDoubleX4# u5) (negateDoubleX4# u6) (negateDoubleX4# u7)
#if defined(USE_FMA)
{-# INLINE [0] negateX32Double #-}
#else
{-# INLINE negateX32Double #-}
#endif
instance NumF X32 Double where
  plusF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX32WithVec256 (plusDoubleX4# u0 v0) (plusDoubleX4# u1 v1) (plusDoubleX4# u2 v2) (plusDoubleX4# u3 v3) (plusDoubleX4# u4 v4) (plusDoubleX4# u5 v5) (plusDoubleX4# u6 v6) (plusDoubleX4# u7 v7)
  minusF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX32WithVec256 (minusDoubleX4# u0 v0) (minusDoubleX4# u1 v1) (minusDoubleX4# u2 v2) (minusDoubleX4# u3 v3) (minusDoubleX4# u4 v4) (minusDoubleX4# u5 v5) (minusDoubleX4# u6 v6) (minusDoubleX4# u7 v7)
  timesF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX32WithVec256 (timesDoubleX4# u0 v0) (timesDoubleX4# u1 v1) (timesDoubleX4# u2 v2) (timesDoubleX4# u3 v3) (timesDoubleX4# u4 v4) (timesDoubleX4# u5 v5) (timesDoubleX4# u6 v6) (timesDoubleX4# u7 v7)
  negateF = negateX32Double
  absF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) = MkDoubleX32WithVec256 (absDoubleX4# u0) (absDoubleX4# u1) (absDoubleX4# u2) (absDoubleX4# u3) (absDoubleX4# u4) (absDoubleX4# u5) (absDoubleX4# u6) (absDoubleX4# u7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X32 Double where
  divideF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX32WithVec256 (divideDoubleX4# u0 v0) (divideDoubleX4# u1 v1) (divideDoubleX4# u2 v2) (divideDoubleX4# u3 v3) (divideDoubleX4# u4 v4) (divideDoubleX4# u5 v5) (divideDoubleX4# u6 v6) (divideDoubleX4# u7 v7)
  {-# INLINE divideF #-}
instance FloatingF X32 Double where
  sqrtF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) = MkDoubleX32WithVec256 (sqrtDoubleX4# u0) (sqrtDoubleX4# u1) (sqrtDoubleX4# u2) (sqrtDoubleX4# u3) (sqrtDoubleX4# u4) (sqrtDoubleX4# u5) (sqrtDoubleX4# u6) (sqrtDoubleX4# u7)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X32 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) (MkDoubleX32WithVec256 w0 w1 w2 w3 w4 w5 w6 w7) = MkDoubleX32WithVec256 (fmaddDoubleX4# u0 v0 w0) (fmaddDoubleX4# u1 v1 w1) (fmaddDoubleX4# u2 v2 w2) (fmaddDoubleX4# u3 v3 w3) (fmaddDoubleX4# u4 v4 w4) (fmaddDoubleX4# u5 v5 w5) (fmaddDoubleX4# u6 v6 w6) (fmaddDoubleX4# u7 v7 w7)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X32 Double" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X32 Double
"Fusible/*-/X32 Double" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX32Double c) :: X32 Double
"Fusible/-*+/X32 Double" forall a b c.
  negateX32Double (a F.* b) F.+ c = fusedMultiplyAdd (negateX32Double a) b c :: X32 Double
"Fusible/-*-/X32 Double" forall a b c.
  negateX32Double (a F.* b) F.- c = fusedMultiplyAdd (negateX32Double a) b (negateX32Double c) :: X32 Double
"Fusible/+*/X32 Double" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X32 Double
"Fusible/-*/X32 Double" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX32Double b) c a :: X32 Double
  #-}
#endif
instance EnumFromZero_ X32 Double where
  enumFromZero = MkDoubleX32WithVec256 (packDoubleX4# (# 0.0##, 1.0##, 2.0##, 3.0## #)) (packDoubleX4# (# 4.0##, 5.0##, 6.0##, 7.0## #)) (packDoubleX4# (# 8.0##, 9.0##, 10.0##, 11.0## #)) (packDoubleX4# (# 12.0##, 13.0##, 14.0##, 15.0## #)) (packDoubleX4# (# 16.0##, 17.0##, 18.0##, 19.0## #)) (packDoubleX4# (# 20.0##, 21.0##, 22.0##, 23.0## #)) (packDoubleX4# (# 24.0##, 25.0##, 26.0##, 27.0## #)) (packDoubleX4# (# 28.0##, 29.0##, 30.0##, 31.0## #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X32 Double where
  indexByteArraySIMD# ba i = MkDoubleX32WithVec256 (indexDoubleArrayAsDoubleX4# ba i) (indexDoubleArrayAsDoubleX4# ba (i +# 4#)) (indexDoubleArrayAsDoubleX4# ba (i +# 8#)) (indexDoubleArrayAsDoubleX4# ba (i +# 12#)) (indexDoubleArrayAsDoubleX4# ba (i +# 16#)) (indexDoubleArrayAsDoubleX4# ba (i +# 20#)) (indexDoubleArrayAsDoubleX4# ba (i +# 24#)) (indexDoubleArrayAsDoubleX4# ba (i +# 28#))
  readByteArraySIMD# mba i s0 = case readDoubleArrayAsDoubleX4# mba i s0 of (# s1, v0 #) -> case readDoubleArrayAsDoubleX4# mba (i +# 4#) s1 of (# s2, v1 #) -> case readDoubleArrayAsDoubleX4# mba (i +# 8#) s2 of (# s3, v2 #) -> case readDoubleArrayAsDoubleX4# mba (i +# 12#) s3 of (# s4, v3 #) -> case readDoubleArrayAsDoubleX4# mba (i +# 16#) s4 of (# s5, v4 #) -> case readDoubleArrayAsDoubleX4# mba (i +# 20#) s5 of (# s6, v5 #) -> case readDoubleArrayAsDoubleX4# mba (i +# 24#) s6 of (# s7, v6 #) -> case readDoubleArrayAsDoubleX4# mba (i +# 28#) s7 of (# s8, v7 #) -> (# s8, MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7 #)
  writeByteArraySIMD# mba i (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) s0 = case writeDoubleArrayAsDoubleX4# mba i v0 s0 of s1 -> case writeDoubleArrayAsDoubleX4# mba (i +# 4#) v1 s1 of s2 -> case writeDoubleArrayAsDoubleX4# mba (i +# 8#) v2 s2 of s3 -> case writeDoubleArrayAsDoubleX4# mba (i +# 12#) v3 s3 of s4 -> case writeDoubleArrayAsDoubleX4# mba (i +# 16#) v4 s4 of s5 -> case writeDoubleArrayAsDoubleX4# mba (i +# 20#) v5 s5 of s6 -> case writeDoubleArrayAsDoubleX4# mba (i +# 24#) v6 s6 of s7 -> writeDoubleArrayAsDoubleX4# mba (i +# 28#) v7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X32 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readDoubleOffAddrAsDoubleX4# addr i s0 of (# s1, v0 #) -> case readDoubleOffAddrAsDoubleX4# addr (i +# 4#) s1 of (# s2, v1 #) -> case readDoubleOffAddrAsDoubleX4# addr (i +# 8#) s2 of (# s3, v2 #) -> case readDoubleOffAddrAsDoubleX4# addr (i +# 12#) s3 of (# s4, v3 #) -> case readDoubleOffAddrAsDoubleX4# addr (i +# 16#) s4 of (# s5, v4 #) -> case readDoubleOffAddrAsDoubleX4# addr (i +# 20#) s5 of (# s6, v5 #) -> case readDoubleOffAddrAsDoubleX4# addr (i +# 24#) s6 of (# s7, v6 #) -> case readDoubleOffAddrAsDoubleX4# addr (i +# 28#) s7 of (# s8, v7 #) -> (# s8, MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = IO (\s0 -> case writeDoubleOffAddrAsDoubleX4# addr i v0 s0 of s1 -> case writeDoubleOffAddrAsDoubleX4# addr (i +# 4#) v1 s1 of s2 -> case writeDoubleOffAddrAsDoubleX4# addr (i +# 8#) v2 s2 of s3 -> case writeDoubleOffAddrAsDoubleX4# addr (i +# 12#) v3 s3 of s4 -> case writeDoubleOffAddrAsDoubleX4# addr (i +# 16#) v4 s4 of s5 -> case writeDoubleOffAddrAsDoubleX4# addr (i +# 20#) v5 s5 of s6 -> case writeDoubleOffAddrAsDoubleX4# addr (i +# 24#) v6 s6 of s7 -> case writeDoubleOffAddrAsDoubleX4# addr (i +# 28#) v7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
instance ImplementationDescription X32 where
  implementationDescription _ = "X32;maxBits=256"
data instance X32 Int8 = MkInt8X32 Int8X32#
instance PackX32 X32 Int8 where
  mkX32 (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) (I8# x8) (I8# x9) (I8# x10) (I8# x11) (I8# x12) (I8# x13) (I8# x14) (I8# x15) (I8# x16) (I8# x17) (I8# x18) (I8# x19) (I8# x20) (I8# x21) (I8# x22) (I8# x23) (I8# x24) (I8# x25) (I8# x26) (I8# x27) (I8# x28) (I8# x29) (I8# x30) (I8# x31) = MkInt8X32 (packInt8X32# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #))
  unpackX32 (MkInt8X32 v0) = case unpackInt8X32# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #) -> (I8# x0, I8# x1, I8# x2, I8# x3, I8# x4, I8# x5, I8# x6, I8# x7, I8# x8, I8# x9, I8# x10, I8# x11, I8# x12, I8# x13, I8# x14, I8# x15, I8# x16, I8# x17, I8# x18, I8# x19, I8# x20, I8# x21, I8# x22, I8# x23, I8# x24, I8# x25, I8# x26, I8# x27, I8# x28, I8# x29, I8# x30, I8# x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 Int8 where
  broadcast (I8# x) = MkInt8X32 (broadcastInt8X32# x)
  {-# INLINE broadcast #-}
instance SelectableF X32 Int8 where
  selectF (MkBoolX32 !cond) (MkInt8X32 x0) (MkInt8X32 y0) = MkInt8X32 (selectInt8X32# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 32, ShuffleMany Int8X32# indices) => UnaryShuffle indices X32 Int8 where
  unaryShuffle (MkInt8X32 x) = MkInt8X32 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 64, ShuffleMany Int8X32# indices) => BinaryShuffle indices X32 Int8 where
  binaryShuffle (MkInt8X32 x0) (MkInt8X32 x1) = MkInt8X32 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X32 Int8 where
  eqF (MkInt8X32 u0) (MkInt8X32 v0) = MkBoolX32 $ (fromIntegral (eqInt8X32# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X32 Int8 where
  ltF (MkInt8X32 u0) (MkInt8X32 v0) = MkBoolX32 $ (fromIntegral (ltInt8X32# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X32 Int8 where
  minF (MkInt8X32 u0) (MkInt8X32 v0) = MkInt8X32 (minInt8X32# u0 v0)
  maxF (MkInt8X32 u0) (MkInt8X32 v0) = MkInt8X32 (maxInt8X32# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X32 Int8 where
  plusF (MkInt8X32 u0) (MkInt8X32 v0) = MkInt8X32 (plusInt8X32# u0 v0)
  minusF (MkInt8X32 u0) (MkInt8X32 v0) = MkInt8X32 (minusInt8X32# u0 v0)
  timesF (MkInt8X32 u0) (MkInt8X32 v0) = MkInt8X32 (timesInt8X32# u0 v0)
  negateF (MkInt8X32 u0) = MkInt8X32 (negateInt8X32# u0)
  absF (MkInt8X32 u0) = MkInt8X32 (absInt8X32# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X32 Int8 where
  andF (MkInt8X32 u0) (MkInt8X32 v0) = MkInt8X32 (andInt8X32# u0 v0)
  orF (MkInt8X32 u0) (MkInt8X32 v0) = MkInt8X32 (orInt8X32# u0 v0)
  xorF (MkInt8X32 u0) (MkInt8X32 v0) = MkInt8X32 (xorInt8X32# u0 v0)
  complementF (MkInt8X32 u0) = MkInt8X32 (complementInt8X32# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X32 Int8 where
  shiftLF (MkInt8X32 u0) (I# i) = MkInt8X32 (shiftLInt8X32# u0 i)
  shiftRF (MkInt8X32 u0) (I# i) = MkInt8X32 (shiftRInt8X32# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X32 Int8 where
  enumFromZero = MkInt8X32 (packInt8X32# (# 0#Int8, 1#Int8, 2#Int8, 3#Int8, 4#Int8, 5#Int8, 6#Int8, 7#Int8, 8#Int8, 9#Int8, 10#Int8, 11#Int8, 12#Int8, 13#Int8, 14#Int8, 15#Int8, 16#Int8, 17#Int8, 18#Int8, 19#Int8, 20#Int8, 21#Int8, 22#Int8, 23#Int8, 24#Int8, 25#Int8, 26#Int8, 27#Int8, 28#Int8, 29#Int8, 30#Int8, 31#Int8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X32 Int8 where
  indexByteArraySIMD# ba i = MkInt8X32 (indexInt8ArrayAsInt8X32# ba i)
  readByteArraySIMD# mba i s0 = case readInt8ArrayAsInt8X32# mba i s0 of (# s1, v0 #) -> (# s1, MkInt8X32 v0 #)
  writeByteArraySIMD# mba i (MkInt8X32 v0) s0 = writeInt8ArrayAsInt8X32# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X32 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt8OffAddrAsInt8X32# addr i s0 of (# s1, v0 #) -> (# s1, MkInt8X32 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X32 v0) = IO (\s0 -> case writeInt8OffAddrAsInt8X32# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X32 Int16 = MkInt16X32WithVec256 Int16X16# Int16X16#
instance PackX32 X32 Int16 where
  mkX32 (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) (I16# x8) (I16# x9) (I16# x10) (I16# x11) (I16# x12) (I16# x13) (I16# x14) (I16# x15) (I16# x16) (I16# x17) (I16# x18) (I16# x19) (I16# x20) (I16# x21) (I16# x22) (I16# x23) (I16# x24) (I16# x25) (I16# x26) (I16# x27) (I16# x28) (I16# x29) (I16# x30) (I16# x31) = MkInt16X32WithVec256 (packInt16X16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #)) (packInt16X16# (# x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #))
  unpackX32 (MkInt16X32WithVec256 v0 v1) = case unpackInt16X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackInt16X16# v1 of (# x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #) -> (I16# x0, I16# x1, I16# x2, I16# x3, I16# x4, I16# x5, I16# x6, I16# x7, I16# x8, I16# x9, I16# x10, I16# x11, I16# x12, I16# x13, I16# x14, I16# x15, I16# x16, I16# x17, I16# x18, I16# x19, I16# x20, I16# x21, I16# x22, I16# x23, I16# x24, I16# x25, I16# x26, I16# x27, I16# x28, I16# x29, I16# x30, I16# x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 Int16 where
  broadcast (I16# x) = let !v = broadcastInt16X16# x in MkInt16X32WithVec256 v v
  {-# INLINE broadcast #-}
instance SelectableF X32 Int16 where
  selectF (MkBoolX32 !cond) (MkInt16X32WithVec256 x0 x1) (MkInt16X32WithVec256 y0 y1) = MkInt16X32WithVec256 (selectInt16X16# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt16X16# (fromIntegral $ cond `unsafeShiftR` 16) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, i16 < 32, i17 < 32, i18 < 32, i19 < 32, i20 < 32, i21 < 32, i22 < 32, i23 < 32, i24 < 32, i25 < 32, i26 < 32, i27 < 32, i28 < 32, i29 < 32, i30 < 32, i31 < 32, ShuffleMany Int16X16# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Int16X16# [i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Int16 where
  unaryShuffle (MkInt16X32WithVec256 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt16X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, ShuffleMany Int16X16# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Int16X16# [i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Int16 where
  binaryShuffle (MkInt16X32WithVec256 x0 x1) (MkInt16X32WithVec256 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt16X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X32 Int16 where
  eqF (MkInt16X32WithVec256 u0 u1) (MkInt16X32WithVec256 v0 v1) = MkBoolX32 $ (fromIntegral (eqInt16X16# u0 v0)) .|. (fromIntegral (eqInt16X16# u1 v1) `unsafeShiftL` 16)
  {-# INLINE eqF #-}
instance OrderedF X32 Int16 where
  ltF (MkInt16X32WithVec256 u0 u1) (MkInt16X32WithVec256 v0 v1) = MkBoolX32 $ (fromIntegral (ltInt16X16# u0 v0)) .|. (fromIntegral (ltInt16X16# u1 v1) `unsafeShiftL` 16)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X32 Int16 where
  minF (MkInt16X32WithVec256 u0 u1) (MkInt16X32WithVec256 v0 v1) = MkInt16X32WithVec256 (minInt16X16# u0 v0) (minInt16X16# u1 v1)
  maxF (MkInt16X32WithVec256 u0 u1) (MkInt16X32WithVec256 v0 v1) = MkInt16X32WithVec256 (maxInt16X16# u0 v0) (maxInt16X16# u1 v1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X32 Int16 where
  plusF (MkInt16X32WithVec256 u0 u1) (MkInt16X32WithVec256 v0 v1) = MkInt16X32WithVec256 (plusInt16X16# u0 v0) (plusInt16X16# u1 v1)
  minusF (MkInt16X32WithVec256 u0 u1) (MkInt16X32WithVec256 v0 v1) = MkInt16X32WithVec256 (minusInt16X16# u0 v0) (minusInt16X16# u1 v1)
  timesF (MkInt16X32WithVec256 u0 u1) (MkInt16X32WithVec256 v0 v1) = MkInt16X32WithVec256 (timesInt16X16# u0 v0) (timesInt16X16# u1 v1)
  negateF (MkInt16X32WithVec256 u0 u1) = MkInt16X32WithVec256 (negateInt16X16# u0) (negateInt16X16# u1)
  absF (MkInt16X32WithVec256 u0 u1) = MkInt16X32WithVec256 (absInt16X16# u0) (absInt16X16# u1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X32 Int16 where
  andF (MkInt16X32WithVec256 u0 u1) (MkInt16X32WithVec256 v0 v1) = MkInt16X32WithVec256 (andInt16X16# u0 v0) (andInt16X16# u1 v1)
  orF (MkInt16X32WithVec256 u0 u1) (MkInt16X32WithVec256 v0 v1) = MkInt16X32WithVec256 (orInt16X16# u0 v0) (orInt16X16# u1 v1)
  xorF (MkInt16X32WithVec256 u0 u1) (MkInt16X32WithVec256 v0 v1) = MkInt16X32WithVec256 (xorInt16X16# u0 v0) (xorInt16X16# u1 v1)
  complementF (MkInt16X32WithVec256 u0 u1) = MkInt16X32WithVec256 (complementInt16X16# u0) (complementInt16X16# u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X32 Int16 where
  shiftLF (MkInt16X32WithVec256 u0 u1) (I# i) = MkInt16X32WithVec256 (shiftLInt16X16# u0 i) (shiftLInt16X16# u1 i)
  shiftRF (MkInt16X32WithVec256 u0 u1) (I# i) = MkInt16X32WithVec256 (shiftRInt16X16# u0 i) (shiftRInt16X16# u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X32 Int16 where
  enumFromZero = MkInt16X32WithVec256 (packInt16X16# (# 0#Int16, 1#Int16, 2#Int16, 3#Int16, 4#Int16, 5#Int16, 6#Int16, 7#Int16, 8#Int16, 9#Int16, 10#Int16, 11#Int16, 12#Int16, 13#Int16, 14#Int16, 15#Int16 #)) (packInt16X16# (# 16#Int16, 17#Int16, 18#Int16, 19#Int16, 20#Int16, 21#Int16, 22#Int16, 23#Int16, 24#Int16, 25#Int16, 26#Int16, 27#Int16, 28#Int16, 29#Int16, 30#Int16, 31#Int16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X32 Int16 where
  indexByteArraySIMD# ba i = MkInt16X32WithVec256 (indexInt16ArrayAsInt16X16# ba i) (indexInt16ArrayAsInt16X16# ba (i +# 16#))
  readByteArraySIMD# mba i s0 = case readInt16ArrayAsInt16X16# mba i s0 of (# s1, v0 #) -> case readInt16ArrayAsInt16X16# mba (i +# 16#) s1 of (# s2, v1 #) -> (# s2, MkInt16X32WithVec256 v0 v1 #)
  writeByteArraySIMD# mba i (MkInt16X32WithVec256 v0 v1) s0 = case writeInt16ArrayAsInt16X16# mba i v0 s0 of s1 -> writeInt16ArrayAsInt16X16# mba (i +# 16#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X32 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt16OffAddrAsInt16X16# addr i s0 of (# s1, v0 #) -> case readInt16OffAddrAsInt16X16# addr (i +# 16#) s1 of (# s2, v1 #) -> (# s2, MkInt16X32WithVec256 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X32WithVec256 v0 v1) = IO (\s0 -> case writeInt16OffAddrAsInt16X16# addr i v0 s0 of s1 -> case writeInt16OffAddrAsInt16X16# addr (i +# 16#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X32 Int32 = MkInt32X32WithVec256 Int32X8# Int32X8# Int32X8# Int32X8#
instance PackX32 X32 Int32 where
  mkX32 (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) (I32# x8) (I32# x9) (I32# x10) (I32# x11) (I32# x12) (I32# x13) (I32# x14) (I32# x15) (I32# x16) (I32# x17) (I32# x18) (I32# x19) (I32# x20) (I32# x21) (I32# x22) (I32# x23) (I32# x24) (I32# x25) (I32# x26) (I32# x27) (I32# x28) (I32# x29) (I32# x30) (I32# x31) = MkInt32X32WithVec256 (packInt32X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packInt32X8# (# x8, x9, x10, x11, x12, x13, x14, x15 #)) (packInt32X8# (# x16, x17, x18, x19, x20, x21, x22, x23 #)) (packInt32X8# (# x24, x25, x26, x27, x28, x29, x30, x31 #))
  unpackX32 (MkInt32X32WithVec256 v0 v1 v2 v3) = case unpackInt32X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackInt32X8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackInt32X8# v2 of (# x16, x17, x18, x19, x20, x21, x22, x23 #) -> case unpackInt32X8# v3 of (# x24, x25, x26, x27, x28, x29, x30, x31 #) -> (I32# x0, I32# x1, I32# x2, I32# x3, I32# x4, I32# x5, I32# x6, I32# x7, I32# x8, I32# x9, I32# x10, I32# x11, I32# x12, I32# x13, I32# x14, I32# x15, I32# x16, I32# x17, I32# x18, I32# x19, I32# x20, I32# x21, I32# x22, I32# x23, I32# x24, I32# x25, I32# x26, I32# x27, I32# x28, I32# x29, I32# x30, I32# x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 Int32 where
  broadcast (I32# x) = let !v = broadcastInt32X8# x in MkInt32X32WithVec256 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X32 Int32 where
  selectF (MkBoolX32 !cond) (MkInt32X32WithVec256 x0 x1 x2 x3) (MkInt32X32WithVec256 y0 y1 y2 y3) = MkInt32X32WithVec256 (selectInt32X8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt32X8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1) (selectInt32X8# (fromIntegral $ cond `unsafeShiftR` 16) x2 y2) (selectInt32X8# (fromIntegral $ cond `unsafeShiftR` 24) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, i16 < 32, i17 < 32, i18 < 32, i19 < 32, i20 < 32, i21 < 32, i22 < 32, i23 < 32, i24 < 32, i25 < 32, i26 < 32, i27 < 32, i28 < 32, i29 < 32, i30 < 32, i31 < 32, ShuffleMany Int32X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Int32X8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Int32X8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany Int32X8# [i24, i25, i26, i27, i28, i29, i30, i31]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Int32 where
  unaryShuffle (MkInt32X32WithVec256 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt32X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, ShuffleMany Int32X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Int32X8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Int32X8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany Int32X8# [i24, i25, i26, i27, i28, i29, i30, i31]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Int32 where
  binaryShuffle (MkInt32X32WithVec256 x0 x1 x2 x3) (MkInt32X32WithVec256 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt32X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X32 Int32 where
  eqF (MkInt32X32WithVec256 u0 u1 u2 u3) (MkInt32X32WithVec256 v0 v1 v2 v3) = MkBoolX32 $ (fromIntegral (eqInt32X8# u0 v0)) .|. (fromIntegral (eqInt32X8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (eqInt32X8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (eqInt32X8# u3 v3) `unsafeShiftL` 24)
  {-# INLINE eqF #-}
instance OrderedF X32 Int32 where
  ltF (MkInt32X32WithVec256 u0 u1 u2 u3) (MkInt32X32WithVec256 v0 v1 v2 v3) = MkBoolX32 $ (fromIntegral (ltInt32X8# u0 v0)) .|. (fromIntegral (ltInt32X8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (ltInt32X8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (ltInt32X8# u3 v3) `unsafeShiftL` 24)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X32 Int32 where
  minF (MkInt32X32WithVec256 u0 u1 u2 u3) (MkInt32X32WithVec256 v0 v1 v2 v3) = MkInt32X32WithVec256 (minInt32X8# u0 v0) (minInt32X8# u1 v1) (minInt32X8# u2 v2) (minInt32X8# u3 v3)
  maxF (MkInt32X32WithVec256 u0 u1 u2 u3) (MkInt32X32WithVec256 v0 v1 v2 v3) = MkInt32X32WithVec256 (maxInt32X8# u0 v0) (maxInt32X8# u1 v1) (maxInt32X8# u2 v2) (maxInt32X8# u3 v3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X32 Int32 where
  plusF (MkInt32X32WithVec256 u0 u1 u2 u3) (MkInt32X32WithVec256 v0 v1 v2 v3) = MkInt32X32WithVec256 (plusInt32X8# u0 v0) (plusInt32X8# u1 v1) (plusInt32X8# u2 v2) (plusInt32X8# u3 v3)
  minusF (MkInt32X32WithVec256 u0 u1 u2 u3) (MkInt32X32WithVec256 v0 v1 v2 v3) = MkInt32X32WithVec256 (minusInt32X8# u0 v0) (minusInt32X8# u1 v1) (minusInt32X8# u2 v2) (minusInt32X8# u3 v3)
  timesF (MkInt32X32WithVec256 u0 u1 u2 u3) (MkInt32X32WithVec256 v0 v1 v2 v3) = MkInt32X32WithVec256 (timesInt32X8# u0 v0) (timesInt32X8# u1 v1) (timesInt32X8# u2 v2) (timesInt32X8# u3 v3)
  negateF (MkInt32X32WithVec256 u0 u1 u2 u3) = MkInt32X32WithVec256 (negateInt32X8# u0) (negateInt32X8# u1) (negateInt32X8# u2) (negateInt32X8# u3)
  absF (MkInt32X32WithVec256 u0 u1 u2 u3) = MkInt32X32WithVec256 (absInt32X8# u0) (absInt32X8# u1) (absInt32X8# u2) (absInt32X8# u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X32 Int32 where
  andF (MkInt32X32WithVec256 u0 u1 u2 u3) (MkInt32X32WithVec256 v0 v1 v2 v3) = MkInt32X32WithVec256 (andInt32X8# u0 v0) (andInt32X8# u1 v1) (andInt32X8# u2 v2) (andInt32X8# u3 v3)
  orF (MkInt32X32WithVec256 u0 u1 u2 u3) (MkInt32X32WithVec256 v0 v1 v2 v3) = MkInt32X32WithVec256 (orInt32X8# u0 v0) (orInt32X8# u1 v1) (orInt32X8# u2 v2) (orInt32X8# u3 v3)
  xorF (MkInt32X32WithVec256 u0 u1 u2 u3) (MkInt32X32WithVec256 v0 v1 v2 v3) = MkInt32X32WithVec256 (xorInt32X8# u0 v0) (xorInt32X8# u1 v1) (xorInt32X8# u2 v2) (xorInt32X8# u3 v3)
  complementF (MkInt32X32WithVec256 u0 u1 u2 u3) = MkInt32X32WithVec256 (complementInt32X8# u0) (complementInt32X8# u1) (complementInt32X8# u2) (complementInt32X8# u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X32 Int32 where
  shiftLF (MkInt32X32WithVec256 u0 u1 u2 u3) (I# i) = MkInt32X32WithVec256 (shiftLInt32X8# u0 i) (shiftLInt32X8# u1 i) (shiftLInt32X8# u2 i) (shiftLInt32X8# u3 i)
  shiftRF (MkInt32X32WithVec256 u0 u1 u2 u3) (I# i) = MkInt32X32WithVec256 (shiftRInt32X8# u0 i) (shiftRInt32X8# u1 i) (shiftRInt32X8# u2 i) (shiftRInt32X8# u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X32 Int32 where
  enumFromZero = MkInt32X32WithVec256 (packInt32X8# (# 0#Int32, 1#Int32, 2#Int32, 3#Int32, 4#Int32, 5#Int32, 6#Int32, 7#Int32 #)) (packInt32X8# (# 8#Int32, 9#Int32, 10#Int32, 11#Int32, 12#Int32, 13#Int32, 14#Int32, 15#Int32 #)) (packInt32X8# (# 16#Int32, 17#Int32, 18#Int32, 19#Int32, 20#Int32, 21#Int32, 22#Int32, 23#Int32 #)) (packInt32X8# (# 24#Int32, 25#Int32, 26#Int32, 27#Int32, 28#Int32, 29#Int32, 30#Int32, 31#Int32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X32 Int32 where
  indexByteArraySIMD# ba i = MkInt32X32WithVec256 (indexInt32ArrayAsInt32X8# ba i) (indexInt32ArrayAsInt32X8# ba (i +# 8#)) (indexInt32ArrayAsInt32X8# ba (i +# 16#)) (indexInt32ArrayAsInt32X8# ba (i +# 24#))
  readByteArraySIMD# mba i s0 = case readInt32ArrayAsInt32X8# mba i s0 of (# s1, v0 #) -> case readInt32ArrayAsInt32X8# mba (i +# 8#) s1 of (# s2, v1 #) -> case readInt32ArrayAsInt32X8# mba (i +# 16#) s2 of (# s3, v2 #) -> case readInt32ArrayAsInt32X8# mba (i +# 24#) s3 of (# s4, v3 #) -> (# s4, MkInt32X32WithVec256 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkInt32X32WithVec256 v0 v1 v2 v3) s0 = case writeInt32ArrayAsInt32X8# mba i v0 s0 of s1 -> case writeInt32ArrayAsInt32X8# mba (i +# 8#) v1 s1 of s2 -> case writeInt32ArrayAsInt32X8# mba (i +# 16#) v2 s2 of s3 -> writeInt32ArrayAsInt32X8# mba (i +# 24#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X32 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt32OffAddrAsInt32X8# addr i s0 of (# s1, v0 #) -> case readInt32OffAddrAsInt32X8# addr (i +# 8#) s1 of (# s2, v1 #) -> case readInt32OffAddrAsInt32X8# addr (i +# 16#) s2 of (# s3, v2 #) -> case readInt32OffAddrAsInt32X8# addr (i +# 24#) s3 of (# s4, v3 #) -> (# s4, MkInt32X32WithVec256 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X32WithVec256 v0 v1 v2 v3) = IO (\s0 -> case writeInt32OffAddrAsInt32X8# addr i v0 s0 of s1 -> case writeInt32OffAddrAsInt32X8# addr (i +# 8#) v1 s1 of s2 -> case writeInt32OffAddrAsInt32X8# addr (i +# 16#) v2 s2 of s3 -> case writeInt32OffAddrAsInt32X8# addr (i +# 24#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X32 Int64 = MkInt64X32WithVec256 Int64X4# Int64X4# Int64X4# Int64X4# Int64X4# Int64X4# Int64X4# Int64X4#
instance PackX32 X32 Int64 where
  mkX32 (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) (I64# x8) (I64# x9) (I64# x10) (I64# x11) (I64# x12) (I64# x13) (I64# x14) (I64# x15) (I64# x16) (I64# x17) (I64# x18) (I64# x19) (I64# x20) (I64# x21) (I64# x22) (I64# x23) (I64# x24) (I64# x25) (I64# x26) (I64# x27) (I64# x28) (I64# x29) (I64# x30) (I64# x31) = MkInt64X32WithVec256 (packInt64X4# (# x0, x1, x2, x3 #)) (packInt64X4# (# x4, x5, x6, x7 #)) (packInt64X4# (# x8, x9, x10, x11 #)) (packInt64X4# (# x12, x13, x14, x15 #)) (packInt64X4# (# x16, x17, x18, x19 #)) (packInt64X4# (# x20, x21, x22, x23 #)) (packInt64X4# (# x24, x25, x26, x27 #)) (packInt64X4# (# x28, x29, x30, x31 #))
  unpackX32 (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = case unpackInt64X4# v0 of (# x0, x1, x2, x3 #) -> case unpackInt64X4# v1 of (# x4, x5, x6, x7 #) -> case unpackInt64X4# v2 of (# x8, x9, x10, x11 #) -> case unpackInt64X4# v3 of (# x12, x13, x14, x15 #) -> case unpackInt64X4# v4 of (# x16, x17, x18, x19 #) -> case unpackInt64X4# v5 of (# x20, x21, x22, x23 #) -> case unpackInt64X4# v6 of (# x24, x25, x26, x27 #) -> case unpackInt64X4# v7 of (# x28, x29, x30, x31 #) -> (I64# x0, I64# x1, I64# x2, I64# x3, I64# x4, I64# x5, I64# x6, I64# x7, I64# x8, I64# x9, I64# x10, I64# x11, I64# x12, I64# x13, I64# x14, I64# x15, I64# x16, I64# x17, I64# x18, I64# x19, I64# x20, I64# x21, I64# x22, I64# x23, I64# x24, I64# x25, I64# x26, I64# x27, I64# x28, I64# x29, I64# x30, I64# x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 Int64 where
  broadcast (I64# x) = let !v = broadcastInt64X4# x in MkInt64X32WithVec256 v v v v v v v v
  {-# INLINE broadcast #-}
instance SelectableF X32 Int64 where
  selectF (MkBoolX32 !cond) (MkInt64X32WithVec256 x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X32WithVec256 y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X32WithVec256 (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 4) x1 y1) (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 8) x2 y2) (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 12) x3 y3) (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 16) x4 y4) (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 20) x5 y5) (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 24) x6 y6) (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 28) x7 y7)
  {-# INLINE selectF #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, i16 < 32, i17 < 32, i18 < 32, i19 < 32, i20 < 32, i21 < 32, i22 < 32, i23 < 32, i24 < 32, i25 < 32, i26 < 32, i27 < 32, i28 < 32, i29 < 32, i30 < 32, i31 < 32, ShuffleMany Int64X4# [i0, i1, i2, i3], ShuffleMany Int64X4# [i4, i5, i6, i7], ShuffleMany Int64X4# [i8, i9, i10, i11], ShuffleMany Int64X4# [i12, i13, i14, i15], ShuffleMany Int64X4# [i16, i17, i18, i19], ShuffleMany Int64X4# [i20, i21, i22, i23], ShuffleMany Int64X4# [i24, i25, i26, i27], ShuffleMany Int64X4# [i28, i29, i30, i31]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Int64 where
  unaryShuffle (MkInt64X32WithVec256 x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt64X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19] sources) (shuffleMany# @_ @_ @[i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27] sources) (shuffleMany# @_ @_ @[i28, i29, i30, i31] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, ShuffleMany Int64X4# [i0, i1, i2, i3], ShuffleMany Int64X4# [i4, i5, i6, i7], ShuffleMany Int64X4# [i8, i9, i10, i11], ShuffleMany Int64X4# [i12, i13, i14, i15], ShuffleMany Int64X4# [i16, i17, i18, i19], ShuffleMany Int64X4# [i20, i21, i22, i23], ShuffleMany Int64X4# [i24, i25, i26, i27], ShuffleMany Int64X4# [i28, i29, i30, i31]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Int64 where
  binaryShuffle (MkInt64X32WithVec256 x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X32WithVec256 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt64X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19] sources) (shuffleMany# @_ @_ @[i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27] sources) (shuffleMany# @_ @_ @[i28, i29, i30, i31] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X32 Int64 where
  eqF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX32 $ (fromIntegral (eqInt64X4# u0 v0)) .|. (fromIntegral (eqInt64X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (eqInt64X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (eqInt64X4# u3 v3) `unsafeShiftL` 12) .|. (fromIntegral (eqInt64X4# u4 v4) `unsafeShiftL` 16) .|. (fromIntegral (eqInt64X4# u5 v5) `unsafeShiftL` 20) .|. (fromIntegral (eqInt64X4# u6 v6) `unsafeShiftL` 24) .|. (fromIntegral (eqInt64X4# u7 v7) `unsafeShiftL` 28)
  {-# INLINE eqF #-}
instance OrderedF X32 Int64 where
  ltF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX32 $ (fromIntegral (ltInt64X4# u0 v0)) .|. (fromIntegral (ltInt64X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (ltInt64X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (ltInt64X4# u3 v3) `unsafeShiftL` 12) .|. (fromIntegral (ltInt64X4# u4 v4) `unsafeShiftL` 16) .|. (fromIntegral (ltInt64X4# u5 v5) `unsafeShiftL` 20) .|. (fromIntegral (ltInt64X4# u6 v6) `unsafeShiftL` 24) .|. (fromIntegral (ltInt64X4# u7 v7) `unsafeShiftL` 28)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X32 Int64 where
  minF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X32WithVec256 (minInt64X4# u0 v0) (minInt64X4# u1 v1) (minInt64X4# u2 v2) (minInt64X4# u3 v3) (minInt64X4# u4 v4) (minInt64X4# u5 v5) (minInt64X4# u6 v6) (minInt64X4# u7 v7)
  maxF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X32WithVec256 (maxInt64X4# u0 v0) (maxInt64X4# u1 v1) (maxInt64X4# u2 v2) (maxInt64X4# u3 v3) (maxInt64X4# u4 v4) (maxInt64X4# u5 v5) (maxInt64X4# u6 v6) (maxInt64X4# u7 v7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X32 Int64 where
  plusF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X32WithVec256 (plusInt64X4# u0 v0) (plusInt64X4# u1 v1) (plusInt64X4# u2 v2) (plusInt64X4# u3 v3) (plusInt64X4# u4 v4) (plusInt64X4# u5 v5) (plusInt64X4# u6 v6) (plusInt64X4# u7 v7)
  minusF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X32WithVec256 (minusInt64X4# u0 v0) (minusInt64X4# u1 v1) (minusInt64X4# u2 v2) (minusInt64X4# u3 v3) (minusInt64X4# u4 v4) (minusInt64X4# u5 v5) (minusInt64X4# u6 v6) (minusInt64X4# u7 v7)
  timesF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X32WithVec256 (timesInt64X4# u0 v0) (timesInt64X4# u1 v1) (timesInt64X4# u2 v2) (timesInt64X4# u3 v3) (timesInt64X4# u4 v4) (timesInt64X4# u5 v5) (timesInt64X4# u6 v6) (timesInt64X4# u7 v7)
  negateF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) = MkInt64X32WithVec256 (negateInt64X4# u0) (negateInt64X4# u1) (negateInt64X4# u2) (negateInt64X4# u3) (negateInt64X4# u4) (negateInt64X4# u5) (negateInt64X4# u6) (negateInt64X4# u7)
  absF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) = MkInt64X32WithVec256 (absInt64X4# u0) (absInt64X4# u1) (absInt64X4# u2) (absInt64X4# u3) (absInt64X4# u4) (absInt64X4# u5) (absInt64X4# u6) (absInt64X4# u7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X32 Int64 where
  andF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X32WithVec256 (andInt64X4# u0 v0) (andInt64X4# u1 v1) (andInt64X4# u2 v2) (andInt64X4# u3 v3) (andInt64X4# u4 v4) (andInt64X4# u5 v5) (andInt64X4# u6 v6) (andInt64X4# u7 v7)
  orF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X32WithVec256 (orInt64X4# u0 v0) (orInt64X4# u1 v1) (orInt64X4# u2 v2) (orInt64X4# u3 v3) (orInt64X4# u4 v4) (orInt64X4# u5 v5) (orInt64X4# u6 v6) (orInt64X4# u7 v7)
  xorF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X32WithVec256 (xorInt64X4# u0 v0) (xorInt64X4# u1 v1) (xorInt64X4# u2 v2) (xorInt64X4# u3 v3) (xorInt64X4# u4 v4) (xorInt64X4# u5 v5) (xorInt64X4# u6 v6) (xorInt64X4# u7 v7)
  complementF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) = MkInt64X32WithVec256 (complementInt64X4# u0) (complementInt64X4# u1) (complementInt64X4# u2) (complementInt64X4# u3) (complementInt64X4# u4) (complementInt64X4# u5) (complementInt64X4# u6) (complementInt64X4# u7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X32 Int64 where
  shiftLF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkInt64X32WithVec256 (shiftLInt64X4# u0 i) (shiftLInt64X4# u1 i) (shiftLInt64X4# u2 i) (shiftLInt64X4# u3 i) (shiftLInt64X4# u4 i) (shiftLInt64X4# u5 i) (shiftLInt64X4# u6 i) (shiftLInt64X4# u7 i)
  shiftRF (MkInt64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkInt64X32WithVec256 (shiftRInt64X4# u0 i) (shiftRInt64X4# u1 i) (shiftRInt64X4# u2 i) (shiftRInt64X4# u3 i) (shiftRInt64X4# u4 i) (shiftRInt64X4# u5 i) (shiftRInt64X4# u6 i) (shiftRInt64X4# u7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X32 Int64 where
  enumFromZero = MkInt64X32WithVec256 (packInt64X4# (# 0#Int64, 1#Int64, 2#Int64, 3#Int64 #)) (packInt64X4# (# 4#Int64, 5#Int64, 6#Int64, 7#Int64 #)) (packInt64X4# (# 8#Int64, 9#Int64, 10#Int64, 11#Int64 #)) (packInt64X4# (# 12#Int64, 13#Int64, 14#Int64, 15#Int64 #)) (packInt64X4# (# 16#Int64, 17#Int64, 18#Int64, 19#Int64 #)) (packInt64X4# (# 20#Int64, 21#Int64, 22#Int64, 23#Int64 #)) (packInt64X4# (# 24#Int64, 25#Int64, 26#Int64, 27#Int64 #)) (packInt64X4# (# 28#Int64, 29#Int64, 30#Int64, 31#Int64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X32 Int64 where
  indexByteArraySIMD# ba i = MkInt64X32WithVec256 (indexInt64ArrayAsInt64X4# ba i) (indexInt64ArrayAsInt64X4# ba (i +# 4#)) (indexInt64ArrayAsInt64X4# ba (i +# 8#)) (indexInt64ArrayAsInt64X4# ba (i +# 12#)) (indexInt64ArrayAsInt64X4# ba (i +# 16#)) (indexInt64ArrayAsInt64X4# ba (i +# 20#)) (indexInt64ArrayAsInt64X4# ba (i +# 24#)) (indexInt64ArrayAsInt64X4# ba (i +# 28#))
  readByteArraySIMD# mba i s0 = case readInt64ArrayAsInt64X4# mba i s0 of (# s1, v0 #) -> case readInt64ArrayAsInt64X4# mba (i +# 4#) s1 of (# s2, v1 #) -> case readInt64ArrayAsInt64X4# mba (i +# 8#) s2 of (# s3, v2 #) -> case readInt64ArrayAsInt64X4# mba (i +# 12#) s3 of (# s4, v3 #) -> case readInt64ArrayAsInt64X4# mba (i +# 16#) s4 of (# s5, v4 #) -> case readInt64ArrayAsInt64X4# mba (i +# 20#) s5 of (# s6, v5 #) -> case readInt64ArrayAsInt64X4# mba (i +# 24#) s6 of (# s7, v6 #) -> case readInt64ArrayAsInt64X4# mba (i +# 28#) s7 of (# s8, v7 #) -> (# s8, MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7 #)
  writeByteArraySIMD# mba i (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) s0 = case writeInt64ArrayAsInt64X4# mba i v0 s0 of s1 -> case writeInt64ArrayAsInt64X4# mba (i +# 4#) v1 s1 of s2 -> case writeInt64ArrayAsInt64X4# mba (i +# 8#) v2 s2 of s3 -> case writeInt64ArrayAsInt64X4# mba (i +# 12#) v3 s3 of s4 -> case writeInt64ArrayAsInt64X4# mba (i +# 16#) v4 s4 of s5 -> case writeInt64ArrayAsInt64X4# mba (i +# 20#) v5 s5 of s6 -> case writeInt64ArrayAsInt64X4# mba (i +# 24#) v6 s6 of s7 -> writeInt64ArrayAsInt64X4# mba (i +# 28#) v7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X32 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt64OffAddrAsInt64X4# addr i s0 of (# s1, v0 #) -> case readInt64OffAddrAsInt64X4# addr (i +# 4#) s1 of (# s2, v1 #) -> case readInt64OffAddrAsInt64X4# addr (i +# 8#) s2 of (# s3, v2 #) -> case readInt64OffAddrAsInt64X4# addr (i +# 12#) s3 of (# s4, v3 #) -> case readInt64OffAddrAsInt64X4# addr (i +# 16#) s4 of (# s5, v4 #) -> case readInt64OffAddrAsInt64X4# addr (i +# 20#) s5 of (# s6, v5 #) -> case readInt64OffAddrAsInt64X4# addr (i +# 24#) s6 of (# s7, v6 #) -> case readInt64OffAddrAsInt64X4# addr (i +# 28#) s7 of (# s8, v7 #) -> (# s8, MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = IO (\s0 -> case writeInt64OffAddrAsInt64X4# addr i v0 s0 of s1 -> case writeInt64OffAddrAsInt64X4# addr (i +# 4#) v1 s1 of s2 -> case writeInt64OffAddrAsInt64X4# addr (i +# 8#) v2 s2 of s3 -> case writeInt64OffAddrAsInt64X4# addr (i +# 12#) v3 s3 of s4 -> case writeInt64OffAddrAsInt64X4# addr (i +# 16#) v4 s4 of s5 -> case writeInt64OffAddrAsInt64X4# addr (i +# 20#) v5 s5 of s6 -> case writeInt64OffAddrAsInt64X4# addr (i +# 24#) v6 s6 of s7 -> case writeInt64OffAddrAsInt64X4# addr (i +# 28#) v7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X32 Word8 = MkWord8X32 Word8X32#
instance PackX32 X32 Word8 where
  mkX32 (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) (W8# x8) (W8# x9) (W8# x10) (W8# x11) (W8# x12) (W8# x13) (W8# x14) (W8# x15) (W8# x16) (W8# x17) (W8# x18) (W8# x19) (W8# x20) (W8# x21) (W8# x22) (W8# x23) (W8# x24) (W8# x25) (W8# x26) (W8# x27) (W8# x28) (W8# x29) (W8# x30) (W8# x31) = MkWord8X32 (packWord8X32# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #))
  unpackX32 (MkWord8X32 v0) = case unpackWord8X32# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #) -> (W8# x0, W8# x1, W8# x2, W8# x3, W8# x4, W8# x5, W8# x6, W8# x7, W8# x8, W8# x9, W8# x10, W8# x11, W8# x12, W8# x13, W8# x14, W8# x15, W8# x16, W8# x17, W8# x18, W8# x19, W8# x20, W8# x21, W8# x22, W8# x23, W8# x24, W8# x25, W8# x26, W8# x27, W8# x28, W8# x29, W8# x30, W8# x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 Word8 where
  broadcast (W8# x) = MkWord8X32 (broadcastWord8X32# x)
  {-# INLINE broadcast #-}
instance SelectableF X32 Word8 where
  selectF (MkBoolX32 !cond) (MkWord8X32 x0) (MkWord8X32 y0) = MkWord8X32 (selectWord8X32# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 32, ShuffleMany Word8X32# indices) => UnaryShuffle indices X32 Word8 where
  unaryShuffle (MkWord8X32 x) = MkWord8X32 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 64, ShuffleMany Word8X32# indices) => BinaryShuffle indices X32 Word8 where
  binaryShuffle (MkWord8X32 x0) (MkWord8X32 x1) = MkWord8X32 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X32 Word8 where
  eqF (MkWord8X32 u0) (MkWord8X32 v0) = MkBoolX32 $ (fromIntegral (eqWord8X32# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X32 Word8 where
  ltF (MkWord8X32 u0) (MkWord8X32 v0) = MkBoolX32 $ (fromIntegral (ltWord8X32# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X32 Word8 where
  minF (MkWord8X32 u0) (MkWord8X32 v0) = MkWord8X32 (minWord8X32# u0 v0)
  maxF (MkWord8X32 u0) (MkWord8X32 v0) = MkWord8X32 (maxWord8X32# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X32 Word8 where
  plusF (MkWord8X32 u0) (MkWord8X32 v0) = MkWord8X32 (plusWord8X32# u0 v0)
  minusF (MkWord8X32 u0) (MkWord8X32 v0) = MkWord8X32 (minusWord8X32# u0 v0)
  timesF (MkWord8X32 u0) (MkWord8X32 v0) = MkWord8X32 (timesWord8X32# u0 v0)
  -- Currently, there is no negateWord8X32#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X32 Word8 where
  andF (MkWord8X32 u0) (MkWord8X32 v0) = MkWord8X32 (andWord8X32# u0 v0)
  orF (MkWord8X32 u0) (MkWord8X32 v0) = MkWord8X32 (orWord8X32# u0 v0)
  xorF (MkWord8X32 u0) (MkWord8X32 v0) = MkWord8X32 (xorWord8X32# u0 v0)
  complementF (MkWord8X32 u0) = MkWord8X32 (complementWord8X32# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X32 Word8 where
  shiftLF (MkWord8X32 u0) (I# i) = MkWord8X32 (shiftLWord8X32# u0 i)
  shiftRF (MkWord8X32 u0) (I# i) = MkWord8X32 (shiftRWord8X32# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X32 Word8 where
  enumFromZero = MkWord8X32 (packWord8X32# (# 0#Word8, 1#Word8, 2#Word8, 3#Word8, 4#Word8, 5#Word8, 6#Word8, 7#Word8, 8#Word8, 9#Word8, 10#Word8, 11#Word8, 12#Word8, 13#Word8, 14#Word8, 15#Word8, 16#Word8, 17#Word8, 18#Word8, 19#Word8, 20#Word8, 21#Word8, 22#Word8, 23#Word8, 24#Word8, 25#Word8, 26#Word8, 27#Word8, 28#Word8, 29#Word8, 30#Word8, 31#Word8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X32 Word8 where
  indexByteArraySIMD# ba i = MkWord8X32 (indexWord8ArrayAsWord8X32# ba i)
  readByteArraySIMD# mba i s0 = case readWord8ArrayAsWord8X32# mba i s0 of (# s1, v0 #) -> (# s1, MkWord8X32 v0 #)
  writeByteArraySIMD# mba i (MkWord8X32 v0) s0 = writeWord8ArrayAsWord8X32# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X32 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord8OffAddrAsWord8X32# addr i s0 of (# s1, v0 #) -> (# s1, MkWord8X32 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X32 v0) = IO (\s0 -> case writeWord8OffAddrAsWord8X32# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X32 Word16 = MkWord16X32WithVec256 Word16X16# Word16X16#
instance PackX32 X32 Word16 where
  mkX32 (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) (W16# x8) (W16# x9) (W16# x10) (W16# x11) (W16# x12) (W16# x13) (W16# x14) (W16# x15) (W16# x16) (W16# x17) (W16# x18) (W16# x19) (W16# x20) (W16# x21) (W16# x22) (W16# x23) (W16# x24) (W16# x25) (W16# x26) (W16# x27) (W16# x28) (W16# x29) (W16# x30) (W16# x31) = MkWord16X32WithVec256 (packWord16X16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #)) (packWord16X16# (# x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #))
  unpackX32 (MkWord16X32WithVec256 v0 v1) = case unpackWord16X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackWord16X16# v1 of (# x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #) -> (W16# x0, W16# x1, W16# x2, W16# x3, W16# x4, W16# x5, W16# x6, W16# x7, W16# x8, W16# x9, W16# x10, W16# x11, W16# x12, W16# x13, W16# x14, W16# x15, W16# x16, W16# x17, W16# x18, W16# x19, W16# x20, W16# x21, W16# x22, W16# x23, W16# x24, W16# x25, W16# x26, W16# x27, W16# x28, W16# x29, W16# x30, W16# x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 Word16 where
  broadcast (W16# x) = let !v = broadcastWord16X16# x in MkWord16X32WithVec256 v v
  {-# INLINE broadcast #-}
instance SelectableF X32 Word16 where
  selectF (MkBoolX32 !cond) (MkWord16X32WithVec256 x0 x1) (MkWord16X32WithVec256 y0 y1) = MkWord16X32WithVec256 (selectWord16X16# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord16X16# (fromIntegral $ cond `unsafeShiftR` 16) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, i16 < 32, i17 < 32, i18 < 32, i19 < 32, i20 < 32, i21 < 32, i22 < 32, i23 < 32, i24 < 32, i25 < 32, i26 < 32, i27 < 32, i28 < 32, i29 < 32, i30 < 32, i31 < 32, ShuffleMany Word16X16# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Word16X16# [i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Word16 where
  unaryShuffle (MkWord16X32WithVec256 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord16X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, ShuffleMany Word16X16# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Word16X16# [i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Word16 where
  binaryShuffle (MkWord16X32WithVec256 x0 x1) (MkWord16X32WithVec256 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord16X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X32 Word16 where
  eqF (MkWord16X32WithVec256 u0 u1) (MkWord16X32WithVec256 v0 v1) = MkBoolX32 $ (fromIntegral (eqWord16X16# u0 v0)) .|. (fromIntegral (eqWord16X16# u1 v1) `unsafeShiftL` 16)
  {-# INLINE eqF #-}
instance OrderedF X32 Word16 where
  ltF (MkWord16X32WithVec256 u0 u1) (MkWord16X32WithVec256 v0 v1) = MkBoolX32 $ (fromIntegral (ltWord16X16# u0 v0)) .|. (fromIntegral (ltWord16X16# u1 v1) `unsafeShiftL` 16)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X32 Word16 where
  minF (MkWord16X32WithVec256 u0 u1) (MkWord16X32WithVec256 v0 v1) = MkWord16X32WithVec256 (minWord16X16# u0 v0) (minWord16X16# u1 v1)
  maxF (MkWord16X32WithVec256 u0 u1) (MkWord16X32WithVec256 v0 v1) = MkWord16X32WithVec256 (maxWord16X16# u0 v0) (maxWord16X16# u1 v1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X32 Word16 where
  plusF (MkWord16X32WithVec256 u0 u1) (MkWord16X32WithVec256 v0 v1) = MkWord16X32WithVec256 (plusWord16X16# u0 v0) (plusWord16X16# u1 v1)
  minusF (MkWord16X32WithVec256 u0 u1) (MkWord16X32WithVec256 v0 v1) = MkWord16X32WithVec256 (minusWord16X16# u0 v0) (minusWord16X16# u1 v1)
  timesF (MkWord16X32WithVec256 u0 u1) (MkWord16X32WithVec256 v0 v1) = MkWord16X32WithVec256 (timesWord16X16# u0 v0) (timesWord16X16# u1 v1)
  -- Currently, there is no negateWord16X16#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X32 Word16 where
  andF (MkWord16X32WithVec256 u0 u1) (MkWord16X32WithVec256 v0 v1) = MkWord16X32WithVec256 (andWord16X16# u0 v0) (andWord16X16# u1 v1)
  orF (MkWord16X32WithVec256 u0 u1) (MkWord16X32WithVec256 v0 v1) = MkWord16X32WithVec256 (orWord16X16# u0 v0) (orWord16X16# u1 v1)
  xorF (MkWord16X32WithVec256 u0 u1) (MkWord16X32WithVec256 v0 v1) = MkWord16X32WithVec256 (xorWord16X16# u0 v0) (xorWord16X16# u1 v1)
  complementF (MkWord16X32WithVec256 u0 u1) = MkWord16X32WithVec256 (complementWord16X16# u0) (complementWord16X16# u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X32 Word16 where
  shiftLF (MkWord16X32WithVec256 u0 u1) (I# i) = MkWord16X32WithVec256 (shiftLWord16X16# u0 i) (shiftLWord16X16# u1 i)
  shiftRF (MkWord16X32WithVec256 u0 u1) (I# i) = MkWord16X32WithVec256 (shiftRWord16X16# u0 i) (shiftRWord16X16# u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X32 Word16 where
  enumFromZero = MkWord16X32WithVec256 (packWord16X16# (# 0#Word16, 1#Word16, 2#Word16, 3#Word16, 4#Word16, 5#Word16, 6#Word16, 7#Word16, 8#Word16, 9#Word16, 10#Word16, 11#Word16, 12#Word16, 13#Word16, 14#Word16, 15#Word16 #)) (packWord16X16# (# 16#Word16, 17#Word16, 18#Word16, 19#Word16, 20#Word16, 21#Word16, 22#Word16, 23#Word16, 24#Word16, 25#Word16, 26#Word16, 27#Word16, 28#Word16, 29#Word16, 30#Word16, 31#Word16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X32 Word16 where
  indexByteArraySIMD# ba i = MkWord16X32WithVec256 (indexWord16ArrayAsWord16X16# ba i) (indexWord16ArrayAsWord16X16# ba (i +# 16#))
  readByteArraySIMD# mba i s0 = case readWord16ArrayAsWord16X16# mba i s0 of (# s1, v0 #) -> case readWord16ArrayAsWord16X16# mba (i +# 16#) s1 of (# s2, v1 #) -> (# s2, MkWord16X32WithVec256 v0 v1 #)
  writeByteArraySIMD# mba i (MkWord16X32WithVec256 v0 v1) s0 = case writeWord16ArrayAsWord16X16# mba i v0 s0 of s1 -> writeWord16ArrayAsWord16X16# mba (i +# 16#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X32 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord16OffAddrAsWord16X16# addr i s0 of (# s1, v0 #) -> case readWord16OffAddrAsWord16X16# addr (i +# 16#) s1 of (# s2, v1 #) -> (# s2, MkWord16X32WithVec256 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X32WithVec256 v0 v1) = IO (\s0 -> case writeWord16OffAddrAsWord16X16# addr i v0 s0 of s1 -> case writeWord16OffAddrAsWord16X16# addr (i +# 16#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X32 Word32 = MkWord32X32WithVec256 Word32X8# Word32X8# Word32X8# Word32X8#
instance PackX32 X32 Word32 where
  mkX32 (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) (W32# x8) (W32# x9) (W32# x10) (W32# x11) (W32# x12) (W32# x13) (W32# x14) (W32# x15) (W32# x16) (W32# x17) (W32# x18) (W32# x19) (W32# x20) (W32# x21) (W32# x22) (W32# x23) (W32# x24) (W32# x25) (W32# x26) (W32# x27) (W32# x28) (W32# x29) (W32# x30) (W32# x31) = MkWord32X32WithVec256 (packWord32X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packWord32X8# (# x8, x9, x10, x11, x12, x13, x14, x15 #)) (packWord32X8# (# x16, x17, x18, x19, x20, x21, x22, x23 #)) (packWord32X8# (# x24, x25, x26, x27, x28, x29, x30, x31 #))
  unpackX32 (MkWord32X32WithVec256 v0 v1 v2 v3) = case unpackWord32X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackWord32X8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackWord32X8# v2 of (# x16, x17, x18, x19, x20, x21, x22, x23 #) -> case unpackWord32X8# v3 of (# x24, x25, x26, x27, x28, x29, x30, x31 #) -> (W32# x0, W32# x1, W32# x2, W32# x3, W32# x4, W32# x5, W32# x6, W32# x7, W32# x8, W32# x9, W32# x10, W32# x11, W32# x12, W32# x13, W32# x14, W32# x15, W32# x16, W32# x17, W32# x18, W32# x19, W32# x20, W32# x21, W32# x22, W32# x23, W32# x24, W32# x25, W32# x26, W32# x27, W32# x28, W32# x29, W32# x30, W32# x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 Word32 where
  broadcast (W32# x) = let !v = broadcastWord32X8# x in MkWord32X32WithVec256 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X32 Word32 where
  selectF (MkBoolX32 !cond) (MkWord32X32WithVec256 x0 x1 x2 x3) (MkWord32X32WithVec256 y0 y1 y2 y3) = MkWord32X32WithVec256 (selectWord32X8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord32X8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1) (selectWord32X8# (fromIntegral $ cond `unsafeShiftR` 16) x2 y2) (selectWord32X8# (fromIntegral $ cond `unsafeShiftR` 24) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, i16 < 32, i17 < 32, i18 < 32, i19 < 32, i20 < 32, i21 < 32, i22 < 32, i23 < 32, i24 < 32, i25 < 32, i26 < 32, i27 < 32, i28 < 32, i29 < 32, i30 < 32, i31 < 32, ShuffleMany Word32X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Word32X8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Word32X8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany Word32X8# [i24, i25, i26, i27, i28, i29, i30, i31]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Word32 where
  unaryShuffle (MkWord32X32WithVec256 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord32X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, ShuffleMany Word32X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Word32X8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Word32X8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany Word32X8# [i24, i25, i26, i27, i28, i29, i30, i31]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Word32 where
  binaryShuffle (MkWord32X32WithVec256 x0 x1 x2 x3) (MkWord32X32WithVec256 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord32X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X32 Word32 where
  eqF (MkWord32X32WithVec256 u0 u1 u2 u3) (MkWord32X32WithVec256 v0 v1 v2 v3) = MkBoolX32 $ (fromIntegral (eqWord32X8# u0 v0)) .|. (fromIntegral (eqWord32X8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (eqWord32X8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (eqWord32X8# u3 v3) `unsafeShiftL` 24)
  {-# INLINE eqF #-}
instance OrderedF X32 Word32 where
  ltF (MkWord32X32WithVec256 u0 u1 u2 u3) (MkWord32X32WithVec256 v0 v1 v2 v3) = MkBoolX32 $ (fromIntegral (ltWord32X8# u0 v0)) .|. (fromIntegral (ltWord32X8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (ltWord32X8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (ltWord32X8# u3 v3) `unsafeShiftL` 24)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X32 Word32 where
  minF (MkWord32X32WithVec256 u0 u1 u2 u3) (MkWord32X32WithVec256 v0 v1 v2 v3) = MkWord32X32WithVec256 (minWord32X8# u0 v0) (minWord32X8# u1 v1) (minWord32X8# u2 v2) (minWord32X8# u3 v3)
  maxF (MkWord32X32WithVec256 u0 u1 u2 u3) (MkWord32X32WithVec256 v0 v1 v2 v3) = MkWord32X32WithVec256 (maxWord32X8# u0 v0) (maxWord32X8# u1 v1) (maxWord32X8# u2 v2) (maxWord32X8# u3 v3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X32 Word32 where
  plusF (MkWord32X32WithVec256 u0 u1 u2 u3) (MkWord32X32WithVec256 v0 v1 v2 v3) = MkWord32X32WithVec256 (plusWord32X8# u0 v0) (plusWord32X8# u1 v1) (plusWord32X8# u2 v2) (plusWord32X8# u3 v3)
  minusF (MkWord32X32WithVec256 u0 u1 u2 u3) (MkWord32X32WithVec256 v0 v1 v2 v3) = MkWord32X32WithVec256 (minusWord32X8# u0 v0) (minusWord32X8# u1 v1) (minusWord32X8# u2 v2) (minusWord32X8# u3 v3)
  timesF (MkWord32X32WithVec256 u0 u1 u2 u3) (MkWord32X32WithVec256 v0 v1 v2 v3) = MkWord32X32WithVec256 (timesWord32X8# u0 v0) (timesWord32X8# u1 v1) (timesWord32X8# u2 v2) (timesWord32X8# u3 v3)
  -- Currently, there is no negateWord32X8#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X32 Word32 where
  andF (MkWord32X32WithVec256 u0 u1 u2 u3) (MkWord32X32WithVec256 v0 v1 v2 v3) = MkWord32X32WithVec256 (andWord32X8# u0 v0) (andWord32X8# u1 v1) (andWord32X8# u2 v2) (andWord32X8# u3 v3)
  orF (MkWord32X32WithVec256 u0 u1 u2 u3) (MkWord32X32WithVec256 v0 v1 v2 v3) = MkWord32X32WithVec256 (orWord32X8# u0 v0) (orWord32X8# u1 v1) (orWord32X8# u2 v2) (orWord32X8# u3 v3)
  xorF (MkWord32X32WithVec256 u0 u1 u2 u3) (MkWord32X32WithVec256 v0 v1 v2 v3) = MkWord32X32WithVec256 (xorWord32X8# u0 v0) (xorWord32X8# u1 v1) (xorWord32X8# u2 v2) (xorWord32X8# u3 v3)
  complementF (MkWord32X32WithVec256 u0 u1 u2 u3) = MkWord32X32WithVec256 (complementWord32X8# u0) (complementWord32X8# u1) (complementWord32X8# u2) (complementWord32X8# u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X32 Word32 where
  shiftLF (MkWord32X32WithVec256 u0 u1 u2 u3) (I# i) = MkWord32X32WithVec256 (shiftLWord32X8# u0 i) (shiftLWord32X8# u1 i) (shiftLWord32X8# u2 i) (shiftLWord32X8# u3 i)
  shiftRF (MkWord32X32WithVec256 u0 u1 u2 u3) (I# i) = MkWord32X32WithVec256 (shiftRWord32X8# u0 i) (shiftRWord32X8# u1 i) (shiftRWord32X8# u2 i) (shiftRWord32X8# u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X32 Word32 where
  enumFromZero = MkWord32X32WithVec256 (packWord32X8# (# 0#Word32, 1#Word32, 2#Word32, 3#Word32, 4#Word32, 5#Word32, 6#Word32, 7#Word32 #)) (packWord32X8# (# 8#Word32, 9#Word32, 10#Word32, 11#Word32, 12#Word32, 13#Word32, 14#Word32, 15#Word32 #)) (packWord32X8# (# 16#Word32, 17#Word32, 18#Word32, 19#Word32, 20#Word32, 21#Word32, 22#Word32, 23#Word32 #)) (packWord32X8# (# 24#Word32, 25#Word32, 26#Word32, 27#Word32, 28#Word32, 29#Word32, 30#Word32, 31#Word32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X32 Word32 where
  indexByteArraySIMD# ba i = MkWord32X32WithVec256 (indexWord32ArrayAsWord32X8# ba i) (indexWord32ArrayAsWord32X8# ba (i +# 8#)) (indexWord32ArrayAsWord32X8# ba (i +# 16#)) (indexWord32ArrayAsWord32X8# ba (i +# 24#))
  readByteArraySIMD# mba i s0 = case readWord32ArrayAsWord32X8# mba i s0 of (# s1, v0 #) -> case readWord32ArrayAsWord32X8# mba (i +# 8#) s1 of (# s2, v1 #) -> case readWord32ArrayAsWord32X8# mba (i +# 16#) s2 of (# s3, v2 #) -> case readWord32ArrayAsWord32X8# mba (i +# 24#) s3 of (# s4, v3 #) -> (# s4, MkWord32X32WithVec256 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkWord32X32WithVec256 v0 v1 v2 v3) s0 = case writeWord32ArrayAsWord32X8# mba i v0 s0 of s1 -> case writeWord32ArrayAsWord32X8# mba (i +# 8#) v1 s1 of s2 -> case writeWord32ArrayAsWord32X8# mba (i +# 16#) v2 s2 of s3 -> writeWord32ArrayAsWord32X8# mba (i +# 24#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X32 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord32OffAddrAsWord32X8# addr i s0 of (# s1, v0 #) -> case readWord32OffAddrAsWord32X8# addr (i +# 8#) s1 of (# s2, v1 #) -> case readWord32OffAddrAsWord32X8# addr (i +# 16#) s2 of (# s3, v2 #) -> case readWord32OffAddrAsWord32X8# addr (i +# 24#) s3 of (# s4, v3 #) -> (# s4, MkWord32X32WithVec256 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X32WithVec256 v0 v1 v2 v3) = IO (\s0 -> case writeWord32OffAddrAsWord32X8# addr i v0 s0 of s1 -> case writeWord32OffAddrAsWord32X8# addr (i +# 8#) v1 s1 of s2 -> case writeWord32OffAddrAsWord32X8# addr (i +# 16#) v2 s2 of s3 -> case writeWord32OffAddrAsWord32X8# addr (i +# 24#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X32 Word64 = MkWord64X32WithVec256 Word64X4# Word64X4# Word64X4# Word64X4# Word64X4# Word64X4# Word64X4# Word64X4#
instance PackX32 X32 Word64 where
  mkX32 (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) (W64# x8) (W64# x9) (W64# x10) (W64# x11) (W64# x12) (W64# x13) (W64# x14) (W64# x15) (W64# x16) (W64# x17) (W64# x18) (W64# x19) (W64# x20) (W64# x21) (W64# x22) (W64# x23) (W64# x24) (W64# x25) (W64# x26) (W64# x27) (W64# x28) (W64# x29) (W64# x30) (W64# x31) = MkWord64X32WithVec256 (packWord64X4# (# x0, x1, x2, x3 #)) (packWord64X4# (# x4, x5, x6, x7 #)) (packWord64X4# (# x8, x9, x10, x11 #)) (packWord64X4# (# x12, x13, x14, x15 #)) (packWord64X4# (# x16, x17, x18, x19 #)) (packWord64X4# (# x20, x21, x22, x23 #)) (packWord64X4# (# x24, x25, x26, x27 #)) (packWord64X4# (# x28, x29, x30, x31 #))
  unpackX32 (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = case unpackWord64X4# v0 of (# x0, x1, x2, x3 #) -> case unpackWord64X4# v1 of (# x4, x5, x6, x7 #) -> case unpackWord64X4# v2 of (# x8, x9, x10, x11 #) -> case unpackWord64X4# v3 of (# x12, x13, x14, x15 #) -> case unpackWord64X4# v4 of (# x16, x17, x18, x19 #) -> case unpackWord64X4# v5 of (# x20, x21, x22, x23 #) -> case unpackWord64X4# v6 of (# x24, x25, x26, x27 #) -> case unpackWord64X4# v7 of (# x28, x29, x30, x31 #) -> (W64# x0, W64# x1, W64# x2, W64# x3, W64# x4, W64# x5, W64# x6, W64# x7, W64# x8, W64# x9, W64# x10, W64# x11, W64# x12, W64# x13, W64# x14, W64# x15, W64# x16, W64# x17, W64# x18, W64# x19, W64# x20, W64# x21, W64# x22, W64# x23, W64# x24, W64# x25, W64# x26, W64# x27, W64# x28, W64# x29, W64# x30, W64# x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 Word64 where
  broadcast (W64# x) = let !v = broadcastWord64X4# x in MkWord64X32WithVec256 v v v v v v v v
  {-# INLINE broadcast #-}
instance SelectableF X32 Word64 where
  selectF (MkBoolX32 !cond) (MkWord64X32WithVec256 x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X32WithVec256 y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X32WithVec256 (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 4) x1 y1) (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 8) x2 y2) (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 12) x3 y3) (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 16) x4 y4) (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 20) x5 y5) (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 24) x6 y6) (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 28) x7 y7)
  {-# INLINE selectF #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, i16 < 32, i17 < 32, i18 < 32, i19 < 32, i20 < 32, i21 < 32, i22 < 32, i23 < 32, i24 < 32, i25 < 32, i26 < 32, i27 < 32, i28 < 32, i29 < 32, i30 < 32, i31 < 32, ShuffleMany Word64X4# [i0, i1, i2, i3], ShuffleMany Word64X4# [i4, i5, i6, i7], ShuffleMany Word64X4# [i8, i9, i10, i11], ShuffleMany Word64X4# [i12, i13, i14, i15], ShuffleMany Word64X4# [i16, i17, i18, i19], ShuffleMany Word64X4# [i20, i21, i22, i23], ShuffleMany Word64X4# [i24, i25, i26, i27], ShuffleMany Word64X4# [i28, i29, i30, i31]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Word64 where
  unaryShuffle (MkWord64X32WithVec256 x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord64X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19] sources) (shuffleMany# @_ @_ @[i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27] sources) (shuffleMany# @_ @_ @[i28, i29, i30, i31] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, ShuffleMany Word64X4# [i0, i1, i2, i3], ShuffleMany Word64X4# [i4, i5, i6, i7], ShuffleMany Word64X4# [i8, i9, i10, i11], ShuffleMany Word64X4# [i12, i13, i14, i15], ShuffleMany Word64X4# [i16, i17, i18, i19], ShuffleMany Word64X4# [i20, i21, i22, i23], ShuffleMany Word64X4# [i24, i25, i26, i27], ShuffleMany Word64X4# [i28, i29, i30, i31]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 Word64 where
  binaryShuffle (MkWord64X32WithVec256 x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X32WithVec256 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord64X32WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19] sources) (shuffleMany# @_ @_ @[i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27] sources) (shuffleMany# @_ @_ @[i28, i29, i30, i31] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X32 Word64 where
  eqF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX32 $ (fromIntegral (eqWord64X4# u0 v0)) .|. (fromIntegral (eqWord64X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (eqWord64X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (eqWord64X4# u3 v3) `unsafeShiftL` 12) .|. (fromIntegral (eqWord64X4# u4 v4) `unsafeShiftL` 16) .|. (fromIntegral (eqWord64X4# u5 v5) `unsafeShiftL` 20) .|. (fromIntegral (eqWord64X4# u6 v6) `unsafeShiftL` 24) .|. (fromIntegral (eqWord64X4# u7 v7) `unsafeShiftL` 28)
  {-# INLINE eqF #-}
instance OrderedF X32 Word64 where
  ltF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX32 $ (fromIntegral (ltWord64X4# u0 v0)) .|. (fromIntegral (ltWord64X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (ltWord64X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (ltWord64X4# u3 v3) `unsafeShiftL` 12) .|. (fromIntegral (ltWord64X4# u4 v4) `unsafeShiftL` 16) .|. (fromIntegral (ltWord64X4# u5 v5) `unsafeShiftL` 20) .|. (fromIntegral (ltWord64X4# u6 v6) `unsafeShiftL` 24) .|. (fromIntegral (ltWord64X4# u7 v7) `unsafeShiftL` 28)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X32 Word64 where
  minF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X32WithVec256 (minWord64X4# u0 v0) (minWord64X4# u1 v1) (minWord64X4# u2 v2) (minWord64X4# u3 v3) (minWord64X4# u4 v4) (minWord64X4# u5 v5) (minWord64X4# u6 v6) (minWord64X4# u7 v7)
  maxF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X32WithVec256 (maxWord64X4# u0 v0) (maxWord64X4# u1 v1) (maxWord64X4# u2 v2) (maxWord64X4# u3 v3) (maxWord64X4# u4 v4) (maxWord64X4# u5 v5) (maxWord64X4# u6 v6) (maxWord64X4# u7 v7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X32 Word64 where
  plusF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X32WithVec256 (plusWord64X4# u0 v0) (plusWord64X4# u1 v1) (plusWord64X4# u2 v2) (plusWord64X4# u3 v3) (plusWord64X4# u4 v4) (plusWord64X4# u5 v5) (plusWord64X4# u6 v6) (plusWord64X4# u7 v7)
  minusF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X32WithVec256 (minusWord64X4# u0 v0) (minusWord64X4# u1 v1) (minusWord64X4# u2 v2) (minusWord64X4# u3 v3) (minusWord64X4# u4 v4) (minusWord64X4# u5 v5) (minusWord64X4# u6 v6) (minusWord64X4# u7 v7)
  timesF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X32WithVec256 (timesWord64X4# u0 v0) (timesWord64X4# u1 v1) (timesWord64X4# u2 v2) (timesWord64X4# u3 v3) (timesWord64X4# u4 v4) (timesWord64X4# u5 v5) (timesWord64X4# u6 v6) (timesWord64X4# u7 v7)
  -- Currently, there is no negateWord64X4#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X32 Word64 where
  andF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X32WithVec256 (andWord64X4# u0 v0) (andWord64X4# u1 v1) (andWord64X4# u2 v2) (andWord64X4# u3 v3) (andWord64X4# u4 v4) (andWord64X4# u5 v5) (andWord64X4# u6 v6) (andWord64X4# u7 v7)
  orF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X32WithVec256 (orWord64X4# u0 v0) (orWord64X4# u1 v1) (orWord64X4# u2 v2) (orWord64X4# u3 v3) (orWord64X4# u4 v4) (orWord64X4# u5 v5) (orWord64X4# u6 v6) (orWord64X4# u7 v7)
  xorF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X32WithVec256 (xorWord64X4# u0 v0) (xorWord64X4# u1 v1) (xorWord64X4# u2 v2) (xorWord64X4# u3 v3) (xorWord64X4# u4 v4) (xorWord64X4# u5 v5) (xorWord64X4# u6 v6) (xorWord64X4# u7 v7)
  complementF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) = MkWord64X32WithVec256 (complementWord64X4# u0) (complementWord64X4# u1) (complementWord64X4# u2) (complementWord64X4# u3) (complementWord64X4# u4) (complementWord64X4# u5) (complementWord64X4# u6) (complementWord64X4# u7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X32 Word64 where
  shiftLF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkWord64X32WithVec256 (shiftLWord64X4# u0 i) (shiftLWord64X4# u1 i) (shiftLWord64X4# u2 i) (shiftLWord64X4# u3 i) (shiftLWord64X4# u4 i) (shiftLWord64X4# u5 i) (shiftLWord64X4# u6 i) (shiftLWord64X4# u7 i)
  shiftRF (MkWord64X32WithVec256 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkWord64X32WithVec256 (shiftRWord64X4# u0 i) (shiftRWord64X4# u1 i) (shiftRWord64X4# u2 i) (shiftRWord64X4# u3 i) (shiftRWord64X4# u4 i) (shiftRWord64X4# u5 i) (shiftRWord64X4# u6 i) (shiftRWord64X4# u7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X32 Word64 where
  enumFromZero = MkWord64X32WithVec256 (packWord64X4# (# 0#Word64, 1#Word64, 2#Word64, 3#Word64 #)) (packWord64X4# (# 4#Word64, 5#Word64, 6#Word64, 7#Word64 #)) (packWord64X4# (# 8#Word64, 9#Word64, 10#Word64, 11#Word64 #)) (packWord64X4# (# 12#Word64, 13#Word64, 14#Word64, 15#Word64 #)) (packWord64X4# (# 16#Word64, 17#Word64, 18#Word64, 19#Word64 #)) (packWord64X4# (# 20#Word64, 21#Word64, 22#Word64, 23#Word64 #)) (packWord64X4# (# 24#Word64, 25#Word64, 26#Word64, 27#Word64 #)) (packWord64X4# (# 28#Word64, 29#Word64, 30#Word64, 31#Word64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X32 Word64 where
  indexByteArraySIMD# ba i = MkWord64X32WithVec256 (indexWord64ArrayAsWord64X4# ba i) (indexWord64ArrayAsWord64X4# ba (i +# 4#)) (indexWord64ArrayAsWord64X4# ba (i +# 8#)) (indexWord64ArrayAsWord64X4# ba (i +# 12#)) (indexWord64ArrayAsWord64X4# ba (i +# 16#)) (indexWord64ArrayAsWord64X4# ba (i +# 20#)) (indexWord64ArrayAsWord64X4# ba (i +# 24#)) (indexWord64ArrayAsWord64X4# ba (i +# 28#))
  readByteArraySIMD# mba i s0 = case readWord64ArrayAsWord64X4# mba i s0 of (# s1, v0 #) -> case readWord64ArrayAsWord64X4# mba (i +# 4#) s1 of (# s2, v1 #) -> case readWord64ArrayAsWord64X4# mba (i +# 8#) s2 of (# s3, v2 #) -> case readWord64ArrayAsWord64X4# mba (i +# 12#) s3 of (# s4, v3 #) -> case readWord64ArrayAsWord64X4# mba (i +# 16#) s4 of (# s5, v4 #) -> case readWord64ArrayAsWord64X4# mba (i +# 20#) s5 of (# s6, v5 #) -> case readWord64ArrayAsWord64X4# mba (i +# 24#) s6 of (# s7, v6 #) -> case readWord64ArrayAsWord64X4# mba (i +# 28#) s7 of (# s8, v7 #) -> (# s8, MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7 #)
  writeByteArraySIMD# mba i (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) s0 = case writeWord64ArrayAsWord64X4# mba i v0 s0 of s1 -> case writeWord64ArrayAsWord64X4# mba (i +# 4#) v1 s1 of s2 -> case writeWord64ArrayAsWord64X4# mba (i +# 8#) v2 s2 of s3 -> case writeWord64ArrayAsWord64X4# mba (i +# 12#) v3 s3 of s4 -> case writeWord64ArrayAsWord64X4# mba (i +# 16#) v4 s4 of s5 -> case writeWord64ArrayAsWord64X4# mba (i +# 20#) v5 s5 of s6 -> case writeWord64ArrayAsWord64X4# mba (i +# 24#) v6 s6 of s7 -> writeWord64ArrayAsWord64X4# mba (i +# 28#) v7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X32 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord64OffAddrAsWord64X4# addr i s0 of (# s1, v0 #) -> case readWord64OffAddrAsWord64X4# addr (i +# 4#) s1 of (# s2, v1 #) -> case readWord64OffAddrAsWord64X4# addr (i +# 8#) s2 of (# s3, v2 #) -> case readWord64OffAddrAsWord64X4# addr (i +# 12#) s3 of (# s4, v3 #) -> case readWord64OffAddrAsWord64X4# addr (i +# 16#) s4 of (# s5, v4 #) -> case readWord64OffAddrAsWord64X4# addr (i +# 20#) s5 of (# s6, v5 #) -> case readWord64OffAddrAsWord64X4# addr (i +# 24#) s6 of (# s7, v6 #) -> case readWord64OffAddrAsWord64X4# addr (i +# 28#) s7 of (# s8, v7 #) -> (# s8, MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X32WithVec256 v0 v1 v2 v3 v4 v5 v6 v7) = IO (\s0 -> case writeWord64OffAddrAsWord64X4# addr i v0 s0 of s1 -> case writeWord64OffAddrAsWord64X4# addr (i +# 4#) v1 s1 of s2 -> case writeWord64OffAddrAsWord64X4# addr (i +# 8#) v2 s2 of s3 -> case writeWord64OffAddrAsWord64X4# addr (i +# 12#) v3 s3 of s4 -> case writeWord64OffAddrAsWord64X4# addr (i +# 16#) v4 s4 of s5 -> case writeWord64OffAddrAsWord64X4# addr (i +# 20#) v5 s5 of s6 -> case writeWord64OffAddrAsWord64X4# addr (i +# 24#) v6 s6 of s7 -> case writeWord64OffAddrAsWord64X4# addr (i +# 28#) v7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
newtype instance X32 (Sum a) = MkSumX32 (X32 a)
instance PackX32 X32 a => PackX32 X32 (Sum a) where
  mkX32 = coerce (mkX32 @X32 @a)
  unpackX32 = coerce (unpackX32 @X32 @a)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 a => Broadcast X32 (Sum a) where
  broadcast = coerce (broadcast @X32 @a)
  {-# INLINE broadcast #-}
instance SelectableF X32 a => SelectableF X32 (Sum a) where
  selectF = coerce (selectF @X32 @a)
  {-# INLINE selectF #-}
newtype instance X32 (Product a) = MkProductX32 (X32 a)
instance PackX32 X32 a => PackX32 X32 (Product a) where
  mkX32 = coerce (mkX32 @X32 @a)
  unpackX32 = coerce (unpackX32 @X32 @a)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 a => Broadcast X32 (Product a) where
  broadcast = coerce (broadcast @X32 @a)
  {-# INLINE broadcast #-}
instance SelectableF X32 a => SelectableF X32 (Product a) where
  selectF = coerce (selectF @X32 @a)
  {-# INLINE selectF #-}
newtype instance X32 (Min a) = MkMinX32 (X32 a)
instance PackX32 X32 a => PackX32 X32 (Min a) where
  mkX32 = coerce (mkX32 @X32 @a)
  unpackX32 = coerce (unpackX32 @X32 @a)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 a => Broadcast X32 (Min a) where
  broadcast = coerce (broadcast @X32 @a)
  {-# INLINE broadcast #-}
instance SelectableF X32 a => SelectableF X32 (Min a) where
  selectF = coerce (selectF @X32 @a)
  {-# INLINE selectF #-}
newtype instance X32 (Max a) = MkMaxX32 (X32 a)
instance PackX32 X32 a => PackX32 X32 (Max a) where
  mkX32 = coerce (mkX32 @X32 @a)
  unpackX32 = coerce (unpackX32 @X32 @a)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 a => Broadcast X32 (Max a) where
  broadcast = coerce (broadcast @X32 @a)
  {-# INLINE broadcast #-}
instance SelectableF X32 a => SelectableF X32 (Max a) where
  selectF = coerce (selectF @X32 @a)
  {-# INLINE selectF #-}
data instance X32 (Complex a) = MkComplexX32 !(X32 a) !(X32 a)
instance PackX32 X32 a => PackX32 X32 (Complex a) where
  mkX32 (x0 :+ y0) (x1 :+ y1) (x2 :+ y2) (x3 :+ y3) (x4 :+ y4) (x5 :+ y5) (x6 :+ y6) (x7 :+ y7) (x8 :+ y8) (x9 :+ y9) (x10 :+ y10) (x11 :+ y11) (x12 :+ y12) (x13 :+ y13) (x14 :+ y14) (x15 :+ y15) (x16 :+ y16) (x17 :+ y17) (x18 :+ y18) (x19 :+ y19) (x20 :+ y20) (x21 :+ y21) (x22 :+ y22) (x23 :+ y23) (x24 :+ y24) (x25 :+ y25) (x26 :+ y26) (x27 :+ y27) (x28 :+ y28) (x29 :+ y29) (x30 :+ y30) (x31 :+ y31) = MkComplexX32 (mkX32 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) (mkX32 y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24 y25 y26 y27 y28 y29 y30 y31)
  unpackX32 (MkComplexX32 s t) = case unpackX32 s of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> case unpackX32 t of (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29, y30, y31) -> (x0 :+ y0, x1 :+ y1, x2 :+ y2, x3 :+ y3, x4 :+ y4, x5 :+ y5, x6 :+ y6, x7 :+ y7, x8 :+ y8, x9 :+ y9, x10 :+ y10, x11 :+ y11, x12 :+ y12, x13 :+ y13, x14 :+ y14, x15 :+ y15, x16 :+ y16, x17 :+ y17, x18 :+ y18, x19 :+ y19, x20 :+ y20, x21 :+ y21, x22 :+ y22, x23 :+ y23, x24 :+ y24, x25 :+ y25, x26 :+ y26, x27 :+ y27, x28 :+ y28, x29 :+ y29, x30 :+ y30, x31 :+ y31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 a => Broadcast X32 (Complex a) where
  broadcast (x :+ y) = MkComplexX32 (broadcast x) (broadcast y)
  {-# INLINE broadcast #-}
instance SelectableF X32 a => SelectableF X32 (Complex a) where
  selectF !cond (MkComplexX32 x y) (MkComplexX32 x' y') = MkComplexX32 (selectF cond x x') (selectF cond y y')
  {-# INLINE selectF #-}
data instance X32 () = MkUnitX32
instance PackX32 X32 () where
  mkX32 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = MkUnitX32
  unpackX32 MkUnitX32 = ((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ())
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance Broadcast X32 () where
  broadcast _ = MkUnitX32
  {-# INLINE broadcast #-}
instance SelectableF X32 () where
  selectF _ _ _ = MkUnitX32
  {-# INLINE selectF #-}
data instance X32 (a0, a1) = MkTuple2X32 !(X32 a0) !(X32 a1)
instance (PackX32 X32 a0, PackX32 X32 a1) => PackX32 X32 (a0, a1) where
  mkX32 (x0_0, x0_1) (x1_0, x1_1) (x2_0, x2_1) (x3_0, x3_1) (x4_0, x4_1) (x5_0, x5_1) (x6_0, x6_1) (x7_0, x7_1) (x8_0, x8_1) (x9_0, x9_1) (x10_0, x10_1) (x11_0, x11_1) (x12_0, x12_1) (x13_0, x13_1) (x14_0, x14_1) (x15_0, x15_1) (x16_0, x16_1) (x17_0, x17_1) (x18_0, x18_1) (x19_0, x19_1) (x20_0, x20_1) (x21_0, x21_1) (x22_0, x22_1) (x23_0, x23_1) (x24_0, x24_1) (x25_0, x25_1) (x26_0, x26_1) (x27_0, x27_1) (x28_0, x28_1) (x29_0, x29_1) (x30_0, x30_1) (x31_0, x31_1) = MkTuple2X32 (mkX32 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0 x16_0 x17_0 x18_0 x19_0 x20_0 x21_0 x22_0 x23_0 x24_0 x25_0 x26_0 x27_0 x28_0 x29_0 x30_0 x31_0) (mkX32 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1 x16_1 x17_1 x18_1 x19_1 x20_1 x21_1 x22_1 x23_1 x24_1 x25_1 x26_1 x27_1 x28_1 x29_1 x30_1 x31_1)
  unpackX32 (MkTuple2X32 v0 v1) = case unpackX32 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0, x16_0, x17_0, x18_0, x19_0, x20_0, x21_0, x22_0, x23_0, x24_0, x25_0, x26_0, x27_0, x28_0, x29_0, x30_0, x31_0) -> case unpackX32 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1, x16_1, x17_1, x18_1, x19_1, x20_1, x21_1, x22_1, x23_1, x24_1, x25_1, x26_1, x27_1, x28_1, x29_1, x30_1, x31_1) -> ((x0_0, x0_1), (x1_0, x1_1), (x2_0, x2_1), (x3_0, x3_1), (x4_0, x4_1), (x5_0, x5_1), (x6_0, x6_1), (x7_0, x7_1), (x8_0, x8_1), (x9_0, x9_1), (x10_0, x10_1), (x11_0, x11_1), (x12_0, x12_1), (x13_0, x13_1), (x14_0, x14_1), (x15_0, x15_1), (x16_0, x16_1), (x17_0, x17_1), (x18_0, x18_1), (x19_0, x19_1), (x20_0, x20_1), (x21_0, x21_1), (x22_0, x22_1), (x23_0, x23_1), (x24_0, x24_1), (x25_0, x25_1), (x26_0, x26_1), (x27_0, x27_1), (x28_0, x28_1), (x29_0, x29_1), (x30_0, x30_1), (x31_0, x31_1))
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance (Broadcast X32 a0, Broadcast X32 a1) => Broadcast X32 (a0, a1) where
  broadcast (x0, x1) = MkTuple2X32 (broadcast x0) (broadcast x1)
  {-# INLINE broadcast #-}
instance (SelectableF X32 a0, SelectableF X32 a1) => SelectableF X32 (a0, a1) where
  selectF !cond (MkTuple2X32 x0 x1) (MkTuple2X32 y0 y1) = MkTuple2X32 (selectF cond x0 y0) (selectF cond x1 y1)
  {-# INLINE selectF #-}
data instance X32 (a0, a1, a2) = MkTuple3X32 !(X32 a0) !(X32 a1) !(X32 a2)
instance (PackX32 X32 a0, PackX32 X32 a1, PackX32 X32 a2) => PackX32 X32 (a0, a1, a2) where
  mkX32 (x0_0, x0_1, x0_2) (x1_0, x1_1, x1_2) (x2_0, x2_1, x2_2) (x3_0, x3_1, x3_2) (x4_0, x4_1, x4_2) (x5_0, x5_1, x5_2) (x6_0, x6_1, x6_2) (x7_0, x7_1, x7_2) (x8_0, x8_1, x8_2) (x9_0, x9_1, x9_2) (x10_0, x10_1, x10_2) (x11_0, x11_1, x11_2) (x12_0, x12_1, x12_2) (x13_0, x13_1, x13_2) (x14_0, x14_1, x14_2) (x15_0, x15_1, x15_2) (x16_0, x16_1, x16_2) (x17_0, x17_1, x17_2) (x18_0, x18_1, x18_2) (x19_0, x19_1, x19_2) (x20_0, x20_1, x20_2) (x21_0, x21_1, x21_2) (x22_0, x22_1, x22_2) (x23_0, x23_1, x23_2) (x24_0, x24_1, x24_2) (x25_0, x25_1, x25_2) (x26_0, x26_1, x26_2) (x27_0, x27_1, x27_2) (x28_0, x28_1, x28_2) (x29_0, x29_1, x29_2) (x30_0, x30_1, x30_2) (x31_0, x31_1, x31_2) = MkTuple3X32 (mkX32 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0 x16_0 x17_0 x18_0 x19_0 x20_0 x21_0 x22_0 x23_0 x24_0 x25_0 x26_0 x27_0 x28_0 x29_0 x30_0 x31_0) (mkX32 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1 x16_1 x17_1 x18_1 x19_1 x20_1 x21_1 x22_1 x23_1 x24_1 x25_1 x26_1 x27_1 x28_1 x29_1 x30_1 x31_1) (mkX32 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2 x16_2 x17_2 x18_2 x19_2 x20_2 x21_2 x22_2 x23_2 x24_2 x25_2 x26_2 x27_2 x28_2 x29_2 x30_2 x31_2)
  unpackX32 (MkTuple3X32 v0 v1 v2) = case unpackX32 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0, x16_0, x17_0, x18_0, x19_0, x20_0, x21_0, x22_0, x23_0, x24_0, x25_0, x26_0, x27_0, x28_0, x29_0, x30_0, x31_0) -> case unpackX32 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1, x16_1, x17_1, x18_1, x19_1, x20_1, x21_1, x22_1, x23_1, x24_1, x25_1, x26_1, x27_1, x28_1, x29_1, x30_1, x31_1) -> case unpackX32 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2, x16_2, x17_2, x18_2, x19_2, x20_2, x21_2, x22_2, x23_2, x24_2, x25_2, x26_2, x27_2, x28_2, x29_2, x30_2, x31_2) -> ((x0_0, x0_1, x0_2), (x1_0, x1_1, x1_2), (x2_0, x2_1, x2_2), (x3_0, x3_1, x3_2), (x4_0, x4_1, x4_2), (x5_0, x5_1, x5_2), (x6_0, x6_1, x6_2), (x7_0, x7_1, x7_2), (x8_0, x8_1, x8_2), (x9_0, x9_1, x9_2), (x10_0, x10_1, x10_2), (x11_0, x11_1, x11_2), (x12_0, x12_1, x12_2), (x13_0, x13_1, x13_2), (x14_0, x14_1, x14_2), (x15_0, x15_1, x15_2), (x16_0, x16_1, x16_2), (x17_0, x17_1, x17_2), (x18_0, x18_1, x18_2), (x19_0, x19_1, x19_2), (x20_0, x20_1, x20_2), (x21_0, x21_1, x21_2), (x22_0, x22_1, x22_2), (x23_0, x23_1, x23_2), (x24_0, x24_1, x24_2), (x25_0, x25_1, x25_2), (x26_0, x26_1, x26_2), (x27_0, x27_1, x27_2), (x28_0, x28_1, x28_2), (x29_0, x29_1, x29_2), (x30_0, x30_1, x30_2), (x31_0, x31_1, x31_2))
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance (Broadcast X32 a0, Broadcast X32 a1, Broadcast X32 a2) => Broadcast X32 (a0, a1, a2) where
  broadcast (x0, x1, x2) = MkTuple3X32 (broadcast x0) (broadcast x1) (broadcast x2)
  {-# INLINE broadcast #-}
instance (SelectableF X32 a0, SelectableF X32 a1, SelectableF X32 a2) => SelectableF X32 (a0, a1, a2) where
  selectF !cond (MkTuple3X32 x0 x1 x2) (MkTuple3X32 y0 y1 y2) = MkTuple3X32 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2)
  {-# INLINE selectF #-}
data instance X32 (a0, a1, a2, a3) = MkTuple4X32 !(X32 a0) !(X32 a1) !(X32 a2) !(X32 a3)
instance (PackX32 X32 a0, PackX32 X32 a1, PackX32 X32 a2, PackX32 X32 a3) => PackX32 X32 (a0, a1, a2, a3) where
  mkX32 (x0_0, x0_1, x0_2, x0_3) (x1_0, x1_1, x1_2, x1_3) (x2_0, x2_1, x2_2, x2_3) (x3_0, x3_1, x3_2, x3_3) (x4_0, x4_1, x4_2, x4_3) (x5_0, x5_1, x5_2, x5_3) (x6_0, x6_1, x6_2, x6_3) (x7_0, x7_1, x7_2, x7_3) (x8_0, x8_1, x8_2, x8_3) (x9_0, x9_1, x9_2, x9_3) (x10_0, x10_1, x10_2, x10_3) (x11_0, x11_1, x11_2, x11_3) (x12_0, x12_1, x12_2, x12_3) (x13_0, x13_1, x13_2, x13_3) (x14_0, x14_1, x14_2, x14_3) (x15_0, x15_1, x15_2, x15_3) (x16_0, x16_1, x16_2, x16_3) (x17_0, x17_1, x17_2, x17_3) (x18_0, x18_1, x18_2, x18_3) (x19_0, x19_1, x19_2, x19_3) (x20_0, x20_1, x20_2, x20_3) (x21_0, x21_1, x21_2, x21_3) (x22_0, x22_1, x22_2, x22_3) (x23_0, x23_1, x23_2, x23_3) (x24_0, x24_1, x24_2, x24_3) (x25_0, x25_1, x25_2, x25_3) (x26_0, x26_1, x26_2, x26_3) (x27_0, x27_1, x27_2, x27_3) (x28_0, x28_1, x28_2, x28_3) (x29_0, x29_1, x29_2, x29_3) (x30_0, x30_1, x30_2, x30_3) (x31_0, x31_1, x31_2, x31_3) = MkTuple4X32 (mkX32 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0 x16_0 x17_0 x18_0 x19_0 x20_0 x21_0 x22_0 x23_0 x24_0 x25_0 x26_0 x27_0 x28_0 x29_0 x30_0 x31_0) (mkX32 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1 x16_1 x17_1 x18_1 x19_1 x20_1 x21_1 x22_1 x23_1 x24_1 x25_1 x26_1 x27_1 x28_1 x29_1 x30_1 x31_1) (mkX32 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2 x16_2 x17_2 x18_2 x19_2 x20_2 x21_2 x22_2 x23_2 x24_2 x25_2 x26_2 x27_2 x28_2 x29_2 x30_2 x31_2) (mkX32 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3 x8_3 x9_3 x10_3 x11_3 x12_3 x13_3 x14_3 x15_3 x16_3 x17_3 x18_3 x19_3 x20_3 x21_3 x22_3 x23_3 x24_3 x25_3 x26_3 x27_3 x28_3 x29_3 x30_3 x31_3)
  unpackX32 (MkTuple4X32 v0 v1 v2 v3) = case unpackX32 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0, x16_0, x17_0, x18_0, x19_0, x20_0, x21_0, x22_0, x23_0, x24_0, x25_0, x26_0, x27_0, x28_0, x29_0, x30_0, x31_0) -> case unpackX32 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1, x16_1, x17_1, x18_1, x19_1, x20_1, x21_1, x22_1, x23_1, x24_1, x25_1, x26_1, x27_1, x28_1, x29_1, x30_1, x31_1) -> case unpackX32 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2, x16_2, x17_2, x18_2, x19_2, x20_2, x21_2, x22_2, x23_2, x24_2, x25_2, x26_2, x27_2, x28_2, x29_2, x30_2, x31_2) -> case unpackX32 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3, x8_3, x9_3, x10_3, x11_3, x12_3, x13_3, x14_3, x15_3, x16_3, x17_3, x18_3, x19_3, x20_3, x21_3, x22_3, x23_3, x24_3, x25_3, x26_3, x27_3, x28_3, x29_3, x30_3, x31_3) -> ((x0_0, x0_1, x0_2, x0_3), (x1_0, x1_1, x1_2, x1_3), (x2_0, x2_1, x2_2, x2_3), (x3_0, x3_1, x3_2, x3_3), (x4_0, x4_1, x4_2, x4_3), (x5_0, x5_1, x5_2, x5_3), (x6_0, x6_1, x6_2, x6_3), (x7_0, x7_1, x7_2, x7_3), (x8_0, x8_1, x8_2, x8_3), (x9_0, x9_1, x9_2, x9_3), (x10_0, x10_1, x10_2, x10_3), (x11_0, x11_1, x11_2, x11_3), (x12_0, x12_1, x12_2, x12_3), (x13_0, x13_1, x13_2, x13_3), (x14_0, x14_1, x14_2, x14_3), (x15_0, x15_1, x15_2, x15_3), (x16_0, x16_1, x16_2, x16_3), (x17_0, x17_1, x17_2, x17_3), (x18_0, x18_1, x18_2, x18_3), (x19_0, x19_1, x19_2, x19_3), (x20_0, x20_1, x20_2, x20_3), (x21_0, x21_1, x21_2, x21_3), (x22_0, x22_1, x22_2, x22_3), (x23_0, x23_1, x23_2, x23_3), (x24_0, x24_1, x24_2, x24_3), (x25_0, x25_1, x25_2, x25_3), (x26_0, x26_1, x26_2, x26_3), (x27_0, x27_1, x27_2, x27_3), (x28_0, x28_1, x28_2, x28_3), (x29_0, x29_1, x29_2, x29_3), (x30_0, x30_1, x30_2, x30_3), (x31_0, x31_1, x31_2, x31_3))
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance (Broadcast X32 a0, Broadcast X32 a1, Broadcast X32 a2, Broadcast X32 a3) => Broadcast X32 (a0, a1, a2, a3) where
  broadcast (x0, x1, x2, x3) = MkTuple4X32 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3)
  {-# INLINE broadcast #-}
instance (SelectableF X32 a0, SelectableF X32 a1, SelectableF X32 a2, SelectableF X32 a3) => SelectableF X32 (a0, a1, a2, a3) where
  selectF !cond (MkTuple4X32 x0 x1 x2 x3) (MkTuple4X32 y0 y1 y2 y3) = MkTuple4X32 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3)
  {-# INLINE selectF #-}
data instance X32 (a0, a1, a2, a3, a4) = MkTuple5X32 !(X32 a0) !(X32 a1) !(X32 a2) !(X32 a3) !(X32 a4)
instance (PackX32 X32 a0, PackX32 X32 a1, PackX32 X32 a2, PackX32 X32 a3, PackX32 X32 a4) => PackX32 X32 (a0, a1, a2, a3, a4) where
  mkX32 (x0_0, x0_1, x0_2, x0_3, x0_4) (x1_0, x1_1, x1_2, x1_3, x1_4) (x2_0, x2_1, x2_2, x2_3, x2_4) (x3_0, x3_1, x3_2, x3_3, x3_4) (x4_0, x4_1, x4_2, x4_3, x4_4) (x5_0, x5_1, x5_2, x5_3, x5_4) (x6_0, x6_1, x6_2, x6_3, x6_4) (x7_0, x7_1, x7_2, x7_3, x7_4) (x8_0, x8_1, x8_2, x8_3, x8_4) (x9_0, x9_1, x9_2, x9_3, x9_4) (x10_0, x10_1, x10_2, x10_3, x10_4) (x11_0, x11_1, x11_2, x11_3, x11_4) (x12_0, x12_1, x12_2, x12_3, x12_4) (x13_0, x13_1, x13_2, x13_3, x13_4) (x14_0, x14_1, x14_2, x14_3, x14_4) (x15_0, x15_1, x15_2, x15_3, x15_4) (x16_0, x16_1, x16_2, x16_3, x16_4) (x17_0, x17_1, x17_2, x17_3, x17_4) (x18_0, x18_1, x18_2, x18_3, x18_4) (x19_0, x19_1, x19_2, x19_3, x19_4) (x20_0, x20_1, x20_2, x20_3, x20_4) (x21_0, x21_1, x21_2, x21_3, x21_4) (x22_0, x22_1, x22_2, x22_3, x22_4) (x23_0, x23_1, x23_2, x23_3, x23_4) (x24_0, x24_1, x24_2, x24_3, x24_4) (x25_0, x25_1, x25_2, x25_3, x25_4) (x26_0, x26_1, x26_2, x26_3, x26_4) (x27_0, x27_1, x27_2, x27_3, x27_4) (x28_0, x28_1, x28_2, x28_3, x28_4) (x29_0, x29_1, x29_2, x29_3, x29_4) (x30_0, x30_1, x30_2, x30_3, x30_4) (x31_0, x31_1, x31_2, x31_3, x31_4) = MkTuple5X32 (mkX32 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0 x16_0 x17_0 x18_0 x19_0 x20_0 x21_0 x22_0 x23_0 x24_0 x25_0 x26_0 x27_0 x28_0 x29_0 x30_0 x31_0) (mkX32 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1 x16_1 x17_1 x18_1 x19_1 x20_1 x21_1 x22_1 x23_1 x24_1 x25_1 x26_1 x27_1 x28_1 x29_1 x30_1 x31_1) (mkX32 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2 x16_2 x17_2 x18_2 x19_2 x20_2 x21_2 x22_2 x23_2 x24_2 x25_2 x26_2 x27_2 x28_2 x29_2 x30_2 x31_2) (mkX32 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3 x8_3 x9_3 x10_3 x11_3 x12_3 x13_3 x14_3 x15_3 x16_3 x17_3 x18_3 x19_3 x20_3 x21_3 x22_3 x23_3 x24_3 x25_3 x26_3 x27_3 x28_3 x29_3 x30_3 x31_3) (mkX32 x0_4 x1_4 x2_4 x3_4 x4_4 x5_4 x6_4 x7_4 x8_4 x9_4 x10_4 x11_4 x12_4 x13_4 x14_4 x15_4 x16_4 x17_4 x18_4 x19_4 x20_4 x21_4 x22_4 x23_4 x24_4 x25_4 x26_4 x27_4 x28_4 x29_4 x30_4 x31_4)
  unpackX32 (MkTuple5X32 v0 v1 v2 v3 v4) = case unpackX32 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0, x16_0, x17_0, x18_0, x19_0, x20_0, x21_0, x22_0, x23_0, x24_0, x25_0, x26_0, x27_0, x28_0, x29_0, x30_0, x31_0) -> case unpackX32 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1, x16_1, x17_1, x18_1, x19_1, x20_1, x21_1, x22_1, x23_1, x24_1, x25_1, x26_1, x27_1, x28_1, x29_1, x30_1, x31_1) -> case unpackX32 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2, x16_2, x17_2, x18_2, x19_2, x20_2, x21_2, x22_2, x23_2, x24_2, x25_2, x26_2, x27_2, x28_2, x29_2, x30_2, x31_2) -> case unpackX32 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3, x8_3, x9_3, x10_3, x11_3, x12_3, x13_3, x14_3, x15_3, x16_3, x17_3, x18_3, x19_3, x20_3, x21_3, x22_3, x23_3, x24_3, x25_3, x26_3, x27_3, x28_3, x29_3, x30_3, x31_3) -> case unpackX32 v4 of (x0_4, x1_4, x2_4, x3_4, x4_4, x5_4, x6_4, x7_4, x8_4, x9_4, x10_4, x11_4, x12_4, x13_4, x14_4, x15_4, x16_4, x17_4, x18_4, x19_4, x20_4, x21_4, x22_4, x23_4, x24_4, x25_4, x26_4, x27_4, x28_4, x29_4, x30_4, x31_4) -> ((x0_0, x0_1, x0_2, x0_3, x0_4), (x1_0, x1_1, x1_2, x1_3, x1_4), (x2_0, x2_1, x2_2, x2_3, x2_4), (x3_0, x3_1, x3_2, x3_3, x3_4), (x4_0, x4_1, x4_2, x4_3, x4_4), (x5_0, x5_1, x5_2, x5_3, x5_4), (x6_0, x6_1, x6_2, x6_3, x6_4), (x7_0, x7_1, x7_2, x7_3, x7_4), (x8_0, x8_1, x8_2, x8_3, x8_4), (x9_0, x9_1, x9_2, x9_3, x9_4), (x10_0, x10_1, x10_2, x10_3, x10_4), (x11_0, x11_1, x11_2, x11_3, x11_4), (x12_0, x12_1, x12_2, x12_3, x12_4), (x13_0, x13_1, x13_2, x13_3, x13_4), (x14_0, x14_1, x14_2, x14_3, x14_4), (x15_0, x15_1, x15_2, x15_3, x15_4), (x16_0, x16_1, x16_2, x16_3, x16_4), (x17_0, x17_1, x17_2, x17_3, x17_4), (x18_0, x18_1, x18_2, x18_3, x18_4), (x19_0, x19_1, x19_2, x19_3, x19_4), (x20_0, x20_1, x20_2, x20_3, x20_4), (x21_0, x21_1, x21_2, x21_3, x21_4), (x22_0, x22_1, x22_2, x22_3, x22_4), (x23_0, x23_1, x23_2, x23_3, x23_4), (x24_0, x24_1, x24_2, x24_3, x24_4), (x25_0, x25_1, x25_2, x25_3, x25_4), (x26_0, x26_1, x26_2, x26_3, x26_4), (x27_0, x27_1, x27_2, x27_3, x27_4), (x28_0, x28_1, x28_2, x28_3, x28_4), (x29_0, x29_1, x29_2, x29_3, x29_4), (x30_0, x30_1, x30_2, x30_3, x30_4), (x31_0, x31_1, x31_2, x31_3, x31_4))
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance (Broadcast X32 a0, Broadcast X32 a1, Broadcast X32 a2, Broadcast X32 a3, Broadcast X32 a4) => Broadcast X32 (a0, a1, a2, a3, a4) where
  broadcast (x0, x1, x2, x3, x4) = MkTuple5X32 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4)
  {-# INLINE broadcast #-}
instance (SelectableF X32 a0, SelectableF X32 a1, SelectableF X32 a2, SelectableF X32 a3, SelectableF X32 a4) => SelectableF X32 (a0, a1, a2, a3, a4) where
  selectF !cond (MkTuple5X32 x0 x1 x2 x3 x4) (MkTuple5X32 y0 y1 y2 y3 y4) = MkTuple5X32 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4)
  {-# INLINE selectF #-}
data instance X32 (a0, a1, a2, a3, a4, a5) = MkTuple6X32 !(X32 a0) !(X32 a1) !(X32 a2) !(X32 a3) !(X32 a4) !(X32 a5)
instance (PackX32 X32 a0, PackX32 X32 a1, PackX32 X32 a2, PackX32 X32 a3, PackX32 X32 a4, PackX32 X32 a5) => PackX32 X32 (a0, a1, a2, a3, a4, a5) where
  mkX32 (x0_0, x0_1, x0_2, x0_3, x0_4, x0_5) (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5) (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5) (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5) (x4_0, x4_1, x4_2, x4_3, x4_4, x4_5) (x5_0, x5_1, x5_2, x5_3, x5_4, x5_5) (x6_0, x6_1, x6_2, x6_3, x6_4, x6_5) (x7_0, x7_1, x7_2, x7_3, x7_4, x7_5) (x8_0, x8_1, x8_2, x8_3, x8_4, x8_5) (x9_0, x9_1, x9_2, x9_3, x9_4, x9_5) (x10_0, x10_1, x10_2, x10_3, x10_4, x10_5) (x11_0, x11_1, x11_2, x11_3, x11_4, x11_5) (x12_0, x12_1, x12_2, x12_3, x12_4, x12_5) (x13_0, x13_1, x13_2, x13_3, x13_4, x13_5) (x14_0, x14_1, x14_2, x14_3, x14_4, x14_5) (x15_0, x15_1, x15_2, x15_3, x15_4, x15_5) (x16_0, x16_1, x16_2, x16_3, x16_4, x16_5) (x17_0, x17_1, x17_2, x17_3, x17_4, x17_5) (x18_0, x18_1, x18_2, x18_3, x18_4, x18_5) (x19_0, x19_1, x19_2, x19_3, x19_4, x19_5) (x20_0, x20_1, x20_2, x20_3, x20_4, x20_5) (x21_0, x21_1, x21_2, x21_3, x21_4, x21_5) (x22_0, x22_1, x22_2, x22_3, x22_4, x22_5) (x23_0, x23_1, x23_2, x23_3, x23_4, x23_5) (x24_0, x24_1, x24_2, x24_3, x24_4, x24_5) (x25_0, x25_1, x25_2, x25_3, x25_4, x25_5) (x26_0, x26_1, x26_2, x26_3, x26_4, x26_5) (x27_0, x27_1, x27_2, x27_3, x27_4, x27_5) (x28_0, x28_1, x28_2, x28_3, x28_4, x28_5) (x29_0, x29_1, x29_2, x29_3, x29_4, x29_5) (x30_0, x30_1, x30_2, x30_3, x30_4, x30_5) (x31_0, x31_1, x31_2, x31_3, x31_4, x31_5) = MkTuple6X32 (mkX32 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0 x16_0 x17_0 x18_0 x19_0 x20_0 x21_0 x22_0 x23_0 x24_0 x25_0 x26_0 x27_0 x28_0 x29_0 x30_0 x31_0) (mkX32 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1 x16_1 x17_1 x18_1 x19_1 x20_1 x21_1 x22_1 x23_1 x24_1 x25_1 x26_1 x27_1 x28_1 x29_1 x30_1 x31_1) (mkX32 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2 x16_2 x17_2 x18_2 x19_2 x20_2 x21_2 x22_2 x23_2 x24_2 x25_2 x26_2 x27_2 x28_2 x29_2 x30_2 x31_2) (mkX32 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3 x8_3 x9_3 x10_3 x11_3 x12_3 x13_3 x14_3 x15_3 x16_3 x17_3 x18_3 x19_3 x20_3 x21_3 x22_3 x23_3 x24_3 x25_3 x26_3 x27_3 x28_3 x29_3 x30_3 x31_3) (mkX32 x0_4 x1_4 x2_4 x3_4 x4_4 x5_4 x6_4 x7_4 x8_4 x9_4 x10_4 x11_4 x12_4 x13_4 x14_4 x15_4 x16_4 x17_4 x18_4 x19_4 x20_4 x21_4 x22_4 x23_4 x24_4 x25_4 x26_4 x27_4 x28_4 x29_4 x30_4 x31_4) (mkX32 x0_5 x1_5 x2_5 x3_5 x4_5 x5_5 x6_5 x7_5 x8_5 x9_5 x10_5 x11_5 x12_5 x13_5 x14_5 x15_5 x16_5 x17_5 x18_5 x19_5 x20_5 x21_5 x22_5 x23_5 x24_5 x25_5 x26_5 x27_5 x28_5 x29_5 x30_5 x31_5)
  unpackX32 (MkTuple6X32 v0 v1 v2 v3 v4 v5) = case unpackX32 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0, x16_0, x17_0, x18_0, x19_0, x20_0, x21_0, x22_0, x23_0, x24_0, x25_0, x26_0, x27_0, x28_0, x29_0, x30_0, x31_0) -> case unpackX32 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1, x16_1, x17_1, x18_1, x19_1, x20_1, x21_1, x22_1, x23_1, x24_1, x25_1, x26_1, x27_1, x28_1, x29_1, x30_1, x31_1) -> case unpackX32 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2, x16_2, x17_2, x18_2, x19_2, x20_2, x21_2, x22_2, x23_2, x24_2, x25_2, x26_2, x27_2, x28_2, x29_2, x30_2, x31_2) -> case unpackX32 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3, x8_3, x9_3, x10_3, x11_3, x12_3, x13_3, x14_3, x15_3, x16_3, x17_3, x18_3, x19_3, x20_3, x21_3, x22_3, x23_3, x24_3, x25_3, x26_3, x27_3, x28_3, x29_3, x30_3, x31_3) -> case unpackX32 v4 of (x0_4, x1_4, x2_4, x3_4, x4_4, x5_4, x6_4, x7_4, x8_4, x9_4, x10_4, x11_4, x12_4, x13_4, x14_4, x15_4, x16_4, x17_4, x18_4, x19_4, x20_4, x21_4, x22_4, x23_4, x24_4, x25_4, x26_4, x27_4, x28_4, x29_4, x30_4, x31_4) -> case unpackX32 v5 of (x0_5, x1_5, x2_5, x3_5, x4_5, x5_5, x6_5, x7_5, x8_5, x9_5, x10_5, x11_5, x12_5, x13_5, x14_5, x15_5, x16_5, x17_5, x18_5, x19_5, x20_5, x21_5, x22_5, x23_5, x24_5, x25_5, x26_5, x27_5, x28_5, x29_5, x30_5, x31_5) -> ((x0_0, x0_1, x0_2, x0_3, x0_4, x0_5), (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5), (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5), (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5), (x4_0, x4_1, x4_2, x4_3, x4_4, x4_5), (x5_0, x5_1, x5_2, x5_3, x5_4, x5_5), (x6_0, x6_1, x6_2, x6_3, x6_4, x6_5), (x7_0, x7_1, x7_2, x7_3, x7_4, x7_5), (x8_0, x8_1, x8_2, x8_3, x8_4, x8_5), (x9_0, x9_1, x9_2, x9_3, x9_4, x9_5), (x10_0, x10_1, x10_2, x10_3, x10_4, x10_5), (x11_0, x11_1, x11_2, x11_3, x11_4, x11_5), (x12_0, x12_1, x12_2, x12_3, x12_4, x12_5), (x13_0, x13_1, x13_2, x13_3, x13_4, x13_5), (x14_0, x14_1, x14_2, x14_3, x14_4, x14_5), (x15_0, x15_1, x15_2, x15_3, x15_4, x15_5), (x16_0, x16_1, x16_2, x16_3, x16_4, x16_5), (x17_0, x17_1, x17_2, x17_3, x17_4, x17_5), (x18_0, x18_1, x18_2, x18_3, x18_4, x18_5), (x19_0, x19_1, x19_2, x19_3, x19_4, x19_5), (x20_0, x20_1, x20_2, x20_3, x20_4, x20_5), (x21_0, x21_1, x21_2, x21_3, x21_4, x21_5), (x22_0, x22_1, x22_2, x22_3, x22_4, x22_5), (x23_0, x23_1, x23_2, x23_3, x23_4, x23_5), (x24_0, x24_1, x24_2, x24_3, x24_4, x24_5), (x25_0, x25_1, x25_2, x25_3, x25_4, x25_5), (x26_0, x26_1, x26_2, x26_3, x26_4, x26_5), (x27_0, x27_1, x27_2, x27_3, x27_4, x27_5), (x28_0, x28_1, x28_2, x28_3, x28_4, x28_5), (x29_0, x29_1, x29_2, x29_3, x29_4, x29_5), (x30_0, x30_1, x30_2, x30_3, x30_4, x30_5), (x31_0, x31_1, x31_2, x31_3, x31_4, x31_5))
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance (Broadcast X32 a0, Broadcast X32 a1, Broadcast X32 a2, Broadcast X32 a3, Broadcast X32 a4, Broadcast X32 a5) => Broadcast X32 (a0, a1, a2, a3, a4, a5) where
  broadcast (x0, x1, x2, x3, x4, x5) = MkTuple6X32 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4) (broadcast x5)
  {-# INLINE broadcast #-}
instance (SelectableF X32 a0, SelectableF X32 a1, SelectableF X32 a2, SelectableF X32 a3, SelectableF X32 a4, SelectableF X32 a5) => SelectableF X32 (a0, a1, a2, a3, a4, a5) where
  selectF !cond (MkTuple6X32 x0 x1 x2 x3 x4 x5) (MkTuple6X32 y0 y1 y2 y3 y4 y5) = MkTuple6X32 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4) (selectF cond x5 y5)
  {-# INLINE selectF #-}
instance (PackX32 X32 a, PackX32 X32 b) => LiftSIMD X32 a b where
  liftSIMD f !v = case unpackX32 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> mkX32 (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9) (f x10) (f x11) (f x12) (f x13) (f x14) (f x15) (f x16) (f x17) (f x18) (f x19) (f x20) (f x21) (f x22) (f x23) (f x24) (f x25) (f x26) (f x27) (f x28) (f x29) (f x30) (f x31)
  {-# INLINE liftSIMD #-}
instance (PackX32 X32 a, PackX32 X32 b, PackX32 X32 c) => LiftSIMD2 X32 a b c where
  liftSIMD2 f !u !v = case unpackX32 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> case unpackX32 v of (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29, y30, y31) -> mkX32 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4) (f x5 y5) (f x6 y6) (f x7 y7) (f x8 y8) (f x9 y9) (f x10 y10) (f x11 y11) (f x12 y12) (f x13 y13) (f x14 y14) (f x15 y15) (f x16 y16) (f x17 y17) (f x18 y18) (f x19 y19) (f x20 y20) (f x21 y21) (f x22 y22) (f x23 y23) (f x24 y24) (f x25 y25) (f x26 y26) (f x27 y27) (f x28 y28) (f x29 y29) (f x30 y30) (f x31 y31)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X32 where
  mkTuple2 = MkTuple2X32
  mkTuple3 = MkTuple3X32
  mkTuple4 = MkTuple4X32
  mkTuple5 = MkTuple5X32
  mkTuple6 = MkTuple6X32
  deconstructTuple2 (MkTuple2X32 v0 v1) = (v0, v1)
  deconstructTuple3 (MkTuple3X32 v0 v1 v2) = (v0, v1, v2)
  deconstructTuple4 (MkTuple4X32 v0 v1 v2 v3) = (v0, v1, v2, v3)
  deconstructTuple5 (MkTuple5X32 v0 v1 v2 v3 v4) = (v0, v1, v2, v3, v4)
  deconstructTuple6 (MkTuple6X32 v0 v1 v2 v3 v4 v5) = (v0, v1, v2, v3, v4, v5)
  mkSum = coerce
  getSum' = coerce
  mkProduct = coerce
  getProduct' = coerce
  mkMin = coerce
  getMin' = coerce
  mkMax = coerce
  getMax' = coerce
  mkComplex = MkComplexX32
  deconstructComplex (MkComplexX32 x y) = (x, y)
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
deriving via WrappedMulti X32 a instance SelectableF X32 a => Selectable (X32 a)
deriving via WrappedMulti X32 a instance NumF X32 a => Num (X32 a)
deriving via WrappedMulti X32 a instance FractionalF X32 a => Fractional (X32 a)
deriving via WrappedMulti X32 a instance FloatingF X32 a => Floating (X32 a)
deriving via WrappedMulti X32 a instance BooleanF X32 a => Boolean (X32 a)
deriving via WrappedMulti X32 a instance BitShiftF X32 a => BitShift (X32 a)
deriving via WrappedMulti X32 a instance MinMaxF X32 a => MinMax (X32 a)
deriving via WrappedMulti X32 a instance FusedMultiplyAddF X32 a => FusedMultiplyAdd (X32 a)
