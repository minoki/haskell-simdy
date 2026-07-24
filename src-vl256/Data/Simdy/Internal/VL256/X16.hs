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
module Data.Simdy.Internal.VL256.X16 where
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
-- | @'X16' a@ is a fixed-length vector of length 16.
--
-- Conceptually, @data 'X16' a = MkX16 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a@.
--
-- You can access the elements by 'mkX16', 'packX16' and 'unpackX16'.
data family X16 a
instance KnownSIMDLength X16 where
  type SIMDLength X16 = 16
  simdLength = 16
  {-# INLINE simdLength #-}
newtype instance X16 Bool = MkBoolX16 Word16
type instance Mask (X16 a) = X16 Bool
instance MaskIsLiftedBool X16 a
instance BooleanF X16 Bool where
  andF (MkBoolX16 x) (MkBoolX16 y) = MkBoolX16 (x .&. y)
  orF (MkBoolX16 x) (MkBoolX16 y) = MkBoolX16 (x .|. y)
  xorF (MkBoolX16 x) (MkBoolX16 y) = MkBoolX16 (xor x y)
  complementF (MkBoolX16 x) = MkBoolX16 (0xffff - x)
deriving via WrappedMulti X16 a instance EquatableF X16 a => Equatable (X16 a)
deriving via WrappedMulti X16 a instance OrderedF X16 a => Ordered (X16 a)
instance PackX16 X16 a => IsList (X16 a) where
  type Item (X16 a) = a
  toList = toListX16
  fromList = fromListX16
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX16 X16 Bool where
  mkX16 !x0 !x1 !x2 !x3 !x4 !x5 !x6 !x7 !x8 !x9 !x10 !x11 !x12 !x13 !x14 !x15 = MkBoolX16 ((if x0 then 0x1 else 0) .|. (if x1 then 0x2 else 0) .|. (if x2 then 0x4 else 0) .|. (if x3 then 0x8 else 0) .|. (if x4 then 0x10 else 0) .|. (if x5 then 0x20 else 0) .|. (if x6 then 0x40 else 0) .|. (if x7 then 0x80 else 0) .|. (if x8 then 0x100 else 0) .|. (if x9 then 0x200 else 0) .|. (if x10 then 0x400 else 0) .|. (if x11 then 0x800 else 0) .|. (if x12 then 0x1000 else 0) .|. (if x13 then 0x2000 else 0) .|. (if x14 then 0x4000 else 0) .|. (if x15 then 0x8000 else 0))
  unpackX16 (MkBoolX16 !x) = (testBit x 0, testBit x 1, testBit x 2, testBit x 3, testBit x 4, testBit x 5, testBit x 6, testBit x 7, testBit x 8, testBit x 9, testBit x 10, testBit x 11, testBit x 12, testBit x 13, testBit x 14, testBit x 15)
instance Broadcast X16 Bool where
  broadcast False = MkBoolX16 0
  broadcast True = MkBoolX16 0xffff
  {-# INLINE broadcast #-}
instance SelectableF X16 Bool where
  selectF (MkBoolX16 !cond) (MkBoolX16 !x) (MkBoolX16 !y) = MkBoolX16 ((cond .&. x) .|. (complement cond .&. y))
  {-# INLINE selectF #-}
data instance X16 Float = MkFloatX16WithVec256 FloatX8# FloatX8#
instance PackX16 X16 Float where
  mkX16 (F# x0) (F# x1) (F# x2) (F# x3) (F# x4) (F# x5) (F# x6) (F# x7) (F# x8) (F# x9) (F# x10) (F# x11) (F# x12) (F# x13) (F# x14) (F# x15) = MkFloatX16WithVec256 (packFloatX8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packFloatX8# (# x8, x9, x10, x11, x12, x13, x14, x15 #))
  unpackX16 (MkFloatX16WithVec256 v0 v1) = case unpackFloatX8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackFloatX8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> (F# x0, F# x1, F# x2, F# x3, F# x4, F# x5, F# x6, F# x7, F# x8, F# x9, F# x10, F# x11, F# x12, F# x13, F# x14, F# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Float where
  broadcast (F# x) = let !v = broadcastFloatX8# x in MkFloatX16WithVec256 v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Float where
  selectF (MkBoolX16 !cond) (MkFloatX16WithVec256 x0 x1) (MkFloatX16WithVec256 y0 y1) = MkFloatX16WithVec256 (selectFloatX8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectFloatX8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany FloatX8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany FloatX8# [i8, i9, i10, i11, i12, i13, i14, i15]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Float where
  unaryShuffle (MkFloatX16WithVec256 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkFloatX16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany FloatX8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany FloatX8# [i8, i9, i10, i11, i12, i13, i14, i15]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Float where
  binaryShuffle (MkFloatX16WithVec256 x0 x1) (MkFloatX16WithVec256 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkFloatX16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Float where
  eqF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkBoolX16 $ (fromIntegral (eqFloatX8# u0 v0)) .|. (fromIntegral (eqFloatX8# u1 v1) `unsafeShiftL` 8)
  {-# INLINE eqF #-}
instance OrderedF X16 Float where
  ltF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkBoolX16 $ (fromIntegral (ltFloatX8# u0 v0)) .|. (fromIntegral (ltFloatX8# u1 v1) `unsafeShiftL` 8)
  leF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkBoolX16 $ (fromIntegral (leFloatX8# u0 v0)) .|. (fromIntegral (leFloatX8# u1 v1) `unsafeShiftL` 8)
  gtF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkBoolX16 $ (fromIntegral (gtFloatX8# u0 v0)) .|. (fromIntegral (gtFloatX8# u1 v1) `unsafeShiftL` 8)
  geF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkBoolX16 $ (fromIntegral (geFloatX8# u0 v0)) .|. (fromIntegral (geFloatX8# u1 v1) `unsafeShiftL` 8)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Float where
  minF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkFloatX16WithVec256 (minimumFloatX8# u0 v0) (minimumFloatX8# u1 v1)
  maxF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkFloatX16WithVec256 (maximumFloatX8# u0 v0) (maximumFloatX8# u1 v1)
  minimumNumberF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkFloatX16WithVec256 (minimumNumberFloatX8# u0 v0) (minimumNumberFloatX8# u1 v1)
  maximumNumberF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkFloatX16WithVec256 (maximumNumberFloatX8# u0 v0) (maximumNumberFloatX8# u1 v1)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX16Float :: X16 Float -> X16 Float
negateX16Float (MkFloatX16WithVec256 u0 u1) = MkFloatX16WithVec256 (negateFloatX8# u0) (negateFloatX8# u1)
#if defined(USE_FMA)
{-# INLINE [0] negateX16Float #-}
#else
{-# INLINE negateX16Float #-}
#endif
instance NumF X16 Float where
  plusF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkFloatX16WithVec256 (plusFloatX8# u0 v0) (plusFloatX8# u1 v1)
  minusF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkFloatX16WithVec256 (minusFloatX8# u0 v0) (minusFloatX8# u1 v1)
  timesF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkFloatX16WithVec256 (timesFloatX8# u0 v0) (timesFloatX8# u1 v1)
  negateF = negateX16Float
  absF (MkFloatX16WithVec256 u0 u1) = MkFloatX16WithVec256 (absFloatX8# u0) (absFloatX8# u1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X16 Float where
  divideF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) = MkFloatX16WithVec256 (divideFloatX8# u0 v0) (divideFloatX8# u1 v1)
  {-# INLINE divideF #-}
instance FloatingF X16 Float where
  sqrtF (MkFloatX16WithVec256 u0 u1) = MkFloatX16WithVec256 (sqrtFloatX8# u0) (sqrtFloatX8# u1)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X16 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX16WithVec256 u0 u1) (MkFloatX16WithVec256 v0 v1) (MkFloatX16WithVec256 w0 w1) = MkFloatX16WithVec256 (fmaddFloatX8# u0 v0 w0) (fmaddFloatX8# u1 v1 w1)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X16 Float" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X16 Float
"Fusible/*-/X16 Float" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX16Float c) :: X16 Float
"Fusible/-*+/X16 Float" forall a b c.
  negateX16Float (a F.* b) F.+ c = fusedMultiplyAdd (negateX16Float a) b c :: X16 Float
"Fusible/-*-/X16 Float" forall a b c.
  negateX16Float (a F.* b) F.- c = fusedMultiplyAdd (negateX16Float a) b (negateX16Float c) :: X16 Float
"Fusible/+*/X16 Float" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X16 Float
"Fusible/-*/X16 Float" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX16Float b) c a :: X16 Float
  #-}
#endif
instance EnumFromZero_ X16 Float where
  enumFromZero = MkFloatX16WithVec256 (packFloatX8# (# 0.0#, 1.0#, 2.0#, 3.0#, 4.0#, 5.0#, 6.0#, 7.0# #)) (packFloatX8# (# 8.0#, 9.0#, 10.0#, 11.0#, 12.0#, 13.0#, 14.0#, 15.0# #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Float where
  indexByteArraySIMD# ba i = MkFloatX16WithVec256 (indexFloatArrayAsFloatX8# ba i) (indexFloatArrayAsFloatX8# ba (i +# 8#))
  readByteArraySIMD# mba i s0 = case readFloatArrayAsFloatX8# mba i s0 of (# s1, v0 #) -> case readFloatArrayAsFloatX8# mba (i +# 8#) s1 of (# s2, v1 #) -> (# s2, MkFloatX16WithVec256 v0 v1 #)
  writeByteArraySIMD# mba i (MkFloatX16WithVec256 v0 v1) s0 = case writeFloatArrayAsFloatX8# mba i v0 s0 of s1 -> writeFloatArrayAsFloatX8# mba (i +# 8#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readFloatOffAddrAsFloatX8# addr i s0 of (# s1, v0 #) -> case readFloatOffAddrAsFloatX8# addr (i +# 8#) s1 of (# s2, v1 #) -> (# s2, MkFloatX16WithVec256 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX16WithVec256 v0 v1) = IO (\s0 -> case writeFloatOffAddrAsFloatX8# addr i v0 s0 of s1 -> case writeFloatOffAddrAsFloatX8# addr (i +# 8#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Double = MkDoubleX16WithVec256 DoubleX4# DoubleX4# DoubleX4# DoubleX4#
instance PackX16 X16 Double where
  mkX16 (D# x0) (D# x1) (D# x2) (D# x3) (D# x4) (D# x5) (D# x6) (D# x7) (D# x8) (D# x9) (D# x10) (D# x11) (D# x12) (D# x13) (D# x14) (D# x15) = MkDoubleX16WithVec256 (packDoubleX4# (# x0, x1, x2, x3 #)) (packDoubleX4# (# x4, x5, x6, x7 #)) (packDoubleX4# (# x8, x9, x10, x11 #)) (packDoubleX4# (# x12, x13, x14, x15 #))
  unpackX16 (MkDoubleX16WithVec256 v0 v1 v2 v3) = case unpackDoubleX4# v0 of (# x0, x1, x2, x3 #) -> case unpackDoubleX4# v1 of (# x4, x5, x6, x7 #) -> case unpackDoubleX4# v2 of (# x8, x9, x10, x11 #) -> case unpackDoubleX4# v3 of (# x12, x13, x14, x15 #) -> (D# x0, D# x1, D# x2, D# x3, D# x4, D# x5, D# x6, D# x7, D# x8, D# x9, D# x10, D# x11, D# x12, D# x13, D# x14, D# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Double where
  broadcast (D# x) = let !v = broadcastDoubleX4# x in MkDoubleX16WithVec256 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Double where
  selectF (MkBoolX16 !cond) (MkDoubleX16WithVec256 x0 x1 x2 x3) (MkDoubleX16WithVec256 y0 y1 y2 y3) = MkDoubleX16WithVec256 (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 4) x1 y1) (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 8) x2 y2) (selectDoubleX4# (fromIntegral $ cond `unsafeShiftR` 12) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany DoubleX4# [i0, i1, i2, i3], ShuffleMany DoubleX4# [i4, i5, i6, i7], ShuffleMany DoubleX4# [i8, i9, i10, i11], ShuffleMany DoubleX4# [i12, i13, i14, i15]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Double where
  unaryShuffle (MkDoubleX16WithVec256 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkDoubleX16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany DoubleX4# [i0, i1, i2, i3], ShuffleMany DoubleX4# [i4, i5, i6, i7], ShuffleMany DoubleX4# [i8, i9, i10, i11], ShuffleMany DoubleX4# [i12, i13, i14, i15]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Double where
  binaryShuffle (MkDoubleX16WithVec256 x0 x1 x2 x3) (MkDoubleX16WithVec256 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkDoubleX16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Double where
  eqF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (eqDoubleX4# u0 v0)) .|. (fromIntegral (eqDoubleX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (eqDoubleX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (eqDoubleX4# u3 v3) `unsafeShiftL` 12)
  {-# INLINE eqF #-}
instance OrderedF X16 Double where
  ltF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (ltDoubleX4# u0 v0)) .|. (fromIntegral (ltDoubleX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (ltDoubleX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (ltDoubleX4# u3 v3) `unsafeShiftL` 12)
  leF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (leDoubleX4# u0 v0)) .|. (fromIntegral (leDoubleX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (leDoubleX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (leDoubleX4# u3 v3) `unsafeShiftL` 12)
  gtF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (gtDoubleX4# u0 v0)) .|. (fromIntegral (gtDoubleX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (gtDoubleX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (gtDoubleX4# u3 v3) `unsafeShiftL` 12)
  geF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (geDoubleX4# u0 v0)) .|. (fromIntegral (geDoubleX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (geDoubleX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (geDoubleX4# u3 v3) `unsafeShiftL` 12)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Double where
  minF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkDoubleX16WithVec256 (minimumDoubleX4# u0 v0) (minimumDoubleX4# u1 v1) (minimumDoubleX4# u2 v2) (minimumDoubleX4# u3 v3)
  maxF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkDoubleX16WithVec256 (maximumDoubleX4# u0 v0) (maximumDoubleX4# u1 v1) (maximumDoubleX4# u2 v2) (maximumDoubleX4# u3 v3)
  minimumNumberF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkDoubleX16WithVec256 (minimumNumberDoubleX4# u0 v0) (minimumNumberDoubleX4# u1 v1) (minimumNumberDoubleX4# u2 v2) (minimumNumberDoubleX4# u3 v3)
  maximumNumberF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkDoubleX16WithVec256 (maximumNumberDoubleX4# u0 v0) (maximumNumberDoubleX4# u1 v1) (maximumNumberDoubleX4# u2 v2) (maximumNumberDoubleX4# u3 v3)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX16Double :: X16 Double -> X16 Double
negateX16Double (MkDoubleX16WithVec256 u0 u1 u2 u3) = MkDoubleX16WithVec256 (negateDoubleX4# u0) (negateDoubleX4# u1) (negateDoubleX4# u2) (negateDoubleX4# u3)
#if defined(USE_FMA)
{-# INLINE [0] negateX16Double #-}
#else
{-# INLINE negateX16Double #-}
#endif
instance NumF X16 Double where
  plusF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkDoubleX16WithVec256 (plusDoubleX4# u0 v0) (plusDoubleX4# u1 v1) (plusDoubleX4# u2 v2) (plusDoubleX4# u3 v3)
  minusF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkDoubleX16WithVec256 (minusDoubleX4# u0 v0) (minusDoubleX4# u1 v1) (minusDoubleX4# u2 v2) (minusDoubleX4# u3 v3)
  timesF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkDoubleX16WithVec256 (timesDoubleX4# u0 v0) (timesDoubleX4# u1 v1) (timesDoubleX4# u2 v2) (timesDoubleX4# u3 v3)
  negateF = negateX16Double
  absF (MkDoubleX16WithVec256 u0 u1 u2 u3) = MkDoubleX16WithVec256 (absDoubleX4# u0) (absDoubleX4# u1) (absDoubleX4# u2) (absDoubleX4# u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X16 Double where
  divideF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) = MkDoubleX16WithVec256 (divideDoubleX4# u0 v0) (divideDoubleX4# u1 v1) (divideDoubleX4# u2 v2) (divideDoubleX4# u3 v3)
  {-# INLINE divideF #-}
instance FloatingF X16 Double where
  sqrtF (MkDoubleX16WithVec256 u0 u1 u2 u3) = MkDoubleX16WithVec256 (sqrtDoubleX4# u0) (sqrtDoubleX4# u1) (sqrtDoubleX4# u2) (sqrtDoubleX4# u3)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X16 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX16WithVec256 u0 u1 u2 u3) (MkDoubleX16WithVec256 v0 v1 v2 v3) (MkDoubleX16WithVec256 w0 w1 w2 w3) = MkDoubleX16WithVec256 (fmaddDoubleX4# u0 v0 w0) (fmaddDoubleX4# u1 v1 w1) (fmaddDoubleX4# u2 v2 w2) (fmaddDoubleX4# u3 v3 w3)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X16 Double" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X16 Double
"Fusible/*-/X16 Double" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX16Double c) :: X16 Double
"Fusible/-*+/X16 Double" forall a b c.
  negateX16Double (a F.* b) F.+ c = fusedMultiplyAdd (negateX16Double a) b c :: X16 Double
"Fusible/-*-/X16 Double" forall a b c.
  negateX16Double (a F.* b) F.- c = fusedMultiplyAdd (negateX16Double a) b (negateX16Double c) :: X16 Double
"Fusible/+*/X16 Double" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X16 Double
"Fusible/-*/X16 Double" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX16Double b) c a :: X16 Double
  #-}
#endif
instance EnumFromZero_ X16 Double where
  enumFromZero = MkDoubleX16WithVec256 (packDoubleX4# (# 0.0##, 1.0##, 2.0##, 3.0## #)) (packDoubleX4# (# 4.0##, 5.0##, 6.0##, 7.0## #)) (packDoubleX4# (# 8.0##, 9.0##, 10.0##, 11.0## #)) (packDoubleX4# (# 12.0##, 13.0##, 14.0##, 15.0## #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Double where
  indexByteArraySIMD# ba i = MkDoubleX16WithVec256 (indexDoubleArrayAsDoubleX4# ba i) (indexDoubleArrayAsDoubleX4# ba (i +# 4#)) (indexDoubleArrayAsDoubleX4# ba (i +# 8#)) (indexDoubleArrayAsDoubleX4# ba (i +# 12#))
  readByteArraySIMD# mba i s0 = case readDoubleArrayAsDoubleX4# mba i s0 of (# s1, v0 #) -> case readDoubleArrayAsDoubleX4# mba (i +# 4#) s1 of (# s2, v1 #) -> case readDoubleArrayAsDoubleX4# mba (i +# 8#) s2 of (# s3, v2 #) -> case readDoubleArrayAsDoubleX4# mba (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkDoubleX16WithVec256 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkDoubleX16WithVec256 v0 v1 v2 v3) s0 = case writeDoubleArrayAsDoubleX4# mba i v0 s0 of s1 -> case writeDoubleArrayAsDoubleX4# mba (i +# 4#) v1 s1 of s2 -> case writeDoubleArrayAsDoubleX4# mba (i +# 8#) v2 s2 of s3 -> writeDoubleArrayAsDoubleX4# mba (i +# 12#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readDoubleOffAddrAsDoubleX4# addr i s0 of (# s1, v0 #) -> case readDoubleOffAddrAsDoubleX4# addr (i +# 4#) s1 of (# s2, v1 #) -> case readDoubleOffAddrAsDoubleX4# addr (i +# 8#) s2 of (# s3, v2 #) -> case readDoubleOffAddrAsDoubleX4# addr (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkDoubleX16WithVec256 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX16WithVec256 v0 v1 v2 v3) = IO (\s0 -> case writeDoubleOffAddrAsDoubleX4# addr i v0 s0 of s1 -> case writeDoubleOffAddrAsDoubleX4# addr (i +# 4#) v1 s1 of s2 -> case writeDoubleOffAddrAsDoubleX4# addr (i +# 8#) v2 s2 of s3 -> case writeDoubleOffAddrAsDoubleX4# addr (i +# 12#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
instance ImplementationDescription X16 where
  implementationDescription _ = "X16;maxBits=256"
data instance X16 Int8 = MkInt8X16 Int8X16#
instance PackX16 X16 Int8 where
  mkX16 (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) (I8# x8) (I8# x9) (I8# x10) (I8# x11) (I8# x12) (I8# x13) (I8# x14) (I8# x15) = MkInt8X16 (packInt8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #))
  unpackX16 (MkInt8X16 v0) = case unpackInt8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> (I8# x0, I8# x1, I8# x2, I8# x3, I8# x4, I8# x5, I8# x6, I8# x7, I8# x8, I8# x9, I8# x10, I8# x11, I8# x12, I8# x13, I8# x14, I8# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int8 where
  broadcast (I8# x) = MkInt8X16 (broadcastInt8X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X16 Int8 where
  selectF (MkBoolX16 !cond) (MkInt8X16 x0) (MkInt8X16 y0) = MkInt8X16 (selectInt8X16# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 16, ShuffleMany Int8X16# indices) => UnaryShuffle indices X16 Int8 where
  unaryShuffle (MkInt8X16 x) = MkInt8X16 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 32, ShuffleMany Int8X16# indices) => BinaryShuffle indices X16 Int8 where
  binaryShuffle (MkInt8X16 x0) (MkInt8X16 x1) = MkInt8X16 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int8 where
  eqF (MkInt8X16 u0) (MkInt8X16 v0) = MkBoolX16 $ (fromIntegral (eqInt8X16# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X16 Int8 where
  ltF (MkInt8X16 u0) (MkInt8X16 v0) = MkBoolX16 $ (fromIntegral (ltInt8X16# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int8 where
  minF (MkInt8X16 u0) (MkInt8X16 v0) = MkInt8X16 (minInt8X16# u0 v0)
  maxF (MkInt8X16 u0) (MkInt8X16 v0) = MkInt8X16 (maxInt8X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int8 where
  plusF (MkInt8X16 u0) (MkInt8X16 v0) = MkInt8X16 (plusInt8X16# u0 v0)
  minusF (MkInt8X16 u0) (MkInt8X16 v0) = MkInt8X16 (minusInt8X16# u0 v0)
  timesF (MkInt8X16 u0) (MkInt8X16 v0) = MkInt8X16 (timesInt8X16# u0 v0)
  negateF (MkInt8X16 u0) = MkInt8X16 (negateInt8X16# u0)
  absF (MkInt8X16 u0) = MkInt8X16 (absInt8X16# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Int8 where
  andF (MkInt8X16 u0) (MkInt8X16 v0) = MkInt8X16 (andInt8X16# u0 v0)
  orF (MkInt8X16 u0) (MkInt8X16 v0) = MkInt8X16 (orInt8X16# u0 v0)
  xorF (MkInt8X16 u0) (MkInt8X16 v0) = MkInt8X16 (xorInt8X16# u0 v0)
  complementF (MkInt8X16 u0) = MkInt8X16 (complementInt8X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int8 where
  shiftLF (MkInt8X16 u0) (I# i) = MkInt8X16 (shiftLInt8X16# u0 i)
  shiftRF (MkInt8X16 u0) (I# i) = MkInt8X16 (shiftRInt8X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Int8 where
  enumFromZero = MkInt8X16 (packInt8X16# (# 0#Int8, 1#Int8, 2#Int8, 3#Int8, 4#Int8, 5#Int8, 6#Int8, 7#Int8, 8#Int8, 9#Int8, 10#Int8, 11#Int8, 12#Int8, 13#Int8, 14#Int8, 15#Int8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int8 where
  indexByteArraySIMD# ba i = MkInt8X16 (indexInt8ArrayAsInt8X16# ba i)
  readByteArraySIMD# mba i s0 = case readInt8ArrayAsInt8X16# mba i s0 of (# s1, v0 #) -> (# s1, MkInt8X16 v0 #)
  writeByteArraySIMD# mba i (MkInt8X16 v0) s0 = writeInt8ArrayAsInt8X16# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt8OffAddrAsInt8X16# addr i s0 of (# s1, v0 #) -> (# s1, MkInt8X16 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X16 v0) = IO (\s0 -> case writeInt8OffAddrAsInt8X16# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Int16 = MkInt16X16 Int16X16#
instance PackX16 X16 Int16 where
  mkX16 (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) (I16# x8) (I16# x9) (I16# x10) (I16# x11) (I16# x12) (I16# x13) (I16# x14) (I16# x15) = MkInt16X16 (packInt16X16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #))
  unpackX16 (MkInt16X16 v0) = case unpackInt16X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> (I16# x0, I16# x1, I16# x2, I16# x3, I16# x4, I16# x5, I16# x6, I16# x7, I16# x8, I16# x9, I16# x10, I16# x11, I16# x12, I16# x13, I16# x14, I16# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int16 where
  broadcast (I16# x) = MkInt16X16 (broadcastInt16X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X16 Int16 where
  selectF (MkBoolX16 !cond) (MkInt16X16 x0) (MkInt16X16 y0) = MkInt16X16 (selectInt16X16# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 16, ShuffleMany Int16X16# indices) => UnaryShuffle indices X16 Int16 where
  unaryShuffle (MkInt16X16 x) = MkInt16X16 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 32, ShuffleMany Int16X16# indices) => BinaryShuffle indices X16 Int16 where
  binaryShuffle (MkInt16X16 x0) (MkInt16X16 x1) = MkInt16X16 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int16 where
  eqF (MkInt16X16 u0) (MkInt16X16 v0) = MkBoolX16 $ (fromIntegral (eqInt16X16# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X16 Int16 where
  ltF (MkInt16X16 u0) (MkInt16X16 v0) = MkBoolX16 $ (fromIntegral (ltInt16X16# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int16 where
  minF (MkInt16X16 u0) (MkInt16X16 v0) = MkInt16X16 (minInt16X16# u0 v0)
  maxF (MkInt16X16 u0) (MkInt16X16 v0) = MkInt16X16 (maxInt16X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int16 where
  plusF (MkInt16X16 u0) (MkInt16X16 v0) = MkInt16X16 (plusInt16X16# u0 v0)
  minusF (MkInt16X16 u0) (MkInt16X16 v0) = MkInt16X16 (minusInt16X16# u0 v0)
  timesF (MkInt16X16 u0) (MkInt16X16 v0) = MkInt16X16 (timesInt16X16# u0 v0)
  negateF (MkInt16X16 u0) = MkInt16X16 (negateInt16X16# u0)
  absF (MkInt16X16 u0) = MkInt16X16 (absInt16X16# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Int16 where
  andF (MkInt16X16 u0) (MkInt16X16 v0) = MkInt16X16 (andInt16X16# u0 v0)
  orF (MkInt16X16 u0) (MkInt16X16 v0) = MkInt16X16 (orInt16X16# u0 v0)
  xorF (MkInt16X16 u0) (MkInt16X16 v0) = MkInt16X16 (xorInt16X16# u0 v0)
  complementF (MkInt16X16 u0) = MkInt16X16 (complementInt16X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int16 where
  shiftLF (MkInt16X16 u0) (I# i) = MkInt16X16 (shiftLInt16X16# u0 i)
  shiftRF (MkInt16X16 u0) (I# i) = MkInt16X16 (shiftRInt16X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Int16 where
  enumFromZero = MkInt16X16 (packInt16X16# (# 0#Int16, 1#Int16, 2#Int16, 3#Int16, 4#Int16, 5#Int16, 6#Int16, 7#Int16, 8#Int16, 9#Int16, 10#Int16, 11#Int16, 12#Int16, 13#Int16, 14#Int16, 15#Int16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int16 where
  indexByteArraySIMD# ba i = MkInt16X16 (indexInt16ArrayAsInt16X16# ba i)
  readByteArraySIMD# mba i s0 = case readInt16ArrayAsInt16X16# mba i s0 of (# s1, v0 #) -> (# s1, MkInt16X16 v0 #)
  writeByteArraySIMD# mba i (MkInt16X16 v0) s0 = writeInt16ArrayAsInt16X16# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt16OffAddrAsInt16X16# addr i s0 of (# s1, v0 #) -> (# s1, MkInt16X16 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X16 v0) = IO (\s0 -> case writeInt16OffAddrAsInt16X16# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Int32 = MkInt32X16WithVec256 Int32X8# Int32X8#
instance PackX16 X16 Int32 where
  mkX16 (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) (I32# x8) (I32# x9) (I32# x10) (I32# x11) (I32# x12) (I32# x13) (I32# x14) (I32# x15) = MkInt32X16WithVec256 (packInt32X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packInt32X8# (# x8, x9, x10, x11, x12, x13, x14, x15 #))
  unpackX16 (MkInt32X16WithVec256 v0 v1) = case unpackInt32X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackInt32X8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> (I32# x0, I32# x1, I32# x2, I32# x3, I32# x4, I32# x5, I32# x6, I32# x7, I32# x8, I32# x9, I32# x10, I32# x11, I32# x12, I32# x13, I32# x14, I32# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int32 where
  broadcast (I32# x) = let !v = broadcastInt32X8# x in MkInt32X16WithVec256 v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Int32 where
  selectF (MkBoolX16 !cond) (MkInt32X16WithVec256 x0 x1) (MkInt32X16WithVec256 y0 y1) = MkInt32X16WithVec256 (selectInt32X8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt32X8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany Int32X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Int32X8# [i8, i9, i10, i11, i12, i13, i14, i15]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int32 where
  unaryShuffle (MkInt32X16WithVec256 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt32X16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany Int32X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Int32X8# [i8, i9, i10, i11, i12, i13, i14, i15]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int32 where
  binaryShuffle (MkInt32X16WithVec256 x0 x1) (MkInt32X16WithVec256 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt32X16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int32 where
  eqF (MkInt32X16WithVec256 u0 u1) (MkInt32X16WithVec256 v0 v1) = MkBoolX16 $ (fromIntegral (eqInt32X8# u0 v0)) .|. (fromIntegral (eqInt32X8# u1 v1) `unsafeShiftL` 8)
  {-# INLINE eqF #-}
instance OrderedF X16 Int32 where
  ltF (MkInt32X16WithVec256 u0 u1) (MkInt32X16WithVec256 v0 v1) = MkBoolX16 $ (fromIntegral (ltInt32X8# u0 v0)) .|. (fromIntegral (ltInt32X8# u1 v1) `unsafeShiftL` 8)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int32 where
  minF (MkInt32X16WithVec256 u0 u1) (MkInt32X16WithVec256 v0 v1) = MkInt32X16WithVec256 (minInt32X8# u0 v0) (minInt32X8# u1 v1)
  maxF (MkInt32X16WithVec256 u0 u1) (MkInt32X16WithVec256 v0 v1) = MkInt32X16WithVec256 (maxInt32X8# u0 v0) (maxInt32X8# u1 v1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int32 where
  plusF (MkInt32X16WithVec256 u0 u1) (MkInt32X16WithVec256 v0 v1) = MkInt32X16WithVec256 (plusInt32X8# u0 v0) (plusInt32X8# u1 v1)
  minusF (MkInt32X16WithVec256 u0 u1) (MkInt32X16WithVec256 v0 v1) = MkInt32X16WithVec256 (minusInt32X8# u0 v0) (minusInt32X8# u1 v1)
  timesF (MkInt32X16WithVec256 u0 u1) (MkInt32X16WithVec256 v0 v1) = MkInt32X16WithVec256 (timesInt32X8# u0 v0) (timesInt32X8# u1 v1)
  negateF (MkInt32X16WithVec256 u0 u1) = MkInt32X16WithVec256 (negateInt32X8# u0) (negateInt32X8# u1)
  absF (MkInt32X16WithVec256 u0 u1) = MkInt32X16WithVec256 (absInt32X8# u0) (absInt32X8# u1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Int32 where
  andF (MkInt32X16WithVec256 u0 u1) (MkInt32X16WithVec256 v0 v1) = MkInt32X16WithVec256 (andInt32X8# u0 v0) (andInt32X8# u1 v1)
  orF (MkInt32X16WithVec256 u0 u1) (MkInt32X16WithVec256 v0 v1) = MkInt32X16WithVec256 (orInt32X8# u0 v0) (orInt32X8# u1 v1)
  xorF (MkInt32X16WithVec256 u0 u1) (MkInt32X16WithVec256 v0 v1) = MkInt32X16WithVec256 (xorInt32X8# u0 v0) (xorInt32X8# u1 v1)
  complementF (MkInt32X16WithVec256 u0 u1) = MkInt32X16WithVec256 (complementInt32X8# u0) (complementInt32X8# u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int32 where
  shiftLF (MkInt32X16WithVec256 u0 u1) (I# i) = MkInt32X16WithVec256 (shiftLInt32X8# u0 i) (shiftLInt32X8# u1 i)
  shiftRF (MkInt32X16WithVec256 u0 u1) (I# i) = MkInt32X16WithVec256 (shiftRInt32X8# u0 i) (shiftRInt32X8# u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Int32 where
  enumFromZero = MkInt32X16WithVec256 (packInt32X8# (# 0#Int32, 1#Int32, 2#Int32, 3#Int32, 4#Int32, 5#Int32, 6#Int32, 7#Int32 #)) (packInt32X8# (# 8#Int32, 9#Int32, 10#Int32, 11#Int32, 12#Int32, 13#Int32, 14#Int32, 15#Int32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int32 where
  indexByteArraySIMD# ba i = MkInt32X16WithVec256 (indexInt32ArrayAsInt32X8# ba i) (indexInt32ArrayAsInt32X8# ba (i +# 8#))
  readByteArraySIMD# mba i s0 = case readInt32ArrayAsInt32X8# mba i s0 of (# s1, v0 #) -> case readInt32ArrayAsInt32X8# mba (i +# 8#) s1 of (# s2, v1 #) -> (# s2, MkInt32X16WithVec256 v0 v1 #)
  writeByteArraySIMD# mba i (MkInt32X16WithVec256 v0 v1) s0 = case writeInt32ArrayAsInt32X8# mba i v0 s0 of s1 -> writeInt32ArrayAsInt32X8# mba (i +# 8#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt32OffAddrAsInt32X8# addr i s0 of (# s1, v0 #) -> case readInt32OffAddrAsInt32X8# addr (i +# 8#) s1 of (# s2, v1 #) -> (# s2, MkInt32X16WithVec256 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X16WithVec256 v0 v1) = IO (\s0 -> case writeInt32OffAddrAsInt32X8# addr i v0 s0 of s1 -> case writeInt32OffAddrAsInt32X8# addr (i +# 8#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Int64 = MkInt64X16WithVec256 Int64X4# Int64X4# Int64X4# Int64X4#
instance PackX16 X16 Int64 where
  mkX16 (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) (I64# x8) (I64# x9) (I64# x10) (I64# x11) (I64# x12) (I64# x13) (I64# x14) (I64# x15) = MkInt64X16WithVec256 (packInt64X4# (# x0, x1, x2, x3 #)) (packInt64X4# (# x4, x5, x6, x7 #)) (packInt64X4# (# x8, x9, x10, x11 #)) (packInt64X4# (# x12, x13, x14, x15 #))
  unpackX16 (MkInt64X16WithVec256 v0 v1 v2 v3) = case unpackInt64X4# v0 of (# x0, x1, x2, x3 #) -> case unpackInt64X4# v1 of (# x4, x5, x6, x7 #) -> case unpackInt64X4# v2 of (# x8, x9, x10, x11 #) -> case unpackInt64X4# v3 of (# x12, x13, x14, x15 #) -> (I64# x0, I64# x1, I64# x2, I64# x3, I64# x4, I64# x5, I64# x6, I64# x7, I64# x8, I64# x9, I64# x10, I64# x11, I64# x12, I64# x13, I64# x14, I64# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int64 where
  broadcast (I64# x) = let !v = broadcastInt64X4# x in MkInt64X16WithVec256 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Int64 where
  selectF (MkBoolX16 !cond) (MkInt64X16WithVec256 x0 x1 x2 x3) (MkInt64X16WithVec256 y0 y1 y2 y3) = MkInt64X16WithVec256 (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 4) x1 y1) (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 8) x2 y2) (selectInt64X4# (fromIntegral $ cond `unsafeShiftR` 12) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany Int64X4# [i0, i1, i2, i3], ShuffleMany Int64X4# [i4, i5, i6, i7], ShuffleMany Int64X4# [i8, i9, i10, i11], ShuffleMany Int64X4# [i12, i13, i14, i15]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int64 where
  unaryShuffle (MkInt64X16WithVec256 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt64X16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany Int64X4# [i0, i1, i2, i3], ShuffleMany Int64X4# [i4, i5, i6, i7], ShuffleMany Int64X4# [i8, i9, i10, i11], ShuffleMany Int64X4# [i12, i13, i14, i15]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int64 where
  binaryShuffle (MkInt64X16WithVec256 x0 x1 x2 x3) (MkInt64X16WithVec256 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt64X16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int64 where
  eqF (MkInt64X16WithVec256 u0 u1 u2 u3) (MkInt64X16WithVec256 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (eqInt64X4# u0 v0)) .|. (fromIntegral (eqInt64X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (eqInt64X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (eqInt64X4# u3 v3) `unsafeShiftL` 12)
  {-# INLINE eqF #-}
instance OrderedF X16 Int64 where
  ltF (MkInt64X16WithVec256 u0 u1 u2 u3) (MkInt64X16WithVec256 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (ltInt64X4# u0 v0)) .|. (fromIntegral (ltInt64X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (ltInt64X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (ltInt64X4# u3 v3) `unsafeShiftL` 12)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int64 where
  minF (MkInt64X16WithVec256 u0 u1 u2 u3) (MkInt64X16WithVec256 v0 v1 v2 v3) = MkInt64X16WithVec256 (minInt64X4# u0 v0) (minInt64X4# u1 v1) (minInt64X4# u2 v2) (minInt64X4# u3 v3)
  maxF (MkInt64X16WithVec256 u0 u1 u2 u3) (MkInt64X16WithVec256 v0 v1 v2 v3) = MkInt64X16WithVec256 (maxInt64X4# u0 v0) (maxInt64X4# u1 v1) (maxInt64X4# u2 v2) (maxInt64X4# u3 v3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int64 where
  plusF (MkInt64X16WithVec256 u0 u1 u2 u3) (MkInt64X16WithVec256 v0 v1 v2 v3) = MkInt64X16WithVec256 (plusInt64X4# u0 v0) (plusInt64X4# u1 v1) (plusInt64X4# u2 v2) (plusInt64X4# u3 v3)
  minusF (MkInt64X16WithVec256 u0 u1 u2 u3) (MkInt64X16WithVec256 v0 v1 v2 v3) = MkInt64X16WithVec256 (minusInt64X4# u0 v0) (minusInt64X4# u1 v1) (minusInt64X4# u2 v2) (minusInt64X4# u3 v3)
  timesF (MkInt64X16WithVec256 u0 u1 u2 u3) (MkInt64X16WithVec256 v0 v1 v2 v3) = MkInt64X16WithVec256 (timesInt64X4# u0 v0) (timesInt64X4# u1 v1) (timesInt64X4# u2 v2) (timesInt64X4# u3 v3)
  negateF (MkInt64X16WithVec256 u0 u1 u2 u3) = MkInt64X16WithVec256 (negateInt64X4# u0) (negateInt64X4# u1) (negateInt64X4# u2) (negateInt64X4# u3)
  absF (MkInt64X16WithVec256 u0 u1 u2 u3) = MkInt64X16WithVec256 (absInt64X4# u0) (absInt64X4# u1) (absInt64X4# u2) (absInt64X4# u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Int64 where
  andF (MkInt64X16WithVec256 u0 u1 u2 u3) (MkInt64X16WithVec256 v0 v1 v2 v3) = MkInt64X16WithVec256 (andInt64X4# u0 v0) (andInt64X4# u1 v1) (andInt64X4# u2 v2) (andInt64X4# u3 v3)
  orF (MkInt64X16WithVec256 u0 u1 u2 u3) (MkInt64X16WithVec256 v0 v1 v2 v3) = MkInt64X16WithVec256 (orInt64X4# u0 v0) (orInt64X4# u1 v1) (orInt64X4# u2 v2) (orInt64X4# u3 v3)
  xorF (MkInt64X16WithVec256 u0 u1 u2 u3) (MkInt64X16WithVec256 v0 v1 v2 v3) = MkInt64X16WithVec256 (xorInt64X4# u0 v0) (xorInt64X4# u1 v1) (xorInt64X4# u2 v2) (xorInt64X4# u3 v3)
  complementF (MkInt64X16WithVec256 u0 u1 u2 u3) = MkInt64X16WithVec256 (complementInt64X4# u0) (complementInt64X4# u1) (complementInt64X4# u2) (complementInt64X4# u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int64 where
  shiftLF (MkInt64X16WithVec256 u0 u1 u2 u3) (I# i) = MkInt64X16WithVec256 (shiftLInt64X4# u0 i) (shiftLInt64X4# u1 i) (shiftLInt64X4# u2 i) (shiftLInt64X4# u3 i)
  shiftRF (MkInt64X16WithVec256 u0 u1 u2 u3) (I# i) = MkInt64X16WithVec256 (shiftRInt64X4# u0 i) (shiftRInt64X4# u1 i) (shiftRInt64X4# u2 i) (shiftRInt64X4# u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Int64 where
  enumFromZero = MkInt64X16WithVec256 (packInt64X4# (# 0#Int64, 1#Int64, 2#Int64, 3#Int64 #)) (packInt64X4# (# 4#Int64, 5#Int64, 6#Int64, 7#Int64 #)) (packInt64X4# (# 8#Int64, 9#Int64, 10#Int64, 11#Int64 #)) (packInt64X4# (# 12#Int64, 13#Int64, 14#Int64, 15#Int64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int64 where
  indexByteArraySIMD# ba i = MkInt64X16WithVec256 (indexInt64ArrayAsInt64X4# ba i) (indexInt64ArrayAsInt64X4# ba (i +# 4#)) (indexInt64ArrayAsInt64X4# ba (i +# 8#)) (indexInt64ArrayAsInt64X4# ba (i +# 12#))
  readByteArraySIMD# mba i s0 = case readInt64ArrayAsInt64X4# mba i s0 of (# s1, v0 #) -> case readInt64ArrayAsInt64X4# mba (i +# 4#) s1 of (# s2, v1 #) -> case readInt64ArrayAsInt64X4# mba (i +# 8#) s2 of (# s3, v2 #) -> case readInt64ArrayAsInt64X4# mba (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkInt64X16WithVec256 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkInt64X16WithVec256 v0 v1 v2 v3) s0 = case writeInt64ArrayAsInt64X4# mba i v0 s0 of s1 -> case writeInt64ArrayAsInt64X4# mba (i +# 4#) v1 s1 of s2 -> case writeInt64ArrayAsInt64X4# mba (i +# 8#) v2 s2 of s3 -> writeInt64ArrayAsInt64X4# mba (i +# 12#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt64OffAddrAsInt64X4# addr i s0 of (# s1, v0 #) -> case readInt64OffAddrAsInt64X4# addr (i +# 4#) s1 of (# s2, v1 #) -> case readInt64OffAddrAsInt64X4# addr (i +# 8#) s2 of (# s3, v2 #) -> case readInt64OffAddrAsInt64X4# addr (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkInt64X16WithVec256 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X16WithVec256 v0 v1 v2 v3) = IO (\s0 -> case writeInt64OffAddrAsInt64X4# addr i v0 s0 of s1 -> case writeInt64OffAddrAsInt64X4# addr (i +# 4#) v1 s1 of s2 -> case writeInt64OffAddrAsInt64X4# addr (i +# 8#) v2 s2 of s3 -> case writeInt64OffAddrAsInt64X4# addr (i +# 12#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Word8 = MkWord8X16 Word8X16#
instance PackX16 X16 Word8 where
  mkX16 (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) (W8# x8) (W8# x9) (W8# x10) (W8# x11) (W8# x12) (W8# x13) (W8# x14) (W8# x15) = MkWord8X16 (packWord8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #))
  unpackX16 (MkWord8X16 v0) = case unpackWord8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> (W8# x0, W8# x1, W8# x2, W8# x3, W8# x4, W8# x5, W8# x6, W8# x7, W8# x8, W8# x9, W8# x10, W8# x11, W8# x12, W8# x13, W8# x14, W8# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word8 where
  broadcast (W8# x) = MkWord8X16 (broadcastWord8X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X16 Word8 where
  selectF (MkBoolX16 !cond) (MkWord8X16 x0) (MkWord8X16 y0) = MkWord8X16 (selectWord8X16# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 16, ShuffleMany Word8X16# indices) => UnaryShuffle indices X16 Word8 where
  unaryShuffle (MkWord8X16 x) = MkWord8X16 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 32, ShuffleMany Word8X16# indices) => BinaryShuffle indices X16 Word8 where
  binaryShuffle (MkWord8X16 x0) (MkWord8X16 x1) = MkWord8X16 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word8 where
  eqF (MkWord8X16 u0) (MkWord8X16 v0) = MkBoolX16 $ (fromIntegral (eqWord8X16# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X16 Word8 where
  ltF (MkWord8X16 u0) (MkWord8X16 v0) = MkBoolX16 $ (fromIntegral (ltWord8X16# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word8 where
  minF (MkWord8X16 u0) (MkWord8X16 v0) = MkWord8X16 (minWord8X16# u0 v0)
  maxF (MkWord8X16 u0) (MkWord8X16 v0) = MkWord8X16 (maxWord8X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word8 where
  plusF (MkWord8X16 u0) (MkWord8X16 v0) = MkWord8X16 (plusWord8X16# u0 v0)
  minusF (MkWord8X16 u0) (MkWord8X16 v0) = MkWord8X16 (minusWord8X16# u0 v0)
  timesF (MkWord8X16 u0) (MkWord8X16 v0) = MkWord8X16 (timesWord8X16# u0 v0)
  -- Currently, there is no negateWord8X16#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Word8 where
  andF (MkWord8X16 u0) (MkWord8X16 v0) = MkWord8X16 (andWord8X16# u0 v0)
  orF (MkWord8X16 u0) (MkWord8X16 v0) = MkWord8X16 (orWord8X16# u0 v0)
  xorF (MkWord8X16 u0) (MkWord8X16 v0) = MkWord8X16 (xorWord8X16# u0 v0)
  complementF (MkWord8X16 u0) = MkWord8X16 (complementWord8X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word8 where
  shiftLF (MkWord8X16 u0) (I# i) = MkWord8X16 (shiftLWord8X16# u0 i)
  shiftRF (MkWord8X16 u0) (I# i) = MkWord8X16 (shiftRWord8X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Word8 where
  enumFromZero = MkWord8X16 (packWord8X16# (# 0#Word8, 1#Word8, 2#Word8, 3#Word8, 4#Word8, 5#Word8, 6#Word8, 7#Word8, 8#Word8, 9#Word8, 10#Word8, 11#Word8, 12#Word8, 13#Word8, 14#Word8, 15#Word8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word8 where
  indexByteArraySIMD# ba i = MkWord8X16 (indexWord8ArrayAsWord8X16# ba i)
  readByteArraySIMD# mba i s0 = case readWord8ArrayAsWord8X16# mba i s0 of (# s1, v0 #) -> (# s1, MkWord8X16 v0 #)
  writeByteArraySIMD# mba i (MkWord8X16 v0) s0 = writeWord8ArrayAsWord8X16# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord8OffAddrAsWord8X16# addr i s0 of (# s1, v0 #) -> (# s1, MkWord8X16 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X16 v0) = IO (\s0 -> case writeWord8OffAddrAsWord8X16# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Word16 = MkWord16X16 Word16X16#
instance PackX16 X16 Word16 where
  mkX16 (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) (W16# x8) (W16# x9) (W16# x10) (W16# x11) (W16# x12) (W16# x13) (W16# x14) (W16# x15) = MkWord16X16 (packWord16X16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #))
  unpackX16 (MkWord16X16 v0) = case unpackWord16X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> (W16# x0, W16# x1, W16# x2, W16# x3, W16# x4, W16# x5, W16# x6, W16# x7, W16# x8, W16# x9, W16# x10, W16# x11, W16# x12, W16# x13, W16# x14, W16# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word16 where
  broadcast (W16# x) = MkWord16X16 (broadcastWord16X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X16 Word16 where
  selectF (MkBoolX16 !cond) (MkWord16X16 x0) (MkWord16X16 y0) = MkWord16X16 (selectWord16X16# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 16, ShuffleMany Word16X16# indices) => UnaryShuffle indices X16 Word16 where
  unaryShuffle (MkWord16X16 x) = MkWord16X16 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 32, ShuffleMany Word16X16# indices) => BinaryShuffle indices X16 Word16 where
  binaryShuffle (MkWord16X16 x0) (MkWord16X16 x1) = MkWord16X16 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word16 where
  eqF (MkWord16X16 u0) (MkWord16X16 v0) = MkBoolX16 $ (fromIntegral (eqWord16X16# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X16 Word16 where
  ltF (MkWord16X16 u0) (MkWord16X16 v0) = MkBoolX16 $ (fromIntegral (ltWord16X16# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word16 where
  minF (MkWord16X16 u0) (MkWord16X16 v0) = MkWord16X16 (minWord16X16# u0 v0)
  maxF (MkWord16X16 u0) (MkWord16X16 v0) = MkWord16X16 (maxWord16X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word16 where
  plusF (MkWord16X16 u0) (MkWord16X16 v0) = MkWord16X16 (plusWord16X16# u0 v0)
  minusF (MkWord16X16 u0) (MkWord16X16 v0) = MkWord16X16 (minusWord16X16# u0 v0)
  timesF (MkWord16X16 u0) (MkWord16X16 v0) = MkWord16X16 (timesWord16X16# u0 v0)
  -- Currently, there is no negateWord16X16#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Word16 where
  andF (MkWord16X16 u0) (MkWord16X16 v0) = MkWord16X16 (andWord16X16# u0 v0)
  orF (MkWord16X16 u0) (MkWord16X16 v0) = MkWord16X16 (orWord16X16# u0 v0)
  xorF (MkWord16X16 u0) (MkWord16X16 v0) = MkWord16X16 (xorWord16X16# u0 v0)
  complementF (MkWord16X16 u0) = MkWord16X16 (complementWord16X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word16 where
  shiftLF (MkWord16X16 u0) (I# i) = MkWord16X16 (shiftLWord16X16# u0 i)
  shiftRF (MkWord16X16 u0) (I# i) = MkWord16X16 (shiftRWord16X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Word16 where
  enumFromZero = MkWord16X16 (packWord16X16# (# 0#Word16, 1#Word16, 2#Word16, 3#Word16, 4#Word16, 5#Word16, 6#Word16, 7#Word16, 8#Word16, 9#Word16, 10#Word16, 11#Word16, 12#Word16, 13#Word16, 14#Word16, 15#Word16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word16 where
  indexByteArraySIMD# ba i = MkWord16X16 (indexWord16ArrayAsWord16X16# ba i)
  readByteArraySIMD# mba i s0 = case readWord16ArrayAsWord16X16# mba i s0 of (# s1, v0 #) -> (# s1, MkWord16X16 v0 #)
  writeByteArraySIMD# mba i (MkWord16X16 v0) s0 = writeWord16ArrayAsWord16X16# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord16OffAddrAsWord16X16# addr i s0 of (# s1, v0 #) -> (# s1, MkWord16X16 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X16 v0) = IO (\s0 -> case writeWord16OffAddrAsWord16X16# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Word32 = MkWord32X16WithVec256 Word32X8# Word32X8#
instance PackX16 X16 Word32 where
  mkX16 (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) (W32# x8) (W32# x9) (W32# x10) (W32# x11) (W32# x12) (W32# x13) (W32# x14) (W32# x15) = MkWord32X16WithVec256 (packWord32X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packWord32X8# (# x8, x9, x10, x11, x12, x13, x14, x15 #))
  unpackX16 (MkWord32X16WithVec256 v0 v1) = case unpackWord32X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackWord32X8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> (W32# x0, W32# x1, W32# x2, W32# x3, W32# x4, W32# x5, W32# x6, W32# x7, W32# x8, W32# x9, W32# x10, W32# x11, W32# x12, W32# x13, W32# x14, W32# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word32 where
  broadcast (W32# x) = let !v = broadcastWord32X8# x in MkWord32X16WithVec256 v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Word32 where
  selectF (MkBoolX16 !cond) (MkWord32X16WithVec256 x0 x1) (MkWord32X16WithVec256 y0 y1) = MkWord32X16WithVec256 (selectWord32X8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord32X8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany Word32X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Word32X8# [i8, i9, i10, i11, i12, i13, i14, i15]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word32 where
  unaryShuffle (MkWord32X16WithVec256 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord32X16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany Word32X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Word32X8# [i8, i9, i10, i11, i12, i13, i14, i15]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word32 where
  binaryShuffle (MkWord32X16WithVec256 x0 x1) (MkWord32X16WithVec256 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord32X16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word32 where
  eqF (MkWord32X16WithVec256 u0 u1) (MkWord32X16WithVec256 v0 v1) = MkBoolX16 $ (fromIntegral (eqWord32X8# u0 v0)) .|. (fromIntegral (eqWord32X8# u1 v1) `unsafeShiftL` 8)
  {-# INLINE eqF #-}
instance OrderedF X16 Word32 where
  ltF (MkWord32X16WithVec256 u0 u1) (MkWord32X16WithVec256 v0 v1) = MkBoolX16 $ (fromIntegral (ltWord32X8# u0 v0)) .|. (fromIntegral (ltWord32X8# u1 v1) `unsafeShiftL` 8)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word32 where
  minF (MkWord32X16WithVec256 u0 u1) (MkWord32X16WithVec256 v0 v1) = MkWord32X16WithVec256 (minWord32X8# u0 v0) (minWord32X8# u1 v1)
  maxF (MkWord32X16WithVec256 u0 u1) (MkWord32X16WithVec256 v0 v1) = MkWord32X16WithVec256 (maxWord32X8# u0 v0) (maxWord32X8# u1 v1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word32 where
  plusF (MkWord32X16WithVec256 u0 u1) (MkWord32X16WithVec256 v0 v1) = MkWord32X16WithVec256 (plusWord32X8# u0 v0) (plusWord32X8# u1 v1)
  minusF (MkWord32X16WithVec256 u0 u1) (MkWord32X16WithVec256 v0 v1) = MkWord32X16WithVec256 (minusWord32X8# u0 v0) (minusWord32X8# u1 v1)
  timesF (MkWord32X16WithVec256 u0 u1) (MkWord32X16WithVec256 v0 v1) = MkWord32X16WithVec256 (timesWord32X8# u0 v0) (timesWord32X8# u1 v1)
  -- Currently, there is no negateWord32X8#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Word32 where
  andF (MkWord32X16WithVec256 u0 u1) (MkWord32X16WithVec256 v0 v1) = MkWord32X16WithVec256 (andWord32X8# u0 v0) (andWord32X8# u1 v1)
  orF (MkWord32X16WithVec256 u0 u1) (MkWord32X16WithVec256 v0 v1) = MkWord32X16WithVec256 (orWord32X8# u0 v0) (orWord32X8# u1 v1)
  xorF (MkWord32X16WithVec256 u0 u1) (MkWord32X16WithVec256 v0 v1) = MkWord32X16WithVec256 (xorWord32X8# u0 v0) (xorWord32X8# u1 v1)
  complementF (MkWord32X16WithVec256 u0 u1) = MkWord32X16WithVec256 (complementWord32X8# u0) (complementWord32X8# u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word32 where
  shiftLF (MkWord32X16WithVec256 u0 u1) (I# i) = MkWord32X16WithVec256 (shiftLWord32X8# u0 i) (shiftLWord32X8# u1 i)
  shiftRF (MkWord32X16WithVec256 u0 u1) (I# i) = MkWord32X16WithVec256 (shiftRWord32X8# u0 i) (shiftRWord32X8# u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Word32 where
  enumFromZero = MkWord32X16WithVec256 (packWord32X8# (# 0#Word32, 1#Word32, 2#Word32, 3#Word32, 4#Word32, 5#Word32, 6#Word32, 7#Word32 #)) (packWord32X8# (# 8#Word32, 9#Word32, 10#Word32, 11#Word32, 12#Word32, 13#Word32, 14#Word32, 15#Word32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word32 where
  indexByteArraySIMD# ba i = MkWord32X16WithVec256 (indexWord32ArrayAsWord32X8# ba i) (indexWord32ArrayAsWord32X8# ba (i +# 8#))
  readByteArraySIMD# mba i s0 = case readWord32ArrayAsWord32X8# mba i s0 of (# s1, v0 #) -> case readWord32ArrayAsWord32X8# mba (i +# 8#) s1 of (# s2, v1 #) -> (# s2, MkWord32X16WithVec256 v0 v1 #)
  writeByteArraySIMD# mba i (MkWord32X16WithVec256 v0 v1) s0 = case writeWord32ArrayAsWord32X8# mba i v0 s0 of s1 -> writeWord32ArrayAsWord32X8# mba (i +# 8#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord32OffAddrAsWord32X8# addr i s0 of (# s1, v0 #) -> case readWord32OffAddrAsWord32X8# addr (i +# 8#) s1 of (# s2, v1 #) -> (# s2, MkWord32X16WithVec256 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X16WithVec256 v0 v1) = IO (\s0 -> case writeWord32OffAddrAsWord32X8# addr i v0 s0 of s1 -> case writeWord32OffAddrAsWord32X8# addr (i +# 8#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Word64 = MkWord64X16WithVec256 Word64X4# Word64X4# Word64X4# Word64X4#
instance PackX16 X16 Word64 where
  mkX16 (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) (W64# x8) (W64# x9) (W64# x10) (W64# x11) (W64# x12) (W64# x13) (W64# x14) (W64# x15) = MkWord64X16WithVec256 (packWord64X4# (# x0, x1, x2, x3 #)) (packWord64X4# (# x4, x5, x6, x7 #)) (packWord64X4# (# x8, x9, x10, x11 #)) (packWord64X4# (# x12, x13, x14, x15 #))
  unpackX16 (MkWord64X16WithVec256 v0 v1 v2 v3) = case unpackWord64X4# v0 of (# x0, x1, x2, x3 #) -> case unpackWord64X4# v1 of (# x4, x5, x6, x7 #) -> case unpackWord64X4# v2 of (# x8, x9, x10, x11 #) -> case unpackWord64X4# v3 of (# x12, x13, x14, x15 #) -> (W64# x0, W64# x1, W64# x2, W64# x3, W64# x4, W64# x5, W64# x6, W64# x7, W64# x8, W64# x9, W64# x10, W64# x11, W64# x12, W64# x13, W64# x14, W64# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word64 where
  broadcast (W64# x) = let !v = broadcastWord64X4# x in MkWord64X16WithVec256 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Word64 where
  selectF (MkBoolX16 !cond) (MkWord64X16WithVec256 x0 x1 x2 x3) (MkWord64X16WithVec256 y0 y1 y2 y3) = MkWord64X16WithVec256 (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 4) x1 y1) (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 8) x2 y2) (selectWord64X4# (fromIntegral $ cond `unsafeShiftR` 12) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany Word64X4# [i0, i1, i2, i3], ShuffleMany Word64X4# [i4, i5, i6, i7], ShuffleMany Word64X4# [i8, i9, i10, i11], ShuffleMany Word64X4# [i12, i13, i14, i15]) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word64 where
  unaryShuffle (MkWord64X16WithVec256 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord64X16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany Word64X4# [i0, i1, i2, i3], ShuffleMany Word64X4# [i4, i5, i6, i7], ShuffleMany Word64X4# [i8, i9, i10, i11], ShuffleMany Word64X4# [i12, i13, i14, i15]) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word64 where
  binaryShuffle (MkWord64X16WithVec256 x0 x1 x2 x3) (MkWord64X16WithVec256 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord64X16WithVec256 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word64 where
  eqF (MkWord64X16WithVec256 u0 u1 u2 u3) (MkWord64X16WithVec256 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (eqWord64X4# u0 v0)) .|. (fromIntegral (eqWord64X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (eqWord64X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (eqWord64X4# u3 v3) `unsafeShiftL` 12)
  {-# INLINE eqF #-}
instance OrderedF X16 Word64 where
  ltF (MkWord64X16WithVec256 u0 u1 u2 u3) (MkWord64X16WithVec256 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (ltWord64X4# u0 v0)) .|. (fromIntegral (ltWord64X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (ltWord64X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (ltWord64X4# u3 v3) `unsafeShiftL` 12)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word64 where
  minF (MkWord64X16WithVec256 u0 u1 u2 u3) (MkWord64X16WithVec256 v0 v1 v2 v3) = MkWord64X16WithVec256 (minWord64X4# u0 v0) (minWord64X4# u1 v1) (minWord64X4# u2 v2) (minWord64X4# u3 v3)
  maxF (MkWord64X16WithVec256 u0 u1 u2 u3) (MkWord64X16WithVec256 v0 v1 v2 v3) = MkWord64X16WithVec256 (maxWord64X4# u0 v0) (maxWord64X4# u1 v1) (maxWord64X4# u2 v2) (maxWord64X4# u3 v3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word64 where
  plusF (MkWord64X16WithVec256 u0 u1 u2 u3) (MkWord64X16WithVec256 v0 v1 v2 v3) = MkWord64X16WithVec256 (plusWord64X4# u0 v0) (plusWord64X4# u1 v1) (plusWord64X4# u2 v2) (plusWord64X4# u3 v3)
  minusF (MkWord64X16WithVec256 u0 u1 u2 u3) (MkWord64X16WithVec256 v0 v1 v2 v3) = MkWord64X16WithVec256 (minusWord64X4# u0 v0) (minusWord64X4# u1 v1) (minusWord64X4# u2 v2) (minusWord64X4# u3 v3)
  timesF (MkWord64X16WithVec256 u0 u1 u2 u3) (MkWord64X16WithVec256 v0 v1 v2 v3) = MkWord64X16WithVec256 (timesWord64X4# u0 v0) (timesWord64X4# u1 v1) (timesWord64X4# u2 v2) (timesWord64X4# u3 v3)
  -- Currently, there is no negateWord64X4#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Word64 where
  andF (MkWord64X16WithVec256 u0 u1 u2 u3) (MkWord64X16WithVec256 v0 v1 v2 v3) = MkWord64X16WithVec256 (andWord64X4# u0 v0) (andWord64X4# u1 v1) (andWord64X4# u2 v2) (andWord64X4# u3 v3)
  orF (MkWord64X16WithVec256 u0 u1 u2 u3) (MkWord64X16WithVec256 v0 v1 v2 v3) = MkWord64X16WithVec256 (orWord64X4# u0 v0) (orWord64X4# u1 v1) (orWord64X4# u2 v2) (orWord64X4# u3 v3)
  xorF (MkWord64X16WithVec256 u0 u1 u2 u3) (MkWord64X16WithVec256 v0 v1 v2 v3) = MkWord64X16WithVec256 (xorWord64X4# u0 v0) (xorWord64X4# u1 v1) (xorWord64X4# u2 v2) (xorWord64X4# u3 v3)
  complementF (MkWord64X16WithVec256 u0 u1 u2 u3) = MkWord64X16WithVec256 (complementWord64X4# u0) (complementWord64X4# u1) (complementWord64X4# u2) (complementWord64X4# u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word64 where
  shiftLF (MkWord64X16WithVec256 u0 u1 u2 u3) (I# i) = MkWord64X16WithVec256 (shiftLWord64X4# u0 i) (shiftLWord64X4# u1 i) (shiftLWord64X4# u2 i) (shiftLWord64X4# u3 i)
  shiftRF (MkWord64X16WithVec256 u0 u1 u2 u3) (I# i) = MkWord64X16WithVec256 (shiftRWord64X4# u0 i) (shiftRWord64X4# u1 i) (shiftRWord64X4# u2 i) (shiftRWord64X4# u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Word64 where
  enumFromZero = MkWord64X16WithVec256 (packWord64X4# (# 0#Word64, 1#Word64, 2#Word64, 3#Word64 #)) (packWord64X4# (# 4#Word64, 5#Word64, 6#Word64, 7#Word64 #)) (packWord64X4# (# 8#Word64, 9#Word64, 10#Word64, 11#Word64 #)) (packWord64X4# (# 12#Word64, 13#Word64, 14#Word64, 15#Word64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word64 where
  indexByteArraySIMD# ba i = MkWord64X16WithVec256 (indexWord64ArrayAsWord64X4# ba i) (indexWord64ArrayAsWord64X4# ba (i +# 4#)) (indexWord64ArrayAsWord64X4# ba (i +# 8#)) (indexWord64ArrayAsWord64X4# ba (i +# 12#))
  readByteArraySIMD# mba i s0 = case readWord64ArrayAsWord64X4# mba i s0 of (# s1, v0 #) -> case readWord64ArrayAsWord64X4# mba (i +# 4#) s1 of (# s2, v1 #) -> case readWord64ArrayAsWord64X4# mba (i +# 8#) s2 of (# s3, v2 #) -> case readWord64ArrayAsWord64X4# mba (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkWord64X16WithVec256 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkWord64X16WithVec256 v0 v1 v2 v3) s0 = case writeWord64ArrayAsWord64X4# mba i v0 s0 of s1 -> case writeWord64ArrayAsWord64X4# mba (i +# 4#) v1 s1 of s2 -> case writeWord64ArrayAsWord64X4# mba (i +# 8#) v2 s2 of s3 -> writeWord64ArrayAsWord64X4# mba (i +# 12#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord64OffAddrAsWord64X4# addr i s0 of (# s1, v0 #) -> case readWord64OffAddrAsWord64X4# addr (i +# 4#) s1 of (# s2, v1 #) -> case readWord64OffAddrAsWord64X4# addr (i +# 8#) s2 of (# s3, v2 #) -> case readWord64OffAddrAsWord64X4# addr (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkWord64X16WithVec256 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X16WithVec256 v0 v1 v2 v3) = IO (\s0 -> case writeWord64OffAddrAsWord64X4# addr i v0 s0 of s1 -> case writeWord64OffAddrAsWord64X4# addr (i +# 4#) v1 s1 of s2 -> case writeWord64OffAddrAsWord64X4# addr (i +# 8#) v2 s2 of s3 -> case writeWord64OffAddrAsWord64X4# addr (i +# 12#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
newtype instance X16 (Sum a) = MkSumX16 (X16 a)
instance PackX16 X16 a => PackX16 X16 (Sum a) where
  mkX16 = coerce (mkX16 @X16 @a)
  unpackX16 = coerce (unpackX16 @X16 @a)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 a => Broadcast X16 (Sum a) where
  broadcast = coerce (broadcast @X16 @a)
  {-# INLINE broadcast #-}
instance SelectableF X16 a => SelectableF X16 (Sum a) where
  selectF = coerce (selectF @X16 @a)
  {-# INLINE selectF #-}
newtype instance X16 (Product a) = MkProductX16 (X16 a)
instance PackX16 X16 a => PackX16 X16 (Product a) where
  mkX16 = coerce (mkX16 @X16 @a)
  unpackX16 = coerce (unpackX16 @X16 @a)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 a => Broadcast X16 (Product a) where
  broadcast = coerce (broadcast @X16 @a)
  {-# INLINE broadcast #-}
instance SelectableF X16 a => SelectableF X16 (Product a) where
  selectF = coerce (selectF @X16 @a)
  {-# INLINE selectF #-}
newtype instance X16 (Min a) = MkMinX16 (X16 a)
instance PackX16 X16 a => PackX16 X16 (Min a) where
  mkX16 = coerce (mkX16 @X16 @a)
  unpackX16 = coerce (unpackX16 @X16 @a)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 a => Broadcast X16 (Min a) where
  broadcast = coerce (broadcast @X16 @a)
  {-# INLINE broadcast #-}
instance SelectableF X16 a => SelectableF X16 (Min a) where
  selectF = coerce (selectF @X16 @a)
  {-# INLINE selectF #-}
newtype instance X16 (Max a) = MkMaxX16 (X16 a)
instance PackX16 X16 a => PackX16 X16 (Max a) where
  mkX16 = coerce (mkX16 @X16 @a)
  unpackX16 = coerce (unpackX16 @X16 @a)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 a => Broadcast X16 (Max a) where
  broadcast = coerce (broadcast @X16 @a)
  {-# INLINE broadcast #-}
instance SelectableF X16 a => SelectableF X16 (Max a) where
  selectF = coerce (selectF @X16 @a)
  {-# INLINE selectF #-}
data instance X16 (Complex a) = MkComplexX16 !(X16 a) !(X16 a)
instance PackX16 X16 a => PackX16 X16 (Complex a) where
  mkX16 (x0 :+ y0) (x1 :+ y1) (x2 :+ y2) (x3 :+ y3) (x4 :+ y4) (x5 :+ y5) (x6 :+ y6) (x7 :+ y7) (x8 :+ y8) (x9 :+ y9) (x10 :+ y10) (x11 :+ y11) (x12 :+ y12) (x13 :+ y13) (x14 :+ y14) (x15 :+ y15) = MkComplexX16 (mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (mkX16 y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15)
  unpackX16 (MkComplexX16 s t) = case unpackX16 s of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> case unpackX16 t of (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15) -> (x0 :+ y0, x1 :+ y1, x2 :+ y2, x3 :+ y3, x4 :+ y4, x5 :+ y5, x6 :+ y6, x7 :+ y7, x8 :+ y8, x9 :+ y9, x10 :+ y10, x11 :+ y11, x12 :+ y12, x13 :+ y13, x14 :+ y14, x15 :+ y15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 a => Broadcast X16 (Complex a) where
  broadcast (x :+ y) = MkComplexX16 (broadcast x) (broadcast y)
  {-# INLINE broadcast #-}
instance SelectableF X16 a => SelectableF X16 (Complex a) where
  selectF !cond (MkComplexX16 x y) (MkComplexX16 x' y') = MkComplexX16 (selectF cond x x') (selectF cond y y')
  {-# INLINE selectF #-}
data instance X16 () = MkUnitX16
instance PackX16 X16 () where
  mkX16 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = MkUnitX16
  unpackX16 MkUnitX16 = ((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ())
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 () where
  broadcast _ = MkUnitX16
  {-# INLINE broadcast #-}
instance SelectableF X16 () where
  selectF _ _ _ = MkUnitX16
  {-# INLINE selectF #-}
data instance X16 (a0, a1) = MkTuple2X16 !(X16 a0) !(X16 a1)
instance (PackX16 X16 a0, PackX16 X16 a1) => PackX16 X16 (a0, a1) where
  mkX16 (x0_0, x0_1) (x1_0, x1_1) (x2_0, x2_1) (x3_0, x3_1) (x4_0, x4_1) (x5_0, x5_1) (x6_0, x6_1) (x7_0, x7_1) (x8_0, x8_1) (x9_0, x9_1) (x10_0, x10_1) (x11_0, x11_1) (x12_0, x12_1) (x13_0, x13_1) (x14_0, x14_1) (x15_0, x15_1) = MkTuple2X16 (mkX16 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0) (mkX16 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1)
  unpackX16 (MkTuple2X16 v0 v1) = case unpackX16 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0) -> case unpackX16 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1) -> ((x0_0, x0_1), (x1_0, x1_1), (x2_0, x2_1), (x3_0, x3_1), (x4_0, x4_1), (x5_0, x5_1), (x6_0, x6_1), (x7_0, x7_1), (x8_0, x8_1), (x9_0, x9_1), (x10_0, x10_1), (x11_0, x11_1), (x12_0, x12_1), (x13_0, x13_1), (x14_0, x14_1), (x15_0, x15_1))
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance (Broadcast X16 a0, Broadcast X16 a1) => Broadcast X16 (a0, a1) where
  broadcast (x0, x1) = MkTuple2X16 (broadcast x0) (broadcast x1)
  {-# INLINE broadcast #-}
instance (SelectableF X16 a0, SelectableF X16 a1) => SelectableF X16 (a0, a1) where
  selectF !cond (MkTuple2X16 x0 x1) (MkTuple2X16 y0 y1) = MkTuple2X16 (selectF cond x0 y0) (selectF cond x1 y1)
  {-# INLINE selectF #-}
data instance X16 (a0, a1, a2) = MkTuple3X16 !(X16 a0) !(X16 a1) !(X16 a2)
instance (PackX16 X16 a0, PackX16 X16 a1, PackX16 X16 a2) => PackX16 X16 (a0, a1, a2) where
  mkX16 (x0_0, x0_1, x0_2) (x1_0, x1_1, x1_2) (x2_0, x2_1, x2_2) (x3_0, x3_1, x3_2) (x4_0, x4_1, x4_2) (x5_0, x5_1, x5_2) (x6_0, x6_1, x6_2) (x7_0, x7_1, x7_2) (x8_0, x8_1, x8_2) (x9_0, x9_1, x9_2) (x10_0, x10_1, x10_2) (x11_0, x11_1, x11_2) (x12_0, x12_1, x12_2) (x13_0, x13_1, x13_2) (x14_0, x14_1, x14_2) (x15_0, x15_1, x15_2) = MkTuple3X16 (mkX16 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0) (mkX16 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1) (mkX16 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2)
  unpackX16 (MkTuple3X16 v0 v1 v2) = case unpackX16 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0) -> case unpackX16 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1) -> case unpackX16 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2) -> ((x0_0, x0_1, x0_2), (x1_0, x1_1, x1_2), (x2_0, x2_1, x2_2), (x3_0, x3_1, x3_2), (x4_0, x4_1, x4_2), (x5_0, x5_1, x5_2), (x6_0, x6_1, x6_2), (x7_0, x7_1, x7_2), (x8_0, x8_1, x8_2), (x9_0, x9_1, x9_2), (x10_0, x10_1, x10_2), (x11_0, x11_1, x11_2), (x12_0, x12_1, x12_2), (x13_0, x13_1, x13_2), (x14_0, x14_1, x14_2), (x15_0, x15_1, x15_2))
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance (Broadcast X16 a0, Broadcast X16 a1, Broadcast X16 a2) => Broadcast X16 (a0, a1, a2) where
  broadcast (x0, x1, x2) = MkTuple3X16 (broadcast x0) (broadcast x1) (broadcast x2)
  {-# INLINE broadcast #-}
instance (SelectableF X16 a0, SelectableF X16 a1, SelectableF X16 a2) => SelectableF X16 (a0, a1, a2) where
  selectF !cond (MkTuple3X16 x0 x1 x2) (MkTuple3X16 y0 y1 y2) = MkTuple3X16 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2)
  {-# INLINE selectF #-}
data instance X16 (a0, a1, a2, a3) = MkTuple4X16 !(X16 a0) !(X16 a1) !(X16 a2) !(X16 a3)
instance (PackX16 X16 a0, PackX16 X16 a1, PackX16 X16 a2, PackX16 X16 a3) => PackX16 X16 (a0, a1, a2, a3) where
  mkX16 (x0_0, x0_1, x0_2, x0_3) (x1_0, x1_1, x1_2, x1_3) (x2_0, x2_1, x2_2, x2_3) (x3_0, x3_1, x3_2, x3_3) (x4_0, x4_1, x4_2, x4_3) (x5_0, x5_1, x5_2, x5_3) (x6_0, x6_1, x6_2, x6_3) (x7_0, x7_1, x7_2, x7_3) (x8_0, x8_1, x8_2, x8_3) (x9_0, x9_1, x9_2, x9_3) (x10_0, x10_1, x10_2, x10_3) (x11_0, x11_1, x11_2, x11_3) (x12_0, x12_1, x12_2, x12_3) (x13_0, x13_1, x13_2, x13_3) (x14_0, x14_1, x14_2, x14_3) (x15_0, x15_1, x15_2, x15_3) = MkTuple4X16 (mkX16 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0) (mkX16 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1) (mkX16 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2) (mkX16 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3 x8_3 x9_3 x10_3 x11_3 x12_3 x13_3 x14_3 x15_3)
  unpackX16 (MkTuple4X16 v0 v1 v2 v3) = case unpackX16 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0) -> case unpackX16 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1) -> case unpackX16 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2) -> case unpackX16 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3, x8_3, x9_3, x10_3, x11_3, x12_3, x13_3, x14_3, x15_3) -> ((x0_0, x0_1, x0_2, x0_3), (x1_0, x1_1, x1_2, x1_3), (x2_0, x2_1, x2_2, x2_3), (x3_0, x3_1, x3_2, x3_3), (x4_0, x4_1, x4_2, x4_3), (x5_0, x5_1, x5_2, x5_3), (x6_0, x6_1, x6_2, x6_3), (x7_0, x7_1, x7_2, x7_3), (x8_0, x8_1, x8_2, x8_3), (x9_0, x9_1, x9_2, x9_3), (x10_0, x10_1, x10_2, x10_3), (x11_0, x11_1, x11_2, x11_3), (x12_0, x12_1, x12_2, x12_3), (x13_0, x13_1, x13_2, x13_3), (x14_0, x14_1, x14_2, x14_3), (x15_0, x15_1, x15_2, x15_3))
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance (Broadcast X16 a0, Broadcast X16 a1, Broadcast X16 a2, Broadcast X16 a3) => Broadcast X16 (a0, a1, a2, a3) where
  broadcast (x0, x1, x2, x3) = MkTuple4X16 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3)
  {-# INLINE broadcast #-}
instance (SelectableF X16 a0, SelectableF X16 a1, SelectableF X16 a2, SelectableF X16 a3) => SelectableF X16 (a0, a1, a2, a3) where
  selectF !cond (MkTuple4X16 x0 x1 x2 x3) (MkTuple4X16 y0 y1 y2 y3) = MkTuple4X16 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3)
  {-# INLINE selectF #-}
data instance X16 (a0, a1, a2, a3, a4) = MkTuple5X16 !(X16 a0) !(X16 a1) !(X16 a2) !(X16 a3) !(X16 a4)
instance (PackX16 X16 a0, PackX16 X16 a1, PackX16 X16 a2, PackX16 X16 a3, PackX16 X16 a4) => PackX16 X16 (a0, a1, a2, a3, a4) where
  mkX16 (x0_0, x0_1, x0_2, x0_3, x0_4) (x1_0, x1_1, x1_2, x1_3, x1_4) (x2_0, x2_1, x2_2, x2_3, x2_4) (x3_0, x3_1, x3_2, x3_3, x3_4) (x4_0, x4_1, x4_2, x4_3, x4_4) (x5_0, x5_1, x5_2, x5_3, x5_4) (x6_0, x6_1, x6_2, x6_3, x6_4) (x7_0, x7_1, x7_2, x7_3, x7_4) (x8_0, x8_1, x8_2, x8_3, x8_4) (x9_0, x9_1, x9_2, x9_3, x9_4) (x10_0, x10_1, x10_2, x10_3, x10_4) (x11_0, x11_1, x11_2, x11_3, x11_4) (x12_0, x12_1, x12_2, x12_3, x12_4) (x13_0, x13_1, x13_2, x13_3, x13_4) (x14_0, x14_1, x14_2, x14_3, x14_4) (x15_0, x15_1, x15_2, x15_3, x15_4) = MkTuple5X16 (mkX16 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0) (mkX16 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1) (mkX16 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2) (mkX16 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3 x8_3 x9_3 x10_3 x11_3 x12_3 x13_3 x14_3 x15_3) (mkX16 x0_4 x1_4 x2_4 x3_4 x4_4 x5_4 x6_4 x7_4 x8_4 x9_4 x10_4 x11_4 x12_4 x13_4 x14_4 x15_4)
  unpackX16 (MkTuple5X16 v0 v1 v2 v3 v4) = case unpackX16 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0) -> case unpackX16 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1) -> case unpackX16 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2) -> case unpackX16 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3, x8_3, x9_3, x10_3, x11_3, x12_3, x13_3, x14_3, x15_3) -> case unpackX16 v4 of (x0_4, x1_4, x2_4, x3_4, x4_4, x5_4, x6_4, x7_4, x8_4, x9_4, x10_4, x11_4, x12_4, x13_4, x14_4, x15_4) -> ((x0_0, x0_1, x0_2, x0_3, x0_4), (x1_0, x1_1, x1_2, x1_3, x1_4), (x2_0, x2_1, x2_2, x2_3, x2_4), (x3_0, x3_1, x3_2, x3_3, x3_4), (x4_0, x4_1, x4_2, x4_3, x4_4), (x5_0, x5_1, x5_2, x5_3, x5_4), (x6_0, x6_1, x6_2, x6_3, x6_4), (x7_0, x7_1, x7_2, x7_3, x7_4), (x8_0, x8_1, x8_2, x8_3, x8_4), (x9_0, x9_1, x9_2, x9_3, x9_4), (x10_0, x10_1, x10_2, x10_3, x10_4), (x11_0, x11_1, x11_2, x11_3, x11_4), (x12_0, x12_1, x12_2, x12_3, x12_4), (x13_0, x13_1, x13_2, x13_3, x13_4), (x14_0, x14_1, x14_2, x14_3, x14_4), (x15_0, x15_1, x15_2, x15_3, x15_4))
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance (Broadcast X16 a0, Broadcast X16 a1, Broadcast X16 a2, Broadcast X16 a3, Broadcast X16 a4) => Broadcast X16 (a0, a1, a2, a3, a4) where
  broadcast (x0, x1, x2, x3, x4) = MkTuple5X16 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4)
  {-# INLINE broadcast #-}
instance (SelectableF X16 a0, SelectableF X16 a1, SelectableF X16 a2, SelectableF X16 a3, SelectableF X16 a4) => SelectableF X16 (a0, a1, a2, a3, a4) where
  selectF !cond (MkTuple5X16 x0 x1 x2 x3 x4) (MkTuple5X16 y0 y1 y2 y3 y4) = MkTuple5X16 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4)
  {-# INLINE selectF #-}
data instance X16 (a0, a1, a2, a3, a4, a5) = MkTuple6X16 !(X16 a0) !(X16 a1) !(X16 a2) !(X16 a3) !(X16 a4) !(X16 a5)
instance (PackX16 X16 a0, PackX16 X16 a1, PackX16 X16 a2, PackX16 X16 a3, PackX16 X16 a4, PackX16 X16 a5) => PackX16 X16 (a0, a1, a2, a3, a4, a5) where
  mkX16 (x0_0, x0_1, x0_2, x0_3, x0_4, x0_5) (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5) (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5) (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5) (x4_0, x4_1, x4_2, x4_3, x4_4, x4_5) (x5_0, x5_1, x5_2, x5_3, x5_4, x5_5) (x6_0, x6_1, x6_2, x6_3, x6_4, x6_5) (x7_0, x7_1, x7_2, x7_3, x7_4, x7_5) (x8_0, x8_1, x8_2, x8_3, x8_4, x8_5) (x9_0, x9_1, x9_2, x9_3, x9_4, x9_5) (x10_0, x10_1, x10_2, x10_3, x10_4, x10_5) (x11_0, x11_1, x11_2, x11_3, x11_4, x11_5) (x12_0, x12_1, x12_2, x12_3, x12_4, x12_5) (x13_0, x13_1, x13_2, x13_3, x13_4, x13_5) (x14_0, x14_1, x14_2, x14_3, x14_4, x14_5) (x15_0, x15_1, x15_2, x15_3, x15_4, x15_5) = MkTuple6X16 (mkX16 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0) (mkX16 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1) (mkX16 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2) (mkX16 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3 x8_3 x9_3 x10_3 x11_3 x12_3 x13_3 x14_3 x15_3) (mkX16 x0_4 x1_4 x2_4 x3_4 x4_4 x5_4 x6_4 x7_4 x8_4 x9_4 x10_4 x11_4 x12_4 x13_4 x14_4 x15_4) (mkX16 x0_5 x1_5 x2_5 x3_5 x4_5 x5_5 x6_5 x7_5 x8_5 x9_5 x10_5 x11_5 x12_5 x13_5 x14_5 x15_5)
  unpackX16 (MkTuple6X16 v0 v1 v2 v3 v4 v5) = case unpackX16 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0) -> case unpackX16 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1) -> case unpackX16 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2) -> case unpackX16 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3, x8_3, x9_3, x10_3, x11_3, x12_3, x13_3, x14_3, x15_3) -> case unpackX16 v4 of (x0_4, x1_4, x2_4, x3_4, x4_4, x5_4, x6_4, x7_4, x8_4, x9_4, x10_4, x11_4, x12_4, x13_4, x14_4, x15_4) -> case unpackX16 v5 of (x0_5, x1_5, x2_5, x3_5, x4_5, x5_5, x6_5, x7_5, x8_5, x9_5, x10_5, x11_5, x12_5, x13_5, x14_5, x15_5) -> ((x0_0, x0_1, x0_2, x0_3, x0_4, x0_5), (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5), (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5), (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5), (x4_0, x4_1, x4_2, x4_3, x4_4, x4_5), (x5_0, x5_1, x5_2, x5_3, x5_4, x5_5), (x6_0, x6_1, x6_2, x6_3, x6_4, x6_5), (x7_0, x7_1, x7_2, x7_3, x7_4, x7_5), (x8_0, x8_1, x8_2, x8_3, x8_4, x8_5), (x9_0, x9_1, x9_2, x9_3, x9_4, x9_5), (x10_0, x10_1, x10_2, x10_3, x10_4, x10_5), (x11_0, x11_1, x11_2, x11_3, x11_4, x11_5), (x12_0, x12_1, x12_2, x12_3, x12_4, x12_5), (x13_0, x13_1, x13_2, x13_3, x13_4, x13_5), (x14_0, x14_1, x14_2, x14_3, x14_4, x14_5), (x15_0, x15_1, x15_2, x15_3, x15_4, x15_5))
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance (Broadcast X16 a0, Broadcast X16 a1, Broadcast X16 a2, Broadcast X16 a3, Broadcast X16 a4, Broadcast X16 a5) => Broadcast X16 (a0, a1, a2, a3, a4, a5) where
  broadcast (x0, x1, x2, x3, x4, x5) = MkTuple6X16 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4) (broadcast x5)
  {-# INLINE broadcast #-}
instance (SelectableF X16 a0, SelectableF X16 a1, SelectableF X16 a2, SelectableF X16 a3, SelectableF X16 a4, SelectableF X16 a5) => SelectableF X16 (a0, a1, a2, a3, a4, a5) where
  selectF !cond (MkTuple6X16 x0 x1 x2 x3 x4 x5) (MkTuple6X16 y0 y1 y2 y3 y4 y5) = MkTuple6X16 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4) (selectF cond x5 y5)
  {-# INLINE selectF #-}
instance (PackX16 X16 a, PackX16 X16 b) => LiftSIMD X16 a b where
  liftSIMD f !v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9) (f x10) (f x11) (f x12) (f x13) (f x14) (f x15)
  {-# INLINE liftSIMD #-}
instance (PackX16 X16 a, PackX16 X16 b, PackX16 X16 c) => LiftSIMD2 X16 a b c where
  liftSIMD2 f !u !v = case unpackX16 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> case unpackX16 v of (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15) -> mkX16 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4) (f x5 y5) (f x6 y6) (f x7 y7) (f x8 y8) (f x9 y9) (f x10 y10) (f x11 y11) (f x12 y12) (f x13 y13) (f x14 y14) (f x15 y15)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X16 where
  mkTuple2 = MkTuple2X16
  mkTuple3 = MkTuple3X16
  mkTuple4 = MkTuple4X16
  mkTuple5 = MkTuple5X16
  mkTuple6 = MkTuple6X16
  deconstructTuple2 (MkTuple2X16 v0 v1) = (v0, v1)
  deconstructTuple3 (MkTuple3X16 v0 v1 v2) = (v0, v1, v2)
  deconstructTuple4 (MkTuple4X16 v0 v1 v2 v3) = (v0, v1, v2, v3)
  deconstructTuple5 (MkTuple5X16 v0 v1 v2 v3 v4) = (v0, v1, v2, v3, v4)
  deconstructTuple6 (MkTuple6X16 v0 v1 v2 v3 v4 v5) = (v0, v1, v2, v3, v4, v5)
  mkSum = coerce
  getSum' = coerce
  mkProduct = coerce
  getProduct' = coerce
  mkMin = coerce
  getMin' = coerce
  mkMax = coerce
  getMax' = coerce
  mkComplex = MkComplexX16
  deconstructComplex (MkComplexX16 x y) = (x, y)
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
deriving via WrappedMulti X16 a instance SelectableF X16 a => Selectable (X16 a)
deriving via WrappedMulti X16 a instance NumF X16 a => Num (X16 a)
deriving via WrappedMulti X16 a instance FractionalF X16 a => Fractional (X16 a)
deriving via WrappedMulti X16 a instance FloatingF X16 a => Floating (X16 a)
deriving via WrappedMulti X16 a instance BooleanF X16 a => Boolean (X16 a)
deriving via WrappedMulti X16 a instance BitShiftF X16 a => BitShift (X16 a)
deriving via WrappedMulti X16 a instance MinMaxF X16 a => MinMax (X16 a)
deriving via WrappedMulti X16 a instance FusedMultiplyAddF X16 a => FusedMultiplyAdd (X16 a)
