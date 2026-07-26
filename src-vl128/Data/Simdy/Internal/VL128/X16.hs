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
module Data.Simdy.Internal.VL128.X16 where
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
import           Data.Simdy.Internal.VL128.Prim
import           Data.Simdy.Internal.VL128.PrimExtra
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
instance UnaryShuffleT indices X16 Bool where
  unaryShuffle = error "not implemented yet"
  {-# NOINLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 Bool where
  binaryShuffle = error "not implemented yet"
  {-# NOINLINE binaryShuffle #-}
data instance X16 Float = MkFloatX16WithVec128 FloatX4# FloatX4# FloatX4# FloatX4#
instance PackX16 X16 Float where
  mkX16 (F# x0) (F# x1) (F# x2) (F# x3) (F# x4) (F# x5) (F# x6) (F# x7) (F# x8) (F# x9) (F# x10) (F# x11) (F# x12) (F# x13) (F# x14) (F# x15) = MkFloatX16WithVec128 (packFloatX4# (# x0, x1, x2, x3 #)) (packFloatX4# (# x4, x5, x6, x7 #)) (packFloatX4# (# x8, x9, x10, x11 #)) (packFloatX4# (# x12, x13, x14, x15 #))
  unpackX16 (MkFloatX16WithVec128 v0 v1 v2 v3) = case unpackFloatX4# v0 of (# x0, x1, x2, x3 #) -> case unpackFloatX4# v1 of (# x4, x5, x6, x7 #) -> case unpackFloatX4# v2 of (# x8, x9, x10, x11 #) -> case unpackFloatX4# v3 of (# x12, x13, x14, x15 #) -> (F# x0, F# x1, F# x2, F# x3, F# x4, F# x5, F# x6, F# x7, F# x8, F# x9, F# x10, F# x11, F# x12, F# x13, F# x14, F# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Float where
  broadcast (F# x) = let !v = broadcastFloatX4# x in MkFloatX16WithVec128 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Float where
  selectF (MkBoolX16 !cond) (MkFloatX16WithVec128 x0 x1 x2 x3) (MkFloatX16WithVec128 y0 y1 y2 y3) = MkFloatX16WithVec128 (selectFloatX4# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectFloatX4# (fromIntegral $ cond `unsafeShiftR` 4) x1 y1) (selectFloatX4# (fromIntegral $ cond `unsafeShiftR` 8) x2 y2) (selectFloatX4# (fromIntegral $ cond `unsafeShiftR` 12) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany FloatX4# [i0, i1, i2, i3], ShuffleMany FloatX4# [i4, i5, i6, i7], ShuffleMany FloatX4# [i8, i9, i10, i11], ShuffleMany FloatX4# [i12, i13, i14, i15]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Float where
  unaryShuffle (MkFloatX16WithVec128 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkFloatX16WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany FloatX4# [i0, i1, i2, i3], ShuffleMany FloatX4# [i4, i5, i6, i7], ShuffleMany FloatX4# [i8, i9, i10, i11], ShuffleMany FloatX4# [i12, i13, i14, i15]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Float where
  binaryShuffle (MkFloatX16WithVec128 x0 x1 x2 x3) (MkFloatX16WithVec128 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkFloatX16WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Float where
  eqF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (eqFloatX4# u0 v0)) .|. (fromIntegral (eqFloatX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (eqFloatX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (eqFloatX4# u3 v3) `unsafeShiftL` 12)
  {-# INLINE eqF #-}
instance OrderedF X16 Float where
  ltF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (ltFloatX4# u0 v0)) .|. (fromIntegral (ltFloatX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (ltFloatX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (ltFloatX4# u3 v3) `unsafeShiftL` 12)
  leF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (leFloatX4# u0 v0)) .|. (fromIntegral (leFloatX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (leFloatX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (leFloatX4# u3 v3) `unsafeShiftL` 12)
  gtF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (gtFloatX4# u0 v0)) .|. (fromIntegral (gtFloatX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (gtFloatX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (gtFloatX4# u3 v3) `unsafeShiftL` 12)
  geF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (geFloatX4# u0 v0)) .|. (fromIntegral (geFloatX4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (geFloatX4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (geFloatX4# u3 v3) `unsafeShiftL` 12)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Float where
  minF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkFloatX16WithVec128 (minimumFloatX4# u0 v0) (minimumFloatX4# u1 v1) (minimumFloatX4# u2 v2) (minimumFloatX4# u3 v3)
  maxF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkFloatX16WithVec128 (maximumFloatX4# u0 v0) (maximumFloatX4# u1 v1) (maximumFloatX4# u2 v2) (maximumFloatX4# u3 v3)
  minimumNumberF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkFloatX16WithVec128 (minimumNumberFloatX4# u0 v0) (minimumNumberFloatX4# u1 v1) (minimumNumberFloatX4# u2 v2) (minimumNumberFloatX4# u3 v3)
  maximumNumberF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkFloatX16WithVec128 (maximumNumberFloatX4# u0 v0) (maximumNumberFloatX4# u1 v1) (maximumNumberFloatX4# u2 v2) (maximumNumberFloatX4# u3 v3)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX16Float :: X16 Float -> X16 Float
negateX16Float (MkFloatX16WithVec128 u0 u1 u2 u3) = MkFloatX16WithVec128 (negateFloatX4# u0) (negateFloatX4# u1) (negateFloatX4# u2) (negateFloatX4# u3)
#if defined(USE_FMA)
{-# INLINE [0] negateX16Float #-}
#else
{-# INLINE negateX16Float #-}
#endif
instance NumF X16 Float where
  plusF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkFloatX16WithVec128 (plusFloatX4# u0 v0) (plusFloatX4# u1 v1) (plusFloatX4# u2 v2) (plusFloatX4# u3 v3)
  minusF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkFloatX16WithVec128 (minusFloatX4# u0 v0) (minusFloatX4# u1 v1) (minusFloatX4# u2 v2) (minusFloatX4# u3 v3)
  timesF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkFloatX16WithVec128 (timesFloatX4# u0 v0) (timesFloatX4# u1 v1) (timesFloatX4# u2 v2) (timesFloatX4# u3 v3)
  negateF = negateX16Float
  absF (MkFloatX16WithVec128 u0 u1 u2 u3) = MkFloatX16WithVec128 (absFloatX4# u0) (absFloatX4# u1) (absFloatX4# u2) (absFloatX4# u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X16 Float where
  divideF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) = MkFloatX16WithVec128 (divideFloatX4# u0 v0) (divideFloatX4# u1 v1) (divideFloatX4# u2 v2) (divideFloatX4# u3 v3)
  {-# INLINE divideF #-}
instance FloatingF X16 Float where
  sqrtF (MkFloatX16WithVec128 u0 u1 u2 u3) = MkFloatX16WithVec128 (sqrtFloatX4# u0) (sqrtFloatX4# u1) (sqrtFloatX4# u2) (sqrtFloatX4# u3)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X16 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX16WithVec128 u0 u1 u2 u3) (MkFloatX16WithVec128 v0 v1 v2 v3) (MkFloatX16WithVec128 w0 w1 w2 w3) = MkFloatX16WithVec128 (fmaddFloatX4# u0 v0 w0) (fmaddFloatX4# u1 v1 w1) (fmaddFloatX4# u2 v2 w2) (fmaddFloatX4# u3 v3 w3)
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
  enumFromZero = MkFloatX16WithVec128 (packFloatX4# (# 0.0#, 1.0#, 2.0#, 3.0# #)) (packFloatX4# (# 4.0#, 5.0#, 6.0#, 7.0# #)) (packFloatX4# (# 8.0#, 9.0#, 10.0#, 11.0# #)) (packFloatX4# (# 12.0#, 13.0#, 14.0#, 15.0# #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Float where
  indexByteArraySIMD# ba i = MkFloatX16WithVec128 (indexFloatArrayAsFloatX4# ba i) (indexFloatArrayAsFloatX4# ba (i +# 4#)) (indexFloatArrayAsFloatX4# ba (i +# 8#)) (indexFloatArrayAsFloatX4# ba (i +# 12#))
  readByteArraySIMD# mba i s0 = case readFloatArrayAsFloatX4# mba i s0 of (# s1, v0 #) -> case readFloatArrayAsFloatX4# mba (i +# 4#) s1 of (# s2, v1 #) -> case readFloatArrayAsFloatX4# mba (i +# 8#) s2 of (# s3, v2 #) -> case readFloatArrayAsFloatX4# mba (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkFloatX16WithVec128 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkFloatX16WithVec128 v0 v1 v2 v3) s0 = case writeFloatArrayAsFloatX4# mba i v0 s0 of s1 -> case writeFloatArrayAsFloatX4# mba (i +# 4#) v1 s1 of s2 -> case writeFloatArrayAsFloatX4# mba (i +# 8#) v2 s2 of s3 -> writeFloatArrayAsFloatX4# mba (i +# 12#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readFloatOffAddrAsFloatX4# addr i s0 of (# s1, v0 #) -> case readFloatOffAddrAsFloatX4# addr (i +# 4#) s1 of (# s2, v1 #) -> case readFloatOffAddrAsFloatX4# addr (i +# 8#) s2 of (# s3, v2 #) -> case readFloatOffAddrAsFloatX4# addr (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkFloatX16WithVec128 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX16WithVec128 v0 v1 v2 v3) = IO (\s0 -> case writeFloatOffAddrAsFloatX4# addr i v0 s0 of s1 -> case writeFloatOffAddrAsFloatX4# addr (i +# 4#) v1 s1 of s2 -> case writeFloatOffAddrAsFloatX4# addr (i +# 8#) v2 s2 of s3 -> case writeFloatOffAddrAsFloatX4# addr (i +# 12#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Double = MkDoubleX16WithVec128 DoubleX2# DoubleX2# DoubleX2# DoubleX2# DoubleX2# DoubleX2# DoubleX2# DoubleX2#
instance PackX16 X16 Double where
  mkX16 (D# x0) (D# x1) (D# x2) (D# x3) (D# x4) (D# x5) (D# x6) (D# x7) (D# x8) (D# x9) (D# x10) (D# x11) (D# x12) (D# x13) (D# x14) (D# x15) = MkDoubleX16WithVec128 (packDoubleX2# (# x0, x1 #)) (packDoubleX2# (# x2, x3 #)) (packDoubleX2# (# x4, x5 #)) (packDoubleX2# (# x6, x7 #)) (packDoubleX2# (# x8, x9 #)) (packDoubleX2# (# x10, x11 #)) (packDoubleX2# (# x12, x13 #)) (packDoubleX2# (# x14, x15 #))
  unpackX16 (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = case unpackDoubleX2# v0 of (# x0, x1 #) -> case unpackDoubleX2# v1 of (# x2, x3 #) -> case unpackDoubleX2# v2 of (# x4, x5 #) -> case unpackDoubleX2# v3 of (# x6, x7 #) -> case unpackDoubleX2# v4 of (# x8, x9 #) -> case unpackDoubleX2# v5 of (# x10, x11 #) -> case unpackDoubleX2# v6 of (# x12, x13 #) -> case unpackDoubleX2# v7 of (# x14, x15 #) -> (D# x0, D# x1, D# x2, D# x3, D# x4, D# x5, D# x6, D# x7, D# x8, D# x9, D# x10, D# x11, D# x12, D# x13, D# x14, D# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Double where
  broadcast (D# x) = let !v = broadcastDoubleX2# x in MkDoubleX16WithVec128 v v v v v v v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Double where
  selectF (MkBoolX16 !cond) (MkDoubleX16WithVec128 x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX16WithVec128 y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX16WithVec128 (selectDoubleX2# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectDoubleX2# (fromIntegral $ cond `unsafeShiftR` 2) x1 y1) (selectDoubleX2# (fromIntegral $ cond `unsafeShiftR` 4) x2 y2) (selectDoubleX2# (fromIntegral $ cond `unsafeShiftR` 6) x3 y3) (selectDoubleX2# (fromIntegral $ cond `unsafeShiftR` 8) x4 y4) (selectDoubleX2# (fromIntegral $ cond `unsafeShiftR` 10) x5 y5) (selectDoubleX2# (fromIntegral $ cond `unsafeShiftR` 12) x6 y6) (selectDoubleX2# (fromIntegral $ cond `unsafeShiftR` 14) x7 y7)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany DoubleX2# [i0, i1], ShuffleMany DoubleX2# [i2, i3], ShuffleMany DoubleX2# [i4, i5], ShuffleMany DoubleX2# [i6, i7], ShuffleMany DoubleX2# [i8, i9], ShuffleMany DoubleX2# [i10, i11], ShuffleMany DoubleX2# [i12, i13], ShuffleMany DoubleX2# [i14, i15]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Double where
  unaryShuffle (MkDoubleX16WithVec128 x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkDoubleX16WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9] sources) (shuffleMany# @_ @_ @[i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13] sources) (shuffleMany# @_ @_ @[i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany DoubleX2# [i0, i1], ShuffleMany DoubleX2# [i2, i3], ShuffleMany DoubleX2# [i4, i5], ShuffleMany DoubleX2# [i6, i7], ShuffleMany DoubleX2# [i8, i9], ShuffleMany DoubleX2# [i10, i11], ShuffleMany DoubleX2# [i12, i13], ShuffleMany DoubleX2# [i14, i15]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Double where
  binaryShuffle (MkDoubleX16WithVec128 x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX16WithVec128 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkDoubleX16WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9] sources) (shuffleMany# @_ @_ @[i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13] sources) (shuffleMany# @_ @_ @[i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Double where
  eqF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX16 $ (fromIntegral (eqDoubleX2# u0 v0)) .|. (fromIntegral (eqDoubleX2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (eqDoubleX2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (eqDoubleX2# u3 v3) `unsafeShiftL` 6) .|. (fromIntegral (eqDoubleX2# u4 v4) `unsafeShiftL` 8) .|. (fromIntegral (eqDoubleX2# u5 v5) `unsafeShiftL` 10) .|. (fromIntegral (eqDoubleX2# u6 v6) `unsafeShiftL` 12) .|. (fromIntegral (eqDoubleX2# u7 v7) `unsafeShiftL` 14)
  {-# INLINE eqF #-}
instance OrderedF X16 Double where
  ltF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX16 $ (fromIntegral (ltDoubleX2# u0 v0)) .|. (fromIntegral (ltDoubleX2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (ltDoubleX2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (ltDoubleX2# u3 v3) `unsafeShiftL` 6) .|. (fromIntegral (ltDoubleX2# u4 v4) `unsafeShiftL` 8) .|. (fromIntegral (ltDoubleX2# u5 v5) `unsafeShiftL` 10) .|. (fromIntegral (ltDoubleX2# u6 v6) `unsafeShiftL` 12) .|. (fromIntegral (ltDoubleX2# u7 v7) `unsafeShiftL` 14)
  leF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX16 $ (fromIntegral (leDoubleX2# u0 v0)) .|. (fromIntegral (leDoubleX2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (leDoubleX2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (leDoubleX2# u3 v3) `unsafeShiftL` 6) .|. (fromIntegral (leDoubleX2# u4 v4) `unsafeShiftL` 8) .|. (fromIntegral (leDoubleX2# u5 v5) `unsafeShiftL` 10) .|. (fromIntegral (leDoubleX2# u6 v6) `unsafeShiftL` 12) .|. (fromIntegral (leDoubleX2# u7 v7) `unsafeShiftL` 14)
  gtF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX16 $ (fromIntegral (gtDoubleX2# u0 v0)) .|. (fromIntegral (gtDoubleX2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (gtDoubleX2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (gtDoubleX2# u3 v3) `unsafeShiftL` 6) .|. (fromIntegral (gtDoubleX2# u4 v4) `unsafeShiftL` 8) .|. (fromIntegral (gtDoubleX2# u5 v5) `unsafeShiftL` 10) .|. (fromIntegral (gtDoubleX2# u6 v6) `unsafeShiftL` 12) .|. (fromIntegral (gtDoubleX2# u7 v7) `unsafeShiftL` 14)
  geF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX16 $ (fromIntegral (geDoubleX2# u0 v0)) .|. (fromIntegral (geDoubleX2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (geDoubleX2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (geDoubleX2# u3 v3) `unsafeShiftL` 6) .|. (fromIntegral (geDoubleX2# u4 v4) `unsafeShiftL` 8) .|. (fromIntegral (geDoubleX2# u5 v5) `unsafeShiftL` 10) .|. (fromIntegral (geDoubleX2# u6 v6) `unsafeShiftL` 12) .|. (fromIntegral (geDoubleX2# u7 v7) `unsafeShiftL` 14)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Double where
  minF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX16WithVec128 (minimumDoubleX2# u0 v0) (minimumDoubleX2# u1 v1) (minimumDoubleX2# u2 v2) (minimumDoubleX2# u3 v3) (minimumDoubleX2# u4 v4) (minimumDoubleX2# u5 v5) (minimumDoubleX2# u6 v6) (minimumDoubleX2# u7 v7)
  maxF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX16WithVec128 (maximumDoubleX2# u0 v0) (maximumDoubleX2# u1 v1) (maximumDoubleX2# u2 v2) (maximumDoubleX2# u3 v3) (maximumDoubleX2# u4 v4) (maximumDoubleX2# u5 v5) (maximumDoubleX2# u6 v6) (maximumDoubleX2# u7 v7)
  minimumNumberF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX16WithVec128 (minimumNumberDoubleX2# u0 v0) (minimumNumberDoubleX2# u1 v1) (minimumNumberDoubleX2# u2 v2) (minimumNumberDoubleX2# u3 v3) (minimumNumberDoubleX2# u4 v4) (minimumNumberDoubleX2# u5 v5) (minimumNumberDoubleX2# u6 v6) (minimumNumberDoubleX2# u7 v7)
  maximumNumberF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX16WithVec128 (maximumNumberDoubleX2# u0 v0) (maximumNumberDoubleX2# u1 v1) (maximumNumberDoubleX2# u2 v2) (maximumNumberDoubleX2# u3 v3) (maximumNumberDoubleX2# u4 v4) (maximumNumberDoubleX2# u5 v5) (maximumNumberDoubleX2# u6 v6) (maximumNumberDoubleX2# u7 v7)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX16Double :: X16 Double -> X16 Double
negateX16Double (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) = MkDoubleX16WithVec128 (negateDoubleX2# u0) (negateDoubleX2# u1) (negateDoubleX2# u2) (negateDoubleX2# u3) (negateDoubleX2# u4) (negateDoubleX2# u5) (negateDoubleX2# u6) (negateDoubleX2# u7)
#if defined(USE_FMA)
{-# INLINE [0] negateX16Double #-}
#else
{-# INLINE negateX16Double #-}
#endif
instance NumF X16 Double where
  plusF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX16WithVec128 (plusDoubleX2# u0 v0) (plusDoubleX2# u1 v1) (plusDoubleX2# u2 v2) (plusDoubleX2# u3 v3) (plusDoubleX2# u4 v4) (plusDoubleX2# u5 v5) (plusDoubleX2# u6 v6) (plusDoubleX2# u7 v7)
  minusF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX16WithVec128 (minusDoubleX2# u0 v0) (minusDoubleX2# u1 v1) (minusDoubleX2# u2 v2) (minusDoubleX2# u3 v3) (minusDoubleX2# u4 v4) (minusDoubleX2# u5 v5) (minusDoubleX2# u6 v6) (minusDoubleX2# u7 v7)
  timesF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX16WithVec128 (timesDoubleX2# u0 v0) (timesDoubleX2# u1 v1) (timesDoubleX2# u2 v2) (timesDoubleX2# u3 v3) (timesDoubleX2# u4 v4) (timesDoubleX2# u5 v5) (timesDoubleX2# u6 v6) (timesDoubleX2# u7 v7)
  negateF = negateX16Double
  absF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) = MkDoubleX16WithVec128 (absDoubleX2# u0) (absDoubleX2# u1) (absDoubleX2# u2) (absDoubleX2# u3) (absDoubleX2# u4) (absDoubleX2# u5) (absDoubleX2# u6) (absDoubleX2# u7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X16 Double where
  divideF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX16WithVec128 (divideDoubleX2# u0 v0) (divideDoubleX2# u1 v1) (divideDoubleX2# u2 v2) (divideDoubleX2# u3 v3) (divideDoubleX2# u4 v4) (divideDoubleX2# u5 v5) (divideDoubleX2# u6 v6) (divideDoubleX2# u7 v7)
  {-# INLINE divideF #-}
instance FloatingF X16 Double where
  sqrtF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) = MkDoubleX16WithVec128 (sqrtDoubleX2# u0) (sqrtDoubleX2# u1) (sqrtDoubleX2# u2) (sqrtDoubleX2# u3) (sqrtDoubleX2# u4) (sqrtDoubleX2# u5) (sqrtDoubleX2# u6) (sqrtDoubleX2# u7)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X16 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) (MkDoubleX16WithVec128 w0 w1 w2 w3 w4 w5 w6 w7) = MkDoubleX16WithVec128 (fmaddDoubleX2# u0 v0 w0) (fmaddDoubleX2# u1 v1 w1) (fmaddDoubleX2# u2 v2 w2) (fmaddDoubleX2# u3 v3 w3) (fmaddDoubleX2# u4 v4 w4) (fmaddDoubleX2# u5 v5 w5) (fmaddDoubleX2# u6 v6 w6) (fmaddDoubleX2# u7 v7 w7)
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
  enumFromZero = MkDoubleX16WithVec128 (packDoubleX2# (# 0.0##, 1.0## #)) (packDoubleX2# (# 2.0##, 3.0## #)) (packDoubleX2# (# 4.0##, 5.0## #)) (packDoubleX2# (# 6.0##, 7.0## #)) (packDoubleX2# (# 8.0##, 9.0## #)) (packDoubleX2# (# 10.0##, 11.0## #)) (packDoubleX2# (# 12.0##, 13.0## #)) (packDoubleX2# (# 14.0##, 15.0## #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Double where
  indexByteArraySIMD# ba i = MkDoubleX16WithVec128 (indexDoubleArrayAsDoubleX2# ba i) (indexDoubleArrayAsDoubleX2# ba (i +# 2#)) (indexDoubleArrayAsDoubleX2# ba (i +# 4#)) (indexDoubleArrayAsDoubleX2# ba (i +# 6#)) (indexDoubleArrayAsDoubleX2# ba (i +# 8#)) (indexDoubleArrayAsDoubleX2# ba (i +# 10#)) (indexDoubleArrayAsDoubleX2# ba (i +# 12#)) (indexDoubleArrayAsDoubleX2# ba (i +# 14#))
  readByteArraySIMD# mba i s0 = case readDoubleArrayAsDoubleX2# mba i s0 of (# s1, v0 #) -> case readDoubleArrayAsDoubleX2# mba (i +# 2#) s1 of (# s2, v1 #) -> case readDoubleArrayAsDoubleX2# mba (i +# 4#) s2 of (# s3, v2 #) -> case readDoubleArrayAsDoubleX2# mba (i +# 6#) s3 of (# s4, v3 #) -> case readDoubleArrayAsDoubleX2# mba (i +# 8#) s4 of (# s5, v4 #) -> case readDoubleArrayAsDoubleX2# mba (i +# 10#) s5 of (# s6, v5 #) -> case readDoubleArrayAsDoubleX2# mba (i +# 12#) s6 of (# s7, v6 #) -> case readDoubleArrayAsDoubleX2# mba (i +# 14#) s7 of (# s8, v7 #) -> (# s8, MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 #)
  writeByteArraySIMD# mba i (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) s0 = case writeDoubleArrayAsDoubleX2# mba i v0 s0 of s1 -> case writeDoubleArrayAsDoubleX2# mba (i +# 2#) v1 s1 of s2 -> case writeDoubleArrayAsDoubleX2# mba (i +# 4#) v2 s2 of s3 -> case writeDoubleArrayAsDoubleX2# mba (i +# 6#) v3 s3 of s4 -> case writeDoubleArrayAsDoubleX2# mba (i +# 8#) v4 s4 of s5 -> case writeDoubleArrayAsDoubleX2# mba (i +# 10#) v5 s5 of s6 -> case writeDoubleArrayAsDoubleX2# mba (i +# 12#) v6 s6 of s7 -> writeDoubleArrayAsDoubleX2# mba (i +# 14#) v7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readDoubleOffAddrAsDoubleX2# addr i s0 of (# s1, v0 #) -> case readDoubleOffAddrAsDoubleX2# addr (i +# 2#) s1 of (# s2, v1 #) -> case readDoubleOffAddrAsDoubleX2# addr (i +# 4#) s2 of (# s3, v2 #) -> case readDoubleOffAddrAsDoubleX2# addr (i +# 6#) s3 of (# s4, v3 #) -> case readDoubleOffAddrAsDoubleX2# addr (i +# 8#) s4 of (# s5, v4 #) -> case readDoubleOffAddrAsDoubleX2# addr (i +# 10#) s5 of (# s6, v5 #) -> case readDoubleOffAddrAsDoubleX2# addr (i +# 12#) s6 of (# s7, v6 #) -> case readDoubleOffAddrAsDoubleX2# addr (i +# 14#) s7 of (# s8, v7 #) -> (# s8, MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = IO (\s0 -> case writeDoubleOffAddrAsDoubleX2# addr i v0 s0 of s1 -> case writeDoubleOffAddrAsDoubleX2# addr (i +# 2#) v1 s1 of s2 -> case writeDoubleOffAddrAsDoubleX2# addr (i +# 4#) v2 s2 of s3 -> case writeDoubleOffAddrAsDoubleX2# addr (i +# 6#) v3 s3 of s4 -> case writeDoubleOffAddrAsDoubleX2# addr (i +# 8#) v4 s4 of s5 -> case writeDoubleOffAddrAsDoubleX2# addr (i +# 10#) v5 s5 of s6 -> case writeDoubleOffAddrAsDoubleX2# addr (i +# 12#) v6 s6 of s7 -> case writeDoubleOffAddrAsDoubleX2# addr (i +# 14#) v7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__) || defined(USE_LLVM)
instance ImplementationDescription X16 where
  implementationDescription _ = "X16;maxBits=128"
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
instance (AllLessThan indices 16, ShuffleMany Int8X16# indices) => UnaryShuffleT indices X16 Int8 where
  unaryShuffle (MkInt8X16 x) = MkInt8X16 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 32, ShuffleMany Int8X16# indices) => BinaryShuffleT indices X16 Int8 where
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
data instance X16 Int16 = MkInt16X16WithVec128 Int16X8# Int16X8#
instance PackX16 X16 Int16 where
  mkX16 (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) (I16# x8) (I16# x9) (I16# x10) (I16# x11) (I16# x12) (I16# x13) (I16# x14) (I16# x15) = MkInt16X16WithVec128 (packInt16X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packInt16X8# (# x8, x9, x10, x11, x12, x13, x14, x15 #))
  unpackX16 (MkInt16X16WithVec128 v0 v1) = case unpackInt16X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackInt16X8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> (I16# x0, I16# x1, I16# x2, I16# x3, I16# x4, I16# x5, I16# x6, I16# x7, I16# x8, I16# x9, I16# x10, I16# x11, I16# x12, I16# x13, I16# x14, I16# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int16 where
  broadcast (I16# x) = let !v = broadcastInt16X8# x in MkInt16X16WithVec128 v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Int16 where
  selectF (MkBoolX16 !cond) (MkInt16X16WithVec128 x0 x1) (MkInt16X16WithVec128 y0 y1) = MkInt16X16WithVec128 (selectInt16X8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt16X8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany Int16X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Int16X8# [i8, i9, i10, i11, i12, i13, i14, i15]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int16 where
  unaryShuffle (MkInt16X16WithVec128 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt16X16WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany Int16X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Int16X8# [i8, i9, i10, i11, i12, i13, i14, i15]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int16 where
  binaryShuffle (MkInt16X16WithVec128 x0 x1) (MkInt16X16WithVec128 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt16X16WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int16 where
  eqF (MkInt16X16WithVec128 u0 u1) (MkInt16X16WithVec128 v0 v1) = MkBoolX16 $ (fromIntegral (eqInt16X8# u0 v0)) .|. (fromIntegral (eqInt16X8# u1 v1) `unsafeShiftL` 8)
  {-# INLINE eqF #-}
instance OrderedF X16 Int16 where
  ltF (MkInt16X16WithVec128 u0 u1) (MkInt16X16WithVec128 v0 v1) = MkBoolX16 $ (fromIntegral (ltInt16X8# u0 v0)) .|. (fromIntegral (ltInt16X8# u1 v1) `unsafeShiftL` 8)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int16 where
  minF (MkInt16X16WithVec128 u0 u1) (MkInt16X16WithVec128 v0 v1) = MkInt16X16WithVec128 (minInt16X8# u0 v0) (minInt16X8# u1 v1)
  maxF (MkInt16X16WithVec128 u0 u1) (MkInt16X16WithVec128 v0 v1) = MkInt16X16WithVec128 (maxInt16X8# u0 v0) (maxInt16X8# u1 v1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int16 where
  plusF (MkInt16X16WithVec128 u0 u1) (MkInt16X16WithVec128 v0 v1) = MkInt16X16WithVec128 (plusInt16X8# u0 v0) (plusInt16X8# u1 v1)
  minusF (MkInt16X16WithVec128 u0 u1) (MkInt16X16WithVec128 v0 v1) = MkInt16X16WithVec128 (minusInt16X8# u0 v0) (minusInt16X8# u1 v1)
  timesF (MkInt16X16WithVec128 u0 u1) (MkInt16X16WithVec128 v0 v1) = MkInt16X16WithVec128 (timesInt16X8# u0 v0) (timesInt16X8# u1 v1)
  negateF (MkInt16X16WithVec128 u0 u1) = MkInt16X16WithVec128 (negateInt16X8# u0) (negateInt16X8# u1)
  absF (MkInt16X16WithVec128 u0 u1) = MkInt16X16WithVec128 (absInt16X8# u0) (absInt16X8# u1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Int16 where
  andF (MkInt16X16WithVec128 u0 u1) (MkInt16X16WithVec128 v0 v1) = MkInt16X16WithVec128 (andInt16X8# u0 v0) (andInt16X8# u1 v1)
  orF (MkInt16X16WithVec128 u0 u1) (MkInt16X16WithVec128 v0 v1) = MkInt16X16WithVec128 (orInt16X8# u0 v0) (orInt16X8# u1 v1)
  xorF (MkInt16X16WithVec128 u0 u1) (MkInt16X16WithVec128 v0 v1) = MkInt16X16WithVec128 (xorInt16X8# u0 v0) (xorInt16X8# u1 v1)
  complementF (MkInt16X16WithVec128 u0 u1) = MkInt16X16WithVec128 (complementInt16X8# u0) (complementInt16X8# u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int16 where
  shiftLF (MkInt16X16WithVec128 u0 u1) (I# i) = MkInt16X16WithVec128 (shiftLInt16X8# u0 i) (shiftLInt16X8# u1 i)
  shiftRF (MkInt16X16WithVec128 u0 u1) (I# i) = MkInt16X16WithVec128 (shiftRInt16X8# u0 i) (shiftRInt16X8# u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Int16 where
  enumFromZero = MkInt16X16WithVec128 (packInt16X8# (# 0#Int16, 1#Int16, 2#Int16, 3#Int16, 4#Int16, 5#Int16, 6#Int16, 7#Int16 #)) (packInt16X8# (# 8#Int16, 9#Int16, 10#Int16, 11#Int16, 12#Int16, 13#Int16, 14#Int16, 15#Int16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int16 where
  indexByteArraySIMD# ba i = MkInt16X16WithVec128 (indexInt16ArrayAsInt16X8# ba i) (indexInt16ArrayAsInt16X8# ba (i +# 8#))
  readByteArraySIMD# mba i s0 = case readInt16ArrayAsInt16X8# mba i s0 of (# s1, v0 #) -> case readInt16ArrayAsInt16X8# mba (i +# 8#) s1 of (# s2, v1 #) -> (# s2, MkInt16X16WithVec128 v0 v1 #)
  writeByteArraySIMD# mba i (MkInt16X16WithVec128 v0 v1) s0 = case writeInt16ArrayAsInt16X8# mba i v0 s0 of s1 -> writeInt16ArrayAsInt16X8# mba (i +# 8#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt16OffAddrAsInt16X8# addr i s0 of (# s1, v0 #) -> case readInt16OffAddrAsInt16X8# addr (i +# 8#) s1 of (# s2, v1 #) -> (# s2, MkInt16X16WithVec128 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X16WithVec128 v0 v1) = IO (\s0 -> case writeInt16OffAddrAsInt16X8# addr i v0 s0 of s1 -> case writeInt16OffAddrAsInt16X8# addr (i +# 8#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Int32 = MkInt32X16WithVec128 Int32X4# Int32X4# Int32X4# Int32X4#
instance PackX16 X16 Int32 where
  mkX16 (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) (I32# x8) (I32# x9) (I32# x10) (I32# x11) (I32# x12) (I32# x13) (I32# x14) (I32# x15) = MkInt32X16WithVec128 (packInt32X4# (# x0, x1, x2, x3 #)) (packInt32X4# (# x4, x5, x6, x7 #)) (packInt32X4# (# x8, x9, x10, x11 #)) (packInt32X4# (# x12, x13, x14, x15 #))
  unpackX16 (MkInt32X16WithVec128 v0 v1 v2 v3) = case unpackInt32X4# v0 of (# x0, x1, x2, x3 #) -> case unpackInt32X4# v1 of (# x4, x5, x6, x7 #) -> case unpackInt32X4# v2 of (# x8, x9, x10, x11 #) -> case unpackInt32X4# v3 of (# x12, x13, x14, x15 #) -> (I32# x0, I32# x1, I32# x2, I32# x3, I32# x4, I32# x5, I32# x6, I32# x7, I32# x8, I32# x9, I32# x10, I32# x11, I32# x12, I32# x13, I32# x14, I32# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int32 where
  broadcast (I32# x) = let !v = broadcastInt32X4# x in MkInt32X16WithVec128 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Int32 where
  selectF (MkBoolX16 !cond) (MkInt32X16WithVec128 x0 x1 x2 x3) (MkInt32X16WithVec128 y0 y1 y2 y3) = MkInt32X16WithVec128 (selectInt32X4# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt32X4# (fromIntegral $ cond `unsafeShiftR` 4) x1 y1) (selectInt32X4# (fromIntegral $ cond `unsafeShiftR` 8) x2 y2) (selectInt32X4# (fromIntegral $ cond `unsafeShiftR` 12) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany Int32X4# [i0, i1, i2, i3], ShuffleMany Int32X4# [i4, i5, i6, i7], ShuffleMany Int32X4# [i8, i9, i10, i11], ShuffleMany Int32X4# [i12, i13, i14, i15]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int32 where
  unaryShuffle (MkInt32X16WithVec128 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt32X16WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany Int32X4# [i0, i1, i2, i3], ShuffleMany Int32X4# [i4, i5, i6, i7], ShuffleMany Int32X4# [i8, i9, i10, i11], ShuffleMany Int32X4# [i12, i13, i14, i15]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int32 where
  binaryShuffle (MkInt32X16WithVec128 x0 x1 x2 x3) (MkInt32X16WithVec128 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt32X16WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int32 where
  eqF (MkInt32X16WithVec128 u0 u1 u2 u3) (MkInt32X16WithVec128 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (eqInt32X4# u0 v0)) .|. (fromIntegral (eqInt32X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (eqInt32X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (eqInt32X4# u3 v3) `unsafeShiftL` 12)
  {-# INLINE eqF #-}
instance OrderedF X16 Int32 where
  ltF (MkInt32X16WithVec128 u0 u1 u2 u3) (MkInt32X16WithVec128 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (ltInt32X4# u0 v0)) .|. (fromIntegral (ltInt32X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (ltInt32X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (ltInt32X4# u3 v3) `unsafeShiftL` 12)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int32 where
  minF (MkInt32X16WithVec128 u0 u1 u2 u3) (MkInt32X16WithVec128 v0 v1 v2 v3) = MkInt32X16WithVec128 (minInt32X4# u0 v0) (minInt32X4# u1 v1) (minInt32X4# u2 v2) (minInt32X4# u3 v3)
  maxF (MkInt32X16WithVec128 u0 u1 u2 u3) (MkInt32X16WithVec128 v0 v1 v2 v3) = MkInt32X16WithVec128 (maxInt32X4# u0 v0) (maxInt32X4# u1 v1) (maxInt32X4# u2 v2) (maxInt32X4# u3 v3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int32 where
  plusF (MkInt32X16WithVec128 u0 u1 u2 u3) (MkInt32X16WithVec128 v0 v1 v2 v3) = MkInt32X16WithVec128 (plusInt32X4# u0 v0) (plusInt32X4# u1 v1) (plusInt32X4# u2 v2) (plusInt32X4# u3 v3)
  minusF (MkInt32X16WithVec128 u0 u1 u2 u3) (MkInt32X16WithVec128 v0 v1 v2 v3) = MkInt32X16WithVec128 (minusInt32X4# u0 v0) (minusInt32X4# u1 v1) (minusInt32X4# u2 v2) (minusInt32X4# u3 v3)
  timesF (MkInt32X16WithVec128 u0 u1 u2 u3) (MkInt32X16WithVec128 v0 v1 v2 v3) = MkInt32X16WithVec128 (timesInt32X4# u0 v0) (timesInt32X4# u1 v1) (timesInt32X4# u2 v2) (timesInt32X4# u3 v3)
  negateF (MkInt32X16WithVec128 u0 u1 u2 u3) = MkInt32X16WithVec128 (negateInt32X4# u0) (negateInt32X4# u1) (negateInt32X4# u2) (negateInt32X4# u3)
  absF (MkInt32X16WithVec128 u0 u1 u2 u3) = MkInt32X16WithVec128 (absInt32X4# u0) (absInt32X4# u1) (absInt32X4# u2) (absInt32X4# u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Int32 where
  andF (MkInt32X16WithVec128 u0 u1 u2 u3) (MkInt32X16WithVec128 v0 v1 v2 v3) = MkInt32X16WithVec128 (andInt32X4# u0 v0) (andInt32X4# u1 v1) (andInt32X4# u2 v2) (andInt32X4# u3 v3)
  orF (MkInt32X16WithVec128 u0 u1 u2 u3) (MkInt32X16WithVec128 v0 v1 v2 v3) = MkInt32X16WithVec128 (orInt32X4# u0 v0) (orInt32X4# u1 v1) (orInt32X4# u2 v2) (orInt32X4# u3 v3)
  xorF (MkInt32X16WithVec128 u0 u1 u2 u3) (MkInt32X16WithVec128 v0 v1 v2 v3) = MkInt32X16WithVec128 (xorInt32X4# u0 v0) (xorInt32X4# u1 v1) (xorInt32X4# u2 v2) (xorInt32X4# u3 v3)
  complementF (MkInt32X16WithVec128 u0 u1 u2 u3) = MkInt32X16WithVec128 (complementInt32X4# u0) (complementInt32X4# u1) (complementInt32X4# u2) (complementInt32X4# u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int32 where
  shiftLF (MkInt32X16WithVec128 u0 u1 u2 u3) (I# i) = MkInt32X16WithVec128 (shiftLInt32X4# u0 i) (shiftLInt32X4# u1 i) (shiftLInt32X4# u2 i) (shiftLInt32X4# u3 i)
  shiftRF (MkInt32X16WithVec128 u0 u1 u2 u3) (I# i) = MkInt32X16WithVec128 (shiftRInt32X4# u0 i) (shiftRInt32X4# u1 i) (shiftRInt32X4# u2 i) (shiftRInt32X4# u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Int32 where
  enumFromZero = MkInt32X16WithVec128 (packInt32X4# (# 0#Int32, 1#Int32, 2#Int32, 3#Int32 #)) (packInt32X4# (# 4#Int32, 5#Int32, 6#Int32, 7#Int32 #)) (packInt32X4# (# 8#Int32, 9#Int32, 10#Int32, 11#Int32 #)) (packInt32X4# (# 12#Int32, 13#Int32, 14#Int32, 15#Int32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int32 where
  indexByteArraySIMD# ba i = MkInt32X16WithVec128 (indexInt32ArrayAsInt32X4# ba i) (indexInt32ArrayAsInt32X4# ba (i +# 4#)) (indexInt32ArrayAsInt32X4# ba (i +# 8#)) (indexInt32ArrayAsInt32X4# ba (i +# 12#))
  readByteArraySIMD# mba i s0 = case readInt32ArrayAsInt32X4# mba i s0 of (# s1, v0 #) -> case readInt32ArrayAsInt32X4# mba (i +# 4#) s1 of (# s2, v1 #) -> case readInt32ArrayAsInt32X4# mba (i +# 8#) s2 of (# s3, v2 #) -> case readInt32ArrayAsInt32X4# mba (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkInt32X16WithVec128 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkInt32X16WithVec128 v0 v1 v2 v3) s0 = case writeInt32ArrayAsInt32X4# mba i v0 s0 of s1 -> case writeInt32ArrayAsInt32X4# mba (i +# 4#) v1 s1 of s2 -> case writeInt32ArrayAsInt32X4# mba (i +# 8#) v2 s2 of s3 -> writeInt32ArrayAsInt32X4# mba (i +# 12#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt32OffAddrAsInt32X4# addr i s0 of (# s1, v0 #) -> case readInt32OffAddrAsInt32X4# addr (i +# 4#) s1 of (# s2, v1 #) -> case readInt32OffAddrAsInt32X4# addr (i +# 8#) s2 of (# s3, v2 #) -> case readInt32OffAddrAsInt32X4# addr (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkInt32X16WithVec128 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X16WithVec128 v0 v1 v2 v3) = IO (\s0 -> case writeInt32OffAddrAsInt32X4# addr i v0 s0 of s1 -> case writeInt32OffAddrAsInt32X4# addr (i +# 4#) v1 s1 of s2 -> case writeInt32OffAddrAsInt32X4# addr (i +# 8#) v2 s2 of s3 -> case writeInt32OffAddrAsInt32X4# addr (i +# 12#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Int64 = MkInt64X16WithVec128 Int64X2# Int64X2# Int64X2# Int64X2# Int64X2# Int64X2# Int64X2# Int64X2#
instance PackX16 X16 Int64 where
  mkX16 (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) (I64# x8) (I64# x9) (I64# x10) (I64# x11) (I64# x12) (I64# x13) (I64# x14) (I64# x15) = MkInt64X16WithVec128 (packInt64X2# (# x0, x1 #)) (packInt64X2# (# x2, x3 #)) (packInt64X2# (# x4, x5 #)) (packInt64X2# (# x6, x7 #)) (packInt64X2# (# x8, x9 #)) (packInt64X2# (# x10, x11 #)) (packInt64X2# (# x12, x13 #)) (packInt64X2# (# x14, x15 #))
  unpackX16 (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = case unpackInt64X2# v0 of (# x0, x1 #) -> case unpackInt64X2# v1 of (# x2, x3 #) -> case unpackInt64X2# v2 of (# x4, x5 #) -> case unpackInt64X2# v3 of (# x6, x7 #) -> case unpackInt64X2# v4 of (# x8, x9 #) -> case unpackInt64X2# v5 of (# x10, x11 #) -> case unpackInt64X2# v6 of (# x12, x13 #) -> case unpackInt64X2# v7 of (# x14, x15 #) -> (I64# x0, I64# x1, I64# x2, I64# x3, I64# x4, I64# x5, I64# x6, I64# x7, I64# x8, I64# x9, I64# x10, I64# x11, I64# x12, I64# x13, I64# x14, I64# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int64 where
  broadcast (I64# x) = let !v = broadcastInt64X2# x in MkInt64X16WithVec128 v v v v v v v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Int64 where
  selectF (MkBoolX16 !cond) (MkInt64X16WithVec128 x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X16WithVec128 y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X16WithVec128 (selectInt64X2# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt64X2# (fromIntegral $ cond `unsafeShiftR` 2) x1 y1) (selectInt64X2# (fromIntegral $ cond `unsafeShiftR` 4) x2 y2) (selectInt64X2# (fromIntegral $ cond `unsafeShiftR` 6) x3 y3) (selectInt64X2# (fromIntegral $ cond `unsafeShiftR` 8) x4 y4) (selectInt64X2# (fromIntegral $ cond `unsafeShiftR` 10) x5 y5) (selectInt64X2# (fromIntegral $ cond `unsafeShiftR` 12) x6 y6) (selectInt64X2# (fromIntegral $ cond `unsafeShiftR` 14) x7 y7)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany Int64X2# [i0, i1], ShuffleMany Int64X2# [i2, i3], ShuffleMany Int64X2# [i4, i5], ShuffleMany Int64X2# [i6, i7], ShuffleMany Int64X2# [i8, i9], ShuffleMany Int64X2# [i10, i11], ShuffleMany Int64X2# [i12, i13], ShuffleMany Int64X2# [i14, i15]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int64 where
  unaryShuffle (MkInt64X16WithVec128 x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt64X16WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9] sources) (shuffleMany# @_ @_ @[i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13] sources) (shuffleMany# @_ @_ @[i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany Int64X2# [i0, i1], ShuffleMany Int64X2# [i2, i3], ShuffleMany Int64X2# [i4, i5], ShuffleMany Int64X2# [i6, i7], ShuffleMany Int64X2# [i8, i9], ShuffleMany Int64X2# [i10, i11], ShuffleMany Int64X2# [i12, i13], ShuffleMany Int64X2# [i14, i15]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int64 where
  binaryShuffle (MkInt64X16WithVec128 x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X16WithVec128 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt64X16WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9] sources) (shuffleMany# @_ @_ @[i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13] sources) (shuffleMany# @_ @_ @[i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int64 where
  eqF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX16 $ (fromIntegral (eqInt64X2# u0 v0)) .|. (fromIntegral (eqInt64X2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (eqInt64X2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (eqInt64X2# u3 v3) `unsafeShiftL` 6) .|. (fromIntegral (eqInt64X2# u4 v4) `unsafeShiftL` 8) .|. (fromIntegral (eqInt64X2# u5 v5) `unsafeShiftL` 10) .|. (fromIntegral (eqInt64X2# u6 v6) `unsafeShiftL` 12) .|. (fromIntegral (eqInt64X2# u7 v7) `unsafeShiftL` 14)
  {-# INLINE eqF #-}
instance OrderedF X16 Int64 where
  ltF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX16 $ (fromIntegral (ltInt64X2# u0 v0)) .|. (fromIntegral (ltInt64X2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (ltInt64X2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (ltInt64X2# u3 v3) `unsafeShiftL` 6) .|. (fromIntegral (ltInt64X2# u4 v4) `unsafeShiftL` 8) .|. (fromIntegral (ltInt64X2# u5 v5) `unsafeShiftL` 10) .|. (fromIntegral (ltInt64X2# u6 v6) `unsafeShiftL` 12) .|. (fromIntegral (ltInt64X2# u7 v7) `unsafeShiftL` 14)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int64 where
  minF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X16WithVec128 (minInt64X2# u0 v0) (minInt64X2# u1 v1) (minInt64X2# u2 v2) (minInt64X2# u3 v3) (minInt64X2# u4 v4) (minInt64X2# u5 v5) (minInt64X2# u6 v6) (minInt64X2# u7 v7)
  maxF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X16WithVec128 (maxInt64X2# u0 v0) (maxInt64X2# u1 v1) (maxInt64X2# u2 v2) (maxInt64X2# u3 v3) (maxInt64X2# u4 v4) (maxInt64X2# u5 v5) (maxInt64X2# u6 v6) (maxInt64X2# u7 v7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int64 where
  plusF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X16WithVec128 (plusInt64X2# u0 v0) (plusInt64X2# u1 v1) (plusInt64X2# u2 v2) (plusInt64X2# u3 v3) (plusInt64X2# u4 v4) (plusInt64X2# u5 v5) (plusInt64X2# u6 v6) (plusInt64X2# u7 v7)
  minusF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X16WithVec128 (minusInt64X2# u0 v0) (minusInt64X2# u1 v1) (minusInt64X2# u2 v2) (minusInt64X2# u3 v3) (minusInt64X2# u4 v4) (minusInt64X2# u5 v5) (minusInt64X2# u6 v6) (minusInt64X2# u7 v7)
  timesF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X16WithVec128 (timesInt64X2# u0 v0) (timesInt64X2# u1 v1) (timesInt64X2# u2 v2) (timesInt64X2# u3 v3) (timesInt64X2# u4 v4) (timesInt64X2# u5 v5) (timesInt64X2# u6 v6) (timesInt64X2# u7 v7)
  negateF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) = MkInt64X16WithVec128 (negateInt64X2# u0) (negateInt64X2# u1) (negateInt64X2# u2) (negateInt64X2# u3) (negateInt64X2# u4) (negateInt64X2# u5) (negateInt64X2# u6) (negateInt64X2# u7)
  absF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) = MkInt64X16WithVec128 (absInt64X2# u0) (absInt64X2# u1) (absInt64X2# u2) (absInt64X2# u3) (absInt64X2# u4) (absInt64X2# u5) (absInt64X2# u6) (absInt64X2# u7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Int64 where
  andF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X16WithVec128 (andInt64X2# u0 v0) (andInt64X2# u1 v1) (andInt64X2# u2 v2) (andInt64X2# u3 v3) (andInt64X2# u4 v4) (andInt64X2# u5 v5) (andInt64X2# u6 v6) (andInt64X2# u7 v7)
  orF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X16WithVec128 (orInt64X2# u0 v0) (orInt64X2# u1 v1) (orInt64X2# u2 v2) (orInt64X2# u3 v3) (orInt64X2# u4 v4) (orInt64X2# u5 v5) (orInt64X2# u6 v6) (orInt64X2# u7 v7)
  xorF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X16WithVec128 (xorInt64X2# u0 v0) (xorInt64X2# u1 v1) (xorInt64X2# u2 v2) (xorInt64X2# u3 v3) (xorInt64X2# u4 v4) (xorInt64X2# u5 v5) (xorInt64X2# u6 v6) (xorInt64X2# u7 v7)
  complementF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) = MkInt64X16WithVec128 (complementInt64X2# u0) (complementInt64X2# u1) (complementInt64X2# u2) (complementInt64X2# u3) (complementInt64X2# u4) (complementInt64X2# u5) (complementInt64X2# u6) (complementInt64X2# u7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int64 where
  shiftLF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkInt64X16WithVec128 (shiftLInt64X2# u0 i) (shiftLInt64X2# u1 i) (shiftLInt64X2# u2 i) (shiftLInt64X2# u3 i) (shiftLInt64X2# u4 i) (shiftLInt64X2# u5 i) (shiftLInt64X2# u6 i) (shiftLInt64X2# u7 i)
  shiftRF (MkInt64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkInt64X16WithVec128 (shiftRInt64X2# u0 i) (shiftRInt64X2# u1 i) (shiftRInt64X2# u2 i) (shiftRInt64X2# u3 i) (shiftRInt64X2# u4 i) (shiftRInt64X2# u5 i) (shiftRInt64X2# u6 i) (shiftRInt64X2# u7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Int64 where
  enumFromZero = MkInt64X16WithVec128 (packInt64X2# (# 0#Int64, 1#Int64 #)) (packInt64X2# (# 2#Int64, 3#Int64 #)) (packInt64X2# (# 4#Int64, 5#Int64 #)) (packInt64X2# (# 6#Int64, 7#Int64 #)) (packInt64X2# (# 8#Int64, 9#Int64 #)) (packInt64X2# (# 10#Int64, 11#Int64 #)) (packInt64X2# (# 12#Int64, 13#Int64 #)) (packInt64X2# (# 14#Int64, 15#Int64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int64 where
  indexByteArraySIMD# ba i = MkInt64X16WithVec128 (indexInt64ArrayAsInt64X2# ba i) (indexInt64ArrayAsInt64X2# ba (i +# 2#)) (indexInt64ArrayAsInt64X2# ba (i +# 4#)) (indexInt64ArrayAsInt64X2# ba (i +# 6#)) (indexInt64ArrayAsInt64X2# ba (i +# 8#)) (indexInt64ArrayAsInt64X2# ba (i +# 10#)) (indexInt64ArrayAsInt64X2# ba (i +# 12#)) (indexInt64ArrayAsInt64X2# ba (i +# 14#))
  readByteArraySIMD# mba i s0 = case readInt64ArrayAsInt64X2# mba i s0 of (# s1, v0 #) -> case readInt64ArrayAsInt64X2# mba (i +# 2#) s1 of (# s2, v1 #) -> case readInt64ArrayAsInt64X2# mba (i +# 4#) s2 of (# s3, v2 #) -> case readInt64ArrayAsInt64X2# mba (i +# 6#) s3 of (# s4, v3 #) -> case readInt64ArrayAsInt64X2# mba (i +# 8#) s4 of (# s5, v4 #) -> case readInt64ArrayAsInt64X2# mba (i +# 10#) s5 of (# s6, v5 #) -> case readInt64ArrayAsInt64X2# mba (i +# 12#) s6 of (# s7, v6 #) -> case readInt64ArrayAsInt64X2# mba (i +# 14#) s7 of (# s8, v7 #) -> (# s8, MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 #)
  writeByteArraySIMD# mba i (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) s0 = case writeInt64ArrayAsInt64X2# mba i v0 s0 of s1 -> case writeInt64ArrayAsInt64X2# mba (i +# 2#) v1 s1 of s2 -> case writeInt64ArrayAsInt64X2# mba (i +# 4#) v2 s2 of s3 -> case writeInt64ArrayAsInt64X2# mba (i +# 6#) v3 s3 of s4 -> case writeInt64ArrayAsInt64X2# mba (i +# 8#) v4 s4 of s5 -> case writeInt64ArrayAsInt64X2# mba (i +# 10#) v5 s5 of s6 -> case writeInt64ArrayAsInt64X2# mba (i +# 12#) v6 s6 of s7 -> writeInt64ArrayAsInt64X2# mba (i +# 14#) v7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt64OffAddrAsInt64X2# addr i s0 of (# s1, v0 #) -> case readInt64OffAddrAsInt64X2# addr (i +# 2#) s1 of (# s2, v1 #) -> case readInt64OffAddrAsInt64X2# addr (i +# 4#) s2 of (# s3, v2 #) -> case readInt64OffAddrAsInt64X2# addr (i +# 6#) s3 of (# s4, v3 #) -> case readInt64OffAddrAsInt64X2# addr (i +# 8#) s4 of (# s5, v4 #) -> case readInt64OffAddrAsInt64X2# addr (i +# 10#) s5 of (# s6, v5 #) -> case readInt64OffAddrAsInt64X2# addr (i +# 12#) s6 of (# s7, v6 #) -> case readInt64OffAddrAsInt64X2# addr (i +# 14#) s7 of (# s8, v7 #) -> (# s8, MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = IO (\s0 -> case writeInt64OffAddrAsInt64X2# addr i v0 s0 of s1 -> case writeInt64OffAddrAsInt64X2# addr (i +# 2#) v1 s1 of s2 -> case writeInt64OffAddrAsInt64X2# addr (i +# 4#) v2 s2 of s3 -> case writeInt64OffAddrAsInt64X2# addr (i +# 6#) v3 s3 of s4 -> case writeInt64OffAddrAsInt64X2# addr (i +# 8#) v4 s4 of s5 -> case writeInt64OffAddrAsInt64X2# addr (i +# 10#) v5 s5 of s6 -> case writeInt64OffAddrAsInt64X2# addr (i +# 12#) v6 s6 of s7 -> case writeInt64OffAddrAsInt64X2# addr (i +# 14#) v7 s7 of s8 -> (# s8, () #))
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
instance (AllLessThan indices 16, ShuffleMany Word8X16# indices) => UnaryShuffleT indices X16 Word8 where
  unaryShuffle (MkWord8X16 x) = MkWord8X16 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 32, ShuffleMany Word8X16# indices) => BinaryShuffleT indices X16 Word8 where
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
data instance X16 Word16 = MkWord16X16WithVec128 Word16X8# Word16X8#
instance PackX16 X16 Word16 where
  mkX16 (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) (W16# x8) (W16# x9) (W16# x10) (W16# x11) (W16# x12) (W16# x13) (W16# x14) (W16# x15) = MkWord16X16WithVec128 (packWord16X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packWord16X8# (# x8, x9, x10, x11, x12, x13, x14, x15 #))
  unpackX16 (MkWord16X16WithVec128 v0 v1) = case unpackWord16X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackWord16X8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> (W16# x0, W16# x1, W16# x2, W16# x3, W16# x4, W16# x5, W16# x6, W16# x7, W16# x8, W16# x9, W16# x10, W16# x11, W16# x12, W16# x13, W16# x14, W16# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word16 where
  broadcast (W16# x) = let !v = broadcastWord16X8# x in MkWord16X16WithVec128 v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Word16 where
  selectF (MkBoolX16 !cond) (MkWord16X16WithVec128 x0 x1) (MkWord16X16WithVec128 y0 y1) = MkWord16X16WithVec128 (selectWord16X8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord16X8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany Word16X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Word16X8# [i8, i9, i10, i11, i12, i13, i14, i15]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word16 where
  unaryShuffle (MkWord16X16WithVec128 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord16X16WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany Word16X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Word16X8# [i8, i9, i10, i11, i12, i13, i14, i15]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word16 where
  binaryShuffle (MkWord16X16WithVec128 x0 x1) (MkWord16X16WithVec128 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord16X16WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word16 where
  eqF (MkWord16X16WithVec128 u0 u1) (MkWord16X16WithVec128 v0 v1) = MkBoolX16 $ (fromIntegral (eqWord16X8# u0 v0)) .|. (fromIntegral (eqWord16X8# u1 v1) `unsafeShiftL` 8)
  {-# INLINE eqF #-}
instance OrderedF X16 Word16 where
  ltF (MkWord16X16WithVec128 u0 u1) (MkWord16X16WithVec128 v0 v1) = MkBoolX16 $ (fromIntegral (ltWord16X8# u0 v0)) .|. (fromIntegral (ltWord16X8# u1 v1) `unsafeShiftL` 8)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word16 where
  minF (MkWord16X16WithVec128 u0 u1) (MkWord16X16WithVec128 v0 v1) = MkWord16X16WithVec128 (minWord16X8# u0 v0) (minWord16X8# u1 v1)
  maxF (MkWord16X16WithVec128 u0 u1) (MkWord16X16WithVec128 v0 v1) = MkWord16X16WithVec128 (maxWord16X8# u0 v0) (maxWord16X8# u1 v1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word16 where
  plusF (MkWord16X16WithVec128 u0 u1) (MkWord16X16WithVec128 v0 v1) = MkWord16X16WithVec128 (plusWord16X8# u0 v0) (plusWord16X8# u1 v1)
  minusF (MkWord16X16WithVec128 u0 u1) (MkWord16X16WithVec128 v0 v1) = MkWord16X16WithVec128 (minusWord16X8# u0 v0) (minusWord16X8# u1 v1)
  timesF (MkWord16X16WithVec128 u0 u1) (MkWord16X16WithVec128 v0 v1) = MkWord16X16WithVec128 (timesWord16X8# u0 v0) (timesWord16X8# u1 v1)
  -- Currently, there is no negateWord16X8#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Word16 where
  andF (MkWord16X16WithVec128 u0 u1) (MkWord16X16WithVec128 v0 v1) = MkWord16X16WithVec128 (andWord16X8# u0 v0) (andWord16X8# u1 v1)
  orF (MkWord16X16WithVec128 u0 u1) (MkWord16X16WithVec128 v0 v1) = MkWord16X16WithVec128 (orWord16X8# u0 v0) (orWord16X8# u1 v1)
  xorF (MkWord16X16WithVec128 u0 u1) (MkWord16X16WithVec128 v0 v1) = MkWord16X16WithVec128 (xorWord16X8# u0 v0) (xorWord16X8# u1 v1)
  complementF (MkWord16X16WithVec128 u0 u1) = MkWord16X16WithVec128 (complementWord16X8# u0) (complementWord16X8# u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word16 where
  shiftLF (MkWord16X16WithVec128 u0 u1) (I# i) = MkWord16X16WithVec128 (shiftLWord16X8# u0 i) (shiftLWord16X8# u1 i)
  shiftRF (MkWord16X16WithVec128 u0 u1) (I# i) = MkWord16X16WithVec128 (shiftRWord16X8# u0 i) (shiftRWord16X8# u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Word16 where
  enumFromZero = MkWord16X16WithVec128 (packWord16X8# (# 0#Word16, 1#Word16, 2#Word16, 3#Word16, 4#Word16, 5#Word16, 6#Word16, 7#Word16 #)) (packWord16X8# (# 8#Word16, 9#Word16, 10#Word16, 11#Word16, 12#Word16, 13#Word16, 14#Word16, 15#Word16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word16 where
  indexByteArraySIMD# ba i = MkWord16X16WithVec128 (indexWord16ArrayAsWord16X8# ba i) (indexWord16ArrayAsWord16X8# ba (i +# 8#))
  readByteArraySIMD# mba i s0 = case readWord16ArrayAsWord16X8# mba i s0 of (# s1, v0 #) -> case readWord16ArrayAsWord16X8# mba (i +# 8#) s1 of (# s2, v1 #) -> (# s2, MkWord16X16WithVec128 v0 v1 #)
  writeByteArraySIMD# mba i (MkWord16X16WithVec128 v0 v1) s0 = case writeWord16ArrayAsWord16X8# mba i v0 s0 of s1 -> writeWord16ArrayAsWord16X8# mba (i +# 8#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord16OffAddrAsWord16X8# addr i s0 of (# s1, v0 #) -> case readWord16OffAddrAsWord16X8# addr (i +# 8#) s1 of (# s2, v1 #) -> (# s2, MkWord16X16WithVec128 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X16WithVec128 v0 v1) = IO (\s0 -> case writeWord16OffAddrAsWord16X8# addr i v0 s0 of s1 -> case writeWord16OffAddrAsWord16X8# addr (i +# 8#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Word32 = MkWord32X16WithVec128 Word32X4# Word32X4# Word32X4# Word32X4#
instance PackX16 X16 Word32 where
  mkX16 (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) (W32# x8) (W32# x9) (W32# x10) (W32# x11) (W32# x12) (W32# x13) (W32# x14) (W32# x15) = MkWord32X16WithVec128 (packWord32X4# (# x0, x1, x2, x3 #)) (packWord32X4# (# x4, x5, x6, x7 #)) (packWord32X4# (# x8, x9, x10, x11 #)) (packWord32X4# (# x12, x13, x14, x15 #))
  unpackX16 (MkWord32X16WithVec128 v0 v1 v2 v3) = case unpackWord32X4# v0 of (# x0, x1, x2, x3 #) -> case unpackWord32X4# v1 of (# x4, x5, x6, x7 #) -> case unpackWord32X4# v2 of (# x8, x9, x10, x11 #) -> case unpackWord32X4# v3 of (# x12, x13, x14, x15 #) -> (W32# x0, W32# x1, W32# x2, W32# x3, W32# x4, W32# x5, W32# x6, W32# x7, W32# x8, W32# x9, W32# x10, W32# x11, W32# x12, W32# x13, W32# x14, W32# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word32 where
  broadcast (W32# x) = let !v = broadcastWord32X4# x in MkWord32X16WithVec128 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Word32 where
  selectF (MkBoolX16 !cond) (MkWord32X16WithVec128 x0 x1 x2 x3) (MkWord32X16WithVec128 y0 y1 y2 y3) = MkWord32X16WithVec128 (selectWord32X4# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord32X4# (fromIntegral $ cond `unsafeShiftR` 4) x1 y1) (selectWord32X4# (fromIntegral $ cond `unsafeShiftR` 8) x2 y2) (selectWord32X4# (fromIntegral $ cond `unsafeShiftR` 12) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany Word32X4# [i0, i1, i2, i3], ShuffleMany Word32X4# [i4, i5, i6, i7], ShuffleMany Word32X4# [i8, i9, i10, i11], ShuffleMany Word32X4# [i12, i13, i14, i15]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word32 where
  unaryShuffle (MkWord32X16WithVec128 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord32X16WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany Word32X4# [i0, i1, i2, i3], ShuffleMany Word32X4# [i4, i5, i6, i7], ShuffleMany Word32X4# [i8, i9, i10, i11], ShuffleMany Word32X4# [i12, i13, i14, i15]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word32 where
  binaryShuffle (MkWord32X16WithVec128 x0 x1 x2 x3) (MkWord32X16WithVec128 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord32X16WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13, i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word32 where
  eqF (MkWord32X16WithVec128 u0 u1 u2 u3) (MkWord32X16WithVec128 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (eqWord32X4# u0 v0)) .|. (fromIntegral (eqWord32X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (eqWord32X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (eqWord32X4# u3 v3) `unsafeShiftL` 12)
  {-# INLINE eqF #-}
instance OrderedF X16 Word32 where
  ltF (MkWord32X16WithVec128 u0 u1 u2 u3) (MkWord32X16WithVec128 v0 v1 v2 v3) = MkBoolX16 $ (fromIntegral (ltWord32X4# u0 v0)) .|. (fromIntegral (ltWord32X4# u1 v1) `unsafeShiftL` 4) .|. (fromIntegral (ltWord32X4# u2 v2) `unsafeShiftL` 8) .|. (fromIntegral (ltWord32X4# u3 v3) `unsafeShiftL` 12)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word32 where
  minF (MkWord32X16WithVec128 u0 u1 u2 u3) (MkWord32X16WithVec128 v0 v1 v2 v3) = MkWord32X16WithVec128 (minWord32X4# u0 v0) (minWord32X4# u1 v1) (minWord32X4# u2 v2) (minWord32X4# u3 v3)
  maxF (MkWord32X16WithVec128 u0 u1 u2 u3) (MkWord32X16WithVec128 v0 v1 v2 v3) = MkWord32X16WithVec128 (maxWord32X4# u0 v0) (maxWord32X4# u1 v1) (maxWord32X4# u2 v2) (maxWord32X4# u3 v3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word32 where
  plusF (MkWord32X16WithVec128 u0 u1 u2 u3) (MkWord32X16WithVec128 v0 v1 v2 v3) = MkWord32X16WithVec128 (plusWord32X4# u0 v0) (plusWord32X4# u1 v1) (plusWord32X4# u2 v2) (plusWord32X4# u3 v3)
  minusF (MkWord32X16WithVec128 u0 u1 u2 u3) (MkWord32X16WithVec128 v0 v1 v2 v3) = MkWord32X16WithVec128 (minusWord32X4# u0 v0) (minusWord32X4# u1 v1) (minusWord32X4# u2 v2) (minusWord32X4# u3 v3)
  timesF (MkWord32X16WithVec128 u0 u1 u2 u3) (MkWord32X16WithVec128 v0 v1 v2 v3) = MkWord32X16WithVec128 (timesWord32X4# u0 v0) (timesWord32X4# u1 v1) (timesWord32X4# u2 v2) (timesWord32X4# u3 v3)
  -- Currently, there is no negateWord32X4#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Word32 where
  andF (MkWord32X16WithVec128 u0 u1 u2 u3) (MkWord32X16WithVec128 v0 v1 v2 v3) = MkWord32X16WithVec128 (andWord32X4# u0 v0) (andWord32X4# u1 v1) (andWord32X4# u2 v2) (andWord32X4# u3 v3)
  orF (MkWord32X16WithVec128 u0 u1 u2 u3) (MkWord32X16WithVec128 v0 v1 v2 v3) = MkWord32X16WithVec128 (orWord32X4# u0 v0) (orWord32X4# u1 v1) (orWord32X4# u2 v2) (orWord32X4# u3 v3)
  xorF (MkWord32X16WithVec128 u0 u1 u2 u3) (MkWord32X16WithVec128 v0 v1 v2 v3) = MkWord32X16WithVec128 (xorWord32X4# u0 v0) (xorWord32X4# u1 v1) (xorWord32X4# u2 v2) (xorWord32X4# u3 v3)
  complementF (MkWord32X16WithVec128 u0 u1 u2 u3) = MkWord32X16WithVec128 (complementWord32X4# u0) (complementWord32X4# u1) (complementWord32X4# u2) (complementWord32X4# u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word32 where
  shiftLF (MkWord32X16WithVec128 u0 u1 u2 u3) (I# i) = MkWord32X16WithVec128 (shiftLWord32X4# u0 i) (shiftLWord32X4# u1 i) (shiftLWord32X4# u2 i) (shiftLWord32X4# u3 i)
  shiftRF (MkWord32X16WithVec128 u0 u1 u2 u3) (I# i) = MkWord32X16WithVec128 (shiftRWord32X4# u0 i) (shiftRWord32X4# u1 i) (shiftRWord32X4# u2 i) (shiftRWord32X4# u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Word32 where
  enumFromZero = MkWord32X16WithVec128 (packWord32X4# (# 0#Word32, 1#Word32, 2#Word32, 3#Word32 #)) (packWord32X4# (# 4#Word32, 5#Word32, 6#Word32, 7#Word32 #)) (packWord32X4# (# 8#Word32, 9#Word32, 10#Word32, 11#Word32 #)) (packWord32X4# (# 12#Word32, 13#Word32, 14#Word32, 15#Word32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word32 where
  indexByteArraySIMD# ba i = MkWord32X16WithVec128 (indexWord32ArrayAsWord32X4# ba i) (indexWord32ArrayAsWord32X4# ba (i +# 4#)) (indexWord32ArrayAsWord32X4# ba (i +# 8#)) (indexWord32ArrayAsWord32X4# ba (i +# 12#))
  readByteArraySIMD# mba i s0 = case readWord32ArrayAsWord32X4# mba i s0 of (# s1, v0 #) -> case readWord32ArrayAsWord32X4# mba (i +# 4#) s1 of (# s2, v1 #) -> case readWord32ArrayAsWord32X4# mba (i +# 8#) s2 of (# s3, v2 #) -> case readWord32ArrayAsWord32X4# mba (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkWord32X16WithVec128 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkWord32X16WithVec128 v0 v1 v2 v3) s0 = case writeWord32ArrayAsWord32X4# mba i v0 s0 of s1 -> case writeWord32ArrayAsWord32X4# mba (i +# 4#) v1 s1 of s2 -> case writeWord32ArrayAsWord32X4# mba (i +# 8#) v2 s2 of s3 -> writeWord32ArrayAsWord32X4# mba (i +# 12#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord32OffAddrAsWord32X4# addr i s0 of (# s1, v0 #) -> case readWord32OffAddrAsWord32X4# addr (i +# 4#) s1 of (# s2, v1 #) -> case readWord32OffAddrAsWord32X4# addr (i +# 8#) s2 of (# s3, v2 #) -> case readWord32OffAddrAsWord32X4# addr (i +# 12#) s3 of (# s4, v3 #) -> (# s4, MkWord32X16WithVec128 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X16WithVec128 v0 v1 v2 v3) = IO (\s0 -> case writeWord32OffAddrAsWord32X4# addr i v0 s0 of s1 -> case writeWord32OffAddrAsWord32X4# addr (i +# 4#) v1 s1 of s2 -> case writeWord32OffAddrAsWord32X4# addr (i +# 8#) v2 s2 of s3 -> case writeWord32OffAddrAsWord32X4# addr (i +# 12#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Word64 = MkWord64X16WithVec128 Word64X2# Word64X2# Word64X2# Word64X2# Word64X2# Word64X2# Word64X2# Word64X2#
instance PackX16 X16 Word64 where
  mkX16 (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) (W64# x8) (W64# x9) (W64# x10) (W64# x11) (W64# x12) (W64# x13) (W64# x14) (W64# x15) = MkWord64X16WithVec128 (packWord64X2# (# x0, x1 #)) (packWord64X2# (# x2, x3 #)) (packWord64X2# (# x4, x5 #)) (packWord64X2# (# x6, x7 #)) (packWord64X2# (# x8, x9 #)) (packWord64X2# (# x10, x11 #)) (packWord64X2# (# x12, x13 #)) (packWord64X2# (# x14, x15 #))
  unpackX16 (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = case unpackWord64X2# v0 of (# x0, x1 #) -> case unpackWord64X2# v1 of (# x2, x3 #) -> case unpackWord64X2# v2 of (# x4, x5 #) -> case unpackWord64X2# v3 of (# x6, x7 #) -> case unpackWord64X2# v4 of (# x8, x9 #) -> case unpackWord64X2# v5 of (# x10, x11 #) -> case unpackWord64X2# v6 of (# x12, x13 #) -> case unpackWord64X2# v7 of (# x14, x15 #) -> (W64# x0, W64# x1, W64# x2, W64# x3, W64# x4, W64# x5, W64# x6, W64# x7, W64# x8, W64# x9, W64# x10, W64# x11, W64# x12, W64# x13, W64# x14, W64# x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word64 where
  broadcast (W64# x) = let !v = broadcastWord64X2# x in MkWord64X16WithVec128 v v v v v v v v
  {-# INLINE broadcast #-}
instance SelectableF X16 Word64 where
  selectF (MkBoolX16 !cond) (MkWord64X16WithVec128 x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X16WithVec128 y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X16WithVec128 (selectWord64X2# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord64X2# (fromIntegral $ cond `unsafeShiftR` 2) x1 y1) (selectWord64X2# (fromIntegral $ cond `unsafeShiftR` 4) x2 y2) (selectWord64X2# (fromIntegral $ cond `unsafeShiftR` 6) x3 y3) (selectWord64X2# (fromIntegral $ cond `unsafeShiftR` 8) x4 y4) (selectWord64X2# (fromIntegral $ cond `unsafeShiftR` 10) x5 y5) (selectWord64X2# (fromIntegral $ cond `unsafeShiftR` 12) x6 y6) (selectWord64X2# (fromIntegral $ cond `unsafeShiftR` 14) x7 y7)
  {-# INLINE selectF #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, ShuffleMany Word64X2# [i0, i1], ShuffleMany Word64X2# [i2, i3], ShuffleMany Word64X2# [i4, i5], ShuffleMany Word64X2# [i6, i7], ShuffleMany Word64X2# [i8, i9], ShuffleMany Word64X2# [i10, i11], ShuffleMany Word64X2# [i12, i13], ShuffleMany Word64X2# [i14, i15]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word64 where
  unaryShuffle (MkWord64X16WithVec128 x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord64X16WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9] sources) (shuffleMany# @_ @_ @[i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13] sources) (shuffleMany# @_ @_ @[i14, i15] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, ShuffleMany Word64X2# [i0, i1], ShuffleMany Word64X2# [i2, i3], ShuffleMany Word64X2# [i4, i5], ShuffleMany Word64X2# [i6, i7], ShuffleMany Word64X2# [i8, i9], ShuffleMany Word64X2# [i10, i11], ShuffleMany Word64X2# [i12, i13], ShuffleMany Word64X2# [i14, i15]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word64 where
  binaryShuffle (MkWord64X16WithVec128 x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X16WithVec128 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord64X16WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9] sources) (shuffleMany# @_ @_ @[i10, i11] sources) (shuffleMany# @_ @_ @[i12, i13] sources) (shuffleMany# @_ @_ @[i14, i15] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word64 where
  eqF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX16 $ (fromIntegral (eqWord64X2# u0 v0)) .|. (fromIntegral (eqWord64X2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (eqWord64X2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (eqWord64X2# u3 v3) `unsafeShiftL` 6) .|. (fromIntegral (eqWord64X2# u4 v4) `unsafeShiftL` 8) .|. (fromIntegral (eqWord64X2# u5 v5) `unsafeShiftL` 10) .|. (fromIntegral (eqWord64X2# u6 v6) `unsafeShiftL` 12) .|. (fromIntegral (eqWord64X2# u7 v7) `unsafeShiftL` 14)
  {-# INLINE eqF #-}
instance OrderedF X16 Word64 where
  ltF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX16 $ (fromIntegral (ltWord64X2# u0 v0)) .|. (fromIntegral (ltWord64X2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (ltWord64X2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (ltWord64X2# u3 v3) `unsafeShiftL` 6) .|. (fromIntegral (ltWord64X2# u4 v4) `unsafeShiftL` 8) .|. (fromIntegral (ltWord64X2# u5 v5) `unsafeShiftL` 10) .|. (fromIntegral (ltWord64X2# u6 v6) `unsafeShiftL` 12) .|. (fromIntegral (ltWord64X2# u7 v7) `unsafeShiftL` 14)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word64 where
  minF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X16WithVec128 (minWord64X2# u0 v0) (minWord64X2# u1 v1) (minWord64X2# u2 v2) (minWord64X2# u3 v3) (minWord64X2# u4 v4) (minWord64X2# u5 v5) (minWord64X2# u6 v6) (minWord64X2# u7 v7)
  maxF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X16WithVec128 (maxWord64X2# u0 v0) (maxWord64X2# u1 v1) (maxWord64X2# u2 v2) (maxWord64X2# u3 v3) (maxWord64X2# u4 v4) (maxWord64X2# u5 v5) (maxWord64X2# u6 v6) (maxWord64X2# u7 v7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word64 where
  plusF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X16WithVec128 (plusWord64X2# u0 v0) (plusWord64X2# u1 v1) (plusWord64X2# u2 v2) (plusWord64X2# u3 v3) (plusWord64X2# u4 v4) (plusWord64X2# u5 v5) (plusWord64X2# u6 v6) (plusWord64X2# u7 v7)
  minusF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X16WithVec128 (minusWord64X2# u0 v0) (minusWord64X2# u1 v1) (minusWord64X2# u2 v2) (minusWord64X2# u3 v3) (minusWord64X2# u4 v4) (minusWord64X2# u5 v5) (minusWord64X2# u6 v6) (minusWord64X2# u7 v7)
  timesF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X16WithVec128 (timesWord64X2# u0 v0) (timesWord64X2# u1 v1) (timesWord64X2# u2 v2) (timesWord64X2# u3 v3) (timesWord64X2# u4 v4) (timesWord64X2# u5 v5) (timesWord64X2# u6 v6) (timesWord64X2# u7 v7)
  -- Currently, there is no negateWord64X2#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X16 Word64 where
  andF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X16WithVec128 (andWord64X2# u0 v0) (andWord64X2# u1 v1) (andWord64X2# u2 v2) (andWord64X2# u3 v3) (andWord64X2# u4 v4) (andWord64X2# u5 v5) (andWord64X2# u6 v6) (andWord64X2# u7 v7)
  orF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X16WithVec128 (orWord64X2# u0 v0) (orWord64X2# u1 v1) (orWord64X2# u2 v2) (orWord64X2# u3 v3) (orWord64X2# u4 v4) (orWord64X2# u5 v5) (orWord64X2# u6 v6) (orWord64X2# u7 v7)
  xorF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X16WithVec128 (xorWord64X2# u0 v0) (xorWord64X2# u1 v1) (xorWord64X2# u2 v2) (xorWord64X2# u3 v3) (xorWord64X2# u4 v4) (xorWord64X2# u5 v5) (xorWord64X2# u6 v6) (xorWord64X2# u7 v7)
  complementF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) = MkWord64X16WithVec128 (complementWord64X2# u0) (complementWord64X2# u1) (complementWord64X2# u2) (complementWord64X2# u3) (complementWord64X2# u4) (complementWord64X2# u5) (complementWord64X2# u6) (complementWord64X2# u7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word64 where
  shiftLF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkWord64X16WithVec128 (shiftLWord64X2# u0 i) (shiftLWord64X2# u1 i) (shiftLWord64X2# u2 i) (shiftLWord64X2# u3 i) (shiftLWord64X2# u4 i) (shiftLWord64X2# u5 i) (shiftLWord64X2# u6 i) (shiftLWord64X2# u7 i)
  shiftRF (MkWord64X16WithVec128 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkWord64X16WithVec128 (shiftRWord64X2# u0 i) (shiftRWord64X2# u1 i) (shiftRWord64X2# u2 i) (shiftRWord64X2# u3 i) (shiftRWord64X2# u4 i) (shiftRWord64X2# u5 i) (shiftRWord64X2# u6 i) (shiftRWord64X2# u7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X16 Word64 where
  enumFromZero = MkWord64X16WithVec128 (packWord64X2# (# 0#Word64, 1#Word64 #)) (packWord64X2# (# 2#Word64, 3#Word64 #)) (packWord64X2# (# 4#Word64, 5#Word64 #)) (packWord64X2# (# 6#Word64, 7#Word64 #)) (packWord64X2# (# 8#Word64, 9#Word64 #)) (packWord64X2# (# 10#Word64, 11#Word64 #)) (packWord64X2# (# 12#Word64, 13#Word64 #)) (packWord64X2# (# 14#Word64, 15#Word64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word64 where
  indexByteArraySIMD# ba i = MkWord64X16WithVec128 (indexWord64ArrayAsWord64X2# ba i) (indexWord64ArrayAsWord64X2# ba (i +# 2#)) (indexWord64ArrayAsWord64X2# ba (i +# 4#)) (indexWord64ArrayAsWord64X2# ba (i +# 6#)) (indexWord64ArrayAsWord64X2# ba (i +# 8#)) (indexWord64ArrayAsWord64X2# ba (i +# 10#)) (indexWord64ArrayAsWord64X2# ba (i +# 12#)) (indexWord64ArrayAsWord64X2# ba (i +# 14#))
  readByteArraySIMD# mba i s0 = case readWord64ArrayAsWord64X2# mba i s0 of (# s1, v0 #) -> case readWord64ArrayAsWord64X2# mba (i +# 2#) s1 of (# s2, v1 #) -> case readWord64ArrayAsWord64X2# mba (i +# 4#) s2 of (# s3, v2 #) -> case readWord64ArrayAsWord64X2# mba (i +# 6#) s3 of (# s4, v3 #) -> case readWord64ArrayAsWord64X2# mba (i +# 8#) s4 of (# s5, v4 #) -> case readWord64ArrayAsWord64X2# mba (i +# 10#) s5 of (# s6, v5 #) -> case readWord64ArrayAsWord64X2# mba (i +# 12#) s6 of (# s7, v6 #) -> case readWord64ArrayAsWord64X2# mba (i +# 14#) s7 of (# s8, v7 #) -> (# s8, MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 #)
  writeByteArraySIMD# mba i (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) s0 = case writeWord64ArrayAsWord64X2# mba i v0 s0 of s1 -> case writeWord64ArrayAsWord64X2# mba (i +# 2#) v1 s1 of s2 -> case writeWord64ArrayAsWord64X2# mba (i +# 4#) v2 s2 of s3 -> case writeWord64ArrayAsWord64X2# mba (i +# 6#) v3 s3 of s4 -> case writeWord64ArrayAsWord64X2# mba (i +# 8#) v4 s4 of s5 -> case writeWord64ArrayAsWord64X2# mba (i +# 10#) v5 s5 of s6 -> case writeWord64ArrayAsWord64X2# mba (i +# 12#) v6 s6 of s7 -> writeWord64ArrayAsWord64X2# mba (i +# 14#) v7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord64OffAddrAsWord64X2# addr i s0 of (# s1, v0 #) -> case readWord64OffAddrAsWord64X2# addr (i +# 2#) s1 of (# s2, v1 #) -> case readWord64OffAddrAsWord64X2# addr (i +# 4#) s2 of (# s3, v2 #) -> case readWord64OffAddrAsWord64X2# addr (i +# 6#) s3 of (# s4, v3 #) -> case readWord64OffAddrAsWord64X2# addr (i +# 8#) s4 of (# s5, v4 #) -> case readWord64OffAddrAsWord64X2# addr (i +# 10#) s5 of (# s6, v5 #) -> case readWord64OffAddrAsWord64X2# addr (i +# 12#) s6 of (# s7, v6 #) -> case readWord64OffAddrAsWord64X2# addr (i +# 14#) s7 of (# s8, v7 #) -> (# s8, MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = IO (\s0 -> case writeWord64OffAddrAsWord64X2# addr i v0 s0 of s1 -> case writeWord64OffAddrAsWord64X2# addr (i +# 2#) v1 s1 of s2 -> case writeWord64OffAddrAsWord64X2# addr (i +# 4#) v2 s2 of s3 -> case writeWord64OffAddrAsWord64X2# addr (i +# 6#) v3 s3 of s4 -> case writeWord64OffAddrAsWord64X2# addr (i +# 8#) v4 s4 of s5 -> case writeWord64OffAddrAsWord64X2# addr (i +# 10#) v5 s5 of s6 -> case writeWord64OffAddrAsWord64X2# addr (i +# 12#) v6 s6 of s7 -> case writeWord64OffAddrAsWord64X2# addr (i +# 14#) v7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
#else
-- The NCG of GHC 9.12 does not support integer vectors
instance ImplementationDescription X16 where
  implementationDescription _ = "X16;maxBits(Float,Double)=128,maxBits(other)=0"
data instance X16 Int8 = MkInt8X16WithElems !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8
instance PackX16 X16 Int8 where
  mkX16 = MkInt8X16WithElems
  unpackX16 (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int8 where
  broadcast !x = MkInt8X16WithElems x x x x x x x x x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X16 Int8 where
  selectF (MkBoolX16 !cond) (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt8X16WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7) (if testBit cond 8 then x8 else y8) (if testBit cond 9 then x9 else y9) (if testBit cond 10 then x10 else y10) (if testBit cond 11 then x11 else y11) (if testBit cond 12 then x12 else y12) (if testBit cond 13 then x13 else y13) (if testBit cond 14 then x14 else y14) (if testBit cond 15 then x15 else y15)
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Int8 i0, Pick Int8 i1, Pick Int8 i2, Pick Int8 i3, Pick Int8 i4, Pick Int8 i5, Pick Int8 i6, Pick Int8 i7, Pick Int8 i8, Pick Int8 i9, Pick Int8 i10, Pick Int8 i11, Pick Int8 i12, Pick Int8 i13, Pick Int8 i14, Pick Int8 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int8 where
  unaryShuffle (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt8X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Int8 i0, Pick Int8 i1, Pick Int8 i2, Pick Int8 i3, Pick Int8 i4, Pick Int8 i5, Pick Int8 i6, Pick Int8 i7, Pick Int8 i8, Pick Int8 i9, Pick Int8 i10, Pick Int8 i11, Pick Int8 i12, Pick Int8 i13, Pick Int8 i14, Pick Int8 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int8 where
  binaryShuffle (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkInt8X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int8 where
  eqF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7) (x8 == y8) (x9 == y9) (x10 == y10) (x11 == y11) (x12 == y12) (x13 == y13) (x14 == y14) (x15 == y15)
  {-# INLINE eqF #-}
instance OrderedF X16 Int8 where
  ltF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7) (x8 < y8) (x9 < y9) (x10 < y10) (x11 < y11) (x12 < y12) (x13 < y13) (x14 < y14) (x15 < y15)
  leF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7) (x8 <= y8) (x9 <= y9) (x10 <= y10) (x11 <= y11) (x12 <= y12) (x13 <= y13) (x14 <= y14) (x15 <= y15)
  gtF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7) (x8 > y8) (x9 > y9) (x10 > y10) (x11 > y11) (x12 > y12) (x13 > y13) (x14 > y14) (x15 > y15)
  geF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7) (x8 >= y8) (x9 >= y9) (x10 >= y10) (x11 >= y11) (x12 >= y12) (x13 >= y13) (x14 >= y14) (x15 >= y15)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int8 where
  minF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt8X16WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7) (min x8 y8) (min x9 y9) (min x10 y10) (min x11 y11) (min x12 y12) (min x13 y13) (min x14 y14) (min x15 y15)
  maxF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt8X16WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7) (max x8 y8) (max x9 y9) (max x10 y10) (max x11 y11) (max x12 y12) (max x13 y13) (max x14 y14) (max x15 y15)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int8 where
  plusF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt8X16WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9) (x10 + y10) (x11 + y11) (x12 + y12) (x13 + y13) (x14 + y14) (x15 + y15)
  minusF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt8X16WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9) (x10 - y10) (x11 - y11) (x12 - y12) (x13 - y13) (x14 - y14) (x15 - y15)
  timesF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt8X16WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9) (x10 * y10) (x11 * y11) (x12 * y12) (x13 * y13) (x14 * y14) (x15 * y15)
  negateF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkInt8X16WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7) (- x8) (- x9) (- x10) (- x11) (- x12) (- x13) (- x14) (- x15)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X16 Int8 where
  andF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt8X16WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7) (x8 .&. y8) (x9 .&. y9) (x10 .&. y10) (x11 .&. y11) (x12 .&. y12) (x13 .&. y13) (x14 .&. y14) (x15 .&. y15)
  orF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt8X16WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7) (x8 .|. y8) (x9 .|. y9) (x10 .|. y10) (x11 .|. y11) (x12 .|. y12) (x13 .|. y13) (x14 .|. y14) (x15 .|. y15)
  xorF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt8X16WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7) (xor x8 y8) (xor x9 y9) (xor x10 y10) (xor x11 y11) (xor x12 y12) (xor x13 y13) (xor x14 y14) (xor x15 y15)
  complementF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkInt8X16WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7) (complement x8) (complement x9) (complement x10) (complement x11) (complement x12) (complement x13) (complement x14) (complement x15)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int8 where
  shiftLF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt8X16WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i) (shiftL x8 i) (shiftL x9 i) (shiftL x10 i) (shiftL x11 i) (shiftL x12 i) (shiftL x13 i) (shiftL x14 i) (shiftL x15 i)
  unsafeShiftLF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt8X16WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i) (unsafeShiftL x8 i) (unsafeShiftL x9 i) (unsafeShiftL x10 i) (unsafeShiftL x11 i) (unsafeShiftL x12 i) (unsafeShiftL x13 i) (unsafeShiftL x14 i) (unsafeShiftL x15 i)
  shiftRF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt8X16WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i) (shiftR x8 i) (shiftR x9 i) (shiftR x10 i) (shiftR x11 i) (shiftR x12 i) (shiftR x13 i) (shiftR x14 i) (shiftR x15 i)
  unsafeShiftRF (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt8X16WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i) (unsafeShiftR x8 i) (unsafeShiftR x9 i) (unsafeShiftR x10 i) (unsafeShiftR x11 i) (unsafeShiftR x12 i) (unsafeShiftR x13 i) (unsafeShiftR x14 i) (unsafeShiftR x15 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X16 Int8 where
  enumFromZero = MkInt8X16WithElems 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int8 where
  indexByteArraySIMD# ba i = MkInt8X16WithElems (I8# (GHC.Exts.indexInt8Array# ba i)) (I8# (GHC.Exts.indexInt8Array# ba (i +# 1#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 2#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 3#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 4#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 5#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 6#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 7#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 8#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 9#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 10#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 11#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 12#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 13#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 14#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 15#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt8Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt8Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt8Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt8Array# mba (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readInt8Array# mba (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readInt8Array# mba (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readInt8Array# mba (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readInt8Array# mba (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readInt8Array# mba (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readInt8Array# mba (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readInt8Array# mba (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readInt8Array# mba (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkInt8X16WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) (I8# x8) (I8# x9) (I8# x10) (I8# x11) (I8# x12) (I8# x13) (I8# x14) (I8# x15) #)
  writeByteArraySIMD# mba i (MkInt8X16WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) (I8# x8) (I8# x9) (I8# x10) (I8# x11) (I8# x12) (I8# x13) (I8# x14) (I8# x15)) s0 = case GHC.Exts.writeInt8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt8Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt8Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt8Array# mba (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeInt8Array# mba (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeInt8Array# mba (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeInt8Array# mba (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeInt8Array# mba (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeInt8Array# mba (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeInt8Array# mba (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeInt8Array# mba (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeInt8Array# mba (i +# 14#) x14 s14 of s15 -> GHC.Exts.writeInt8Array# mba (i +# 15#) x15 s15
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkInt8X16WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) (I8# x8) (I8# x9) (I8# x10) (I8# x11) (I8# x12) (I8# x13) (I8# x14) (I8# x15) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X16WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) (I8# x8) (I8# x9) (I8# x10) (I8# x11) (I8# x12) (I8# x13) (I8# x14) (I8# x15)) = IO (\s0 -> case GHC.Exts.writeInt8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 14#) x14 s14 of s15 -> (# GHC.Exts.writeInt8OffAddr# addr (i +# 15#) x15 s15, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Int16 = MkInt16X16WithElems !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16
instance PackX16 X16 Int16 where
  mkX16 = MkInt16X16WithElems
  unpackX16 (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int16 where
  broadcast !x = MkInt16X16WithElems x x x x x x x x x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X16 Int16 where
  selectF (MkBoolX16 !cond) (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt16X16WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7) (if testBit cond 8 then x8 else y8) (if testBit cond 9 then x9 else y9) (if testBit cond 10 then x10 else y10) (if testBit cond 11 then x11 else y11) (if testBit cond 12 then x12 else y12) (if testBit cond 13 then x13 else y13) (if testBit cond 14 then x14 else y14) (if testBit cond 15 then x15 else y15)
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Int16 i0, Pick Int16 i1, Pick Int16 i2, Pick Int16 i3, Pick Int16 i4, Pick Int16 i5, Pick Int16 i6, Pick Int16 i7, Pick Int16 i8, Pick Int16 i9, Pick Int16 i10, Pick Int16 i11, Pick Int16 i12, Pick Int16 i13, Pick Int16 i14, Pick Int16 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int16 where
  unaryShuffle (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt16X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Int16 i0, Pick Int16 i1, Pick Int16 i2, Pick Int16 i3, Pick Int16 i4, Pick Int16 i5, Pick Int16 i6, Pick Int16 i7, Pick Int16 i8, Pick Int16 i9, Pick Int16 i10, Pick Int16 i11, Pick Int16 i12, Pick Int16 i13, Pick Int16 i14, Pick Int16 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int16 where
  binaryShuffle (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkInt16X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int16 where
  eqF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7) (x8 == y8) (x9 == y9) (x10 == y10) (x11 == y11) (x12 == y12) (x13 == y13) (x14 == y14) (x15 == y15)
  {-# INLINE eqF #-}
instance OrderedF X16 Int16 where
  ltF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7) (x8 < y8) (x9 < y9) (x10 < y10) (x11 < y11) (x12 < y12) (x13 < y13) (x14 < y14) (x15 < y15)
  leF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7) (x8 <= y8) (x9 <= y9) (x10 <= y10) (x11 <= y11) (x12 <= y12) (x13 <= y13) (x14 <= y14) (x15 <= y15)
  gtF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7) (x8 > y8) (x9 > y9) (x10 > y10) (x11 > y11) (x12 > y12) (x13 > y13) (x14 > y14) (x15 > y15)
  geF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7) (x8 >= y8) (x9 >= y9) (x10 >= y10) (x11 >= y11) (x12 >= y12) (x13 >= y13) (x14 >= y14) (x15 >= y15)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int16 where
  minF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt16X16WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7) (min x8 y8) (min x9 y9) (min x10 y10) (min x11 y11) (min x12 y12) (min x13 y13) (min x14 y14) (min x15 y15)
  maxF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt16X16WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7) (max x8 y8) (max x9 y9) (max x10 y10) (max x11 y11) (max x12 y12) (max x13 y13) (max x14 y14) (max x15 y15)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int16 where
  plusF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt16X16WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9) (x10 + y10) (x11 + y11) (x12 + y12) (x13 + y13) (x14 + y14) (x15 + y15)
  minusF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt16X16WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9) (x10 - y10) (x11 - y11) (x12 - y12) (x13 - y13) (x14 - y14) (x15 - y15)
  timesF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt16X16WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9) (x10 * y10) (x11 * y11) (x12 * y12) (x13 * y13) (x14 * y14) (x15 * y15)
  negateF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkInt16X16WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7) (- x8) (- x9) (- x10) (- x11) (- x12) (- x13) (- x14) (- x15)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X16 Int16 where
  andF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt16X16WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7) (x8 .&. y8) (x9 .&. y9) (x10 .&. y10) (x11 .&. y11) (x12 .&. y12) (x13 .&. y13) (x14 .&. y14) (x15 .&. y15)
  orF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt16X16WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7) (x8 .|. y8) (x9 .|. y9) (x10 .|. y10) (x11 .|. y11) (x12 .|. y12) (x13 .|. y13) (x14 .|. y14) (x15 .|. y15)
  xorF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt16X16WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7) (xor x8 y8) (xor x9 y9) (xor x10 y10) (xor x11 y11) (xor x12 y12) (xor x13 y13) (xor x14 y14) (xor x15 y15)
  complementF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkInt16X16WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7) (complement x8) (complement x9) (complement x10) (complement x11) (complement x12) (complement x13) (complement x14) (complement x15)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int16 where
  shiftLF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt16X16WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i) (shiftL x8 i) (shiftL x9 i) (shiftL x10 i) (shiftL x11 i) (shiftL x12 i) (shiftL x13 i) (shiftL x14 i) (shiftL x15 i)
  unsafeShiftLF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt16X16WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i) (unsafeShiftL x8 i) (unsafeShiftL x9 i) (unsafeShiftL x10 i) (unsafeShiftL x11 i) (unsafeShiftL x12 i) (unsafeShiftL x13 i) (unsafeShiftL x14 i) (unsafeShiftL x15 i)
  shiftRF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt16X16WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i) (shiftR x8 i) (shiftR x9 i) (shiftR x10 i) (shiftR x11 i) (shiftR x12 i) (shiftR x13 i) (shiftR x14 i) (shiftR x15 i)
  unsafeShiftRF (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt16X16WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i) (unsafeShiftR x8 i) (unsafeShiftR x9 i) (unsafeShiftR x10 i) (unsafeShiftR x11 i) (unsafeShiftR x12 i) (unsafeShiftR x13 i) (unsafeShiftR x14 i) (unsafeShiftR x15 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X16 Int16 where
  enumFromZero = MkInt16X16WithElems 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int16 where
  indexByteArraySIMD# ba i = MkInt16X16WithElems (I16# (GHC.Exts.indexInt16Array# ba i)) (I16# (GHC.Exts.indexInt16Array# ba (i +# 1#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 2#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 3#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 4#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 5#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 6#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 7#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 8#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 9#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 10#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 11#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 12#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 13#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 14#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 15#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt16Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt16Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt16Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt16Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt16Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt16Array# mba (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readInt16Array# mba (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readInt16Array# mba (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readInt16Array# mba (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readInt16Array# mba (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readInt16Array# mba (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readInt16Array# mba (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readInt16Array# mba (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readInt16Array# mba (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkInt16X16WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) (I16# x8) (I16# x9) (I16# x10) (I16# x11) (I16# x12) (I16# x13) (I16# x14) (I16# x15) #)
  writeByteArraySIMD# mba i (MkInt16X16WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) (I16# x8) (I16# x9) (I16# x10) (I16# x11) (I16# x12) (I16# x13) (I16# x14) (I16# x15)) s0 = case GHC.Exts.writeInt16Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt16Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt16Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt16Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt16Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt16Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt16Array# mba (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeInt16Array# mba (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeInt16Array# mba (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeInt16Array# mba (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeInt16Array# mba (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeInt16Array# mba (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeInt16Array# mba (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeInt16Array# mba (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeInt16Array# mba (i +# 14#) x14 s14 of s15 -> GHC.Exts.writeInt16Array# mba (i +# 15#) x15 s15
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkInt16X16WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) (I16# x8) (I16# x9) (I16# x10) (I16# x11) (I16# x12) (I16# x13) (I16# x14) (I16# x15) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X16WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) (I16# x8) (I16# x9) (I16# x10) (I16# x11) (I16# x12) (I16# x13) (I16# x14) (I16# x15)) = IO (\s0 -> case GHC.Exts.writeInt16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 14#) x14 s14 of s15 -> (# GHC.Exts.writeInt16OffAddr# addr (i +# 15#) x15 s15, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Int32 = MkInt32X16WithElems !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32
instance PackX16 X16 Int32 where
  mkX16 = MkInt32X16WithElems
  unpackX16 (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int32 where
  broadcast !x = MkInt32X16WithElems x x x x x x x x x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X16 Int32 where
  selectF (MkBoolX16 !cond) (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt32X16WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7) (if testBit cond 8 then x8 else y8) (if testBit cond 9 then x9 else y9) (if testBit cond 10 then x10 else y10) (if testBit cond 11 then x11 else y11) (if testBit cond 12 then x12 else y12) (if testBit cond 13 then x13 else y13) (if testBit cond 14 then x14 else y14) (if testBit cond 15 then x15 else y15)
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Int32 i0, Pick Int32 i1, Pick Int32 i2, Pick Int32 i3, Pick Int32 i4, Pick Int32 i5, Pick Int32 i6, Pick Int32 i7, Pick Int32 i8, Pick Int32 i9, Pick Int32 i10, Pick Int32 i11, Pick Int32 i12, Pick Int32 i13, Pick Int32 i14, Pick Int32 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int32 where
  unaryShuffle (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt32X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Int32 i0, Pick Int32 i1, Pick Int32 i2, Pick Int32 i3, Pick Int32 i4, Pick Int32 i5, Pick Int32 i6, Pick Int32 i7, Pick Int32 i8, Pick Int32 i9, Pick Int32 i10, Pick Int32 i11, Pick Int32 i12, Pick Int32 i13, Pick Int32 i14, Pick Int32 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int32 where
  binaryShuffle (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkInt32X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int32 where
  eqF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7) (x8 == y8) (x9 == y9) (x10 == y10) (x11 == y11) (x12 == y12) (x13 == y13) (x14 == y14) (x15 == y15)
  {-# INLINE eqF #-}
instance OrderedF X16 Int32 where
  ltF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7) (x8 < y8) (x9 < y9) (x10 < y10) (x11 < y11) (x12 < y12) (x13 < y13) (x14 < y14) (x15 < y15)
  leF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7) (x8 <= y8) (x9 <= y9) (x10 <= y10) (x11 <= y11) (x12 <= y12) (x13 <= y13) (x14 <= y14) (x15 <= y15)
  gtF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7) (x8 > y8) (x9 > y9) (x10 > y10) (x11 > y11) (x12 > y12) (x13 > y13) (x14 > y14) (x15 > y15)
  geF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7) (x8 >= y8) (x9 >= y9) (x10 >= y10) (x11 >= y11) (x12 >= y12) (x13 >= y13) (x14 >= y14) (x15 >= y15)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int32 where
  minF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt32X16WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7) (min x8 y8) (min x9 y9) (min x10 y10) (min x11 y11) (min x12 y12) (min x13 y13) (min x14 y14) (min x15 y15)
  maxF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt32X16WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7) (max x8 y8) (max x9 y9) (max x10 y10) (max x11 y11) (max x12 y12) (max x13 y13) (max x14 y14) (max x15 y15)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int32 where
  plusF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt32X16WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9) (x10 + y10) (x11 + y11) (x12 + y12) (x13 + y13) (x14 + y14) (x15 + y15)
  minusF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt32X16WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9) (x10 - y10) (x11 - y11) (x12 - y12) (x13 - y13) (x14 - y14) (x15 - y15)
  timesF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt32X16WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9) (x10 * y10) (x11 * y11) (x12 * y12) (x13 * y13) (x14 * y14) (x15 * y15)
  negateF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkInt32X16WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7) (- x8) (- x9) (- x10) (- x11) (- x12) (- x13) (- x14) (- x15)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X16 Int32 where
  andF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt32X16WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7) (x8 .&. y8) (x9 .&. y9) (x10 .&. y10) (x11 .&. y11) (x12 .&. y12) (x13 .&. y13) (x14 .&. y14) (x15 .&. y15)
  orF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt32X16WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7) (x8 .|. y8) (x9 .|. y9) (x10 .|. y10) (x11 .|. y11) (x12 .|. y12) (x13 .|. y13) (x14 .|. y14) (x15 .|. y15)
  xorF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt32X16WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7) (xor x8 y8) (xor x9 y9) (xor x10 y10) (xor x11 y11) (xor x12 y12) (xor x13 y13) (xor x14 y14) (xor x15 y15)
  complementF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkInt32X16WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7) (complement x8) (complement x9) (complement x10) (complement x11) (complement x12) (complement x13) (complement x14) (complement x15)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int32 where
  shiftLF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt32X16WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i) (shiftL x8 i) (shiftL x9 i) (shiftL x10 i) (shiftL x11 i) (shiftL x12 i) (shiftL x13 i) (shiftL x14 i) (shiftL x15 i)
  unsafeShiftLF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt32X16WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i) (unsafeShiftL x8 i) (unsafeShiftL x9 i) (unsafeShiftL x10 i) (unsafeShiftL x11 i) (unsafeShiftL x12 i) (unsafeShiftL x13 i) (unsafeShiftL x14 i) (unsafeShiftL x15 i)
  shiftRF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt32X16WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i) (shiftR x8 i) (shiftR x9 i) (shiftR x10 i) (shiftR x11 i) (shiftR x12 i) (shiftR x13 i) (shiftR x14 i) (shiftR x15 i)
  unsafeShiftRF (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt32X16WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i) (unsafeShiftR x8 i) (unsafeShiftR x9 i) (unsafeShiftR x10 i) (unsafeShiftR x11 i) (unsafeShiftR x12 i) (unsafeShiftR x13 i) (unsafeShiftR x14 i) (unsafeShiftR x15 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X16 Int32 where
  enumFromZero = MkInt32X16WithElems 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int32 where
  indexByteArraySIMD# ba i = MkInt32X16WithElems (I32# (GHC.Exts.indexInt32Array# ba i)) (I32# (GHC.Exts.indexInt32Array# ba (i +# 1#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 2#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 3#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 4#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 5#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 6#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 7#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 8#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 9#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 10#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 11#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 12#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 13#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 14#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 15#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt32Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt32Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt32Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt32Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt32Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt32Array# mba (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readInt32Array# mba (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readInt32Array# mba (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readInt32Array# mba (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readInt32Array# mba (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readInt32Array# mba (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readInt32Array# mba (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readInt32Array# mba (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readInt32Array# mba (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkInt32X16WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) (I32# x8) (I32# x9) (I32# x10) (I32# x11) (I32# x12) (I32# x13) (I32# x14) (I32# x15) #)
  writeByteArraySIMD# mba i (MkInt32X16WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) (I32# x8) (I32# x9) (I32# x10) (I32# x11) (I32# x12) (I32# x13) (I32# x14) (I32# x15)) s0 = case GHC.Exts.writeInt32Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt32Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt32Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt32Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt32Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt32Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt32Array# mba (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeInt32Array# mba (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeInt32Array# mba (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeInt32Array# mba (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeInt32Array# mba (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeInt32Array# mba (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeInt32Array# mba (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeInt32Array# mba (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeInt32Array# mba (i +# 14#) x14 s14 of s15 -> GHC.Exts.writeInt32Array# mba (i +# 15#) x15 s15
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkInt32X16WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) (I32# x8) (I32# x9) (I32# x10) (I32# x11) (I32# x12) (I32# x13) (I32# x14) (I32# x15) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X16WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) (I32# x8) (I32# x9) (I32# x10) (I32# x11) (I32# x12) (I32# x13) (I32# x14) (I32# x15)) = IO (\s0 -> case GHC.Exts.writeInt32OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 14#) x14 s14 of s15 -> (# GHC.Exts.writeInt32OffAddr# addr (i +# 15#) x15 s15, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Int64 = MkInt64X16WithElems !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64
instance PackX16 X16 Int64 where
  mkX16 = MkInt64X16WithElems
  unpackX16 (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Int64 where
  broadcast !x = MkInt64X16WithElems x x x x x x x x x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X16 Int64 where
  selectF (MkBoolX16 !cond) (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt64X16WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7) (if testBit cond 8 then x8 else y8) (if testBit cond 9 then x9 else y9) (if testBit cond 10 then x10 else y10) (if testBit cond 11 then x11 else y11) (if testBit cond 12 then x12 else y12) (if testBit cond 13 then x13 else y13) (if testBit cond 14 then x14 else y14) (if testBit cond 15 then x15 else y15)
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Int64 i0, Pick Int64 i1, Pick Int64 i2, Pick Int64 i3, Pick Int64 i4, Pick Int64 i5, Pick Int64 i6, Pick Int64 i7, Pick Int64 i8, Pick Int64 i9, Pick Int64 i10, Pick Int64 i11, Pick Int64 i12, Pick Int64 i13, Pick Int64 i14, Pick Int64 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int64 where
  unaryShuffle (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt64X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Int64 i0, Pick Int64 i1, Pick Int64 i2, Pick Int64 i3, Pick Int64 i4, Pick Int64 i5, Pick Int64 i6, Pick Int64 i7, Pick Int64 i8, Pick Int64 i9, Pick Int64 i10, Pick Int64 i11, Pick Int64 i12, Pick Int64 i13, Pick Int64 i14, Pick Int64 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Int64 where
  binaryShuffle (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkInt64X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Int64 where
  eqF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7) (x8 == y8) (x9 == y9) (x10 == y10) (x11 == y11) (x12 == y12) (x13 == y13) (x14 == y14) (x15 == y15)
  {-# INLINE eqF #-}
instance OrderedF X16 Int64 where
  ltF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7) (x8 < y8) (x9 < y9) (x10 < y10) (x11 < y11) (x12 < y12) (x13 < y13) (x14 < y14) (x15 < y15)
  leF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7) (x8 <= y8) (x9 <= y9) (x10 <= y10) (x11 <= y11) (x12 <= y12) (x13 <= y13) (x14 <= y14) (x15 <= y15)
  gtF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7) (x8 > y8) (x9 > y9) (x10 > y10) (x11 > y11) (x12 > y12) (x13 > y13) (x14 > y14) (x15 > y15)
  geF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7) (x8 >= y8) (x9 >= y9) (x10 >= y10) (x11 >= y11) (x12 >= y12) (x13 >= y13) (x14 >= y14) (x15 >= y15)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Int64 where
  minF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt64X16WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7) (min x8 y8) (min x9 y9) (min x10 y10) (min x11 y11) (min x12 y12) (min x13 y13) (min x14 y14) (min x15 y15)
  maxF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt64X16WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7) (max x8 y8) (max x9 y9) (max x10 y10) (max x11 y11) (max x12 y12) (max x13 y13) (max x14 y14) (max x15 y15)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Int64 where
  plusF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt64X16WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9) (x10 + y10) (x11 + y11) (x12 + y12) (x13 + y13) (x14 + y14) (x15 + y15)
  minusF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt64X16WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9) (x10 - y10) (x11 - y11) (x12 - y12) (x13 - y13) (x14 - y14) (x15 - y15)
  timesF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt64X16WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9) (x10 * y10) (x11 * y11) (x12 * y12) (x13 * y13) (x14 * y14) (x15 * y15)
  negateF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkInt64X16WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7) (- x8) (- x9) (- x10) (- x11) (- x12) (- x13) (- x14) (- x15)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X16 Int64 where
  andF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt64X16WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7) (x8 .&. y8) (x9 .&. y9) (x10 .&. y10) (x11 .&. y11) (x12 .&. y12) (x13 .&. y13) (x14 .&. y14) (x15 .&. y15)
  orF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt64X16WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7) (x8 .|. y8) (x9 .|. y9) (x10 .|. y10) (x11 .|. y11) (x12 .|. y12) (x13 .|. y13) (x14 .|. y14) (x15 .|. y15)
  xorF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkInt64X16WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7) (xor x8 y8) (xor x9 y9) (xor x10 y10) (xor x11 y11) (xor x12 y12) (xor x13 y13) (xor x14 y14) (xor x15 y15)
  complementF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkInt64X16WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7) (complement x8) (complement x9) (complement x10) (complement x11) (complement x12) (complement x13) (complement x14) (complement x15)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Int64 where
  shiftLF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt64X16WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i) (shiftL x8 i) (shiftL x9 i) (shiftL x10 i) (shiftL x11 i) (shiftL x12 i) (shiftL x13 i) (shiftL x14 i) (shiftL x15 i)
  unsafeShiftLF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt64X16WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i) (unsafeShiftL x8 i) (unsafeShiftL x9 i) (unsafeShiftL x10 i) (unsafeShiftL x11 i) (unsafeShiftL x12 i) (unsafeShiftL x13 i) (unsafeShiftL x14 i) (unsafeShiftL x15 i)
  shiftRF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt64X16WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i) (shiftR x8 i) (shiftR x9 i) (shiftR x10 i) (shiftR x11 i) (shiftR x12 i) (shiftR x13 i) (shiftR x14 i) (shiftR x15 i)
  unsafeShiftRF (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkInt64X16WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i) (unsafeShiftR x8 i) (unsafeShiftR x9 i) (unsafeShiftR x10 i) (unsafeShiftR x11 i) (unsafeShiftR x12 i) (unsafeShiftR x13 i) (unsafeShiftR x14 i) (unsafeShiftR x15 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X16 Int64 where
  enumFromZero = MkInt64X16WithElems 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Int64 where
  indexByteArraySIMD# ba i = MkInt64X16WithElems (I64# (GHC.Exts.indexInt64Array# ba i)) (I64# (GHC.Exts.indexInt64Array# ba (i +# 1#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 2#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 3#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 4#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 5#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 6#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 7#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 8#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 9#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 10#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 11#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 12#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 13#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 14#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 15#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt64Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt64Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt64Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt64Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt64Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt64Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt64Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt64Array# mba (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readInt64Array# mba (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readInt64Array# mba (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readInt64Array# mba (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readInt64Array# mba (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readInt64Array# mba (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readInt64Array# mba (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readInt64Array# mba (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readInt64Array# mba (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkInt64X16WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) (I64# x8) (I64# x9) (I64# x10) (I64# x11) (I64# x12) (I64# x13) (I64# x14) (I64# x15) #)
  writeByteArraySIMD# mba i (MkInt64X16WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) (I64# x8) (I64# x9) (I64# x10) (I64# x11) (I64# x12) (I64# x13) (I64# x14) (I64# x15)) s0 = case GHC.Exts.writeInt64Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt64Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt64Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt64Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt64Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt64Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt64Array# mba (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeInt64Array# mba (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeInt64Array# mba (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeInt64Array# mba (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeInt64Array# mba (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeInt64Array# mba (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeInt64Array# mba (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeInt64Array# mba (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeInt64Array# mba (i +# 14#) x14 s14 of s15 -> GHC.Exts.writeInt64Array# mba (i +# 15#) x15 s15
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt64OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkInt64X16WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) (I64# x8) (I64# x9) (I64# x10) (I64# x11) (I64# x12) (I64# x13) (I64# x14) (I64# x15) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X16WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) (I64# x8) (I64# x9) (I64# x10) (I64# x11) (I64# x12) (I64# x13) (I64# x14) (I64# x15)) = IO (\s0 -> case GHC.Exts.writeInt64OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 14#) x14 s14 of s15 -> (# GHC.Exts.writeInt64OffAddr# addr (i +# 15#) x15 s15, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Word8 = MkWord8X16WithElems !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
instance PackX16 X16 Word8 where
  mkX16 = MkWord8X16WithElems
  unpackX16 (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word8 where
  broadcast !x = MkWord8X16WithElems x x x x x x x x x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X16 Word8 where
  selectF (MkBoolX16 !cond) (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord8X16WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7) (if testBit cond 8 then x8 else y8) (if testBit cond 9 then x9 else y9) (if testBit cond 10 then x10 else y10) (if testBit cond 11 then x11 else y11) (if testBit cond 12 then x12 else y12) (if testBit cond 13 then x13 else y13) (if testBit cond 14 then x14 else y14) (if testBit cond 15 then x15 else y15)
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Word8 i0, Pick Word8 i1, Pick Word8 i2, Pick Word8 i3, Pick Word8 i4, Pick Word8 i5, Pick Word8 i6, Pick Word8 i7, Pick Word8 i8, Pick Word8 i9, Pick Word8 i10, Pick Word8 i11, Pick Word8 i12, Pick Word8 i13, Pick Word8 i14, Pick Word8 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word8 where
  unaryShuffle (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord8X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Word8 i0, Pick Word8 i1, Pick Word8 i2, Pick Word8 i3, Pick Word8 i4, Pick Word8 i5, Pick Word8 i6, Pick Word8 i7, Pick Word8 i8, Pick Word8 i9, Pick Word8 i10, Pick Word8 i11, Pick Word8 i12, Pick Word8 i13, Pick Word8 i14, Pick Word8 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word8 where
  binaryShuffle (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkWord8X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word8 where
  eqF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7) (x8 == y8) (x9 == y9) (x10 == y10) (x11 == y11) (x12 == y12) (x13 == y13) (x14 == y14) (x15 == y15)
  {-# INLINE eqF #-}
instance OrderedF X16 Word8 where
  ltF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7) (x8 < y8) (x9 < y9) (x10 < y10) (x11 < y11) (x12 < y12) (x13 < y13) (x14 < y14) (x15 < y15)
  leF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7) (x8 <= y8) (x9 <= y9) (x10 <= y10) (x11 <= y11) (x12 <= y12) (x13 <= y13) (x14 <= y14) (x15 <= y15)
  gtF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7) (x8 > y8) (x9 > y9) (x10 > y10) (x11 > y11) (x12 > y12) (x13 > y13) (x14 > y14) (x15 > y15)
  geF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7) (x8 >= y8) (x9 >= y9) (x10 >= y10) (x11 >= y11) (x12 >= y12) (x13 >= y13) (x14 >= y14) (x15 >= y15)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word8 where
  minF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord8X16WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7) (min x8 y8) (min x9 y9) (min x10 y10) (min x11 y11) (min x12 y12) (min x13 y13) (min x14 y14) (min x15 y15)
  maxF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord8X16WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7) (max x8 y8) (max x9 y9) (max x10 y10) (max x11 y11) (max x12 y12) (max x13 y13) (max x14 y14) (max x15 y15)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word8 where
  plusF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord8X16WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9) (x10 + y10) (x11 + y11) (x12 + y12) (x13 + y13) (x14 + y14) (x15 + y15)
  minusF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord8X16WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9) (x10 - y10) (x11 - y11) (x12 - y12) (x13 - y13) (x14 - y14) (x15 - y15)
  timesF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord8X16WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9) (x10 * y10) (x11 * y11) (x12 * y12) (x13 * y13) (x14 * y14) (x15 * y15)
  negateF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkWord8X16WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7) (- x8) (- x9) (- x10) (- x11) (- x12) (- x13) (- x14) (- x15)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X16 Word8 where
  andF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord8X16WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7) (x8 .&. y8) (x9 .&. y9) (x10 .&. y10) (x11 .&. y11) (x12 .&. y12) (x13 .&. y13) (x14 .&. y14) (x15 .&. y15)
  orF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord8X16WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7) (x8 .|. y8) (x9 .|. y9) (x10 .|. y10) (x11 .|. y11) (x12 .|. y12) (x13 .|. y13) (x14 .|. y14) (x15 .|. y15)
  xorF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord8X16WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7) (xor x8 y8) (xor x9 y9) (xor x10 y10) (xor x11 y11) (xor x12 y12) (xor x13 y13) (xor x14 y14) (xor x15 y15)
  complementF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkWord8X16WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7) (complement x8) (complement x9) (complement x10) (complement x11) (complement x12) (complement x13) (complement x14) (complement x15)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word8 where
  shiftLF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord8X16WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i) (shiftL x8 i) (shiftL x9 i) (shiftL x10 i) (shiftL x11 i) (shiftL x12 i) (shiftL x13 i) (shiftL x14 i) (shiftL x15 i)
  unsafeShiftLF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord8X16WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i) (unsafeShiftL x8 i) (unsafeShiftL x9 i) (unsafeShiftL x10 i) (unsafeShiftL x11 i) (unsafeShiftL x12 i) (unsafeShiftL x13 i) (unsafeShiftL x14 i) (unsafeShiftL x15 i)
  shiftRF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord8X16WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i) (shiftR x8 i) (shiftR x9 i) (shiftR x10 i) (shiftR x11 i) (shiftR x12 i) (shiftR x13 i) (shiftR x14 i) (shiftR x15 i)
  unsafeShiftRF (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord8X16WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i) (unsafeShiftR x8 i) (unsafeShiftR x9 i) (unsafeShiftR x10 i) (unsafeShiftR x11 i) (unsafeShiftR x12 i) (unsafeShiftR x13 i) (unsafeShiftR x14 i) (unsafeShiftR x15 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X16 Word8 where
  enumFromZero = MkWord8X16WithElems 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word8 where
  indexByteArraySIMD# ba i = MkWord8X16WithElems (W8# (GHC.Exts.indexWord8Array# ba i)) (W8# (GHC.Exts.indexWord8Array# ba (i +# 1#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 2#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 3#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 4#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 5#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 6#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 7#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 8#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 9#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 10#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 11#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 12#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 13#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 14#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 15#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord8Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord8Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord8Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord8Array# mba (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readWord8Array# mba (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readWord8Array# mba (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readWord8Array# mba (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readWord8Array# mba (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readWord8Array# mba (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readWord8Array# mba (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readWord8Array# mba (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readWord8Array# mba (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkWord8X16WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) (W8# x8) (W8# x9) (W8# x10) (W8# x11) (W8# x12) (W8# x13) (W8# x14) (W8# x15) #)
  writeByteArraySIMD# mba i (MkWord8X16WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) (W8# x8) (W8# x9) (W8# x10) (W8# x11) (W8# x12) (W8# x13) (W8# x14) (W8# x15)) s0 = case GHC.Exts.writeWord8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord8Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord8Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord8Array# mba (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeWord8Array# mba (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeWord8Array# mba (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeWord8Array# mba (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeWord8Array# mba (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeWord8Array# mba (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeWord8Array# mba (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeWord8Array# mba (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeWord8Array# mba (i +# 14#) x14 s14 of s15 -> GHC.Exts.writeWord8Array# mba (i +# 15#) x15 s15
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkWord8X16WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) (W8# x8) (W8# x9) (W8# x10) (W8# x11) (W8# x12) (W8# x13) (W8# x14) (W8# x15) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X16WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) (W8# x8) (W8# x9) (W8# x10) (W8# x11) (W8# x12) (W8# x13) (W8# x14) (W8# x15)) = IO (\s0 -> case GHC.Exts.writeWord8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 14#) x14 s14 of s15 -> (# GHC.Exts.writeWord8OffAddr# addr (i +# 15#) x15 s15, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Word16 = MkWord16X16WithElems !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16
instance PackX16 X16 Word16 where
  mkX16 = MkWord16X16WithElems
  unpackX16 (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word16 where
  broadcast !x = MkWord16X16WithElems x x x x x x x x x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X16 Word16 where
  selectF (MkBoolX16 !cond) (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord16X16WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7) (if testBit cond 8 then x8 else y8) (if testBit cond 9 then x9 else y9) (if testBit cond 10 then x10 else y10) (if testBit cond 11 then x11 else y11) (if testBit cond 12 then x12 else y12) (if testBit cond 13 then x13 else y13) (if testBit cond 14 then x14 else y14) (if testBit cond 15 then x15 else y15)
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Word16 i0, Pick Word16 i1, Pick Word16 i2, Pick Word16 i3, Pick Word16 i4, Pick Word16 i5, Pick Word16 i6, Pick Word16 i7, Pick Word16 i8, Pick Word16 i9, Pick Word16 i10, Pick Word16 i11, Pick Word16 i12, Pick Word16 i13, Pick Word16 i14, Pick Word16 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word16 where
  unaryShuffle (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord16X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Word16 i0, Pick Word16 i1, Pick Word16 i2, Pick Word16 i3, Pick Word16 i4, Pick Word16 i5, Pick Word16 i6, Pick Word16 i7, Pick Word16 i8, Pick Word16 i9, Pick Word16 i10, Pick Word16 i11, Pick Word16 i12, Pick Word16 i13, Pick Word16 i14, Pick Word16 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word16 where
  binaryShuffle (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkWord16X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word16 where
  eqF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7) (x8 == y8) (x9 == y9) (x10 == y10) (x11 == y11) (x12 == y12) (x13 == y13) (x14 == y14) (x15 == y15)
  {-# INLINE eqF #-}
instance OrderedF X16 Word16 where
  ltF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7) (x8 < y8) (x9 < y9) (x10 < y10) (x11 < y11) (x12 < y12) (x13 < y13) (x14 < y14) (x15 < y15)
  leF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7) (x8 <= y8) (x9 <= y9) (x10 <= y10) (x11 <= y11) (x12 <= y12) (x13 <= y13) (x14 <= y14) (x15 <= y15)
  gtF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7) (x8 > y8) (x9 > y9) (x10 > y10) (x11 > y11) (x12 > y12) (x13 > y13) (x14 > y14) (x15 > y15)
  geF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7) (x8 >= y8) (x9 >= y9) (x10 >= y10) (x11 >= y11) (x12 >= y12) (x13 >= y13) (x14 >= y14) (x15 >= y15)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word16 where
  minF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord16X16WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7) (min x8 y8) (min x9 y9) (min x10 y10) (min x11 y11) (min x12 y12) (min x13 y13) (min x14 y14) (min x15 y15)
  maxF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord16X16WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7) (max x8 y8) (max x9 y9) (max x10 y10) (max x11 y11) (max x12 y12) (max x13 y13) (max x14 y14) (max x15 y15)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word16 where
  plusF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord16X16WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9) (x10 + y10) (x11 + y11) (x12 + y12) (x13 + y13) (x14 + y14) (x15 + y15)
  minusF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord16X16WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9) (x10 - y10) (x11 - y11) (x12 - y12) (x13 - y13) (x14 - y14) (x15 - y15)
  timesF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord16X16WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9) (x10 * y10) (x11 * y11) (x12 * y12) (x13 * y13) (x14 * y14) (x15 * y15)
  negateF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkWord16X16WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7) (- x8) (- x9) (- x10) (- x11) (- x12) (- x13) (- x14) (- x15)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X16 Word16 where
  andF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord16X16WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7) (x8 .&. y8) (x9 .&. y9) (x10 .&. y10) (x11 .&. y11) (x12 .&. y12) (x13 .&. y13) (x14 .&. y14) (x15 .&. y15)
  orF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord16X16WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7) (x8 .|. y8) (x9 .|. y9) (x10 .|. y10) (x11 .|. y11) (x12 .|. y12) (x13 .|. y13) (x14 .|. y14) (x15 .|. y15)
  xorF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord16X16WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7) (xor x8 y8) (xor x9 y9) (xor x10 y10) (xor x11 y11) (xor x12 y12) (xor x13 y13) (xor x14 y14) (xor x15 y15)
  complementF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkWord16X16WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7) (complement x8) (complement x9) (complement x10) (complement x11) (complement x12) (complement x13) (complement x14) (complement x15)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word16 where
  shiftLF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord16X16WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i) (shiftL x8 i) (shiftL x9 i) (shiftL x10 i) (shiftL x11 i) (shiftL x12 i) (shiftL x13 i) (shiftL x14 i) (shiftL x15 i)
  unsafeShiftLF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord16X16WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i) (unsafeShiftL x8 i) (unsafeShiftL x9 i) (unsafeShiftL x10 i) (unsafeShiftL x11 i) (unsafeShiftL x12 i) (unsafeShiftL x13 i) (unsafeShiftL x14 i) (unsafeShiftL x15 i)
  shiftRF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord16X16WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i) (shiftR x8 i) (shiftR x9 i) (shiftR x10 i) (shiftR x11 i) (shiftR x12 i) (shiftR x13 i) (shiftR x14 i) (shiftR x15 i)
  unsafeShiftRF (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord16X16WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i) (unsafeShiftR x8 i) (unsafeShiftR x9 i) (unsafeShiftR x10 i) (unsafeShiftR x11 i) (unsafeShiftR x12 i) (unsafeShiftR x13 i) (unsafeShiftR x14 i) (unsafeShiftR x15 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X16 Word16 where
  enumFromZero = MkWord16X16WithElems 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word16 where
  indexByteArraySIMD# ba i = MkWord16X16WithElems (W16# (GHC.Exts.indexWord16Array# ba i)) (W16# (GHC.Exts.indexWord16Array# ba (i +# 1#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 2#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 3#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 4#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 5#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 6#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 7#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 8#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 9#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 10#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 11#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 12#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 13#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 14#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 15#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord16Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord16Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord16Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord16Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord16Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord16Array# mba (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readWord16Array# mba (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readWord16Array# mba (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readWord16Array# mba (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readWord16Array# mba (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readWord16Array# mba (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readWord16Array# mba (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readWord16Array# mba (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readWord16Array# mba (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkWord16X16WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) (W16# x8) (W16# x9) (W16# x10) (W16# x11) (W16# x12) (W16# x13) (W16# x14) (W16# x15) #)
  writeByteArraySIMD# mba i (MkWord16X16WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) (W16# x8) (W16# x9) (W16# x10) (W16# x11) (W16# x12) (W16# x13) (W16# x14) (W16# x15)) s0 = case GHC.Exts.writeWord16Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord16Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord16Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord16Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord16Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord16Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord16Array# mba (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeWord16Array# mba (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeWord16Array# mba (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeWord16Array# mba (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeWord16Array# mba (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeWord16Array# mba (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeWord16Array# mba (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeWord16Array# mba (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeWord16Array# mba (i +# 14#) x14 s14 of s15 -> GHC.Exts.writeWord16Array# mba (i +# 15#) x15 s15
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkWord16X16WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) (W16# x8) (W16# x9) (W16# x10) (W16# x11) (W16# x12) (W16# x13) (W16# x14) (W16# x15) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X16WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) (W16# x8) (W16# x9) (W16# x10) (W16# x11) (W16# x12) (W16# x13) (W16# x14) (W16# x15)) = IO (\s0 -> case GHC.Exts.writeWord16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 14#) x14 s14 of s15 -> (# GHC.Exts.writeWord16OffAddr# addr (i +# 15#) x15 s15, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Word32 = MkWord32X16WithElems !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32
instance PackX16 X16 Word32 where
  mkX16 = MkWord32X16WithElems
  unpackX16 (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word32 where
  broadcast !x = MkWord32X16WithElems x x x x x x x x x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X16 Word32 where
  selectF (MkBoolX16 !cond) (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord32X16WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7) (if testBit cond 8 then x8 else y8) (if testBit cond 9 then x9 else y9) (if testBit cond 10 then x10 else y10) (if testBit cond 11 then x11 else y11) (if testBit cond 12 then x12 else y12) (if testBit cond 13 then x13 else y13) (if testBit cond 14 then x14 else y14) (if testBit cond 15 then x15 else y15)
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Word32 i0, Pick Word32 i1, Pick Word32 i2, Pick Word32 i3, Pick Word32 i4, Pick Word32 i5, Pick Word32 i6, Pick Word32 i7, Pick Word32 i8, Pick Word32 i9, Pick Word32 i10, Pick Word32 i11, Pick Word32 i12, Pick Word32 i13, Pick Word32 i14, Pick Word32 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word32 where
  unaryShuffle (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord32X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Word32 i0, Pick Word32 i1, Pick Word32 i2, Pick Word32 i3, Pick Word32 i4, Pick Word32 i5, Pick Word32 i6, Pick Word32 i7, Pick Word32 i8, Pick Word32 i9, Pick Word32 i10, Pick Word32 i11, Pick Word32 i12, Pick Word32 i13, Pick Word32 i14, Pick Word32 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word32 where
  binaryShuffle (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkWord32X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word32 where
  eqF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7) (x8 == y8) (x9 == y9) (x10 == y10) (x11 == y11) (x12 == y12) (x13 == y13) (x14 == y14) (x15 == y15)
  {-# INLINE eqF #-}
instance OrderedF X16 Word32 where
  ltF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7) (x8 < y8) (x9 < y9) (x10 < y10) (x11 < y11) (x12 < y12) (x13 < y13) (x14 < y14) (x15 < y15)
  leF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7) (x8 <= y8) (x9 <= y9) (x10 <= y10) (x11 <= y11) (x12 <= y12) (x13 <= y13) (x14 <= y14) (x15 <= y15)
  gtF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7) (x8 > y8) (x9 > y9) (x10 > y10) (x11 > y11) (x12 > y12) (x13 > y13) (x14 > y14) (x15 > y15)
  geF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7) (x8 >= y8) (x9 >= y9) (x10 >= y10) (x11 >= y11) (x12 >= y12) (x13 >= y13) (x14 >= y14) (x15 >= y15)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word32 where
  minF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord32X16WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7) (min x8 y8) (min x9 y9) (min x10 y10) (min x11 y11) (min x12 y12) (min x13 y13) (min x14 y14) (min x15 y15)
  maxF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord32X16WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7) (max x8 y8) (max x9 y9) (max x10 y10) (max x11 y11) (max x12 y12) (max x13 y13) (max x14 y14) (max x15 y15)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word32 where
  plusF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord32X16WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9) (x10 + y10) (x11 + y11) (x12 + y12) (x13 + y13) (x14 + y14) (x15 + y15)
  minusF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord32X16WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9) (x10 - y10) (x11 - y11) (x12 - y12) (x13 - y13) (x14 - y14) (x15 - y15)
  timesF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord32X16WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9) (x10 * y10) (x11 * y11) (x12 * y12) (x13 * y13) (x14 * y14) (x15 * y15)
  negateF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkWord32X16WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7) (- x8) (- x9) (- x10) (- x11) (- x12) (- x13) (- x14) (- x15)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X16 Word32 where
  andF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord32X16WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7) (x8 .&. y8) (x9 .&. y9) (x10 .&. y10) (x11 .&. y11) (x12 .&. y12) (x13 .&. y13) (x14 .&. y14) (x15 .&. y15)
  orF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord32X16WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7) (x8 .|. y8) (x9 .|. y9) (x10 .|. y10) (x11 .|. y11) (x12 .|. y12) (x13 .|. y13) (x14 .|. y14) (x15 .|. y15)
  xorF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord32X16WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7) (xor x8 y8) (xor x9 y9) (xor x10 y10) (xor x11 y11) (xor x12 y12) (xor x13 y13) (xor x14 y14) (xor x15 y15)
  complementF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkWord32X16WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7) (complement x8) (complement x9) (complement x10) (complement x11) (complement x12) (complement x13) (complement x14) (complement x15)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word32 where
  shiftLF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord32X16WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i) (shiftL x8 i) (shiftL x9 i) (shiftL x10 i) (shiftL x11 i) (shiftL x12 i) (shiftL x13 i) (shiftL x14 i) (shiftL x15 i)
  unsafeShiftLF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord32X16WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i) (unsafeShiftL x8 i) (unsafeShiftL x9 i) (unsafeShiftL x10 i) (unsafeShiftL x11 i) (unsafeShiftL x12 i) (unsafeShiftL x13 i) (unsafeShiftL x14 i) (unsafeShiftL x15 i)
  shiftRF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord32X16WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i) (shiftR x8 i) (shiftR x9 i) (shiftR x10 i) (shiftR x11 i) (shiftR x12 i) (shiftR x13 i) (shiftR x14 i) (shiftR x15 i)
  unsafeShiftRF (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord32X16WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i) (unsafeShiftR x8 i) (unsafeShiftR x9 i) (unsafeShiftR x10 i) (unsafeShiftR x11 i) (unsafeShiftR x12 i) (unsafeShiftR x13 i) (unsafeShiftR x14 i) (unsafeShiftR x15 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X16 Word32 where
  enumFromZero = MkWord32X16WithElems 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word32 where
  indexByteArraySIMD# ba i = MkWord32X16WithElems (W32# (GHC.Exts.indexWord32Array# ba i)) (W32# (GHC.Exts.indexWord32Array# ba (i +# 1#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 2#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 3#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 4#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 5#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 6#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 7#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 8#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 9#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 10#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 11#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 12#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 13#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 14#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 15#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord32Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord32Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord32Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord32Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord32Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord32Array# mba (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readWord32Array# mba (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readWord32Array# mba (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readWord32Array# mba (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readWord32Array# mba (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readWord32Array# mba (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readWord32Array# mba (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readWord32Array# mba (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readWord32Array# mba (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkWord32X16WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) (W32# x8) (W32# x9) (W32# x10) (W32# x11) (W32# x12) (W32# x13) (W32# x14) (W32# x15) #)
  writeByteArraySIMD# mba i (MkWord32X16WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) (W32# x8) (W32# x9) (W32# x10) (W32# x11) (W32# x12) (W32# x13) (W32# x14) (W32# x15)) s0 = case GHC.Exts.writeWord32Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord32Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord32Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord32Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord32Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord32Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord32Array# mba (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeWord32Array# mba (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeWord32Array# mba (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeWord32Array# mba (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeWord32Array# mba (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeWord32Array# mba (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeWord32Array# mba (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeWord32Array# mba (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeWord32Array# mba (i +# 14#) x14 s14 of s15 -> GHC.Exts.writeWord32Array# mba (i +# 15#) x15 s15
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkWord32X16WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) (W32# x8) (W32# x9) (W32# x10) (W32# x11) (W32# x12) (W32# x13) (W32# x14) (W32# x15) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X16WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) (W32# x8) (W32# x9) (W32# x10) (W32# x11) (W32# x12) (W32# x13) (W32# x14) (W32# x15)) = IO (\s0 -> case GHC.Exts.writeWord32OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 14#) x14 s14 of s15 -> (# GHC.Exts.writeWord32OffAddr# addr (i +# 15#) x15 s15, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X16 Word64 = MkWord64X16WithElems !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64
instance PackX16 X16 Word64 where
  mkX16 = MkWord64X16WithElems
  unpackX16 (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance Broadcast X16 Word64 where
  broadcast !x = MkWord64X16WithElems x x x x x x x x x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X16 Word64 where
  selectF (MkBoolX16 !cond) (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord64X16WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7) (if testBit cond 8 then x8 else y8) (if testBit cond 9 then x9 else y9) (if testBit cond 10 then x10 else y10) (if testBit cond 11 then x11 else y11) (if testBit cond 12 then x12 else y12) (if testBit cond 13 then x13 else y13) (if testBit cond 14 then x14 else y14) (if testBit cond 15 then x15 else y15)
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16, Pick Word64 i0, Pick Word64 i1, Pick Word64 i2, Pick Word64 i3, Pick Word64 i4, Pick Word64 i5, Pick Word64 i6, Pick Word64 i7, Pick Word64 i8, Pick Word64 i9, Pick Word64 i10, Pick Word64 i11, Pick Word64 i12, Pick Word64 i13, Pick Word64 i14, Pick Word64 i15) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word64 where
  unaryShuffle (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord64X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32, Pick Word64 i0, Pick Word64 i1, Pick Word64 i2, Pick Word64 i3, Pick Word64 i4, Pick Word64 i5, Pick Word64 i6, Pick Word64 i7, Pick Word64 i8, Pick Word64 i9, Pick Word64 i10, Pick Word64 i11, Pick Word64 i12, Pick Word64 i13, Pick Word64 i14, Pick Word64 i15) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 Word64 where
  binaryShuffle (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; 15 -> x15; 16 -> x16; 17 -> x17; 18 -> x18; 19 -> x19; 20 -> x20; 21 -> x21; 22 -> x22; 23 -> x23; 24 -> x24; 25 -> x25; 26 -> x26; 27 -> x27; 28 -> x28; 29 -> x29; 30 -> x30; _ -> x31 } } in MkWord64X16WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources) (pick @_ @i8 sources) (pick @_ @i9 sources) (pick @_ @i10 sources) (pick @_ @i11 sources) (pick @_ @i12 sources) (pick @_ @i13 sources) (pick @_ @i14 sources) (pick @_ @i15 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X16 Word64 where
  eqF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7) (x8 == y8) (x9 == y9) (x10 == y10) (x11 == y11) (x12 == y12) (x13 == y13) (x14 == y14) (x15 == y15)
  {-# INLINE eqF #-}
instance OrderedF X16 Word64 where
  ltF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7) (x8 < y8) (x9 < y9) (x10 < y10) (x11 < y11) (x12 < y12) (x13 < y13) (x14 < y14) (x15 < y15)
  leF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7) (x8 <= y8) (x9 <= y9) (x10 <= y10) (x11 <= y11) (x12 <= y12) (x13 <= y13) (x14 <= y14) (x15 <= y15)
  gtF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7) (x8 > y8) (x9 > y9) (x10 > y10) (x11 > y11) (x12 > y12) (x13 > y13) (x14 > y14) (x15 > y15)
  geF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = mkX16 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7) (x8 >= y8) (x9 >= y9) (x10 >= y10) (x11 >= y11) (x12 >= y12) (x13 >= y13) (x14 >= y14) (x15 >= y15)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X16 Word64 where
  minF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord64X16WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7) (min x8 y8) (min x9 y9) (min x10 y10) (min x11 y11) (min x12 y12) (min x13 y13) (min x14 y14) (min x15 y15)
  maxF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord64X16WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7) (max x8 y8) (max x9 y9) (max x10 y10) (max x11 y11) (max x12 y12) (max x13 y13) (max x14 y14) (max x15 y15)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X16 Word64 where
  plusF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord64X16WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7) (x8 + y8) (x9 + y9) (x10 + y10) (x11 + y11) (x12 + y12) (x13 + y13) (x14 + y14) (x15 + y15)
  minusF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord64X16WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7) (x8 - y8) (x9 - y9) (x10 - y10) (x11 - y11) (x12 - y12) (x13 - y13) (x14 - y14) (x15 - y15)
  timesF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord64X16WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7) (x8 * y8) (x9 * y9) (x10 * y10) (x11 * y11) (x12 * y12) (x13 * y13) (x14 * y14) (x15 * y15)
  negateF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkWord64X16WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7) (- x8) (- x9) (- x10) (- x11) (- x12) (- x13) (- x14) (- x15)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X16 Word64 where
  andF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord64X16WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7) (x8 .&. y8) (x9 .&. y9) (x10 .&. y10) (x11 .&. y11) (x12 .&. y12) (x13 .&. y13) (x14 .&. y14) (x15 .&. y15)
  orF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord64X16WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7) (x8 .|. y8) (x9 .|. y9) (x10 .|. y10) (x11 .|. y11) (x12 .|. y12) (x13 .|. y13) (x14 .|. y14) (x15 .|. y15)
  xorF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) = MkWord64X16WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7) (xor x8 y8) (xor x9 y9) (xor x10 y10) (xor x11 y11) (xor x12 y12) (xor x13 y13) (xor x14 y14) (xor x15 y15)
  complementF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = MkWord64X16WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7) (complement x8) (complement x9) (complement x10) (complement x11) (complement x12) (complement x13) (complement x14) (complement x15)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X16 Word64 where
  shiftLF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord64X16WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i) (shiftL x8 i) (shiftL x9 i) (shiftL x10 i) (shiftL x11 i) (shiftL x12 i) (shiftL x13 i) (shiftL x14 i) (shiftL x15 i)
  unsafeShiftLF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord64X16WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i) (unsafeShiftL x8 i) (unsafeShiftL x9 i) (unsafeShiftL x10 i) (unsafeShiftL x11 i) (unsafeShiftL x12 i) (unsafeShiftL x13 i) (unsafeShiftL x14 i) (unsafeShiftL x15 i)
  shiftRF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord64X16WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i) (shiftR x8 i) (shiftR x9 i) (shiftR x10 i) (shiftR x11 i) (shiftR x12 i) (shiftR x13 i) (shiftR x14 i) (shiftR x15 i)
  unsafeShiftRF (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) !i = MkWord64X16WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i) (unsafeShiftR x8 i) (unsafeShiftR x9 i) (unsafeShiftR x10 i) (unsafeShiftR x11 i) (unsafeShiftR x12 i) (unsafeShiftR x13 i) (unsafeShiftR x14 i) (unsafeShiftR x15 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X16 Word64 where
  enumFromZero = MkWord64X16WithElems 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X16 Word64 where
  indexByteArraySIMD# ba i = MkWord64X16WithElems (W64# (GHC.Exts.indexWord64Array# ba i)) (W64# (GHC.Exts.indexWord64Array# ba (i +# 1#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 2#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 3#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 4#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 5#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 6#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 7#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 8#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 9#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 10#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 11#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 12#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 13#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 14#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 15#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord64Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord64Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord64Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord64Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord64Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord64Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord64Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord64Array# mba (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readWord64Array# mba (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readWord64Array# mba (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readWord64Array# mba (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readWord64Array# mba (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readWord64Array# mba (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readWord64Array# mba (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readWord64Array# mba (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readWord64Array# mba (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkWord64X16WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) (W64# x8) (W64# x9) (W64# x10) (W64# x11) (W64# x12) (W64# x13) (W64# x14) (W64# x15) #)
  writeByteArraySIMD# mba i (MkWord64X16WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) (W64# x8) (W64# x9) (W64# x10) (W64# x11) (W64# x12) (W64# x13) (W64# x14) (W64# x15)) s0 = case GHC.Exts.writeWord64Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord64Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord64Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord64Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord64Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord64Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord64Array# mba (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeWord64Array# mba (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeWord64Array# mba (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeWord64Array# mba (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeWord64Array# mba (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeWord64Array# mba (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeWord64Array# mba (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeWord64Array# mba (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeWord64Array# mba (i +# 14#) x14 s14 of s15 -> GHC.Exts.writeWord64Array# mba (i +# 15#) x15 s15
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X16 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord64OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 8#) s8 of (# s9, x8 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 9#) s9 of (# s10, x9 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 10#) s10 of (# s11, x10 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 11#) s11 of (# s12, x11 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 12#) s12 of (# s13, x12 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 13#) s13 of (# s14, x13 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 14#) s14 of (# s15, x14 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 15#) s15 of (# s16, x15 #) -> (# s16, MkWord64X16WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) (W64# x8) (W64# x9) (W64# x10) (W64# x11) (W64# x12) (W64# x13) (W64# x14) (W64# x15) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X16WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) (W64# x8) (W64# x9) (W64# x10) (W64# x11) (W64# x12) (W64# x13) (W64# x14) (W64# x15)) = IO (\s0 -> case GHC.Exts.writeWord64OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 7#) x7 s7 of s8 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 8#) x8 s8 of s9 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 9#) x9 s9 of s10 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 10#) x10 s10 of s11 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 11#) x11 s11 of s12 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 12#) x12 s12 of s13 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 13#) x13 s13 of s14 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 14#) x14 s14 of s15 -> (# GHC.Exts.writeWord64OffAddr# addr (i +# 15#) x15 s15, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
#endif
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
instance UnaryShuffleT indices X16 a => UnaryShuffleT indices X16 (Sum a) where
  unaryShuffle = coerce (unaryShuffle @indices @X16 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 a => BinaryShuffleT indices X16 (Sum a) where
  binaryShuffle = coerce (binaryShuffle @indices @X16 @a)
  {-# INLINE binaryShuffle #-}
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
instance UnaryShuffleT indices X16 a => UnaryShuffleT indices X16 (Product a) where
  unaryShuffle = coerce (unaryShuffle @indices @X16 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 a => BinaryShuffleT indices X16 (Product a) where
  binaryShuffle = coerce (binaryShuffle @indices @X16 @a)
  {-# INLINE binaryShuffle #-}
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
instance UnaryShuffleT indices X16 a => UnaryShuffleT indices X16 (Min a) where
  unaryShuffle = coerce (unaryShuffle @indices @X16 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 a => BinaryShuffleT indices X16 (Min a) where
  binaryShuffle = coerce (binaryShuffle @indices @X16 @a)
  {-# INLINE binaryShuffle #-}
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
instance UnaryShuffleT indices X16 a => UnaryShuffleT indices X16 (Max a) where
  unaryShuffle = coerce (unaryShuffle @indices @X16 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 a => BinaryShuffleT indices X16 (Max a) where
  binaryShuffle = coerce (binaryShuffle @indices @X16 @a)
  {-# INLINE binaryShuffle #-}
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
instance UnaryShuffleT indices X16 a => UnaryShuffleT indices X16 (Complex a) where
  unaryShuffle (MkComplexX16 x y) = MkComplexX16 (unaryShuffle @indices x) (unaryShuffle @indices y)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X16 a => BinaryShuffleT indices X16 (Complex a) where
  binaryShuffle (MkComplexX16 x y) (MkComplexX16 u v) = MkComplexX16 (binaryShuffle @indices x u) (binaryShuffle @indices y v)
  {-# INLINE binaryShuffle #-}
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
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, i8 < 16, i9 < 16, i10 < 16, i11 < 16, i12 < 16, i13 < 16, i14 < 16, i15 < 16) => UnaryShuffleT '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 () where
  unaryShuffle _ = MkUnitX16
  {-# INLINE unaryShuffle #-}
instance (i0 < 32, i1 < 32, i2 < 32, i3 < 32, i4 < 32, i5 < 32, i6 < 32, i7 < 32, i8 < 32, i9 < 32, i10 < 32, i11 < 32, i12 < 32, i13 < 32, i14 < 32, i15 < 32) => BinaryShuffleT '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 () where
  binaryShuffle _ _ = MkUnitX16
  {-# INLINE binaryShuffle #-}
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
instance (UnaryShuffleT indices X16 a0, UnaryShuffleT indices X16 a1) => UnaryShuffleT indices X16 (a0, a1) where
  unaryShuffle (MkTuple2X16 x0 x1) = MkTuple2X16 (unaryShuffle @indices x0) (unaryShuffle @indices x1)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X16 a0, BinaryShuffleT indices X16 a1) => BinaryShuffleT indices X16 (a0, a1) where
  binaryShuffle (MkTuple2X16 x0 x1) (MkTuple2X16 y0 y1) = MkTuple2X16 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1)
  {-# INLINE binaryShuffle #-}
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
instance (UnaryShuffleT indices X16 a0, UnaryShuffleT indices X16 a1, UnaryShuffleT indices X16 a2) => UnaryShuffleT indices X16 (a0, a1, a2) where
  unaryShuffle (MkTuple3X16 x0 x1 x2) = MkTuple3X16 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X16 a0, BinaryShuffleT indices X16 a1, BinaryShuffleT indices X16 a2) => BinaryShuffleT indices X16 (a0, a1, a2) where
  binaryShuffle (MkTuple3X16 x0 x1 x2) (MkTuple3X16 y0 y1 y2) = MkTuple3X16 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2)
  {-# INLINE binaryShuffle #-}
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
instance (UnaryShuffleT indices X16 a0, UnaryShuffleT indices X16 a1, UnaryShuffleT indices X16 a2, UnaryShuffleT indices X16 a3) => UnaryShuffleT indices X16 (a0, a1, a2, a3) where
  unaryShuffle (MkTuple4X16 x0 x1 x2 x3) = MkTuple4X16 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X16 a0, BinaryShuffleT indices X16 a1, BinaryShuffleT indices X16 a2, BinaryShuffleT indices X16 a3) => BinaryShuffleT indices X16 (a0, a1, a2, a3) where
  binaryShuffle (MkTuple4X16 x0 x1 x2 x3) (MkTuple4X16 y0 y1 y2 y3) = MkTuple4X16 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3)
  {-# INLINE binaryShuffle #-}
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
instance (UnaryShuffleT indices X16 a0, UnaryShuffleT indices X16 a1, UnaryShuffleT indices X16 a2, UnaryShuffleT indices X16 a3, UnaryShuffleT indices X16 a4) => UnaryShuffleT indices X16 (a0, a1, a2, a3, a4) where
  unaryShuffle (MkTuple5X16 x0 x1 x2 x3 x4) = MkTuple5X16 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X16 a0, BinaryShuffleT indices X16 a1, BinaryShuffleT indices X16 a2, BinaryShuffleT indices X16 a3, BinaryShuffleT indices X16 a4) => BinaryShuffleT indices X16 (a0, a1, a2, a3, a4) where
  binaryShuffle (MkTuple5X16 x0 x1 x2 x3 x4) (MkTuple5X16 y0 y1 y2 y3 y4) = MkTuple5X16 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4)
  {-# INLINE binaryShuffle #-}
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
instance (UnaryShuffleT indices X16 a0, UnaryShuffleT indices X16 a1, UnaryShuffleT indices X16 a2, UnaryShuffleT indices X16 a3, UnaryShuffleT indices X16 a4, UnaryShuffleT indices X16 a5) => UnaryShuffleT indices X16 (a0, a1, a2, a3, a4, a5) where
  unaryShuffle (MkTuple6X16 x0 x1 x2 x3 x4 x5) = MkTuple6X16 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4) (unaryShuffle @indices x5)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X16 a0, BinaryShuffleT indices X16 a1, BinaryShuffleT indices X16 a2, BinaryShuffleT indices X16 a3, BinaryShuffleT indices X16 a4, BinaryShuffleT indices X16 a5) => BinaryShuffleT indices X16 (a0, a1, a2, a3, a4, a5) where
  binaryShuffle (MkTuple6X16 x0 x1 x2 x3 x4 x5) (MkTuple6X16 y0 y1 y2 y3 y4 y5) = MkTuple6X16 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4) (binaryShuffle @indices x5 y5)
  {-# INLINE binaryShuffle #-}
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
