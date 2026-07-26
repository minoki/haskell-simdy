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
module Data.Simdy.Internal.VL128.X8 where
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
-- | @'X8' a@ is a fixed-length vector of length 8.
--
-- Conceptually, @data 'X8' a = MkX8 !a !a !a !a !a !a !a !a@.
--
-- You can access the elements by 'mkX8', 'packX8' and 'unpackX8'.
data family X8 a
instance KnownSIMDLength X8 where
  type SIMDLength X8 = 8
  simdLength = 8
  {-# INLINE simdLength #-}
newtype instance X8 Bool = MkBoolX8 Word8
type instance Mask (X8 a) = X8 Bool
instance MaskIsLiftedBool X8 a
instance BooleanF X8 Bool where
  andF (MkBoolX8 x) (MkBoolX8 y) = MkBoolX8 (x .&. y)
  orF (MkBoolX8 x) (MkBoolX8 y) = MkBoolX8 (x .|. y)
  xorF (MkBoolX8 x) (MkBoolX8 y) = MkBoolX8 (xor x y)
  complementF (MkBoolX8 x) = MkBoolX8 (0xff - x)
deriving via WrappedMulti X8 a instance EquatableF X8 a => Equatable (X8 a)
deriving via WrappedMulti X8 a instance OrderedF X8 a => Ordered (X8 a)
instance PackX8 X8 a => IsList (X8 a) where
  type Item (X8 a) = a
  toList = toListX8
  fromList = fromListX8
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX8 X8 Bool where
  mkX8 !x0 !x1 !x2 !x3 !x4 !x5 !x6 !x7 = MkBoolX8 ((if x0 then 0x1 else 0) .|. (if x1 then 0x2 else 0) .|. (if x2 then 0x4 else 0) .|. (if x3 then 0x8 else 0) .|. (if x4 then 0x10 else 0) .|. (if x5 then 0x20 else 0) .|. (if x6 then 0x40 else 0) .|. (if x7 then 0x80 else 0))
  unpackX8 (MkBoolX8 !x) = (testBit x 0, testBit x 1, testBit x 2, testBit x 3, testBit x 4, testBit x 5, testBit x 6, testBit x 7)
instance Broadcast X8 Bool where
  broadcast False = MkBoolX8 0
  broadcast True = MkBoolX8 0xff
  {-# INLINE broadcast #-}
instance SelectableF X8 Bool where
  selectF (MkBoolX8 !cond) (MkBoolX8 !x) (MkBoolX8 !y) = MkBoolX8 ((cond .&. x) .|. (complement cond .&. y))
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 Bool where
  unaryShuffle = error "not implemented yet"
  {-# NOINLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 Bool where
  binaryShuffle = error "not implemented yet"
  {-# NOINLINE binaryShuffle #-}
data instance X8 Float = MkFloatX8WithVec128 FloatX4# FloatX4#
instance PackX8 X8 Float where
  mkX8 (F# x0) (F# x1) (F# x2) (F# x3) (F# x4) (F# x5) (F# x6) (F# x7) = MkFloatX8WithVec128 (packFloatX4# (# x0, x1, x2, x3 #)) (packFloatX4# (# x4, x5, x6, x7 #))
  unpackX8 (MkFloatX8WithVec128 v0 v1) = case unpackFloatX4# v0 of (# x0, x1, x2, x3 #) -> case unpackFloatX4# v1 of (# x4, x5, x6, x7 #) -> (F# x0, F# x1, F# x2, F# x3, F# x4, F# x5, F# x6, F# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Float where
  broadcast (F# x) = let !v = broadcastFloatX4# x in MkFloatX8WithVec128 v v
  {-# INLINE broadcast #-}
instance SelectableF X8 Float where
  selectF (MkBoolX8 !cond) (MkFloatX8WithVec128 x0 x1) (MkFloatX8WithVec128 y0 y1) = MkFloatX8WithVec128 (selectFloatX4# (cond `unsafeShiftR` 0) x0 y0) (selectFloatX4# (cond `unsafeShiftR` 4) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, ShuffleMany FloatX4# [i0, i1, i2, i3], ShuffleMany FloatX4# [i4, i5, i6, i7]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Float where
  unaryShuffle (MkFloatX8WithVec128 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkFloatX8WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, ShuffleMany FloatX4# [i0, i1, i2, i3], ShuffleMany FloatX4# [i4, i5, i6, i7]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Float where
  binaryShuffle (MkFloatX8WithVec128 x0 x1) (MkFloatX8WithVec128 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkFloatX8WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Float where
  eqF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkBoolX8 $ (fromIntegral (eqFloatX4# u0 v0)) .|. (fromIntegral (eqFloatX4# u1 v1) `unsafeShiftL` 4)
  {-# INLINE eqF #-}
instance OrderedF X8 Float where
  ltF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkBoolX8 $ (fromIntegral (ltFloatX4# u0 v0)) .|. (fromIntegral (ltFloatX4# u1 v1) `unsafeShiftL` 4)
  leF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkBoolX8 $ (fromIntegral (leFloatX4# u0 v0)) .|. (fromIntegral (leFloatX4# u1 v1) `unsafeShiftL` 4)
  gtF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkBoolX8 $ (fromIntegral (gtFloatX4# u0 v0)) .|. (fromIntegral (gtFloatX4# u1 v1) `unsafeShiftL` 4)
  geF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkBoolX8 $ (fromIntegral (geFloatX4# u0 v0)) .|. (fromIntegral (geFloatX4# u1 v1) `unsafeShiftL` 4)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Float where
  minF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkFloatX8WithVec128 (minimumFloatX4# u0 v0) (minimumFloatX4# u1 v1)
  maxF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkFloatX8WithVec128 (maximumFloatX4# u0 v0) (maximumFloatX4# u1 v1)
  minimumNumberF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkFloatX8WithVec128 (minimumNumberFloatX4# u0 v0) (minimumNumberFloatX4# u1 v1)
  maximumNumberF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkFloatX8WithVec128 (maximumNumberFloatX4# u0 v0) (maximumNumberFloatX4# u1 v1)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX8Float :: X8 Float -> X8 Float
negateX8Float (MkFloatX8WithVec128 u0 u1) = MkFloatX8WithVec128 (negateFloatX4# u0) (negateFloatX4# u1)
#if defined(USE_FMA)
{-# INLINE [0] negateX8Float #-}
#else
{-# INLINE negateX8Float #-}
#endif
instance NumF X8 Float where
  plusF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkFloatX8WithVec128 (plusFloatX4# u0 v0) (plusFloatX4# u1 v1)
  minusF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkFloatX8WithVec128 (minusFloatX4# u0 v0) (minusFloatX4# u1 v1)
  timesF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkFloatX8WithVec128 (timesFloatX4# u0 v0) (timesFloatX4# u1 v1)
  negateF = negateX8Float
  absF (MkFloatX8WithVec128 u0 u1) = MkFloatX8WithVec128 (absFloatX4# u0) (absFloatX4# u1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X8 Float where
  divideF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) = MkFloatX8WithVec128 (divideFloatX4# u0 v0) (divideFloatX4# u1 v1)
  {-# INLINE divideF #-}
instance FloatingF X8 Float where
  sqrtF (MkFloatX8WithVec128 u0 u1) = MkFloatX8WithVec128 (sqrtFloatX4# u0) (sqrtFloatX4# u1)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X8 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX8WithVec128 u0 u1) (MkFloatX8WithVec128 v0 v1) (MkFloatX8WithVec128 w0 w1) = MkFloatX8WithVec128 (fmaddFloatX4# u0 v0 w0) (fmaddFloatX4# u1 v1 w1)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X8 Float" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X8 Float
"Fusible/*-/X8 Float" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX8Float c) :: X8 Float
"Fusible/-*+/X8 Float" forall a b c.
  negateX8Float (a F.* b) F.+ c = fusedMultiplyAdd (negateX8Float a) b c :: X8 Float
"Fusible/-*-/X8 Float" forall a b c.
  negateX8Float (a F.* b) F.- c = fusedMultiplyAdd (negateX8Float a) b (negateX8Float c) :: X8 Float
"Fusible/+*/X8 Float" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X8 Float
"Fusible/-*/X8 Float" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX8Float b) c a :: X8 Float
  #-}
#endif
instance EnumFromZero_ X8 Float where
  enumFromZero = MkFloatX8WithVec128 (packFloatX4# (# 0.0#, 1.0#, 2.0#, 3.0# #)) (packFloatX4# (# 4.0#, 5.0#, 6.0#, 7.0# #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Float where
  indexByteArraySIMD# ba i = MkFloatX8WithVec128 (indexFloatArrayAsFloatX4# ba i) (indexFloatArrayAsFloatX4# ba (i +# 4#))
  readByteArraySIMD# mba i s0 = case readFloatArrayAsFloatX4# mba i s0 of (# s1, v0 #) -> case readFloatArrayAsFloatX4# mba (i +# 4#) s1 of (# s2, v1 #) -> (# s2, MkFloatX8WithVec128 v0 v1 #)
  writeByteArraySIMD# mba i (MkFloatX8WithVec128 v0 v1) s0 = case writeFloatArrayAsFloatX4# mba i v0 s0 of s1 -> writeFloatArrayAsFloatX4# mba (i +# 4#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readFloatOffAddrAsFloatX4# addr i s0 of (# s1, v0 #) -> case readFloatOffAddrAsFloatX4# addr (i +# 4#) s1 of (# s2, v1 #) -> (# s2, MkFloatX8WithVec128 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX8WithVec128 v0 v1) = IO (\s0 -> case writeFloatOffAddrAsFloatX4# addr i v0 s0 of s1 -> case writeFloatOffAddrAsFloatX4# addr (i +# 4#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Double = MkDoubleX8WithVec128 DoubleX2# DoubleX2# DoubleX2# DoubleX2#
instance PackX8 X8 Double where
  mkX8 (D# x0) (D# x1) (D# x2) (D# x3) (D# x4) (D# x5) (D# x6) (D# x7) = MkDoubleX8WithVec128 (packDoubleX2# (# x0, x1 #)) (packDoubleX2# (# x2, x3 #)) (packDoubleX2# (# x4, x5 #)) (packDoubleX2# (# x6, x7 #))
  unpackX8 (MkDoubleX8WithVec128 v0 v1 v2 v3) = case unpackDoubleX2# v0 of (# x0, x1 #) -> case unpackDoubleX2# v1 of (# x2, x3 #) -> case unpackDoubleX2# v2 of (# x4, x5 #) -> case unpackDoubleX2# v3 of (# x6, x7 #) -> (D# x0, D# x1, D# x2, D# x3, D# x4, D# x5, D# x6, D# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Double where
  broadcast (D# x) = let !v = broadcastDoubleX2# x in MkDoubleX8WithVec128 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X8 Double where
  selectF (MkBoolX8 !cond) (MkDoubleX8WithVec128 x0 x1 x2 x3) (MkDoubleX8WithVec128 y0 y1 y2 y3) = MkDoubleX8WithVec128 (selectDoubleX2# (cond `unsafeShiftR` 0) x0 y0) (selectDoubleX2# (cond `unsafeShiftR` 2) x1 y1) (selectDoubleX2# (cond `unsafeShiftR` 4) x2 y2) (selectDoubleX2# (cond `unsafeShiftR` 6) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, ShuffleMany DoubleX2# [i0, i1], ShuffleMany DoubleX2# [i2, i3], ShuffleMany DoubleX2# [i4, i5], ShuffleMany DoubleX2# [i6, i7]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Double where
  unaryShuffle (MkDoubleX8WithVec128 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkDoubleX8WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, ShuffleMany DoubleX2# [i0, i1], ShuffleMany DoubleX2# [i2, i3], ShuffleMany DoubleX2# [i4, i5], ShuffleMany DoubleX2# [i6, i7]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Double where
  binaryShuffle (MkDoubleX8WithVec128 x0 x1 x2 x3) (MkDoubleX8WithVec128 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkDoubleX8WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Double where
  eqF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkBoolX8 $ (fromIntegral (eqDoubleX2# u0 v0)) .|. (fromIntegral (eqDoubleX2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (eqDoubleX2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (eqDoubleX2# u3 v3) `unsafeShiftL` 6)
  {-# INLINE eqF #-}
instance OrderedF X8 Double where
  ltF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkBoolX8 $ (fromIntegral (ltDoubleX2# u0 v0)) .|. (fromIntegral (ltDoubleX2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (ltDoubleX2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (ltDoubleX2# u3 v3) `unsafeShiftL` 6)
  leF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkBoolX8 $ (fromIntegral (leDoubleX2# u0 v0)) .|. (fromIntegral (leDoubleX2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (leDoubleX2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (leDoubleX2# u3 v3) `unsafeShiftL` 6)
  gtF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkBoolX8 $ (fromIntegral (gtDoubleX2# u0 v0)) .|. (fromIntegral (gtDoubleX2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (gtDoubleX2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (gtDoubleX2# u3 v3) `unsafeShiftL` 6)
  geF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkBoolX8 $ (fromIntegral (geDoubleX2# u0 v0)) .|. (fromIntegral (geDoubleX2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (geDoubleX2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (geDoubleX2# u3 v3) `unsafeShiftL` 6)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Double where
  minF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkDoubleX8WithVec128 (minimumDoubleX2# u0 v0) (minimumDoubleX2# u1 v1) (minimumDoubleX2# u2 v2) (minimumDoubleX2# u3 v3)
  maxF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkDoubleX8WithVec128 (maximumDoubleX2# u0 v0) (maximumDoubleX2# u1 v1) (maximumDoubleX2# u2 v2) (maximumDoubleX2# u3 v3)
  minimumNumberF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkDoubleX8WithVec128 (minimumNumberDoubleX2# u0 v0) (minimumNumberDoubleX2# u1 v1) (minimumNumberDoubleX2# u2 v2) (minimumNumberDoubleX2# u3 v3)
  maximumNumberF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkDoubleX8WithVec128 (maximumNumberDoubleX2# u0 v0) (maximumNumberDoubleX2# u1 v1) (maximumNumberDoubleX2# u2 v2) (maximumNumberDoubleX2# u3 v3)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX8Double :: X8 Double -> X8 Double
negateX8Double (MkDoubleX8WithVec128 u0 u1 u2 u3) = MkDoubleX8WithVec128 (negateDoubleX2# u0) (negateDoubleX2# u1) (negateDoubleX2# u2) (negateDoubleX2# u3)
#if defined(USE_FMA)
{-# INLINE [0] negateX8Double #-}
#else
{-# INLINE negateX8Double #-}
#endif
instance NumF X8 Double where
  plusF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkDoubleX8WithVec128 (plusDoubleX2# u0 v0) (plusDoubleX2# u1 v1) (plusDoubleX2# u2 v2) (plusDoubleX2# u3 v3)
  minusF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkDoubleX8WithVec128 (minusDoubleX2# u0 v0) (minusDoubleX2# u1 v1) (minusDoubleX2# u2 v2) (minusDoubleX2# u3 v3)
  timesF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkDoubleX8WithVec128 (timesDoubleX2# u0 v0) (timesDoubleX2# u1 v1) (timesDoubleX2# u2 v2) (timesDoubleX2# u3 v3)
  negateF = negateX8Double
  absF (MkDoubleX8WithVec128 u0 u1 u2 u3) = MkDoubleX8WithVec128 (absDoubleX2# u0) (absDoubleX2# u1) (absDoubleX2# u2) (absDoubleX2# u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X8 Double where
  divideF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) = MkDoubleX8WithVec128 (divideDoubleX2# u0 v0) (divideDoubleX2# u1 v1) (divideDoubleX2# u2 v2) (divideDoubleX2# u3 v3)
  {-# INLINE divideF #-}
instance FloatingF X8 Double where
  sqrtF (MkDoubleX8WithVec128 u0 u1 u2 u3) = MkDoubleX8WithVec128 (sqrtDoubleX2# u0) (sqrtDoubleX2# u1) (sqrtDoubleX2# u2) (sqrtDoubleX2# u3)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X8 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX8WithVec128 u0 u1 u2 u3) (MkDoubleX8WithVec128 v0 v1 v2 v3) (MkDoubleX8WithVec128 w0 w1 w2 w3) = MkDoubleX8WithVec128 (fmaddDoubleX2# u0 v0 w0) (fmaddDoubleX2# u1 v1 w1) (fmaddDoubleX2# u2 v2 w2) (fmaddDoubleX2# u3 v3 w3)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X8 Double" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X8 Double
"Fusible/*-/X8 Double" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX8Double c) :: X8 Double
"Fusible/-*+/X8 Double" forall a b c.
  negateX8Double (a F.* b) F.+ c = fusedMultiplyAdd (negateX8Double a) b c :: X8 Double
"Fusible/-*-/X8 Double" forall a b c.
  negateX8Double (a F.* b) F.- c = fusedMultiplyAdd (negateX8Double a) b (negateX8Double c) :: X8 Double
"Fusible/+*/X8 Double" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X8 Double
"Fusible/-*/X8 Double" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX8Double b) c a :: X8 Double
  #-}
#endif
instance EnumFromZero_ X8 Double where
  enumFromZero = MkDoubleX8WithVec128 (packDoubleX2# (# 0.0##, 1.0## #)) (packDoubleX2# (# 2.0##, 3.0## #)) (packDoubleX2# (# 4.0##, 5.0## #)) (packDoubleX2# (# 6.0##, 7.0## #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Double where
  indexByteArraySIMD# ba i = MkDoubleX8WithVec128 (indexDoubleArrayAsDoubleX2# ba i) (indexDoubleArrayAsDoubleX2# ba (i +# 2#)) (indexDoubleArrayAsDoubleX2# ba (i +# 4#)) (indexDoubleArrayAsDoubleX2# ba (i +# 6#))
  readByteArraySIMD# mba i s0 = case readDoubleArrayAsDoubleX2# mba i s0 of (# s1, v0 #) -> case readDoubleArrayAsDoubleX2# mba (i +# 2#) s1 of (# s2, v1 #) -> case readDoubleArrayAsDoubleX2# mba (i +# 4#) s2 of (# s3, v2 #) -> case readDoubleArrayAsDoubleX2# mba (i +# 6#) s3 of (# s4, v3 #) -> (# s4, MkDoubleX8WithVec128 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkDoubleX8WithVec128 v0 v1 v2 v3) s0 = case writeDoubleArrayAsDoubleX2# mba i v0 s0 of s1 -> case writeDoubleArrayAsDoubleX2# mba (i +# 2#) v1 s1 of s2 -> case writeDoubleArrayAsDoubleX2# mba (i +# 4#) v2 s2 of s3 -> writeDoubleArrayAsDoubleX2# mba (i +# 6#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readDoubleOffAddrAsDoubleX2# addr i s0 of (# s1, v0 #) -> case readDoubleOffAddrAsDoubleX2# addr (i +# 2#) s1 of (# s2, v1 #) -> case readDoubleOffAddrAsDoubleX2# addr (i +# 4#) s2 of (# s3, v2 #) -> case readDoubleOffAddrAsDoubleX2# addr (i +# 6#) s3 of (# s4, v3 #) -> (# s4, MkDoubleX8WithVec128 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX8WithVec128 v0 v1 v2 v3) = IO (\s0 -> case writeDoubleOffAddrAsDoubleX2# addr i v0 s0 of s1 -> case writeDoubleOffAddrAsDoubleX2# addr (i +# 2#) v1 s1 of s2 -> case writeDoubleOffAddrAsDoubleX2# addr (i +# 4#) v2 s2 of s3 -> case writeDoubleOffAddrAsDoubleX2# addr (i +# 6#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__) || defined(USE_LLVM)
instance ImplementationDescription X8 where
  implementationDescription _ = "X8;maxBits=128"
data instance X8 Int8 = MkInt8X8 Int8X16#
instance PackX8 X8 Int8 where
  mkX8 (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) = MkInt8X8 (packInt8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  unpackX8 (MkInt8X8 v0) = case unpackInt8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> (I8# x0, I8# x1, I8# x2, I8# x3, I8# x4, I8# x5, I8# x6, I8# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int8 where
  broadcast (I8# x) = MkInt8X8 (broadcastInt8X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Int8 where
  selectF (MkBoolX8 !cond) (MkInt8X8 x0) (MkInt8X8 y0) = MkInt8X8 (selectInt8X16# (fromIntegral cond) x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Int8X16# indices) => UnaryShuffleT indices X8 Int8 where
  unaryShuffle (MkInt8X8 x) = MkInt8X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Int8X16# indices) => BinaryShuffleT indices X8 Int8 where
  binaryShuffle (MkInt8X8 x0) (MkInt8X8 x1) = MkInt8X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int8 where
  eqF (MkInt8X8 u0) (MkInt8X8 v0) = MkBoolX8 $ (fromIntegral (eqInt8X16# u0 v0 .&. 0xff))
  {-# INLINE eqF #-}
instance OrderedF X8 Int8 where
  ltF (MkInt8X8 u0) (MkInt8X8 v0) = MkBoolX8 $ (fromIntegral (ltInt8X16# u0 v0 .&. 0xff))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int8 where
  minF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (minInt8X16# u0 v0)
  maxF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (maxInt8X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int8 where
  plusF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (plusInt8X16# u0 v0)
  minusF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (minusInt8X16# u0 v0)
  timesF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (timesInt8X16# u0 v0)
  negateF (MkInt8X8 u0) = MkInt8X8 (negateInt8X16# u0)
  absF (MkInt8X8 u0) = MkInt8X8 (absInt8X16# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Int8 where
  andF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (andInt8X16# u0 v0)
  orF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (orInt8X16# u0 v0)
  xorF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (xorInt8X16# u0 v0)
  complementF (MkInt8X8 u0) = MkInt8X8 (complementInt8X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int8 where
  shiftLF (MkInt8X8 u0) (I# i) = MkInt8X8 (shiftLInt8X16# u0 i)
  shiftRF (MkInt8X8 u0) (I# i) = MkInt8X8 (shiftRInt8X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Int8 where
  enumFromZero = MkInt8X8 (packInt8X16# (# 0#Int8, 1#Int8, 2#Int8, 3#Int8, 4#Int8, 5#Int8, 6#Int8, 7#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int8 where
  indexByteArraySIMD# ba i = MkInt8X8 (packInt8X16# (# GHC.Exts.indexInt8Array# ba i, GHC.Exts.indexInt8Array# ba (i +# 1#), GHC.Exts.indexInt8Array# ba (i +# 2#), GHC.Exts.indexInt8Array# ba (i +# 3#), GHC.Exts.indexInt8Array# ba (i +# 4#), GHC.Exts.indexInt8Array# ba (i +# 5#), GHC.Exts.indexInt8Array# ba (i +# 6#), GHC.Exts.indexInt8Array# ba (i +# 7#), 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt8Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt8Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt8Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt8Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt8X8 (packInt8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #)) #)
  writeByteArraySIMD# mba i (MkInt8X8 v0) s0 = case unpackInt8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeInt8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt8Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt8Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt8Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeInt8Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt8X8 (packInt8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X8 v0) = IO (\s0 -> case unpackInt8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeInt8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 7#) x7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int16 = MkInt16X8 Int16X8#
instance PackX8 X8 Int16 where
  mkX8 (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) = MkInt16X8 (packInt16X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))
  unpackX8 (MkInt16X8 v0) = case unpackInt16X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (I16# x0, I16# x1, I16# x2, I16# x3, I16# x4, I16# x5, I16# x6, I16# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int16 where
  broadcast (I16# x) = MkInt16X8 (broadcastInt16X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Int16 where
  selectF (MkBoolX8 !cond) (MkInt16X8 x0) (MkInt16X8 y0) = MkInt16X8 (selectInt16X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Int16X8# indices) => UnaryShuffleT indices X8 Int16 where
  unaryShuffle (MkInt16X8 x) = MkInt16X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Int16X8# indices) => BinaryShuffleT indices X8 Int16 where
  binaryShuffle (MkInt16X8 x0) (MkInt16X8 x1) = MkInt16X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int16 where
  eqF (MkInt16X8 u0) (MkInt16X8 v0) = MkBoolX8 $ (fromIntegral (eqInt16X8# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X8 Int16 where
  ltF (MkInt16X8 u0) (MkInt16X8 v0) = MkBoolX8 $ (fromIntegral (ltInt16X8# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int16 where
  minF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (minInt16X8# u0 v0)
  maxF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (maxInt16X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int16 where
  plusF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (plusInt16X8# u0 v0)
  minusF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (minusInt16X8# u0 v0)
  timesF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (timesInt16X8# u0 v0)
  negateF (MkInt16X8 u0) = MkInt16X8 (negateInt16X8# u0)
  absF (MkInt16X8 u0) = MkInt16X8 (absInt16X8# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Int16 where
  andF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (andInt16X8# u0 v0)
  orF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (orInt16X8# u0 v0)
  xorF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (xorInt16X8# u0 v0)
  complementF (MkInt16X8 u0) = MkInt16X8 (complementInt16X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int16 where
  shiftLF (MkInt16X8 u0) (I# i) = MkInt16X8 (shiftLInt16X8# u0 i)
  shiftRF (MkInt16X8 u0) (I# i) = MkInt16X8 (shiftRInt16X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Int16 where
  enumFromZero = MkInt16X8 (packInt16X8# (# 0#Int16, 1#Int16, 2#Int16, 3#Int16, 4#Int16, 5#Int16, 6#Int16, 7#Int16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int16 where
  indexByteArraySIMD# ba i = MkInt16X8 (indexInt16ArrayAsInt16X8# ba i)
  readByteArraySIMD# mba i s0 = case readInt16ArrayAsInt16X8# mba i s0 of (# s1, v0 #) -> (# s1, MkInt16X8 v0 #)
  writeByteArraySIMD# mba i (MkInt16X8 v0) s0 = writeInt16ArrayAsInt16X8# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt16OffAddrAsInt16X8# addr i s0 of (# s1, v0 #) -> (# s1, MkInt16X8 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X8 v0) = IO (\s0 -> case writeInt16OffAddrAsInt16X8# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int32 = MkInt32X8WithVec128 Int32X4# Int32X4#
instance PackX8 X8 Int32 where
  mkX8 (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) = MkInt32X8WithVec128 (packInt32X4# (# x0, x1, x2, x3 #)) (packInt32X4# (# x4, x5, x6, x7 #))
  unpackX8 (MkInt32X8WithVec128 v0 v1) = case unpackInt32X4# v0 of (# x0, x1, x2, x3 #) -> case unpackInt32X4# v1 of (# x4, x5, x6, x7 #) -> (I32# x0, I32# x1, I32# x2, I32# x3, I32# x4, I32# x5, I32# x6, I32# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int32 where
  broadcast (I32# x) = let !v = broadcastInt32X4# x in MkInt32X8WithVec128 v v
  {-# INLINE broadcast #-}
instance SelectableF X8 Int32 where
  selectF (MkBoolX8 !cond) (MkInt32X8WithVec128 x0 x1) (MkInt32X8WithVec128 y0 y1) = MkInt32X8WithVec128 (selectInt32X4# (cond `unsafeShiftR` 0) x0 y0) (selectInt32X4# (cond `unsafeShiftR` 4) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, ShuffleMany Int32X4# [i0, i1, i2, i3], ShuffleMany Int32X4# [i4, i5, i6, i7]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int32 where
  unaryShuffle (MkInt32X8WithVec128 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt32X8WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, ShuffleMany Int32X4# [i0, i1, i2, i3], ShuffleMany Int32X4# [i4, i5, i6, i7]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int32 where
  binaryShuffle (MkInt32X8WithVec128 x0 x1) (MkInt32X8WithVec128 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt32X8WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int32 where
  eqF (MkInt32X8WithVec128 u0 u1) (MkInt32X8WithVec128 v0 v1) = MkBoolX8 $ (fromIntegral (eqInt32X4# u0 v0)) .|. (fromIntegral (eqInt32X4# u1 v1) `unsafeShiftL` 4)
  {-# INLINE eqF #-}
instance OrderedF X8 Int32 where
  ltF (MkInt32X8WithVec128 u0 u1) (MkInt32X8WithVec128 v0 v1) = MkBoolX8 $ (fromIntegral (ltInt32X4# u0 v0)) .|. (fromIntegral (ltInt32X4# u1 v1) `unsafeShiftL` 4)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int32 where
  minF (MkInt32X8WithVec128 u0 u1) (MkInt32X8WithVec128 v0 v1) = MkInt32X8WithVec128 (minInt32X4# u0 v0) (minInt32X4# u1 v1)
  maxF (MkInt32X8WithVec128 u0 u1) (MkInt32X8WithVec128 v0 v1) = MkInt32X8WithVec128 (maxInt32X4# u0 v0) (maxInt32X4# u1 v1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int32 where
  plusF (MkInt32X8WithVec128 u0 u1) (MkInt32X8WithVec128 v0 v1) = MkInt32X8WithVec128 (plusInt32X4# u0 v0) (plusInt32X4# u1 v1)
  minusF (MkInt32X8WithVec128 u0 u1) (MkInt32X8WithVec128 v0 v1) = MkInt32X8WithVec128 (minusInt32X4# u0 v0) (minusInt32X4# u1 v1)
  timesF (MkInt32X8WithVec128 u0 u1) (MkInt32X8WithVec128 v0 v1) = MkInt32X8WithVec128 (timesInt32X4# u0 v0) (timesInt32X4# u1 v1)
  negateF (MkInt32X8WithVec128 u0 u1) = MkInt32X8WithVec128 (negateInt32X4# u0) (negateInt32X4# u1)
  absF (MkInt32X8WithVec128 u0 u1) = MkInt32X8WithVec128 (absInt32X4# u0) (absInt32X4# u1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Int32 where
  andF (MkInt32X8WithVec128 u0 u1) (MkInt32X8WithVec128 v0 v1) = MkInt32X8WithVec128 (andInt32X4# u0 v0) (andInt32X4# u1 v1)
  orF (MkInt32X8WithVec128 u0 u1) (MkInt32X8WithVec128 v0 v1) = MkInt32X8WithVec128 (orInt32X4# u0 v0) (orInt32X4# u1 v1)
  xorF (MkInt32X8WithVec128 u0 u1) (MkInt32X8WithVec128 v0 v1) = MkInt32X8WithVec128 (xorInt32X4# u0 v0) (xorInt32X4# u1 v1)
  complementF (MkInt32X8WithVec128 u0 u1) = MkInt32X8WithVec128 (complementInt32X4# u0) (complementInt32X4# u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int32 where
  shiftLF (MkInt32X8WithVec128 u0 u1) (I# i) = MkInt32X8WithVec128 (shiftLInt32X4# u0 i) (shiftLInt32X4# u1 i)
  shiftRF (MkInt32X8WithVec128 u0 u1) (I# i) = MkInt32X8WithVec128 (shiftRInt32X4# u0 i) (shiftRInt32X4# u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Int32 where
  enumFromZero = MkInt32X8WithVec128 (packInt32X4# (# 0#Int32, 1#Int32, 2#Int32, 3#Int32 #)) (packInt32X4# (# 4#Int32, 5#Int32, 6#Int32, 7#Int32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int32 where
  indexByteArraySIMD# ba i = MkInt32X8WithVec128 (indexInt32ArrayAsInt32X4# ba i) (indexInt32ArrayAsInt32X4# ba (i +# 4#))
  readByteArraySIMD# mba i s0 = case readInt32ArrayAsInt32X4# mba i s0 of (# s1, v0 #) -> case readInt32ArrayAsInt32X4# mba (i +# 4#) s1 of (# s2, v1 #) -> (# s2, MkInt32X8WithVec128 v0 v1 #)
  writeByteArraySIMD# mba i (MkInt32X8WithVec128 v0 v1) s0 = case writeInt32ArrayAsInt32X4# mba i v0 s0 of s1 -> writeInt32ArrayAsInt32X4# mba (i +# 4#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt32OffAddrAsInt32X4# addr i s0 of (# s1, v0 #) -> case readInt32OffAddrAsInt32X4# addr (i +# 4#) s1 of (# s2, v1 #) -> (# s2, MkInt32X8WithVec128 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X8WithVec128 v0 v1) = IO (\s0 -> case writeInt32OffAddrAsInt32X4# addr i v0 s0 of s1 -> case writeInt32OffAddrAsInt32X4# addr (i +# 4#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int64 = MkInt64X8WithVec128 Int64X2# Int64X2# Int64X2# Int64X2#
instance PackX8 X8 Int64 where
  mkX8 (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) = MkInt64X8WithVec128 (packInt64X2# (# x0, x1 #)) (packInt64X2# (# x2, x3 #)) (packInt64X2# (# x4, x5 #)) (packInt64X2# (# x6, x7 #))
  unpackX8 (MkInt64X8WithVec128 v0 v1 v2 v3) = case unpackInt64X2# v0 of (# x0, x1 #) -> case unpackInt64X2# v1 of (# x2, x3 #) -> case unpackInt64X2# v2 of (# x4, x5 #) -> case unpackInt64X2# v3 of (# x6, x7 #) -> (I64# x0, I64# x1, I64# x2, I64# x3, I64# x4, I64# x5, I64# x6, I64# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int64 where
  broadcast (I64# x) = let !v = broadcastInt64X2# x in MkInt64X8WithVec128 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X8 Int64 where
  selectF (MkBoolX8 !cond) (MkInt64X8WithVec128 x0 x1 x2 x3) (MkInt64X8WithVec128 y0 y1 y2 y3) = MkInt64X8WithVec128 (selectInt64X2# (cond `unsafeShiftR` 0) x0 y0) (selectInt64X2# (cond `unsafeShiftR` 2) x1 y1) (selectInt64X2# (cond `unsafeShiftR` 4) x2 y2) (selectInt64X2# (cond `unsafeShiftR` 6) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, ShuffleMany Int64X2# [i0, i1], ShuffleMany Int64X2# [i2, i3], ShuffleMany Int64X2# [i4, i5], ShuffleMany Int64X2# [i6, i7]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int64 where
  unaryShuffle (MkInt64X8WithVec128 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt64X8WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, ShuffleMany Int64X2# [i0, i1], ShuffleMany Int64X2# [i2, i3], ShuffleMany Int64X2# [i4, i5], ShuffleMany Int64X2# [i6, i7]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int64 where
  binaryShuffle (MkInt64X8WithVec128 x0 x1 x2 x3) (MkInt64X8WithVec128 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt64X8WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int64 where
  eqF (MkInt64X8WithVec128 u0 u1 u2 u3) (MkInt64X8WithVec128 v0 v1 v2 v3) = MkBoolX8 $ (fromIntegral (eqInt64X2# u0 v0)) .|. (fromIntegral (eqInt64X2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (eqInt64X2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (eqInt64X2# u3 v3) `unsafeShiftL` 6)
  {-# INLINE eqF #-}
instance OrderedF X8 Int64 where
  ltF (MkInt64X8WithVec128 u0 u1 u2 u3) (MkInt64X8WithVec128 v0 v1 v2 v3) = MkBoolX8 $ (fromIntegral (ltInt64X2# u0 v0)) .|. (fromIntegral (ltInt64X2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (ltInt64X2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (ltInt64X2# u3 v3) `unsafeShiftL` 6)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int64 where
  minF (MkInt64X8WithVec128 u0 u1 u2 u3) (MkInt64X8WithVec128 v0 v1 v2 v3) = MkInt64X8WithVec128 (minInt64X2# u0 v0) (minInt64X2# u1 v1) (minInt64X2# u2 v2) (minInt64X2# u3 v3)
  maxF (MkInt64X8WithVec128 u0 u1 u2 u3) (MkInt64X8WithVec128 v0 v1 v2 v3) = MkInt64X8WithVec128 (maxInt64X2# u0 v0) (maxInt64X2# u1 v1) (maxInt64X2# u2 v2) (maxInt64X2# u3 v3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int64 where
  plusF (MkInt64X8WithVec128 u0 u1 u2 u3) (MkInt64X8WithVec128 v0 v1 v2 v3) = MkInt64X8WithVec128 (plusInt64X2# u0 v0) (plusInt64X2# u1 v1) (plusInt64X2# u2 v2) (plusInt64X2# u3 v3)
  minusF (MkInt64X8WithVec128 u0 u1 u2 u3) (MkInt64X8WithVec128 v0 v1 v2 v3) = MkInt64X8WithVec128 (minusInt64X2# u0 v0) (minusInt64X2# u1 v1) (minusInt64X2# u2 v2) (minusInt64X2# u3 v3)
  timesF (MkInt64X8WithVec128 u0 u1 u2 u3) (MkInt64X8WithVec128 v0 v1 v2 v3) = MkInt64X8WithVec128 (timesInt64X2# u0 v0) (timesInt64X2# u1 v1) (timesInt64X2# u2 v2) (timesInt64X2# u3 v3)
  negateF (MkInt64X8WithVec128 u0 u1 u2 u3) = MkInt64X8WithVec128 (negateInt64X2# u0) (negateInt64X2# u1) (negateInt64X2# u2) (negateInt64X2# u3)
  absF (MkInt64X8WithVec128 u0 u1 u2 u3) = MkInt64X8WithVec128 (absInt64X2# u0) (absInt64X2# u1) (absInt64X2# u2) (absInt64X2# u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Int64 where
  andF (MkInt64X8WithVec128 u0 u1 u2 u3) (MkInt64X8WithVec128 v0 v1 v2 v3) = MkInt64X8WithVec128 (andInt64X2# u0 v0) (andInt64X2# u1 v1) (andInt64X2# u2 v2) (andInt64X2# u3 v3)
  orF (MkInt64X8WithVec128 u0 u1 u2 u3) (MkInt64X8WithVec128 v0 v1 v2 v3) = MkInt64X8WithVec128 (orInt64X2# u0 v0) (orInt64X2# u1 v1) (orInt64X2# u2 v2) (orInt64X2# u3 v3)
  xorF (MkInt64X8WithVec128 u0 u1 u2 u3) (MkInt64X8WithVec128 v0 v1 v2 v3) = MkInt64X8WithVec128 (xorInt64X2# u0 v0) (xorInt64X2# u1 v1) (xorInt64X2# u2 v2) (xorInt64X2# u3 v3)
  complementF (MkInt64X8WithVec128 u0 u1 u2 u3) = MkInt64X8WithVec128 (complementInt64X2# u0) (complementInt64X2# u1) (complementInt64X2# u2) (complementInt64X2# u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int64 where
  shiftLF (MkInt64X8WithVec128 u0 u1 u2 u3) (I# i) = MkInt64X8WithVec128 (shiftLInt64X2# u0 i) (shiftLInt64X2# u1 i) (shiftLInt64X2# u2 i) (shiftLInt64X2# u3 i)
  shiftRF (MkInt64X8WithVec128 u0 u1 u2 u3) (I# i) = MkInt64X8WithVec128 (shiftRInt64X2# u0 i) (shiftRInt64X2# u1 i) (shiftRInt64X2# u2 i) (shiftRInt64X2# u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Int64 where
  enumFromZero = MkInt64X8WithVec128 (packInt64X2# (# 0#Int64, 1#Int64 #)) (packInt64X2# (# 2#Int64, 3#Int64 #)) (packInt64X2# (# 4#Int64, 5#Int64 #)) (packInt64X2# (# 6#Int64, 7#Int64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int64 where
  indexByteArraySIMD# ba i = MkInt64X8WithVec128 (indexInt64ArrayAsInt64X2# ba i) (indexInt64ArrayAsInt64X2# ba (i +# 2#)) (indexInt64ArrayAsInt64X2# ba (i +# 4#)) (indexInt64ArrayAsInt64X2# ba (i +# 6#))
  readByteArraySIMD# mba i s0 = case readInt64ArrayAsInt64X2# mba i s0 of (# s1, v0 #) -> case readInt64ArrayAsInt64X2# mba (i +# 2#) s1 of (# s2, v1 #) -> case readInt64ArrayAsInt64X2# mba (i +# 4#) s2 of (# s3, v2 #) -> case readInt64ArrayAsInt64X2# mba (i +# 6#) s3 of (# s4, v3 #) -> (# s4, MkInt64X8WithVec128 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkInt64X8WithVec128 v0 v1 v2 v3) s0 = case writeInt64ArrayAsInt64X2# mba i v0 s0 of s1 -> case writeInt64ArrayAsInt64X2# mba (i +# 2#) v1 s1 of s2 -> case writeInt64ArrayAsInt64X2# mba (i +# 4#) v2 s2 of s3 -> writeInt64ArrayAsInt64X2# mba (i +# 6#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt64OffAddrAsInt64X2# addr i s0 of (# s1, v0 #) -> case readInt64OffAddrAsInt64X2# addr (i +# 2#) s1 of (# s2, v1 #) -> case readInt64OffAddrAsInt64X2# addr (i +# 4#) s2 of (# s3, v2 #) -> case readInt64OffAddrAsInt64X2# addr (i +# 6#) s3 of (# s4, v3 #) -> (# s4, MkInt64X8WithVec128 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X8WithVec128 v0 v1 v2 v3) = IO (\s0 -> case writeInt64OffAddrAsInt64X2# addr i v0 s0 of s1 -> case writeInt64OffAddrAsInt64X2# addr (i +# 2#) v1 s1 of s2 -> case writeInt64OffAddrAsInt64X2# addr (i +# 4#) v2 s2 of s3 -> case writeInt64OffAddrAsInt64X2# addr (i +# 6#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word8 = MkWord8X8 Word8X16#
instance PackX8 X8 Word8 where
  mkX8 (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) = MkWord8X8 (packWord8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  unpackX8 (MkWord8X8 v0) = case unpackWord8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> (W8# x0, W8# x1, W8# x2, W8# x3, W8# x4, W8# x5, W8# x6, W8# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word8 where
  broadcast (W8# x) = MkWord8X8 (broadcastWord8X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Word8 where
  selectF (MkBoolX8 !cond) (MkWord8X8 x0) (MkWord8X8 y0) = MkWord8X8 (selectWord8X16# (fromIntegral cond) x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Word8X16# indices) => UnaryShuffleT indices X8 Word8 where
  unaryShuffle (MkWord8X8 x) = MkWord8X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Word8X16# indices) => BinaryShuffleT indices X8 Word8 where
  binaryShuffle (MkWord8X8 x0) (MkWord8X8 x1) = MkWord8X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word8 where
  eqF (MkWord8X8 u0) (MkWord8X8 v0) = MkBoolX8 $ (fromIntegral (eqWord8X16# u0 v0 .&. 0xff))
  {-# INLINE eqF #-}
instance OrderedF X8 Word8 where
  ltF (MkWord8X8 u0) (MkWord8X8 v0) = MkBoolX8 $ (fromIntegral (ltWord8X16# u0 v0 .&. 0xff))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word8 where
  minF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (minWord8X16# u0 v0)
  maxF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (maxWord8X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word8 where
  plusF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (plusWord8X16# u0 v0)
  minusF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (minusWord8X16# u0 v0)
  timesF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (timesWord8X16# u0 v0)
  -- Currently, there is no negateWord8X16#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Word8 where
  andF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (andWord8X16# u0 v0)
  orF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (orWord8X16# u0 v0)
  xorF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (xorWord8X16# u0 v0)
  complementF (MkWord8X8 u0) = MkWord8X8 (complementWord8X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word8 where
  shiftLF (MkWord8X8 u0) (I# i) = MkWord8X8 (shiftLWord8X16# u0 i)
  shiftRF (MkWord8X8 u0) (I# i) = MkWord8X8 (shiftRWord8X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Word8 where
  enumFromZero = MkWord8X8 (packWord8X16# (# 0#Word8, 1#Word8, 2#Word8, 3#Word8, 4#Word8, 5#Word8, 6#Word8, 7#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word8 where
  indexByteArraySIMD# ba i = MkWord8X8 (packWord8X16# (# GHC.Exts.indexWord8Array# ba i, GHC.Exts.indexWord8Array# ba (i +# 1#), GHC.Exts.indexWord8Array# ba (i +# 2#), GHC.Exts.indexWord8Array# ba (i +# 3#), GHC.Exts.indexWord8Array# ba (i +# 4#), GHC.Exts.indexWord8Array# ba (i +# 5#), GHC.Exts.indexWord8Array# ba (i +# 6#), GHC.Exts.indexWord8Array# ba (i +# 7#), 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord8Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord8Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord8Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord8Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord8X8 (packWord8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #)) #)
  writeByteArraySIMD# mba i (MkWord8X8 v0) s0 = case unpackWord8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeWord8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord8Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord8Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord8Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeWord8Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord8X8 (packWord8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X8 v0) = IO (\s0 -> case unpackWord8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeWord8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 7#) x7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word16 = MkWord16X8 Word16X8#
instance PackX8 X8 Word16 where
  mkX8 (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) = MkWord16X8 (packWord16X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))
  unpackX8 (MkWord16X8 v0) = case unpackWord16X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (W16# x0, W16# x1, W16# x2, W16# x3, W16# x4, W16# x5, W16# x6, W16# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word16 where
  broadcast (W16# x) = MkWord16X8 (broadcastWord16X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Word16 where
  selectF (MkBoolX8 !cond) (MkWord16X8 x0) (MkWord16X8 y0) = MkWord16X8 (selectWord16X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Word16X8# indices) => UnaryShuffleT indices X8 Word16 where
  unaryShuffle (MkWord16X8 x) = MkWord16X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Word16X8# indices) => BinaryShuffleT indices X8 Word16 where
  binaryShuffle (MkWord16X8 x0) (MkWord16X8 x1) = MkWord16X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word16 where
  eqF (MkWord16X8 u0) (MkWord16X8 v0) = MkBoolX8 $ (fromIntegral (eqWord16X8# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X8 Word16 where
  ltF (MkWord16X8 u0) (MkWord16X8 v0) = MkBoolX8 $ (fromIntegral (ltWord16X8# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word16 where
  minF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (minWord16X8# u0 v0)
  maxF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (maxWord16X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word16 where
  plusF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (plusWord16X8# u0 v0)
  minusF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (minusWord16X8# u0 v0)
  timesF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (timesWord16X8# u0 v0)
  -- Currently, there is no negateWord16X8#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Word16 where
  andF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (andWord16X8# u0 v0)
  orF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (orWord16X8# u0 v0)
  xorF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (xorWord16X8# u0 v0)
  complementF (MkWord16X8 u0) = MkWord16X8 (complementWord16X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word16 where
  shiftLF (MkWord16X8 u0) (I# i) = MkWord16X8 (shiftLWord16X8# u0 i)
  shiftRF (MkWord16X8 u0) (I# i) = MkWord16X8 (shiftRWord16X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Word16 where
  enumFromZero = MkWord16X8 (packWord16X8# (# 0#Word16, 1#Word16, 2#Word16, 3#Word16, 4#Word16, 5#Word16, 6#Word16, 7#Word16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word16 where
  indexByteArraySIMD# ba i = MkWord16X8 (indexWord16ArrayAsWord16X8# ba i)
  readByteArraySIMD# mba i s0 = case readWord16ArrayAsWord16X8# mba i s0 of (# s1, v0 #) -> (# s1, MkWord16X8 v0 #)
  writeByteArraySIMD# mba i (MkWord16X8 v0) s0 = writeWord16ArrayAsWord16X8# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord16OffAddrAsWord16X8# addr i s0 of (# s1, v0 #) -> (# s1, MkWord16X8 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X8 v0) = IO (\s0 -> case writeWord16OffAddrAsWord16X8# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word32 = MkWord32X8WithVec128 Word32X4# Word32X4#
instance PackX8 X8 Word32 where
  mkX8 (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) = MkWord32X8WithVec128 (packWord32X4# (# x0, x1, x2, x3 #)) (packWord32X4# (# x4, x5, x6, x7 #))
  unpackX8 (MkWord32X8WithVec128 v0 v1) = case unpackWord32X4# v0 of (# x0, x1, x2, x3 #) -> case unpackWord32X4# v1 of (# x4, x5, x6, x7 #) -> (W32# x0, W32# x1, W32# x2, W32# x3, W32# x4, W32# x5, W32# x6, W32# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word32 where
  broadcast (W32# x) = let !v = broadcastWord32X4# x in MkWord32X8WithVec128 v v
  {-# INLINE broadcast #-}
instance SelectableF X8 Word32 where
  selectF (MkBoolX8 !cond) (MkWord32X8WithVec128 x0 x1) (MkWord32X8WithVec128 y0 y1) = MkWord32X8WithVec128 (selectWord32X4# (cond `unsafeShiftR` 0) x0 y0) (selectWord32X4# (cond `unsafeShiftR` 4) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, ShuffleMany Word32X4# [i0, i1, i2, i3], ShuffleMany Word32X4# [i4, i5, i6, i7]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word32 where
  unaryShuffle (MkWord32X8WithVec128 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord32X8WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, ShuffleMany Word32X4# [i0, i1, i2, i3], ShuffleMany Word32X4# [i4, i5, i6, i7]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word32 where
  binaryShuffle (MkWord32X8WithVec128 x0 x1) (MkWord32X8WithVec128 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord32X8WithVec128 (shuffleMany# @_ @_ @[i0, i1, i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5, i6, i7] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word32 where
  eqF (MkWord32X8WithVec128 u0 u1) (MkWord32X8WithVec128 v0 v1) = MkBoolX8 $ (fromIntegral (eqWord32X4# u0 v0)) .|. (fromIntegral (eqWord32X4# u1 v1) `unsafeShiftL` 4)
  {-# INLINE eqF #-}
instance OrderedF X8 Word32 where
  ltF (MkWord32X8WithVec128 u0 u1) (MkWord32X8WithVec128 v0 v1) = MkBoolX8 $ (fromIntegral (ltWord32X4# u0 v0)) .|. (fromIntegral (ltWord32X4# u1 v1) `unsafeShiftL` 4)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word32 where
  minF (MkWord32X8WithVec128 u0 u1) (MkWord32X8WithVec128 v0 v1) = MkWord32X8WithVec128 (minWord32X4# u0 v0) (minWord32X4# u1 v1)
  maxF (MkWord32X8WithVec128 u0 u1) (MkWord32X8WithVec128 v0 v1) = MkWord32X8WithVec128 (maxWord32X4# u0 v0) (maxWord32X4# u1 v1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word32 where
  plusF (MkWord32X8WithVec128 u0 u1) (MkWord32X8WithVec128 v0 v1) = MkWord32X8WithVec128 (plusWord32X4# u0 v0) (plusWord32X4# u1 v1)
  minusF (MkWord32X8WithVec128 u0 u1) (MkWord32X8WithVec128 v0 v1) = MkWord32X8WithVec128 (minusWord32X4# u0 v0) (minusWord32X4# u1 v1)
  timesF (MkWord32X8WithVec128 u0 u1) (MkWord32X8WithVec128 v0 v1) = MkWord32X8WithVec128 (timesWord32X4# u0 v0) (timesWord32X4# u1 v1)
  -- Currently, there is no negateWord32X4#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Word32 where
  andF (MkWord32X8WithVec128 u0 u1) (MkWord32X8WithVec128 v0 v1) = MkWord32X8WithVec128 (andWord32X4# u0 v0) (andWord32X4# u1 v1)
  orF (MkWord32X8WithVec128 u0 u1) (MkWord32X8WithVec128 v0 v1) = MkWord32X8WithVec128 (orWord32X4# u0 v0) (orWord32X4# u1 v1)
  xorF (MkWord32X8WithVec128 u0 u1) (MkWord32X8WithVec128 v0 v1) = MkWord32X8WithVec128 (xorWord32X4# u0 v0) (xorWord32X4# u1 v1)
  complementF (MkWord32X8WithVec128 u0 u1) = MkWord32X8WithVec128 (complementWord32X4# u0) (complementWord32X4# u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word32 where
  shiftLF (MkWord32X8WithVec128 u0 u1) (I# i) = MkWord32X8WithVec128 (shiftLWord32X4# u0 i) (shiftLWord32X4# u1 i)
  shiftRF (MkWord32X8WithVec128 u0 u1) (I# i) = MkWord32X8WithVec128 (shiftRWord32X4# u0 i) (shiftRWord32X4# u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Word32 where
  enumFromZero = MkWord32X8WithVec128 (packWord32X4# (# 0#Word32, 1#Word32, 2#Word32, 3#Word32 #)) (packWord32X4# (# 4#Word32, 5#Word32, 6#Word32, 7#Word32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word32 where
  indexByteArraySIMD# ba i = MkWord32X8WithVec128 (indexWord32ArrayAsWord32X4# ba i) (indexWord32ArrayAsWord32X4# ba (i +# 4#))
  readByteArraySIMD# mba i s0 = case readWord32ArrayAsWord32X4# mba i s0 of (# s1, v0 #) -> case readWord32ArrayAsWord32X4# mba (i +# 4#) s1 of (# s2, v1 #) -> (# s2, MkWord32X8WithVec128 v0 v1 #)
  writeByteArraySIMD# mba i (MkWord32X8WithVec128 v0 v1) s0 = case writeWord32ArrayAsWord32X4# mba i v0 s0 of s1 -> writeWord32ArrayAsWord32X4# mba (i +# 4#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord32OffAddrAsWord32X4# addr i s0 of (# s1, v0 #) -> case readWord32OffAddrAsWord32X4# addr (i +# 4#) s1 of (# s2, v1 #) -> (# s2, MkWord32X8WithVec128 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X8WithVec128 v0 v1) = IO (\s0 -> case writeWord32OffAddrAsWord32X4# addr i v0 s0 of s1 -> case writeWord32OffAddrAsWord32X4# addr (i +# 4#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word64 = MkWord64X8WithVec128 Word64X2# Word64X2# Word64X2# Word64X2#
instance PackX8 X8 Word64 where
  mkX8 (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) = MkWord64X8WithVec128 (packWord64X2# (# x0, x1 #)) (packWord64X2# (# x2, x3 #)) (packWord64X2# (# x4, x5 #)) (packWord64X2# (# x6, x7 #))
  unpackX8 (MkWord64X8WithVec128 v0 v1 v2 v3) = case unpackWord64X2# v0 of (# x0, x1 #) -> case unpackWord64X2# v1 of (# x2, x3 #) -> case unpackWord64X2# v2 of (# x4, x5 #) -> case unpackWord64X2# v3 of (# x6, x7 #) -> (W64# x0, W64# x1, W64# x2, W64# x3, W64# x4, W64# x5, W64# x6, W64# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word64 where
  broadcast (W64# x) = let !v = broadcastWord64X2# x in MkWord64X8WithVec128 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X8 Word64 where
  selectF (MkBoolX8 !cond) (MkWord64X8WithVec128 x0 x1 x2 x3) (MkWord64X8WithVec128 y0 y1 y2 y3) = MkWord64X8WithVec128 (selectWord64X2# (cond `unsafeShiftR` 0) x0 y0) (selectWord64X2# (cond `unsafeShiftR` 2) x1 y1) (selectWord64X2# (cond `unsafeShiftR` 4) x2 y2) (selectWord64X2# (cond `unsafeShiftR` 6) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, ShuffleMany Word64X2# [i0, i1], ShuffleMany Word64X2# [i2, i3], ShuffleMany Word64X2# [i4, i5], ShuffleMany Word64X2# [i6, i7]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word64 where
  unaryShuffle (MkWord64X8WithVec128 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord64X8WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, ShuffleMany Word64X2# [i0, i1], ShuffleMany Word64X2# [i2, i3], ShuffleMany Word64X2# [i4, i5], ShuffleMany Word64X2# [i6, i7]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word64 where
  binaryShuffle (MkWord64X8WithVec128 x0 x1 x2 x3) (MkWord64X8WithVec128 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord64X8WithVec128 (shuffleMany# @_ @_ @[i0, i1] sources) (shuffleMany# @_ @_ @[i2, i3] sources) (shuffleMany# @_ @_ @[i4, i5] sources) (shuffleMany# @_ @_ @[i6, i7] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word64 where
  eqF (MkWord64X8WithVec128 u0 u1 u2 u3) (MkWord64X8WithVec128 v0 v1 v2 v3) = MkBoolX8 $ (fromIntegral (eqWord64X2# u0 v0)) .|. (fromIntegral (eqWord64X2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (eqWord64X2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (eqWord64X2# u3 v3) `unsafeShiftL` 6)
  {-# INLINE eqF #-}
instance OrderedF X8 Word64 where
  ltF (MkWord64X8WithVec128 u0 u1 u2 u3) (MkWord64X8WithVec128 v0 v1 v2 v3) = MkBoolX8 $ (fromIntegral (ltWord64X2# u0 v0)) .|. (fromIntegral (ltWord64X2# u1 v1) `unsafeShiftL` 2) .|. (fromIntegral (ltWord64X2# u2 v2) `unsafeShiftL` 4) .|. (fromIntegral (ltWord64X2# u3 v3) `unsafeShiftL` 6)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word64 where
  minF (MkWord64X8WithVec128 u0 u1 u2 u3) (MkWord64X8WithVec128 v0 v1 v2 v3) = MkWord64X8WithVec128 (minWord64X2# u0 v0) (minWord64X2# u1 v1) (minWord64X2# u2 v2) (minWord64X2# u3 v3)
  maxF (MkWord64X8WithVec128 u0 u1 u2 u3) (MkWord64X8WithVec128 v0 v1 v2 v3) = MkWord64X8WithVec128 (maxWord64X2# u0 v0) (maxWord64X2# u1 v1) (maxWord64X2# u2 v2) (maxWord64X2# u3 v3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word64 where
  plusF (MkWord64X8WithVec128 u0 u1 u2 u3) (MkWord64X8WithVec128 v0 v1 v2 v3) = MkWord64X8WithVec128 (plusWord64X2# u0 v0) (plusWord64X2# u1 v1) (plusWord64X2# u2 v2) (plusWord64X2# u3 v3)
  minusF (MkWord64X8WithVec128 u0 u1 u2 u3) (MkWord64X8WithVec128 v0 v1 v2 v3) = MkWord64X8WithVec128 (minusWord64X2# u0 v0) (minusWord64X2# u1 v1) (minusWord64X2# u2 v2) (minusWord64X2# u3 v3)
  timesF (MkWord64X8WithVec128 u0 u1 u2 u3) (MkWord64X8WithVec128 v0 v1 v2 v3) = MkWord64X8WithVec128 (timesWord64X2# u0 v0) (timesWord64X2# u1 v1) (timesWord64X2# u2 v2) (timesWord64X2# u3 v3)
  -- Currently, there is no negateWord64X2#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Word64 where
  andF (MkWord64X8WithVec128 u0 u1 u2 u3) (MkWord64X8WithVec128 v0 v1 v2 v3) = MkWord64X8WithVec128 (andWord64X2# u0 v0) (andWord64X2# u1 v1) (andWord64X2# u2 v2) (andWord64X2# u3 v3)
  orF (MkWord64X8WithVec128 u0 u1 u2 u3) (MkWord64X8WithVec128 v0 v1 v2 v3) = MkWord64X8WithVec128 (orWord64X2# u0 v0) (orWord64X2# u1 v1) (orWord64X2# u2 v2) (orWord64X2# u3 v3)
  xorF (MkWord64X8WithVec128 u0 u1 u2 u3) (MkWord64X8WithVec128 v0 v1 v2 v3) = MkWord64X8WithVec128 (xorWord64X2# u0 v0) (xorWord64X2# u1 v1) (xorWord64X2# u2 v2) (xorWord64X2# u3 v3)
  complementF (MkWord64X8WithVec128 u0 u1 u2 u3) = MkWord64X8WithVec128 (complementWord64X2# u0) (complementWord64X2# u1) (complementWord64X2# u2) (complementWord64X2# u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word64 where
  shiftLF (MkWord64X8WithVec128 u0 u1 u2 u3) (I# i) = MkWord64X8WithVec128 (shiftLWord64X2# u0 i) (shiftLWord64X2# u1 i) (shiftLWord64X2# u2 i) (shiftLWord64X2# u3 i)
  shiftRF (MkWord64X8WithVec128 u0 u1 u2 u3) (I# i) = MkWord64X8WithVec128 (shiftRWord64X2# u0 i) (shiftRWord64X2# u1 i) (shiftRWord64X2# u2 i) (shiftRWord64X2# u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Word64 where
  enumFromZero = MkWord64X8WithVec128 (packWord64X2# (# 0#Word64, 1#Word64 #)) (packWord64X2# (# 2#Word64, 3#Word64 #)) (packWord64X2# (# 4#Word64, 5#Word64 #)) (packWord64X2# (# 6#Word64, 7#Word64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word64 where
  indexByteArraySIMD# ba i = MkWord64X8WithVec128 (indexWord64ArrayAsWord64X2# ba i) (indexWord64ArrayAsWord64X2# ba (i +# 2#)) (indexWord64ArrayAsWord64X2# ba (i +# 4#)) (indexWord64ArrayAsWord64X2# ba (i +# 6#))
  readByteArraySIMD# mba i s0 = case readWord64ArrayAsWord64X2# mba i s0 of (# s1, v0 #) -> case readWord64ArrayAsWord64X2# mba (i +# 2#) s1 of (# s2, v1 #) -> case readWord64ArrayAsWord64X2# mba (i +# 4#) s2 of (# s3, v2 #) -> case readWord64ArrayAsWord64X2# mba (i +# 6#) s3 of (# s4, v3 #) -> (# s4, MkWord64X8WithVec128 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkWord64X8WithVec128 v0 v1 v2 v3) s0 = case writeWord64ArrayAsWord64X2# mba i v0 s0 of s1 -> case writeWord64ArrayAsWord64X2# mba (i +# 2#) v1 s1 of s2 -> case writeWord64ArrayAsWord64X2# mba (i +# 4#) v2 s2 of s3 -> writeWord64ArrayAsWord64X2# mba (i +# 6#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord64OffAddrAsWord64X2# addr i s0 of (# s1, v0 #) -> case readWord64OffAddrAsWord64X2# addr (i +# 2#) s1 of (# s2, v1 #) -> case readWord64OffAddrAsWord64X2# addr (i +# 4#) s2 of (# s3, v2 #) -> case readWord64OffAddrAsWord64X2# addr (i +# 6#) s3 of (# s4, v3 #) -> (# s4, MkWord64X8WithVec128 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X8WithVec128 v0 v1 v2 v3) = IO (\s0 -> case writeWord64OffAddrAsWord64X2# addr i v0 s0 of s1 -> case writeWord64OffAddrAsWord64X2# addr (i +# 2#) v1 s1 of s2 -> case writeWord64OffAddrAsWord64X2# addr (i +# 4#) v2 s2 of s3 -> case writeWord64OffAddrAsWord64X2# addr (i +# 6#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
#else
-- The NCG of GHC 9.12 does not support integer vectors
instance ImplementationDescription X8 where
  implementationDescription _ = "X8;maxBits(Float,Double)=128,maxBits(other)=0"
data instance X8 Int8 = MkInt8X8WithElems !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8
instance PackX8 X8 Int8 where
  mkX8 = MkInt8X8WithElems
  unpackX8 (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int8 where
  broadcast !x = MkInt8X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Int8 where
  selectF (MkBoolX8 !cond) (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Int8 i0, Pick Int8 i1, Pick Int8 i2, Pick Int8 i3, Pick Int8 i4, Pick Int8 i5, Pick Int8 i6, Pick Int8 i7) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int8 where
  unaryShuffle (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Int8 i0, Pick Int8 i1, Pick Int8 i2, Pick Int8 i3, Pick Int8 i4, Pick Int8 i5, Pick Int8 i6, Pick Int8 i7) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int8 where
  binaryShuffle (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int8 where
  eqF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Int8 where
  ltF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int8 where
  minF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int8 where
  plusF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt8X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Int8 where
  andF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt8X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int8 where
  shiftLF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt8X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt8X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt8X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt8X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Int8 where
  enumFromZero = MkInt8X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int8 where
  indexByteArraySIMD# ba i = MkInt8X8WithElems (I8# (GHC.Exts.indexInt8Array# ba i)) (I8# (GHC.Exts.indexInt8Array# ba (i +# 1#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 2#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 3#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 4#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 5#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 6#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt8Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt8Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt8Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt8Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt8X8WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) #)
  writeByteArraySIMD# mba i (MkInt8X8WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7)) s0 = case GHC.Exts.writeInt8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt8Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt8Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt8Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeInt8Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt8X8WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X8WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7)) = IO (\s0 -> case GHC.Exts.writeInt8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeInt8OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int16 = MkInt16X8WithElems !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16
instance PackX8 X8 Int16 where
  mkX8 = MkInt16X8WithElems
  unpackX8 (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int16 where
  broadcast !x = MkInt16X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Int16 where
  selectF (MkBoolX8 !cond) (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Int16 i0, Pick Int16 i1, Pick Int16 i2, Pick Int16 i3, Pick Int16 i4, Pick Int16 i5, Pick Int16 i6, Pick Int16 i7) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int16 where
  unaryShuffle (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Int16 i0, Pick Int16 i1, Pick Int16 i2, Pick Int16 i3, Pick Int16 i4, Pick Int16 i5, Pick Int16 i6, Pick Int16 i7) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int16 where
  binaryShuffle (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int16 where
  eqF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Int16 where
  ltF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int16 where
  minF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int16 where
  plusF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt16X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Int16 where
  andF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt16X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int16 where
  shiftLF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt16X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt16X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt16X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt16X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Int16 where
  enumFromZero = MkInt16X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int16 where
  indexByteArraySIMD# ba i = MkInt16X8WithElems (I16# (GHC.Exts.indexInt16Array# ba i)) (I16# (GHC.Exts.indexInt16Array# ba (i +# 1#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 2#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 3#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 4#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 5#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 6#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt16Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt16Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt16Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt16Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt16Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt16Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt16X8WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) #)
  writeByteArraySIMD# mba i (MkInt16X8WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7)) s0 = case GHC.Exts.writeInt16Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt16Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt16Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt16Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt16Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt16Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt16Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeInt16Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt16X8WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X8WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7)) = IO (\s0 -> case GHC.Exts.writeInt16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeInt16OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int32 = MkInt32X8WithElems !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32
instance PackX8 X8 Int32 where
  mkX8 = MkInt32X8WithElems
  unpackX8 (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int32 where
  broadcast !x = MkInt32X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Int32 where
  selectF (MkBoolX8 !cond) (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Int32 i0, Pick Int32 i1, Pick Int32 i2, Pick Int32 i3, Pick Int32 i4, Pick Int32 i5, Pick Int32 i6, Pick Int32 i7) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int32 where
  unaryShuffle (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Int32 i0, Pick Int32 i1, Pick Int32 i2, Pick Int32 i3, Pick Int32 i4, Pick Int32 i5, Pick Int32 i6, Pick Int32 i7) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int32 where
  binaryShuffle (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int32 where
  eqF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Int32 where
  ltF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int32 where
  minF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int32 where
  plusF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt32X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Int32 where
  andF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt32X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int32 where
  shiftLF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt32X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt32X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt32X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt32X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Int32 where
  enumFromZero = MkInt32X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int32 where
  indexByteArraySIMD# ba i = MkInt32X8WithElems (I32# (GHC.Exts.indexInt32Array# ba i)) (I32# (GHC.Exts.indexInt32Array# ba (i +# 1#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 2#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 3#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 4#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 5#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 6#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt32Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt32Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt32Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt32Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt32Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt32Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt32X8WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) #)
  writeByteArraySIMD# mba i (MkInt32X8WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7)) s0 = case GHC.Exts.writeInt32Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt32Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt32Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt32Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt32Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt32Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt32Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeInt32Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt32X8WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X8WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7)) = IO (\s0 -> case GHC.Exts.writeInt32OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeInt32OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int64 = MkInt64X8WithElems !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64
instance PackX8 X8 Int64 where
  mkX8 = MkInt64X8WithElems
  unpackX8 (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int64 where
  broadcast !x = MkInt64X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Int64 where
  selectF (MkBoolX8 !cond) (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Int64 i0, Pick Int64 i1, Pick Int64 i2, Pick Int64 i3, Pick Int64 i4, Pick Int64 i5, Pick Int64 i6, Pick Int64 i7) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int64 where
  unaryShuffle (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Int64 i0, Pick Int64 i1, Pick Int64 i2, Pick Int64 i3, Pick Int64 i4, Pick Int64 i5, Pick Int64 i6, Pick Int64 i7) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int64 where
  binaryShuffle (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int64 where
  eqF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Int64 where
  ltF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int64 where
  minF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int64 where
  plusF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt64X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Int64 where
  andF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt64X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int64 where
  shiftLF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt64X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt64X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt64X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt64X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Int64 where
  enumFromZero = MkInt64X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int64 where
  indexByteArraySIMD# ba i = MkInt64X8WithElems (I64# (GHC.Exts.indexInt64Array# ba i)) (I64# (GHC.Exts.indexInt64Array# ba (i +# 1#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 2#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 3#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 4#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 5#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 6#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt64Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt64Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt64Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt64Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt64Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt64Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt64Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt64Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt64X8WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) #)
  writeByteArraySIMD# mba i (MkInt64X8WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7)) s0 = case GHC.Exts.writeInt64Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt64Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt64Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt64Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt64Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt64Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt64Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeInt64Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt64OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt64X8WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X8WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7)) = IO (\s0 -> case GHC.Exts.writeInt64OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeInt64OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word8 = MkWord8X8WithElems !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
instance PackX8 X8 Word8 where
  mkX8 = MkWord8X8WithElems
  unpackX8 (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word8 where
  broadcast !x = MkWord8X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Word8 where
  selectF (MkBoolX8 !cond) (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Word8 i0, Pick Word8 i1, Pick Word8 i2, Pick Word8 i3, Pick Word8 i4, Pick Word8 i5, Pick Word8 i6, Pick Word8 i7) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word8 where
  unaryShuffle (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Word8 i0, Pick Word8 i1, Pick Word8 i2, Pick Word8 i3, Pick Word8 i4, Pick Word8 i5, Pick Word8 i6, Pick Word8 i7) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word8 where
  binaryShuffle (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word8 where
  eqF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Word8 where
  ltF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word8 where
  minF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word8 where
  plusF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord8X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Word8 where
  andF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord8X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word8 where
  shiftLF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord8X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord8X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord8X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord8X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Word8 where
  enumFromZero = MkWord8X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word8 where
  indexByteArraySIMD# ba i = MkWord8X8WithElems (W8# (GHC.Exts.indexWord8Array# ba i)) (W8# (GHC.Exts.indexWord8Array# ba (i +# 1#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 2#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 3#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 4#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 5#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 6#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord8Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord8Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord8Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord8Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord8X8WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) #)
  writeByteArraySIMD# mba i (MkWord8X8WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7)) s0 = case GHC.Exts.writeWord8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord8Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord8Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord8Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeWord8Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord8X8WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X8WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7)) = IO (\s0 -> case GHC.Exts.writeWord8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeWord8OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word16 = MkWord16X8WithElems !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16
instance PackX8 X8 Word16 where
  mkX8 = MkWord16X8WithElems
  unpackX8 (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word16 where
  broadcast !x = MkWord16X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Word16 where
  selectF (MkBoolX8 !cond) (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Word16 i0, Pick Word16 i1, Pick Word16 i2, Pick Word16 i3, Pick Word16 i4, Pick Word16 i5, Pick Word16 i6, Pick Word16 i7) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word16 where
  unaryShuffle (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Word16 i0, Pick Word16 i1, Pick Word16 i2, Pick Word16 i3, Pick Word16 i4, Pick Word16 i5, Pick Word16 i6, Pick Word16 i7) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word16 where
  binaryShuffle (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word16 where
  eqF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Word16 where
  ltF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word16 where
  minF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word16 where
  plusF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord16X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Word16 where
  andF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord16X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word16 where
  shiftLF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord16X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord16X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord16X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord16X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Word16 where
  enumFromZero = MkWord16X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word16 where
  indexByteArraySIMD# ba i = MkWord16X8WithElems (W16# (GHC.Exts.indexWord16Array# ba i)) (W16# (GHC.Exts.indexWord16Array# ba (i +# 1#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 2#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 3#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 4#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 5#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 6#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord16Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord16Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord16Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord16Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord16Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord16Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord16X8WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) #)
  writeByteArraySIMD# mba i (MkWord16X8WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7)) s0 = case GHC.Exts.writeWord16Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord16Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord16Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord16Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord16Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord16Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord16Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeWord16Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord16X8WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X8WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7)) = IO (\s0 -> case GHC.Exts.writeWord16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeWord16OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word32 = MkWord32X8WithElems !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32
instance PackX8 X8 Word32 where
  mkX8 = MkWord32X8WithElems
  unpackX8 (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word32 where
  broadcast !x = MkWord32X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Word32 where
  selectF (MkBoolX8 !cond) (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Word32 i0, Pick Word32 i1, Pick Word32 i2, Pick Word32 i3, Pick Word32 i4, Pick Word32 i5, Pick Word32 i6, Pick Word32 i7) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word32 where
  unaryShuffle (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Word32 i0, Pick Word32 i1, Pick Word32 i2, Pick Word32 i3, Pick Word32 i4, Pick Word32 i5, Pick Word32 i6, Pick Word32 i7) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word32 where
  binaryShuffle (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word32 where
  eqF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Word32 where
  ltF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word32 where
  minF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word32 where
  plusF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord32X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Word32 where
  andF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord32X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word32 where
  shiftLF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord32X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord32X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord32X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord32X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Word32 where
  enumFromZero = MkWord32X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word32 where
  indexByteArraySIMD# ba i = MkWord32X8WithElems (W32# (GHC.Exts.indexWord32Array# ba i)) (W32# (GHC.Exts.indexWord32Array# ba (i +# 1#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 2#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 3#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 4#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 5#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 6#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord32Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord32Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord32Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord32Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord32Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord32Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord32X8WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) #)
  writeByteArraySIMD# mba i (MkWord32X8WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7)) s0 = case GHC.Exts.writeWord32Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord32Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord32Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord32Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord32Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord32Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord32Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeWord32Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord32X8WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X8WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7)) = IO (\s0 -> case GHC.Exts.writeWord32OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeWord32OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word64 = MkWord64X8WithElems !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64
instance PackX8 X8 Word64 where
  mkX8 = MkWord64X8WithElems
  unpackX8 (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word64 where
  broadcast !x = MkWord64X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Word64 where
  selectF (MkBoolX8 !cond) (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Word64 i0, Pick Word64 i1, Pick Word64 i2, Pick Word64 i3, Pick Word64 i4, Pick Word64 i5, Pick Word64 i6, Pick Word64 i7) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word64 where
  unaryShuffle (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Word64 i0, Pick Word64 i1, Pick Word64 i2, Pick Word64 i3, Pick Word64 i4, Pick Word64 i5, Pick Word64 i6, Pick Word64 i7) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word64 where
  binaryShuffle (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word64 where
  eqF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Word64 where
  ltF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word64 where
  minF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word64 where
  plusF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord64X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Word64 where
  andF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord64X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word64 where
  shiftLF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord64X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord64X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord64X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord64X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Word64 where
  enumFromZero = MkWord64X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word64 where
  indexByteArraySIMD# ba i = MkWord64X8WithElems (W64# (GHC.Exts.indexWord64Array# ba i)) (W64# (GHC.Exts.indexWord64Array# ba (i +# 1#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 2#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 3#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 4#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 5#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 6#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord64Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord64Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord64Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord64Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord64Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord64Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord64Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord64Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord64X8WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) #)
  writeByteArraySIMD# mba i (MkWord64X8WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7)) s0 = case GHC.Exts.writeWord64Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord64Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord64Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord64Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord64Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord64Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord64Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeWord64Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord64OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord64X8WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X8WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7)) = IO (\s0 -> case GHC.Exts.writeWord64OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeWord64OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
#endif
newtype instance X8 (Sum a) = MkSumX8 (X8 a)
instance PackX8 X8 a => PackX8 X8 (Sum a) where
  mkX8 = coerce (mkX8 @X8 @a)
  unpackX8 = coerce (unpackX8 @X8 @a)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 a => Broadcast X8 (Sum a) where
  broadcast = coerce (broadcast @X8 @a)
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X8 (Sum a) where
  selectF = coerce (selectF @X8 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 a => UnaryShuffleT indices X8 (Sum a) where
  unaryShuffle = coerce (unaryShuffle @indices @X8 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 a => BinaryShuffleT indices X8 (Sum a) where
  binaryShuffle = coerce (binaryShuffle @indices @X8 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X8 (Product a) = MkProductX8 (X8 a)
instance PackX8 X8 a => PackX8 X8 (Product a) where
  mkX8 = coerce (mkX8 @X8 @a)
  unpackX8 = coerce (unpackX8 @X8 @a)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 a => Broadcast X8 (Product a) where
  broadcast = coerce (broadcast @X8 @a)
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X8 (Product a) where
  selectF = coerce (selectF @X8 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 a => UnaryShuffleT indices X8 (Product a) where
  unaryShuffle = coerce (unaryShuffle @indices @X8 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 a => BinaryShuffleT indices X8 (Product a) where
  binaryShuffle = coerce (binaryShuffle @indices @X8 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X8 (Min a) = MkMinX8 (X8 a)
instance PackX8 X8 a => PackX8 X8 (Min a) where
  mkX8 = coerce (mkX8 @X8 @a)
  unpackX8 = coerce (unpackX8 @X8 @a)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 a => Broadcast X8 (Min a) where
  broadcast = coerce (broadcast @X8 @a)
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X8 (Min a) where
  selectF = coerce (selectF @X8 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 a => UnaryShuffleT indices X8 (Min a) where
  unaryShuffle = coerce (unaryShuffle @indices @X8 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 a => BinaryShuffleT indices X8 (Min a) where
  binaryShuffle = coerce (binaryShuffle @indices @X8 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X8 (Max a) = MkMaxX8 (X8 a)
instance PackX8 X8 a => PackX8 X8 (Max a) where
  mkX8 = coerce (mkX8 @X8 @a)
  unpackX8 = coerce (unpackX8 @X8 @a)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 a => Broadcast X8 (Max a) where
  broadcast = coerce (broadcast @X8 @a)
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X8 (Max a) where
  selectF = coerce (selectF @X8 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 a => UnaryShuffleT indices X8 (Max a) where
  unaryShuffle = coerce (unaryShuffle @indices @X8 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 a => BinaryShuffleT indices X8 (Max a) where
  binaryShuffle = coerce (binaryShuffle @indices @X8 @a)
  {-# INLINE binaryShuffle #-}
data instance X8 (Complex a) = MkComplexX8 !(X8 a) !(X8 a)
instance PackX8 X8 a => PackX8 X8 (Complex a) where
  mkX8 (x0 :+ y0) (x1 :+ y1) (x2 :+ y2) (x3 :+ y3) (x4 :+ y4) (x5 :+ y5) (x6 :+ y6) (x7 :+ y7) = MkComplexX8 (mkX8 x0 x1 x2 x3 x4 x5 x6 x7) (mkX8 y0 y1 y2 y3 y4 y5 y6 y7)
  unpackX8 (MkComplexX8 s t) = case unpackX8 s of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 t of (y0, y1, y2, y3, y4, y5, y6, y7) -> (x0 :+ y0, x1 :+ y1, x2 :+ y2, x3 :+ y3, x4 :+ y4, x5 :+ y5, x6 :+ y6, x7 :+ y7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 a => Broadcast X8 (Complex a) where
  broadcast (x :+ y) = MkComplexX8 (broadcast x) (broadcast y)
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X8 (Complex a) where
  selectF !cond (MkComplexX8 x y) (MkComplexX8 x' y') = MkComplexX8 (selectF cond x x') (selectF cond y y')
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 a => UnaryShuffleT indices X8 (Complex a) where
  unaryShuffle (MkComplexX8 x y) = MkComplexX8 (unaryShuffle @indices x) (unaryShuffle @indices y)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 a => BinaryShuffleT indices X8 (Complex a) where
  binaryShuffle (MkComplexX8 x y) (MkComplexX8 u v) = MkComplexX8 (binaryShuffle @indices x u) (binaryShuffle @indices y v)
  {-# INLINE binaryShuffle #-}
data instance X8 () = MkUnitX8
instance PackX8 X8 () where
  mkX8 _ _ _ _ _ _ _ _ = MkUnitX8
  unpackX8 MkUnitX8 = ((), (), (), (), (), (), (), ())
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 () where
  broadcast _ = MkUnitX8
  {-# INLINE broadcast #-}
instance SelectableF X8 () where
  selectF _ _ _ = MkUnitX8
  {-# INLINE selectF #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8) => UnaryShuffleT '[i0, i1, i2, i3, i4, i5, i6, i7] X8 () where
  unaryShuffle _ = MkUnitX8
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16) => BinaryShuffleT '[i0, i1, i2, i3, i4, i5, i6, i7] X8 () where
  binaryShuffle _ _ = MkUnitX8
  {-# INLINE binaryShuffle #-}
data instance X8 (a0, a1) = MkTuple2X8 !(X8 a0) !(X8 a1)
instance (PackX8 X8 a0, PackX8 X8 a1) => PackX8 X8 (a0, a1) where
  mkX8 (x0_0, x0_1) (x1_0, x1_1) (x2_0, x2_1) (x3_0, x3_1) (x4_0, x4_1) (x5_0, x5_1) (x6_0, x6_1) (x7_0, x7_1) = MkTuple2X8 (mkX8 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0) (mkX8 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1)
  unpackX8 (MkTuple2X8 v0 v1) = case unpackX8 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0) -> case unpackX8 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1) -> ((x0_0, x0_1), (x1_0, x1_1), (x2_0, x2_1), (x3_0, x3_1), (x4_0, x4_1), (x5_0, x5_1), (x6_0, x6_1), (x7_0, x7_1))
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance (Broadcast X8 a0, Broadcast X8 a1) => Broadcast X8 (a0, a1) where
  broadcast (x0, x1) = MkTuple2X8 (broadcast x0) (broadcast x1)
  {-# INLINE broadcast #-}
instance (SelectableF X8 a0, SelectableF X8 a1) => SelectableF X8 (a0, a1) where
  selectF !cond (MkTuple2X8 x0 x1) (MkTuple2X8 y0 y1) = MkTuple2X8 (selectF cond x0 y0) (selectF cond x1 y1)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X8 a0, UnaryShuffleT indices X8 a1) => UnaryShuffleT indices X8 (a0, a1) where
  unaryShuffle (MkTuple2X8 x0 x1) = MkTuple2X8 (unaryShuffle @indices x0) (unaryShuffle @indices x1)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X8 a0, BinaryShuffleT indices X8 a1) => BinaryShuffleT indices X8 (a0, a1) where
  binaryShuffle (MkTuple2X8 x0 x1) (MkTuple2X8 y0 y1) = MkTuple2X8 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1)
  {-# INLINE binaryShuffle #-}
data instance X8 (a0, a1, a2) = MkTuple3X8 !(X8 a0) !(X8 a1) !(X8 a2)
instance (PackX8 X8 a0, PackX8 X8 a1, PackX8 X8 a2) => PackX8 X8 (a0, a1, a2) where
  mkX8 (x0_0, x0_1, x0_2) (x1_0, x1_1, x1_2) (x2_0, x2_1, x2_2) (x3_0, x3_1, x3_2) (x4_0, x4_1, x4_2) (x5_0, x5_1, x5_2) (x6_0, x6_1, x6_2) (x7_0, x7_1, x7_2) = MkTuple3X8 (mkX8 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0) (mkX8 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1) (mkX8 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2)
  unpackX8 (MkTuple3X8 v0 v1 v2) = case unpackX8 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0) -> case unpackX8 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1) -> case unpackX8 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2) -> ((x0_0, x0_1, x0_2), (x1_0, x1_1, x1_2), (x2_0, x2_1, x2_2), (x3_0, x3_1, x3_2), (x4_0, x4_1, x4_2), (x5_0, x5_1, x5_2), (x6_0, x6_1, x6_2), (x7_0, x7_1, x7_2))
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance (Broadcast X8 a0, Broadcast X8 a1, Broadcast X8 a2) => Broadcast X8 (a0, a1, a2) where
  broadcast (x0, x1, x2) = MkTuple3X8 (broadcast x0) (broadcast x1) (broadcast x2)
  {-# INLINE broadcast #-}
instance (SelectableF X8 a0, SelectableF X8 a1, SelectableF X8 a2) => SelectableF X8 (a0, a1, a2) where
  selectF !cond (MkTuple3X8 x0 x1 x2) (MkTuple3X8 y0 y1 y2) = MkTuple3X8 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X8 a0, UnaryShuffleT indices X8 a1, UnaryShuffleT indices X8 a2) => UnaryShuffleT indices X8 (a0, a1, a2) where
  unaryShuffle (MkTuple3X8 x0 x1 x2) = MkTuple3X8 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X8 a0, BinaryShuffleT indices X8 a1, BinaryShuffleT indices X8 a2) => BinaryShuffleT indices X8 (a0, a1, a2) where
  binaryShuffle (MkTuple3X8 x0 x1 x2) (MkTuple3X8 y0 y1 y2) = MkTuple3X8 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2)
  {-# INLINE binaryShuffle #-}
data instance X8 (a0, a1, a2, a3) = MkTuple4X8 !(X8 a0) !(X8 a1) !(X8 a2) !(X8 a3)
instance (PackX8 X8 a0, PackX8 X8 a1, PackX8 X8 a2, PackX8 X8 a3) => PackX8 X8 (a0, a1, a2, a3) where
  mkX8 (x0_0, x0_1, x0_2, x0_3) (x1_0, x1_1, x1_2, x1_3) (x2_0, x2_1, x2_2, x2_3) (x3_0, x3_1, x3_2, x3_3) (x4_0, x4_1, x4_2, x4_3) (x5_0, x5_1, x5_2, x5_3) (x6_0, x6_1, x6_2, x6_3) (x7_0, x7_1, x7_2, x7_3) = MkTuple4X8 (mkX8 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0) (mkX8 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1) (mkX8 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2) (mkX8 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3)
  unpackX8 (MkTuple4X8 v0 v1 v2 v3) = case unpackX8 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0) -> case unpackX8 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1) -> case unpackX8 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2) -> case unpackX8 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3) -> ((x0_0, x0_1, x0_2, x0_3), (x1_0, x1_1, x1_2, x1_3), (x2_0, x2_1, x2_2, x2_3), (x3_0, x3_1, x3_2, x3_3), (x4_0, x4_1, x4_2, x4_3), (x5_0, x5_1, x5_2, x5_3), (x6_0, x6_1, x6_2, x6_3), (x7_0, x7_1, x7_2, x7_3))
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance (Broadcast X8 a0, Broadcast X8 a1, Broadcast X8 a2, Broadcast X8 a3) => Broadcast X8 (a0, a1, a2, a3) where
  broadcast (x0, x1, x2, x3) = MkTuple4X8 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3)
  {-# INLINE broadcast #-}
instance (SelectableF X8 a0, SelectableF X8 a1, SelectableF X8 a2, SelectableF X8 a3) => SelectableF X8 (a0, a1, a2, a3) where
  selectF !cond (MkTuple4X8 x0 x1 x2 x3) (MkTuple4X8 y0 y1 y2 y3) = MkTuple4X8 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X8 a0, UnaryShuffleT indices X8 a1, UnaryShuffleT indices X8 a2, UnaryShuffleT indices X8 a3) => UnaryShuffleT indices X8 (a0, a1, a2, a3) where
  unaryShuffle (MkTuple4X8 x0 x1 x2 x3) = MkTuple4X8 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X8 a0, BinaryShuffleT indices X8 a1, BinaryShuffleT indices X8 a2, BinaryShuffleT indices X8 a3) => BinaryShuffleT indices X8 (a0, a1, a2, a3) where
  binaryShuffle (MkTuple4X8 x0 x1 x2 x3) (MkTuple4X8 y0 y1 y2 y3) = MkTuple4X8 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3)
  {-# INLINE binaryShuffle #-}
data instance X8 (a0, a1, a2, a3, a4) = MkTuple5X8 !(X8 a0) !(X8 a1) !(X8 a2) !(X8 a3) !(X8 a4)
instance (PackX8 X8 a0, PackX8 X8 a1, PackX8 X8 a2, PackX8 X8 a3, PackX8 X8 a4) => PackX8 X8 (a0, a1, a2, a3, a4) where
  mkX8 (x0_0, x0_1, x0_2, x0_3, x0_4) (x1_0, x1_1, x1_2, x1_3, x1_4) (x2_0, x2_1, x2_2, x2_3, x2_4) (x3_0, x3_1, x3_2, x3_3, x3_4) (x4_0, x4_1, x4_2, x4_3, x4_4) (x5_0, x5_1, x5_2, x5_3, x5_4) (x6_0, x6_1, x6_2, x6_3, x6_4) (x7_0, x7_1, x7_2, x7_3, x7_4) = MkTuple5X8 (mkX8 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0) (mkX8 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1) (mkX8 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2) (mkX8 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3) (mkX8 x0_4 x1_4 x2_4 x3_4 x4_4 x5_4 x6_4 x7_4)
  unpackX8 (MkTuple5X8 v0 v1 v2 v3 v4) = case unpackX8 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0) -> case unpackX8 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1) -> case unpackX8 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2) -> case unpackX8 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3) -> case unpackX8 v4 of (x0_4, x1_4, x2_4, x3_4, x4_4, x5_4, x6_4, x7_4) -> ((x0_0, x0_1, x0_2, x0_3, x0_4), (x1_0, x1_1, x1_2, x1_3, x1_4), (x2_0, x2_1, x2_2, x2_3, x2_4), (x3_0, x3_1, x3_2, x3_3, x3_4), (x4_0, x4_1, x4_2, x4_3, x4_4), (x5_0, x5_1, x5_2, x5_3, x5_4), (x6_0, x6_1, x6_2, x6_3, x6_4), (x7_0, x7_1, x7_2, x7_3, x7_4))
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance (Broadcast X8 a0, Broadcast X8 a1, Broadcast X8 a2, Broadcast X8 a3, Broadcast X8 a4) => Broadcast X8 (a0, a1, a2, a3, a4) where
  broadcast (x0, x1, x2, x3, x4) = MkTuple5X8 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4)
  {-# INLINE broadcast #-}
instance (SelectableF X8 a0, SelectableF X8 a1, SelectableF X8 a2, SelectableF X8 a3, SelectableF X8 a4) => SelectableF X8 (a0, a1, a2, a3, a4) where
  selectF !cond (MkTuple5X8 x0 x1 x2 x3 x4) (MkTuple5X8 y0 y1 y2 y3 y4) = MkTuple5X8 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X8 a0, UnaryShuffleT indices X8 a1, UnaryShuffleT indices X8 a2, UnaryShuffleT indices X8 a3, UnaryShuffleT indices X8 a4) => UnaryShuffleT indices X8 (a0, a1, a2, a3, a4) where
  unaryShuffle (MkTuple5X8 x0 x1 x2 x3 x4) = MkTuple5X8 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X8 a0, BinaryShuffleT indices X8 a1, BinaryShuffleT indices X8 a2, BinaryShuffleT indices X8 a3, BinaryShuffleT indices X8 a4) => BinaryShuffleT indices X8 (a0, a1, a2, a3, a4) where
  binaryShuffle (MkTuple5X8 x0 x1 x2 x3 x4) (MkTuple5X8 y0 y1 y2 y3 y4) = MkTuple5X8 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4)
  {-# INLINE binaryShuffle #-}
data instance X8 (a0, a1, a2, a3, a4, a5) = MkTuple6X8 !(X8 a0) !(X8 a1) !(X8 a2) !(X8 a3) !(X8 a4) !(X8 a5)
instance (PackX8 X8 a0, PackX8 X8 a1, PackX8 X8 a2, PackX8 X8 a3, PackX8 X8 a4, PackX8 X8 a5) => PackX8 X8 (a0, a1, a2, a3, a4, a5) where
  mkX8 (x0_0, x0_1, x0_2, x0_3, x0_4, x0_5) (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5) (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5) (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5) (x4_0, x4_1, x4_2, x4_3, x4_4, x4_5) (x5_0, x5_1, x5_2, x5_3, x5_4, x5_5) (x6_0, x6_1, x6_2, x6_3, x6_4, x6_5) (x7_0, x7_1, x7_2, x7_3, x7_4, x7_5) = MkTuple6X8 (mkX8 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0) (mkX8 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1) (mkX8 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2) (mkX8 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3) (mkX8 x0_4 x1_4 x2_4 x3_4 x4_4 x5_4 x6_4 x7_4) (mkX8 x0_5 x1_5 x2_5 x3_5 x4_5 x5_5 x6_5 x7_5)
  unpackX8 (MkTuple6X8 v0 v1 v2 v3 v4 v5) = case unpackX8 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0) -> case unpackX8 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1) -> case unpackX8 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2) -> case unpackX8 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3) -> case unpackX8 v4 of (x0_4, x1_4, x2_4, x3_4, x4_4, x5_4, x6_4, x7_4) -> case unpackX8 v5 of (x0_5, x1_5, x2_5, x3_5, x4_5, x5_5, x6_5, x7_5) -> ((x0_0, x0_1, x0_2, x0_3, x0_4, x0_5), (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5), (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5), (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5), (x4_0, x4_1, x4_2, x4_3, x4_4, x4_5), (x5_0, x5_1, x5_2, x5_3, x5_4, x5_5), (x6_0, x6_1, x6_2, x6_3, x6_4, x6_5), (x7_0, x7_1, x7_2, x7_3, x7_4, x7_5))
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance (Broadcast X8 a0, Broadcast X8 a1, Broadcast X8 a2, Broadcast X8 a3, Broadcast X8 a4, Broadcast X8 a5) => Broadcast X8 (a0, a1, a2, a3, a4, a5) where
  broadcast (x0, x1, x2, x3, x4, x5) = MkTuple6X8 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4) (broadcast x5)
  {-# INLINE broadcast #-}
instance (SelectableF X8 a0, SelectableF X8 a1, SelectableF X8 a2, SelectableF X8 a3, SelectableF X8 a4, SelectableF X8 a5) => SelectableF X8 (a0, a1, a2, a3, a4, a5) where
  selectF !cond (MkTuple6X8 x0 x1 x2 x3 x4 x5) (MkTuple6X8 y0 y1 y2 y3 y4 y5) = MkTuple6X8 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4) (selectF cond x5 y5)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X8 a0, UnaryShuffleT indices X8 a1, UnaryShuffleT indices X8 a2, UnaryShuffleT indices X8 a3, UnaryShuffleT indices X8 a4, UnaryShuffleT indices X8 a5) => UnaryShuffleT indices X8 (a0, a1, a2, a3, a4, a5) where
  unaryShuffle (MkTuple6X8 x0 x1 x2 x3 x4 x5) = MkTuple6X8 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4) (unaryShuffle @indices x5)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X8 a0, BinaryShuffleT indices X8 a1, BinaryShuffleT indices X8 a2, BinaryShuffleT indices X8 a3, BinaryShuffleT indices X8 a4, BinaryShuffleT indices X8 a5) => BinaryShuffleT indices X8 (a0, a1, a2, a3, a4, a5) where
  binaryShuffle (MkTuple6X8 x0 x1 x2 x3 x4 x5) (MkTuple6X8 y0 y1 y2 y3 y4 y5) = MkTuple6X8 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4) (binaryShuffle @indices x5 y5)
  {-# INLINE binaryShuffle #-}
instance (PackX8 X8 a, PackX8 X8 b) => LiftSIMD X8 a b where
  liftSIMD f !v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> mkX8 (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7)
  {-# INLINE liftSIMD #-}
instance (PackX8 X8 a, PackX8 X8 b, PackX8 X8 c) => LiftSIMD2 X8 a b c where
  liftSIMD2 f !u !v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (y0, y1, y2, y3, y4, y5, y6, y7) -> mkX8 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4) (f x5 y5) (f x6 y6) (f x7 y7)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X8 where
  mkTuple2 = MkTuple2X8
  mkTuple3 = MkTuple3X8
  mkTuple4 = MkTuple4X8
  mkTuple5 = MkTuple5X8
  mkTuple6 = MkTuple6X8
  deconstructTuple2 (MkTuple2X8 v0 v1) = (v0, v1)
  deconstructTuple3 (MkTuple3X8 v0 v1 v2) = (v0, v1, v2)
  deconstructTuple4 (MkTuple4X8 v0 v1 v2 v3) = (v0, v1, v2, v3)
  deconstructTuple5 (MkTuple5X8 v0 v1 v2 v3 v4) = (v0, v1, v2, v3, v4)
  deconstructTuple6 (MkTuple6X8 v0 v1 v2 v3 v4 v5) = (v0, v1, v2, v3, v4, v5)
  mkSum = coerce
  getSum' = coerce
  mkProduct = coerce
  getProduct' = coerce
  mkMin = coerce
  getMin' = coerce
  mkMax = coerce
  getMax' = coerce
  mkComplex = MkComplexX8
  deconstructComplex (MkComplexX8 x y) = (x, y)
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
deriving via WrappedMulti X8 a instance SelectableF X8 a => Selectable (X8 a)
deriving via WrappedMulti X8 a instance NumF X8 a => Num (X8 a)
deriving via WrappedMulti X8 a instance FractionalF X8 a => Fractional (X8 a)
deriving via WrappedMulti X8 a instance FloatingF X8 a => Floating (X8 a)
deriving via WrappedMulti X8 a instance BooleanF X8 a => Boolean (X8 a)
deriving via WrappedMulti X8 a instance BitShiftF X8 a => BitShift (X8 a)
deriving via WrappedMulti X8 a instance MinMaxF X8 a => MinMax (X8 a)
deriving via WrappedMulti X8 a instance FusedMultiplyAddF X8 a => FusedMultiplyAdd (X8 a)
