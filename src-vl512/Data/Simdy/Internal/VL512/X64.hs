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
module Data.Simdy.Internal.VL512.X64 where
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
import           Data.Simdy.Internal.VL512.Prim
import           Data.Simdy.Internal.VL128.PrimExtra
import           Data.Simdy.Internal.VL256.PrimExtra
import           Data.Simdy.Internal.VL512.PrimExtra
import qualified GHC.Exts
import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..))
import           GHC.Int
import           GHC.IO
import           GHC.Word
import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=), min, max)
-- | @'X64' a@ is a fixed-length vector of length 64.
--
-- Conceptually, @data 'X64' a = MkX64 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a@.
--
-- You can access the elements by 'mkX64', 'packX64' and 'unpackX64'.
data family X64 a
instance KnownSIMDLength X64 where
  type SIMDLength X64 = 64
  simdLength = 64
  {-# INLINE simdLength #-}
newtype instance X64 Bool = MkBoolX64 Word64
type instance Mask (X64 a) = X64 Bool
instance MaskIsLiftedBool X64 a
instance BooleanF X64 Bool where
  andF (MkBoolX64 x) (MkBoolX64 y) = MkBoolX64 (x .&. y)
  orF (MkBoolX64 x) (MkBoolX64 y) = MkBoolX64 (x .|. y)
  xorF (MkBoolX64 x) (MkBoolX64 y) = MkBoolX64 (xor x y)
  complementF (MkBoolX64 x) = MkBoolX64 (0xffffffffffffffff - x)
deriving via WrappedMulti X64 a instance EquatableF X64 a => Equatable (X64 a)
deriving via WrappedMulti X64 a instance OrderedF X64 a => Ordered (X64 a)
instance PackX64 X64 a => IsList (X64 a) where
  type Item (X64 a) = a
  toList = toListX64
  fromList = fromListX64
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX64 X64 Bool where
  mkX64 !x0 !x1 !x2 !x3 !x4 !x5 !x6 !x7 !x8 !x9 !x10 !x11 !x12 !x13 !x14 !x15 !x16 !x17 !x18 !x19 !x20 !x21 !x22 !x23 !x24 !x25 !x26 !x27 !x28 !x29 !x30 !x31 !x32 !x33 !x34 !x35 !x36 !x37 !x38 !x39 !x40 !x41 !x42 !x43 !x44 !x45 !x46 !x47 !x48 !x49 !x50 !x51 !x52 !x53 !x54 !x55 !x56 !x57 !x58 !x59 !x60 !x61 !x62 !x63 = MkBoolX64 ((if x0 then 0x1 else 0) .|. (if x1 then 0x2 else 0) .|. (if x2 then 0x4 else 0) .|. (if x3 then 0x8 else 0) .|. (if x4 then 0x10 else 0) .|. (if x5 then 0x20 else 0) .|. (if x6 then 0x40 else 0) .|. (if x7 then 0x80 else 0) .|. (if x8 then 0x100 else 0) .|. (if x9 then 0x200 else 0) .|. (if x10 then 0x400 else 0) .|. (if x11 then 0x800 else 0) .|. (if x12 then 0x1000 else 0) .|. (if x13 then 0x2000 else 0) .|. (if x14 then 0x4000 else 0) .|. (if x15 then 0x8000 else 0) .|. (if x16 then 0x10000 else 0) .|. (if x17 then 0x20000 else 0) .|. (if x18 then 0x40000 else 0) .|. (if x19 then 0x80000 else 0) .|. (if x20 then 0x100000 else 0) .|. (if x21 then 0x200000 else 0) .|. (if x22 then 0x400000 else 0) .|. (if x23 then 0x800000 else 0) .|. (if x24 then 0x1000000 else 0) .|. (if x25 then 0x2000000 else 0) .|. (if x26 then 0x4000000 else 0) .|. (if x27 then 0x8000000 else 0) .|. (if x28 then 0x10000000 else 0) .|. (if x29 then 0x20000000 else 0) .|. (if x30 then 0x40000000 else 0) .|. (if x31 then 0x80000000 else 0) .|. (if x32 then 0x100000000 else 0) .|. (if x33 then 0x200000000 else 0) .|. (if x34 then 0x400000000 else 0) .|. (if x35 then 0x800000000 else 0) .|. (if x36 then 0x1000000000 else 0) .|. (if x37 then 0x2000000000 else 0) .|. (if x38 then 0x4000000000 else 0) .|. (if x39 then 0x8000000000 else 0) .|. (if x40 then 0x10000000000 else 0) .|. (if x41 then 0x20000000000 else 0) .|. (if x42 then 0x40000000000 else 0) .|. (if x43 then 0x80000000000 else 0) .|. (if x44 then 0x100000000000 else 0) .|. (if x45 then 0x200000000000 else 0) .|. (if x46 then 0x400000000000 else 0) .|. (if x47 then 0x800000000000 else 0) .|. (if x48 then 0x1000000000000 else 0) .|. (if x49 then 0x2000000000000 else 0) .|. (if x50 then 0x4000000000000 else 0) .|. (if x51 then 0x8000000000000 else 0) .|. (if x52 then 0x10000000000000 else 0) .|. (if x53 then 0x20000000000000 else 0) .|. (if x54 then 0x40000000000000 else 0) .|. (if x55 then 0x80000000000000 else 0) .|. (if x56 then 0x100000000000000 else 0) .|. (if x57 then 0x200000000000000 else 0) .|. (if x58 then 0x400000000000000 else 0) .|. (if x59 then 0x800000000000000 else 0) .|. (if x60 then 0x1000000000000000 else 0) .|. (if x61 then 0x2000000000000000 else 0) .|. (if x62 then 0x4000000000000000 else 0) .|. (if x63 then 0x8000000000000000 else 0))
  unpackX64 (MkBoolX64 !x) = (testBit x 0, testBit x 1, testBit x 2, testBit x 3, testBit x 4, testBit x 5, testBit x 6, testBit x 7, testBit x 8, testBit x 9, testBit x 10, testBit x 11, testBit x 12, testBit x 13, testBit x 14, testBit x 15, testBit x 16, testBit x 17, testBit x 18, testBit x 19, testBit x 20, testBit x 21, testBit x 22, testBit x 23, testBit x 24, testBit x 25, testBit x 26, testBit x 27, testBit x 28, testBit x 29, testBit x 30, testBit x 31, testBit x 32, testBit x 33, testBit x 34, testBit x 35, testBit x 36, testBit x 37, testBit x 38, testBit x 39, testBit x 40, testBit x 41, testBit x 42, testBit x 43, testBit x 44, testBit x 45, testBit x 46, testBit x 47, testBit x 48, testBit x 49, testBit x 50, testBit x 51, testBit x 52, testBit x 53, testBit x 54, testBit x 55, testBit x 56, testBit x 57, testBit x 58, testBit x 59, testBit x 60, testBit x 61, testBit x 62, testBit x 63)
instance Broadcast X64 Bool where
  broadcast False = MkBoolX64 0
  broadcast True = MkBoolX64 0xffffffffffffffff
  {-# INLINE broadcast #-}
instance SelectableF X64 Bool where
  selectF (MkBoolX64 !cond) (MkBoolX64 !x) (MkBoolX64 !y) = MkBoolX64 ((cond .&. x) .|. (complement cond .&. y))
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X64 Bool where
  unaryShuffle = error "not implemented yet"
  {-# NOINLINE unaryShuffle #-}
instance BinaryShuffleT indices X64 Bool where
  binaryShuffle = error "not implemented yet"
  {-# NOINLINE binaryShuffle #-}
data instance X64 Float = MkFloatX64WithVec512 FloatX16# FloatX16# FloatX16# FloatX16#
instance PackX64 X64 Float where
  mkX64 (F# x0) (F# x1) (F# x2) (F# x3) (F# x4) (F# x5) (F# x6) (F# x7) (F# x8) (F# x9) (F# x10) (F# x11) (F# x12) (F# x13) (F# x14) (F# x15) (F# x16) (F# x17) (F# x18) (F# x19) (F# x20) (F# x21) (F# x22) (F# x23) (F# x24) (F# x25) (F# x26) (F# x27) (F# x28) (F# x29) (F# x30) (F# x31) (F# x32) (F# x33) (F# x34) (F# x35) (F# x36) (F# x37) (F# x38) (F# x39) (F# x40) (F# x41) (F# x42) (F# x43) (F# x44) (F# x45) (F# x46) (F# x47) (F# x48) (F# x49) (F# x50) (F# x51) (F# x52) (F# x53) (F# x54) (F# x55) (F# x56) (F# x57) (F# x58) (F# x59) (F# x60) (F# x61) (F# x62) (F# x63) = MkFloatX64WithVec512 (packFloatX16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #)) (packFloatX16# (# x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #)) (packFloatX16# (# x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47 #)) (packFloatX16# (# x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #))
  unpackX64 (MkFloatX64WithVec512 v0 v1 v2 v3) = case unpackFloatX16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackFloatX16# v1 of (# x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #) -> case unpackFloatX16# v2 of (# x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47 #) -> case unpackFloatX16# v3 of (# x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #) -> (F# x0, F# x1, F# x2, F# x3, F# x4, F# x5, F# x6, F# x7, F# x8, F# x9, F# x10, F# x11, F# x12, F# x13, F# x14, F# x15, F# x16, F# x17, F# x18, F# x19, F# x20, F# x21, F# x22, F# x23, F# x24, F# x25, F# x26, F# x27, F# x28, F# x29, F# x30, F# x31, F# x32, F# x33, F# x34, F# x35, F# x36, F# x37, F# x38, F# x39, F# x40, F# x41, F# x42, F# x43, F# x44, F# x45, F# x46, F# x47, F# x48, F# x49, F# x50, F# x51, F# x52, F# x53, F# x54, F# x55, F# x56, F# x57, F# x58, F# x59, F# x60, F# x61, F# x62, F# x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 Float where
  broadcast (F# x) = let !v = broadcastFloatX16# x in MkFloatX64WithVec512 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X64 Float where
  selectF (MkBoolX64 !cond) (MkFloatX64WithVec512 x0 x1 x2 x3) (MkFloatX64WithVec512 y0 y1 y2 y3) = MkFloatX64WithVec512 (selectFloatX16# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectFloatX16# (fromIntegral $ cond `unsafeShiftR` 16) x1 y1) (selectFloatX16# (fromIntegral $ cond `unsafeShiftR` 32) x2 y2) (selectFloatX16# (fromIntegral $ cond `unsafeShiftR` 48) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, i32 < 64, i33 < 64, i34 < 64, i35 < 64, i36 < 64, i37 < 64, i38 < 64, i39 < 64, i40 < 64, i41 < 64, i42 < 64, i43 < 64, i44 < 64, i45 < 64, i46 < 64, i47 < 64, i48 < 64, i49 < 64, i50 < 64, i51 < 64, i52 < 64, i53 < 64, i54 < 64, i55 < 64, i56 < 64, i57 < 64, i58 < 64, i59 < 64, i60 < 64, i61 < 64, i62 < 64, i63 < 64, ShuffleMany FloatX16# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany FloatX16# [i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany FloatX16# [i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany FloatX16# [i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Float where
  unaryShuffle (MkFloatX64WithVec512 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkFloatX64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 128, i1 < 128, i2 < 128, i3 < 128, i4 < 128, i5 < 128, i6 < 128, i7 < 128, i8 < 128, i9 < 128, i10 < 128, i11 < 128, i12 < 128, i13 < 128, i14 < 128, i15 < 128, i16 < 128, i17 < 128, i18 < 128, i19 < 128, i20 < 128, i21 < 128, i22 < 128, i23 < 128, i24 < 128, i25 < 128, i26 < 128, i27 < 128, i28 < 128, i29 < 128, i30 < 128, i31 < 128, i32 < 128, i33 < 128, i34 < 128, i35 < 128, i36 < 128, i37 < 128, i38 < 128, i39 < 128, i40 < 128, i41 < 128, i42 < 128, i43 < 128, i44 < 128, i45 < 128, i46 < 128, i47 < 128, i48 < 128, i49 < 128, i50 < 128, i51 < 128, i52 < 128, i53 < 128, i54 < 128, i55 < 128, i56 < 128, i57 < 128, i58 < 128, i59 < 128, i60 < 128, i61 < 128, i62 < 128, i63 < 128, ShuffleMany FloatX16# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany FloatX16# [i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany FloatX16# [i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany FloatX16# [i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Float where
  binaryShuffle (MkFloatX64WithVec512 x0 x1 x2 x3) (MkFloatX64WithVec512 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkFloatX64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X64 Float where
  eqF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkBoolX64 $ (fromIntegral (eqFloatX16# u0 v0)) .|. (fromIntegral (eqFloatX16# u1 v1) `unsafeShiftL` 16) .|. (fromIntegral (eqFloatX16# u2 v2) `unsafeShiftL` 32) .|. (fromIntegral (eqFloatX16# u3 v3) `unsafeShiftL` 48)
  {-# INLINE eqF #-}
instance OrderedF X64 Float where
  ltF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkBoolX64 $ (fromIntegral (ltFloatX16# u0 v0)) .|. (fromIntegral (ltFloatX16# u1 v1) `unsafeShiftL` 16) .|. (fromIntegral (ltFloatX16# u2 v2) `unsafeShiftL` 32) .|. (fromIntegral (ltFloatX16# u3 v3) `unsafeShiftL` 48)
  leF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkBoolX64 $ (fromIntegral (leFloatX16# u0 v0)) .|. (fromIntegral (leFloatX16# u1 v1) `unsafeShiftL` 16) .|. (fromIntegral (leFloatX16# u2 v2) `unsafeShiftL` 32) .|. (fromIntegral (leFloatX16# u3 v3) `unsafeShiftL` 48)
  gtF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkBoolX64 $ (fromIntegral (gtFloatX16# u0 v0)) .|. (fromIntegral (gtFloatX16# u1 v1) `unsafeShiftL` 16) .|. (fromIntegral (gtFloatX16# u2 v2) `unsafeShiftL` 32) .|. (fromIntegral (gtFloatX16# u3 v3) `unsafeShiftL` 48)
  geF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkBoolX64 $ (fromIntegral (geFloatX16# u0 v0)) .|. (fromIntegral (geFloatX16# u1 v1) `unsafeShiftL` 16) .|. (fromIntegral (geFloatX16# u2 v2) `unsafeShiftL` 32) .|. (fromIntegral (geFloatX16# u3 v3) `unsafeShiftL` 48)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X64 Float where
  minF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkFloatX64WithVec512 (minimumFloatX16# u0 v0) (minimumFloatX16# u1 v1) (minimumFloatX16# u2 v2) (minimumFloatX16# u3 v3)
  maxF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkFloatX64WithVec512 (maximumFloatX16# u0 v0) (maximumFloatX16# u1 v1) (maximumFloatX16# u2 v2) (maximumFloatX16# u3 v3)
  minimumNumberF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkFloatX64WithVec512 (minimumNumberFloatX16# u0 v0) (minimumNumberFloatX16# u1 v1) (minimumNumberFloatX16# u2 v2) (minimumNumberFloatX16# u3 v3)
  maximumNumberF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkFloatX64WithVec512 (maximumNumberFloatX16# u0 v0) (maximumNumberFloatX16# u1 v1) (maximumNumberFloatX16# u2 v2) (maximumNumberFloatX16# u3 v3)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX64Float :: X64 Float -> X64 Float
negateX64Float (MkFloatX64WithVec512 u0 u1 u2 u3) = MkFloatX64WithVec512 (negateFloatX16# u0) (negateFloatX16# u1) (negateFloatX16# u2) (negateFloatX16# u3)
#if defined(USE_FMA)
{-# INLINE [0] negateX64Float #-}
#else
{-# INLINE negateX64Float #-}
#endif
instance NumF X64 Float where
  plusF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkFloatX64WithVec512 (plusFloatX16# u0 v0) (plusFloatX16# u1 v1) (plusFloatX16# u2 v2) (plusFloatX16# u3 v3)
  minusF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkFloatX64WithVec512 (minusFloatX16# u0 v0) (minusFloatX16# u1 v1) (minusFloatX16# u2 v2) (minusFloatX16# u3 v3)
  timesF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkFloatX64WithVec512 (timesFloatX16# u0 v0) (timesFloatX16# u1 v1) (timesFloatX16# u2 v2) (timesFloatX16# u3 v3)
  negateF = negateX64Float
  absF (MkFloatX64WithVec512 u0 u1 u2 u3) = MkFloatX64WithVec512 (absFloatX16# u0) (absFloatX16# u1) (absFloatX16# u2) (absFloatX16# u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X64 Float where
  divideF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) = MkFloatX64WithVec512 (divideFloatX16# u0 v0) (divideFloatX16# u1 v1) (divideFloatX16# u2 v2) (divideFloatX16# u3 v3)
  {-# INLINE divideF #-}
instance FloatingF X64 Float where
  sqrtF (MkFloatX64WithVec512 u0 u1 u2 u3) = MkFloatX64WithVec512 (sqrtFloatX16# u0) (sqrtFloatX16# u1) (sqrtFloatX16# u2) (sqrtFloatX16# u3)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X64 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX64WithVec512 u0 u1 u2 u3) (MkFloatX64WithVec512 v0 v1 v2 v3) (MkFloatX64WithVec512 w0 w1 w2 w3) = MkFloatX64WithVec512 (fmaddFloatX16# u0 v0 w0) (fmaddFloatX16# u1 v1 w1) (fmaddFloatX16# u2 v2 w2) (fmaddFloatX16# u3 v3 w3)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X64 Float" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X64 Float
"Fusible/*-/X64 Float" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX64Float c) :: X64 Float
"Fusible/-*+/X64 Float" forall a b c.
  negateX64Float (a F.* b) F.+ c = fusedMultiplyAdd (negateX64Float a) b c :: X64 Float
"Fusible/-*-/X64 Float" forall a b c.
  negateX64Float (a F.* b) F.- c = fusedMultiplyAdd (negateX64Float a) b (negateX64Float c) :: X64 Float
"Fusible/+*/X64 Float" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X64 Float
"Fusible/-*/X64 Float" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX64Float b) c a :: X64 Float
  #-}
#endif
instance EnumFromZero_ X64 Float where
  enumFromZero = MkFloatX64WithVec512 (packFloatX16# (# 0.0#, 1.0#, 2.0#, 3.0#, 4.0#, 5.0#, 6.0#, 7.0#, 8.0#, 9.0#, 10.0#, 11.0#, 12.0#, 13.0#, 14.0#, 15.0# #)) (packFloatX16# (# 16.0#, 17.0#, 18.0#, 19.0#, 20.0#, 21.0#, 22.0#, 23.0#, 24.0#, 25.0#, 26.0#, 27.0#, 28.0#, 29.0#, 30.0#, 31.0# #)) (packFloatX16# (# 32.0#, 33.0#, 34.0#, 35.0#, 36.0#, 37.0#, 38.0#, 39.0#, 40.0#, 41.0#, 42.0#, 43.0#, 44.0#, 45.0#, 46.0#, 47.0# #)) (packFloatX16# (# 48.0#, 49.0#, 50.0#, 51.0#, 52.0#, 53.0#, 54.0#, 55.0#, 56.0#, 57.0#, 58.0#, 59.0#, 60.0#, 61.0#, 62.0#, 63.0# #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X64 Float where
  indexByteArraySIMD# ba i = MkFloatX64WithVec512 (indexFloatArrayAsFloatX16# ba i) (indexFloatArrayAsFloatX16# ba (i +# 16#)) (indexFloatArrayAsFloatX16# ba (i +# 32#)) (indexFloatArrayAsFloatX16# ba (i +# 48#))
  readByteArraySIMD# mba i s0 = case readFloatArrayAsFloatX16# mba i s0 of (# s1, v0 #) -> case readFloatArrayAsFloatX16# mba (i +# 16#) s1 of (# s2, v1 #) -> case readFloatArrayAsFloatX16# mba (i +# 32#) s2 of (# s3, v2 #) -> case readFloatArrayAsFloatX16# mba (i +# 48#) s3 of (# s4, v3 #) -> (# s4, MkFloatX64WithVec512 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkFloatX64WithVec512 v0 v1 v2 v3) s0 = case writeFloatArrayAsFloatX16# mba i v0 s0 of s1 -> case writeFloatArrayAsFloatX16# mba (i +# 16#) v1 s1 of s2 -> case writeFloatArrayAsFloatX16# mba (i +# 32#) v2 s2 of s3 -> writeFloatArrayAsFloatX16# mba (i +# 48#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X64 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readFloatOffAddrAsFloatX16# addr i s0 of (# s1, v0 #) -> case readFloatOffAddrAsFloatX16# addr (i +# 16#) s1 of (# s2, v1 #) -> case readFloatOffAddrAsFloatX16# addr (i +# 32#) s2 of (# s3, v2 #) -> case readFloatOffAddrAsFloatX16# addr (i +# 48#) s3 of (# s4, v3 #) -> (# s4, MkFloatX64WithVec512 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX64WithVec512 v0 v1 v2 v3) = IO (\s0 -> case writeFloatOffAddrAsFloatX16# addr i v0 s0 of s1 -> case writeFloatOffAddrAsFloatX16# addr (i +# 16#) v1 s1 of s2 -> case writeFloatOffAddrAsFloatX16# addr (i +# 32#) v2 s2 of s3 -> case writeFloatOffAddrAsFloatX16# addr (i +# 48#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X64 Double = MkDoubleX64WithVec512 DoubleX8# DoubleX8# DoubleX8# DoubleX8# DoubleX8# DoubleX8# DoubleX8# DoubleX8#
instance PackX64 X64 Double where
  mkX64 (D# x0) (D# x1) (D# x2) (D# x3) (D# x4) (D# x5) (D# x6) (D# x7) (D# x8) (D# x9) (D# x10) (D# x11) (D# x12) (D# x13) (D# x14) (D# x15) (D# x16) (D# x17) (D# x18) (D# x19) (D# x20) (D# x21) (D# x22) (D# x23) (D# x24) (D# x25) (D# x26) (D# x27) (D# x28) (D# x29) (D# x30) (D# x31) (D# x32) (D# x33) (D# x34) (D# x35) (D# x36) (D# x37) (D# x38) (D# x39) (D# x40) (D# x41) (D# x42) (D# x43) (D# x44) (D# x45) (D# x46) (D# x47) (D# x48) (D# x49) (D# x50) (D# x51) (D# x52) (D# x53) (D# x54) (D# x55) (D# x56) (D# x57) (D# x58) (D# x59) (D# x60) (D# x61) (D# x62) (D# x63) = MkDoubleX64WithVec512 (packDoubleX8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packDoubleX8# (# x8, x9, x10, x11, x12, x13, x14, x15 #)) (packDoubleX8# (# x16, x17, x18, x19, x20, x21, x22, x23 #)) (packDoubleX8# (# x24, x25, x26, x27, x28, x29, x30, x31 #)) (packDoubleX8# (# x32, x33, x34, x35, x36, x37, x38, x39 #)) (packDoubleX8# (# x40, x41, x42, x43, x44, x45, x46, x47 #)) (packDoubleX8# (# x48, x49, x50, x51, x52, x53, x54, x55 #)) (packDoubleX8# (# x56, x57, x58, x59, x60, x61, x62, x63 #))
  unpackX64 (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = case unpackDoubleX8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackDoubleX8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackDoubleX8# v2 of (# x16, x17, x18, x19, x20, x21, x22, x23 #) -> case unpackDoubleX8# v3 of (# x24, x25, x26, x27, x28, x29, x30, x31 #) -> case unpackDoubleX8# v4 of (# x32, x33, x34, x35, x36, x37, x38, x39 #) -> case unpackDoubleX8# v5 of (# x40, x41, x42, x43, x44, x45, x46, x47 #) -> case unpackDoubleX8# v6 of (# x48, x49, x50, x51, x52, x53, x54, x55 #) -> case unpackDoubleX8# v7 of (# x56, x57, x58, x59, x60, x61, x62, x63 #) -> (D# x0, D# x1, D# x2, D# x3, D# x4, D# x5, D# x6, D# x7, D# x8, D# x9, D# x10, D# x11, D# x12, D# x13, D# x14, D# x15, D# x16, D# x17, D# x18, D# x19, D# x20, D# x21, D# x22, D# x23, D# x24, D# x25, D# x26, D# x27, D# x28, D# x29, D# x30, D# x31, D# x32, D# x33, D# x34, D# x35, D# x36, D# x37, D# x38, D# x39, D# x40, D# x41, D# x42, D# x43, D# x44, D# x45, D# x46, D# x47, D# x48, D# x49, D# x50, D# x51, D# x52, D# x53, D# x54, D# x55, D# x56, D# x57, D# x58, D# x59, D# x60, D# x61, D# x62, D# x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 Double where
  broadcast (D# x) = let !v = broadcastDoubleX8# x in MkDoubleX64WithVec512 v v v v v v v v
  {-# INLINE broadcast #-}
instance SelectableF X64 Double where
  selectF (MkBoolX64 !cond) (MkDoubleX64WithVec512 x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX64WithVec512 y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX64WithVec512 (selectDoubleX8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectDoubleX8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1) (selectDoubleX8# (fromIntegral $ cond `unsafeShiftR` 16) x2 y2) (selectDoubleX8# (fromIntegral $ cond `unsafeShiftR` 24) x3 y3) (selectDoubleX8# (fromIntegral $ cond `unsafeShiftR` 32) x4 y4) (selectDoubleX8# (fromIntegral $ cond `unsafeShiftR` 40) x5 y5) (selectDoubleX8# (fromIntegral $ cond `unsafeShiftR` 48) x6 y6) (selectDoubleX8# (fromIntegral $ cond `unsafeShiftR` 56) x7 y7)
  {-# INLINE selectF #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, i32 < 64, i33 < 64, i34 < 64, i35 < 64, i36 < 64, i37 < 64, i38 < 64, i39 < 64, i40 < 64, i41 < 64, i42 < 64, i43 < 64, i44 < 64, i45 < 64, i46 < 64, i47 < 64, i48 < 64, i49 < 64, i50 < 64, i51 < 64, i52 < 64, i53 < 64, i54 < 64, i55 < 64, i56 < 64, i57 < 64, i58 < 64, i59 < 64, i60 < 64, i61 < 64, i62 < 64, i63 < 64, ShuffleMany DoubleX8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany DoubleX8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany DoubleX8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany DoubleX8# [i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany DoubleX8# [i32, i33, i34, i35, i36, i37, i38, i39], ShuffleMany DoubleX8# [i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany DoubleX8# [i48, i49, i50, i51, i52, i53, i54, i55], ShuffleMany DoubleX8# [i56, i57, i58, i59, i60, i61, i62, i63]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Double where
  unaryShuffle (MkDoubleX64WithVec512 x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkDoubleX64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39] sources) (shuffleMany# @_ @_ @[i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55] sources) (shuffleMany# @_ @_ @[i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 128, i1 < 128, i2 < 128, i3 < 128, i4 < 128, i5 < 128, i6 < 128, i7 < 128, i8 < 128, i9 < 128, i10 < 128, i11 < 128, i12 < 128, i13 < 128, i14 < 128, i15 < 128, i16 < 128, i17 < 128, i18 < 128, i19 < 128, i20 < 128, i21 < 128, i22 < 128, i23 < 128, i24 < 128, i25 < 128, i26 < 128, i27 < 128, i28 < 128, i29 < 128, i30 < 128, i31 < 128, i32 < 128, i33 < 128, i34 < 128, i35 < 128, i36 < 128, i37 < 128, i38 < 128, i39 < 128, i40 < 128, i41 < 128, i42 < 128, i43 < 128, i44 < 128, i45 < 128, i46 < 128, i47 < 128, i48 < 128, i49 < 128, i50 < 128, i51 < 128, i52 < 128, i53 < 128, i54 < 128, i55 < 128, i56 < 128, i57 < 128, i58 < 128, i59 < 128, i60 < 128, i61 < 128, i62 < 128, i63 < 128, ShuffleMany DoubleX8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany DoubleX8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany DoubleX8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany DoubleX8# [i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany DoubleX8# [i32, i33, i34, i35, i36, i37, i38, i39], ShuffleMany DoubleX8# [i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany DoubleX8# [i48, i49, i50, i51, i52, i53, i54, i55], ShuffleMany DoubleX8# [i56, i57, i58, i59, i60, i61, i62, i63]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Double where
  binaryShuffle (MkDoubleX64WithVec512 x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX64WithVec512 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkDoubleX64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39] sources) (shuffleMany# @_ @_ @[i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55] sources) (shuffleMany# @_ @_ @[i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X64 Double where
  eqF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX64 $ (fromIntegral (eqDoubleX8# u0 v0)) .|. (fromIntegral (eqDoubleX8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (eqDoubleX8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (eqDoubleX8# u3 v3) `unsafeShiftL` 24) .|. (fromIntegral (eqDoubleX8# u4 v4) `unsafeShiftL` 32) .|. (fromIntegral (eqDoubleX8# u5 v5) `unsafeShiftL` 40) .|. (fromIntegral (eqDoubleX8# u6 v6) `unsafeShiftL` 48) .|. (fromIntegral (eqDoubleX8# u7 v7) `unsafeShiftL` 56)
  {-# INLINE eqF #-}
instance OrderedF X64 Double where
  ltF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX64 $ (fromIntegral (ltDoubleX8# u0 v0)) .|. (fromIntegral (ltDoubleX8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (ltDoubleX8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (ltDoubleX8# u3 v3) `unsafeShiftL` 24) .|. (fromIntegral (ltDoubleX8# u4 v4) `unsafeShiftL` 32) .|. (fromIntegral (ltDoubleX8# u5 v5) `unsafeShiftL` 40) .|. (fromIntegral (ltDoubleX8# u6 v6) `unsafeShiftL` 48) .|. (fromIntegral (ltDoubleX8# u7 v7) `unsafeShiftL` 56)
  leF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX64 $ (fromIntegral (leDoubleX8# u0 v0)) .|. (fromIntegral (leDoubleX8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (leDoubleX8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (leDoubleX8# u3 v3) `unsafeShiftL` 24) .|. (fromIntegral (leDoubleX8# u4 v4) `unsafeShiftL` 32) .|. (fromIntegral (leDoubleX8# u5 v5) `unsafeShiftL` 40) .|. (fromIntegral (leDoubleX8# u6 v6) `unsafeShiftL` 48) .|. (fromIntegral (leDoubleX8# u7 v7) `unsafeShiftL` 56)
  gtF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX64 $ (fromIntegral (gtDoubleX8# u0 v0)) .|. (fromIntegral (gtDoubleX8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (gtDoubleX8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (gtDoubleX8# u3 v3) `unsafeShiftL` 24) .|. (fromIntegral (gtDoubleX8# u4 v4) `unsafeShiftL` 32) .|. (fromIntegral (gtDoubleX8# u5 v5) `unsafeShiftL` 40) .|. (fromIntegral (gtDoubleX8# u6 v6) `unsafeShiftL` 48) .|. (fromIntegral (gtDoubleX8# u7 v7) `unsafeShiftL` 56)
  geF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX64 $ (fromIntegral (geDoubleX8# u0 v0)) .|. (fromIntegral (geDoubleX8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (geDoubleX8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (geDoubleX8# u3 v3) `unsafeShiftL` 24) .|. (fromIntegral (geDoubleX8# u4 v4) `unsafeShiftL` 32) .|. (fromIntegral (geDoubleX8# u5 v5) `unsafeShiftL` 40) .|. (fromIntegral (geDoubleX8# u6 v6) `unsafeShiftL` 48) .|. (fromIntegral (geDoubleX8# u7 v7) `unsafeShiftL` 56)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X64 Double where
  minF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX64WithVec512 (minimumDoubleX8# u0 v0) (minimumDoubleX8# u1 v1) (minimumDoubleX8# u2 v2) (minimumDoubleX8# u3 v3) (minimumDoubleX8# u4 v4) (minimumDoubleX8# u5 v5) (minimumDoubleX8# u6 v6) (minimumDoubleX8# u7 v7)
  maxF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX64WithVec512 (maximumDoubleX8# u0 v0) (maximumDoubleX8# u1 v1) (maximumDoubleX8# u2 v2) (maximumDoubleX8# u3 v3) (maximumDoubleX8# u4 v4) (maximumDoubleX8# u5 v5) (maximumDoubleX8# u6 v6) (maximumDoubleX8# u7 v7)
  minimumNumberF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX64WithVec512 (minimumNumberDoubleX8# u0 v0) (minimumNumberDoubleX8# u1 v1) (minimumNumberDoubleX8# u2 v2) (minimumNumberDoubleX8# u3 v3) (minimumNumberDoubleX8# u4 v4) (minimumNumberDoubleX8# u5 v5) (minimumNumberDoubleX8# u6 v6) (minimumNumberDoubleX8# u7 v7)
  maximumNumberF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX64WithVec512 (maximumNumberDoubleX8# u0 v0) (maximumNumberDoubleX8# u1 v1) (maximumNumberDoubleX8# u2 v2) (maximumNumberDoubleX8# u3 v3) (maximumNumberDoubleX8# u4 v4) (maximumNumberDoubleX8# u5 v5) (maximumNumberDoubleX8# u6 v6) (maximumNumberDoubleX8# u7 v7)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX64Double :: X64 Double -> X64 Double
negateX64Double (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) = MkDoubleX64WithVec512 (negateDoubleX8# u0) (negateDoubleX8# u1) (negateDoubleX8# u2) (negateDoubleX8# u3) (negateDoubleX8# u4) (negateDoubleX8# u5) (negateDoubleX8# u6) (negateDoubleX8# u7)
#if defined(USE_FMA)
{-# INLINE [0] negateX64Double #-}
#else
{-# INLINE negateX64Double #-}
#endif
instance NumF X64 Double where
  plusF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX64WithVec512 (plusDoubleX8# u0 v0) (plusDoubleX8# u1 v1) (plusDoubleX8# u2 v2) (plusDoubleX8# u3 v3) (plusDoubleX8# u4 v4) (plusDoubleX8# u5 v5) (plusDoubleX8# u6 v6) (plusDoubleX8# u7 v7)
  minusF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX64WithVec512 (minusDoubleX8# u0 v0) (minusDoubleX8# u1 v1) (minusDoubleX8# u2 v2) (minusDoubleX8# u3 v3) (minusDoubleX8# u4 v4) (minusDoubleX8# u5 v5) (minusDoubleX8# u6 v6) (minusDoubleX8# u7 v7)
  timesF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX64WithVec512 (timesDoubleX8# u0 v0) (timesDoubleX8# u1 v1) (timesDoubleX8# u2 v2) (timesDoubleX8# u3 v3) (timesDoubleX8# u4 v4) (timesDoubleX8# u5 v5) (timesDoubleX8# u6 v6) (timesDoubleX8# u7 v7)
  negateF = negateX64Double
  absF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) = MkDoubleX64WithVec512 (absDoubleX8# u0) (absDoubleX8# u1) (absDoubleX8# u2) (absDoubleX8# u3) (absDoubleX8# u4) (absDoubleX8# u5) (absDoubleX8# u6) (absDoubleX8# u7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X64 Double where
  divideF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkDoubleX64WithVec512 (divideDoubleX8# u0 v0) (divideDoubleX8# u1 v1) (divideDoubleX8# u2 v2) (divideDoubleX8# u3 v3) (divideDoubleX8# u4 v4) (divideDoubleX8# u5 v5) (divideDoubleX8# u6 v6) (divideDoubleX8# u7 v7)
  {-# INLINE divideF #-}
instance FloatingF X64 Double where
  sqrtF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) = MkDoubleX64WithVec512 (sqrtDoubleX8# u0) (sqrtDoubleX8# u1) (sqrtDoubleX8# u2) (sqrtDoubleX8# u3) (sqrtDoubleX8# u4) (sqrtDoubleX8# u5) (sqrtDoubleX8# u6) (sqrtDoubleX8# u7)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X64 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) (MkDoubleX64WithVec512 w0 w1 w2 w3 w4 w5 w6 w7) = MkDoubleX64WithVec512 (fmaddDoubleX8# u0 v0 w0) (fmaddDoubleX8# u1 v1 w1) (fmaddDoubleX8# u2 v2 w2) (fmaddDoubleX8# u3 v3 w3) (fmaddDoubleX8# u4 v4 w4) (fmaddDoubleX8# u5 v5 w5) (fmaddDoubleX8# u6 v6 w6) (fmaddDoubleX8# u7 v7 w7)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X64 Double" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X64 Double
"Fusible/*-/X64 Double" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX64Double c) :: X64 Double
"Fusible/-*+/X64 Double" forall a b c.
  negateX64Double (a F.* b) F.+ c = fusedMultiplyAdd (negateX64Double a) b c :: X64 Double
"Fusible/-*-/X64 Double" forall a b c.
  negateX64Double (a F.* b) F.- c = fusedMultiplyAdd (negateX64Double a) b (negateX64Double c) :: X64 Double
"Fusible/+*/X64 Double" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X64 Double
"Fusible/-*/X64 Double" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX64Double b) c a :: X64 Double
  #-}
#endif
instance EnumFromZero_ X64 Double where
  enumFromZero = MkDoubleX64WithVec512 (packDoubleX8# (# 0.0##, 1.0##, 2.0##, 3.0##, 4.0##, 5.0##, 6.0##, 7.0## #)) (packDoubleX8# (# 8.0##, 9.0##, 10.0##, 11.0##, 12.0##, 13.0##, 14.0##, 15.0## #)) (packDoubleX8# (# 16.0##, 17.0##, 18.0##, 19.0##, 20.0##, 21.0##, 22.0##, 23.0## #)) (packDoubleX8# (# 24.0##, 25.0##, 26.0##, 27.0##, 28.0##, 29.0##, 30.0##, 31.0## #)) (packDoubleX8# (# 32.0##, 33.0##, 34.0##, 35.0##, 36.0##, 37.0##, 38.0##, 39.0## #)) (packDoubleX8# (# 40.0##, 41.0##, 42.0##, 43.0##, 44.0##, 45.0##, 46.0##, 47.0## #)) (packDoubleX8# (# 48.0##, 49.0##, 50.0##, 51.0##, 52.0##, 53.0##, 54.0##, 55.0## #)) (packDoubleX8# (# 56.0##, 57.0##, 58.0##, 59.0##, 60.0##, 61.0##, 62.0##, 63.0## #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X64 Double where
  indexByteArraySIMD# ba i = MkDoubleX64WithVec512 (indexDoubleArrayAsDoubleX8# ba i) (indexDoubleArrayAsDoubleX8# ba (i +# 8#)) (indexDoubleArrayAsDoubleX8# ba (i +# 16#)) (indexDoubleArrayAsDoubleX8# ba (i +# 24#)) (indexDoubleArrayAsDoubleX8# ba (i +# 32#)) (indexDoubleArrayAsDoubleX8# ba (i +# 40#)) (indexDoubleArrayAsDoubleX8# ba (i +# 48#)) (indexDoubleArrayAsDoubleX8# ba (i +# 56#))
  readByteArraySIMD# mba i s0 = case readDoubleArrayAsDoubleX8# mba i s0 of (# s1, v0 #) -> case readDoubleArrayAsDoubleX8# mba (i +# 8#) s1 of (# s2, v1 #) -> case readDoubleArrayAsDoubleX8# mba (i +# 16#) s2 of (# s3, v2 #) -> case readDoubleArrayAsDoubleX8# mba (i +# 24#) s3 of (# s4, v3 #) -> case readDoubleArrayAsDoubleX8# mba (i +# 32#) s4 of (# s5, v4 #) -> case readDoubleArrayAsDoubleX8# mba (i +# 40#) s5 of (# s6, v5 #) -> case readDoubleArrayAsDoubleX8# mba (i +# 48#) s6 of (# s7, v6 #) -> case readDoubleArrayAsDoubleX8# mba (i +# 56#) s7 of (# s8, v7 #) -> (# s8, MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7 #)
  writeByteArraySIMD# mba i (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) s0 = case writeDoubleArrayAsDoubleX8# mba i v0 s0 of s1 -> case writeDoubleArrayAsDoubleX8# mba (i +# 8#) v1 s1 of s2 -> case writeDoubleArrayAsDoubleX8# mba (i +# 16#) v2 s2 of s3 -> case writeDoubleArrayAsDoubleX8# mba (i +# 24#) v3 s3 of s4 -> case writeDoubleArrayAsDoubleX8# mba (i +# 32#) v4 s4 of s5 -> case writeDoubleArrayAsDoubleX8# mba (i +# 40#) v5 s5 of s6 -> case writeDoubleArrayAsDoubleX8# mba (i +# 48#) v6 s6 of s7 -> writeDoubleArrayAsDoubleX8# mba (i +# 56#) v7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X64 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readDoubleOffAddrAsDoubleX8# addr i s0 of (# s1, v0 #) -> case readDoubleOffAddrAsDoubleX8# addr (i +# 8#) s1 of (# s2, v1 #) -> case readDoubleOffAddrAsDoubleX8# addr (i +# 16#) s2 of (# s3, v2 #) -> case readDoubleOffAddrAsDoubleX8# addr (i +# 24#) s3 of (# s4, v3 #) -> case readDoubleOffAddrAsDoubleX8# addr (i +# 32#) s4 of (# s5, v4 #) -> case readDoubleOffAddrAsDoubleX8# addr (i +# 40#) s5 of (# s6, v5 #) -> case readDoubleOffAddrAsDoubleX8# addr (i +# 48#) s6 of (# s7, v6 #) -> case readDoubleOffAddrAsDoubleX8# addr (i +# 56#) s7 of (# s8, v7 #) -> (# s8, MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = IO (\s0 -> case writeDoubleOffAddrAsDoubleX8# addr i v0 s0 of s1 -> case writeDoubleOffAddrAsDoubleX8# addr (i +# 8#) v1 s1 of s2 -> case writeDoubleOffAddrAsDoubleX8# addr (i +# 16#) v2 s2 of s3 -> case writeDoubleOffAddrAsDoubleX8# addr (i +# 24#) v3 s3 of s4 -> case writeDoubleOffAddrAsDoubleX8# addr (i +# 32#) v4 s4 of s5 -> case writeDoubleOffAddrAsDoubleX8# addr (i +# 40#) v5 s5 of s6 -> case writeDoubleOffAddrAsDoubleX8# addr (i +# 48#) v6 s6 of s7 -> case writeDoubleOffAddrAsDoubleX8# addr (i +# 56#) v7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
instance ImplementationDescription X64 where
  implementationDescription _ = "X64;maxBits=512"
data instance X64 Int8 = MkInt8X64 Int8X64#
instance PackX64 X64 Int8 where
  mkX64 (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) (I8# x8) (I8# x9) (I8# x10) (I8# x11) (I8# x12) (I8# x13) (I8# x14) (I8# x15) (I8# x16) (I8# x17) (I8# x18) (I8# x19) (I8# x20) (I8# x21) (I8# x22) (I8# x23) (I8# x24) (I8# x25) (I8# x26) (I8# x27) (I8# x28) (I8# x29) (I8# x30) (I8# x31) (I8# x32) (I8# x33) (I8# x34) (I8# x35) (I8# x36) (I8# x37) (I8# x38) (I8# x39) (I8# x40) (I8# x41) (I8# x42) (I8# x43) (I8# x44) (I8# x45) (I8# x46) (I8# x47) (I8# x48) (I8# x49) (I8# x50) (I8# x51) (I8# x52) (I8# x53) (I8# x54) (I8# x55) (I8# x56) (I8# x57) (I8# x58) (I8# x59) (I8# x60) (I8# x61) (I8# x62) (I8# x63) = MkInt8X64 (packInt8X64# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #))
  unpackX64 (MkInt8X64 v0) = case unpackInt8X64# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #) -> (I8# x0, I8# x1, I8# x2, I8# x3, I8# x4, I8# x5, I8# x6, I8# x7, I8# x8, I8# x9, I8# x10, I8# x11, I8# x12, I8# x13, I8# x14, I8# x15, I8# x16, I8# x17, I8# x18, I8# x19, I8# x20, I8# x21, I8# x22, I8# x23, I8# x24, I8# x25, I8# x26, I8# x27, I8# x28, I8# x29, I8# x30, I8# x31, I8# x32, I8# x33, I8# x34, I8# x35, I8# x36, I8# x37, I8# x38, I8# x39, I8# x40, I8# x41, I8# x42, I8# x43, I8# x44, I8# x45, I8# x46, I8# x47, I8# x48, I8# x49, I8# x50, I8# x51, I8# x52, I8# x53, I8# x54, I8# x55, I8# x56, I8# x57, I8# x58, I8# x59, I8# x60, I8# x61, I8# x62, I8# x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 Int8 where
  broadcast (I8# x) = MkInt8X64 (broadcastInt8X64# x)
  {-# INLINE broadcast #-}
instance SelectableF X64 Int8 where
  selectF (MkBoolX64 !cond) (MkInt8X64 x0) (MkInt8X64 y0) = MkInt8X64 (selectInt8X64# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 64, ShuffleMany Int8X64# indices) => UnaryShuffleT indices X64 Int8 where
  unaryShuffle (MkInt8X64 x) = MkInt8X64 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 128, ShuffleMany Int8X64# indices) => BinaryShuffleT indices X64 Int8 where
  binaryShuffle (MkInt8X64 x0) (MkInt8X64 x1) = MkInt8X64 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X64 Int8 where
  eqF (MkInt8X64 u0) (MkInt8X64 v0) = MkBoolX64 $ (fromIntegral (eqInt8X64# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X64 Int8 where
  ltF (MkInt8X64 u0) (MkInt8X64 v0) = MkBoolX64 $ (fromIntegral (ltInt8X64# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X64 Int8 where
  minF (MkInt8X64 u0) (MkInt8X64 v0) = MkInt8X64 (minInt8X64# u0 v0)
  maxF (MkInt8X64 u0) (MkInt8X64 v0) = MkInt8X64 (maxInt8X64# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X64 Int8 where
  plusF (MkInt8X64 u0) (MkInt8X64 v0) = MkInt8X64 (plusInt8X64# u0 v0)
  minusF (MkInt8X64 u0) (MkInt8X64 v0) = MkInt8X64 (minusInt8X64# u0 v0)
  timesF (MkInt8X64 u0) (MkInt8X64 v0) = MkInt8X64 (timesInt8X64# u0 v0)
  negateF (MkInt8X64 u0) = MkInt8X64 (negateInt8X64# u0)
  absF (MkInt8X64 u0) = MkInt8X64 (absInt8X64# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X64 Int8 where
  andF (MkInt8X64 u0) (MkInt8X64 v0) = MkInt8X64 (andInt8X64# u0 v0)
  orF (MkInt8X64 u0) (MkInt8X64 v0) = MkInt8X64 (orInt8X64# u0 v0)
  xorF (MkInt8X64 u0) (MkInt8X64 v0) = MkInt8X64 (xorInt8X64# u0 v0)
  complementF (MkInt8X64 u0) = MkInt8X64 (complementInt8X64# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X64 Int8 where
  shiftLF (MkInt8X64 u0) (I# i) = MkInt8X64 (shiftLInt8X64# u0 i)
  shiftRF (MkInt8X64 u0) (I# i) = MkInt8X64 (shiftRInt8X64# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X64 Int8 where
  enumFromZero = MkInt8X64 (packInt8X64# (# 0#Int8, 1#Int8, 2#Int8, 3#Int8, 4#Int8, 5#Int8, 6#Int8, 7#Int8, 8#Int8, 9#Int8, 10#Int8, 11#Int8, 12#Int8, 13#Int8, 14#Int8, 15#Int8, 16#Int8, 17#Int8, 18#Int8, 19#Int8, 20#Int8, 21#Int8, 22#Int8, 23#Int8, 24#Int8, 25#Int8, 26#Int8, 27#Int8, 28#Int8, 29#Int8, 30#Int8, 31#Int8, 32#Int8, 33#Int8, 34#Int8, 35#Int8, 36#Int8, 37#Int8, 38#Int8, 39#Int8, 40#Int8, 41#Int8, 42#Int8, 43#Int8, 44#Int8, 45#Int8, 46#Int8, 47#Int8, 48#Int8, 49#Int8, 50#Int8, 51#Int8, 52#Int8, 53#Int8, 54#Int8, 55#Int8, 56#Int8, 57#Int8, 58#Int8, 59#Int8, 60#Int8, 61#Int8, 62#Int8, 63#Int8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X64 Int8 where
  indexByteArraySIMD# ba i = MkInt8X64 (indexInt8ArrayAsInt8X64# ba i)
  readByteArraySIMD# mba i s0 = case readInt8ArrayAsInt8X64# mba i s0 of (# s1, v0 #) -> (# s1, MkInt8X64 v0 #)
  writeByteArraySIMD# mba i (MkInt8X64 v0) s0 = writeInt8ArrayAsInt8X64# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X64 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt8OffAddrAsInt8X64# addr i s0 of (# s1, v0 #) -> (# s1, MkInt8X64 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X64 v0) = IO (\s0 -> case writeInt8OffAddrAsInt8X64# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X64 Int16 = MkInt16X64WithVec512 Int16X32# Int16X32#
instance PackX64 X64 Int16 where
  mkX64 (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) (I16# x8) (I16# x9) (I16# x10) (I16# x11) (I16# x12) (I16# x13) (I16# x14) (I16# x15) (I16# x16) (I16# x17) (I16# x18) (I16# x19) (I16# x20) (I16# x21) (I16# x22) (I16# x23) (I16# x24) (I16# x25) (I16# x26) (I16# x27) (I16# x28) (I16# x29) (I16# x30) (I16# x31) (I16# x32) (I16# x33) (I16# x34) (I16# x35) (I16# x36) (I16# x37) (I16# x38) (I16# x39) (I16# x40) (I16# x41) (I16# x42) (I16# x43) (I16# x44) (I16# x45) (I16# x46) (I16# x47) (I16# x48) (I16# x49) (I16# x50) (I16# x51) (I16# x52) (I16# x53) (I16# x54) (I16# x55) (I16# x56) (I16# x57) (I16# x58) (I16# x59) (I16# x60) (I16# x61) (I16# x62) (I16# x63) = MkInt16X64WithVec512 (packInt16X32# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #)) (packInt16X32# (# x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #))
  unpackX64 (MkInt16X64WithVec512 v0 v1) = case unpackInt16X32# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #) -> case unpackInt16X32# v1 of (# x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #) -> (I16# x0, I16# x1, I16# x2, I16# x3, I16# x4, I16# x5, I16# x6, I16# x7, I16# x8, I16# x9, I16# x10, I16# x11, I16# x12, I16# x13, I16# x14, I16# x15, I16# x16, I16# x17, I16# x18, I16# x19, I16# x20, I16# x21, I16# x22, I16# x23, I16# x24, I16# x25, I16# x26, I16# x27, I16# x28, I16# x29, I16# x30, I16# x31, I16# x32, I16# x33, I16# x34, I16# x35, I16# x36, I16# x37, I16# x38, I16# x39, I16# x40, I16# x41, I16# x42, I16# x43, I16# x44, I16# x45, I16# x46, I16# x47, I16# x48, I16# x49, I16# x50, I16# x51, I16# x52, I16# x53, I16# x54, I16# x55, I16# x56, I16# x57, I16# x58, I16# x59, I16# x60, I16# x61, I16# x62, I16# x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 Int16 where
  broadcast (I16# x) = let !v = broadcastInt16X32# x in MkInt16X64WithVec512 v v
  {-# INLINE broadcast #-}
instance SelectableF X64 Int16 where
  selectF (MkBoolX64 !cond) (MkInt16X64WithVec512 x0 x1) (MkInt16X64WithVec512 y0 y1) = MkInt16X64WithVec512 (selectInt16X32# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt16X32# (fromIntegral $ cond `unsafeShiftR` 32) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, i32 < 64, i33 < 64, i34 < 64, i35 < 64, i36 < 64, i37 < 64, i38 < 64, i39 < 64, i40 < 64, i41 < 64, i42 < 64, i43 < 64, i44 < 64, i45 < 64, i46 < 64, i47 < 64, i48 < 64, i49 < 64, i50 < 64, i51 < 64, i52 < 64, i53 < 64, i54 < 64, i55 < 64, i56 < 64, i57 < 64, i58 < 64, i59 < 64, i60 < 64, i61 < 64, i62 < 64, i63 < 64, ShuffleMany Int16X32# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Int16X32# [i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Int16 where
  unaryShuffle (MkInt16X64WithVec512 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt16X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 128, i1 < 128, i2 < 128, i3 < 128, i4 < 128, i5 < 128, i6 < 128, i7 < 128, i8 < 128, i9 < 128, i10 < 128, i11 < 128, i12 < 128, i13 < 128, i14 < 128, i15 < 128, i16 < 128, i17 < 128, i18 < 128, i19 < 128, i20 < 128, i21 < 128, i22 < 128, i23 < 128, i24 < 128, i25 < 128, i26 < 128, i27 < 128, i28 < 128, i29 < 128, i30 < 128, i31 < 128, i32 < 128, i33 < 128, i34 < 128, i35 < 128, i36 < 128, i37 < 128, i38 < 128, i39 < 128, i40 < 128, i41 < 128, i42 < 128, i43 < 128, i44 < 128, i45 < 128, i46 < 128, i47 < 128, i48 < 128, i49 < 128, i50 < 128, i51 < 128, i52 < 128, i53 < 128, i54 < 128, i55 < 128, i56 < 128, i57 < 128, i58 < 128, i59 < 128, i60 < 128, i61 < 128, i62 < 128, i63 < 128, ShuffleMany Int16X32# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Int16X32# [i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Int16 where
  binaryShuffle (MkInt16X64WithVec512 x0 x1) (MkInt16X64WithVec512 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt16X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X64 Int16 where
  eqF (MkInt16X64WithVec512 u0 u1) (MkInt16X64WithVec512 v0 v1) = MkBoolX64 $ (fromIntegral (eqInt16X32# u0 v0)) .|. (fromIntegral (eqInt16X32# u1 v1) `unsafeShiftL` 32)
  {-# INLINE eqF #-}
instance OrderedF X64 Int16 where
  ltF (MkInt16X64WithVec512 u0 u1) (MkInt16X64WithVec512 v0 v1) = MkBoolX64 $ (fromIntegral (ltInt16X32# u0 v0)) .|. (fromIntegral (ltInt16X32# u1 v1) `unsafeShiftL` 32)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X64 Int16 where
  minF (MkInt16X64WithVec512 u0 u1) (MkInt16X64WithVec512 v0 v1) = MkInt16X64WithVec512 (minInt16X32# u0 v0) (minInt16X32# u1 v1)
  maxF (MkInt16X64WithVec512 u0 u1) (MkInt16X64WithVec512 v0 v1) = MkInt16X64WithVec512 (maxInt16X32# u0 v0) (maxInt16X32# u1 v1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X64 Int16 where
  plusF (MkInt16X64WithVec512 u0 u1) (MkInt16X64WithVec512 v0 v1) = MkInt16X64WithVec512 (plusInt16X32# u0 v0) (plusInt16X32# u1 v1)
  minusF (MkInt16X64WithVec512 u0 u1) (MkInt16X64WithVec512 v0 v1) = MkInt16X64WithVec512 (minusInt16X32# u0 v0) (minusInt16X32# u1 v1)
  timesF (MkInt16X64WithVec512 u0 u1) (MkInt16X64WithVec512 v0 v1) = MkInt16X64WithVec512 (timesInt16X32# u0 v0) (timesInt16X32# u1 v1)
  negateF (MkInt16X64WithVec512 u0 u1) = MkInt16X64WithVec512 (negateInt16X32# u0) (negateInt16X32# u1)
  absF (MkInt16X64WithVec512 u0 u1) = MkInt16X64WithVec512 (absInt16X32# u0) (absInt16X32# u1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X64 Int16 where
  andF (MkInt16X64WithVec512 u0 u1) (MkInt16X64WithVec512 v0 v1) = MkInt16X64WithVec512 (andInt16X32# u0 v0) (andInt16X32# u1 v1)
  orF (MkInt16X64WithVec512 u0 u1) (MkInt16X64WithVec512 v0 v1) = MkInt16X64WithVec512 (orInt16X32# u0 v0) (orInt16X32# u1 v1)
  xorF (MkInt16X64WithVec512 u0 u1) (MkInt16X64WithVec512 v0 v1) = MkInt16X64WithVec512 (xorInt16X32# u0 v0) (xorInt16X32# u1 v1)
  complementF (MkInt16X64WithVec512 u0 u1) = MkInt16X64WithVec512 (complementInt16X32# u0) (complementInt16X32# u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X64 Int16 where
  shiftLF (MkInt16X64WithVec512 u0 u1) (I# i) = MkInt16X64WithVec512 (shiftLInt16X32# u0 i) (shiftLInt16X32# u1 i)
  shiftRF (MkInt16X64WithVec512 u0 u1) (I# i) = MkInt16X64WithVec512 (shiftRInt16X32# u0 i) (shiftRInt16X32# u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X64 Int16 where
  enumFromZero = MkInt16X64WithVec512 (packInt16X32# (# 0#Int16, 1#Int16, 2#Int16, 3#Int16, 4#Int16, 5#Int16, 6#Int16, 7#Int16, 8#Int16, 9#Int16, 10#Int16, 11#Int16, 12#Int16, 13#Int16, 14#Int16, 15#Int16, 16#Int16, 17#Int16, 18#Int16, 19#Int16, 20#Int16, 21#Int16, 22#Int16, 23#Int16, 24#Int16, 25#Int16, 26#Int16, 27#Int16, 28#Int16, 29#Int16, 30#Int16, 31#Int16 #)) (packInt16X32# (# 32#Int16, 33#Int16, 34#Int16, 35#Int16, 36#Int16, 37#Int16, 38#Int16, 39#Int16, 40#Int16, 41#Int16, 42#Int16, 43#Int16, 44#Int16, 45#Int16, 46#Int16, 47#Int16, 48#Int16, 49#Int16, 50#Int16, 51#Int16, 52#Int16, 53#Int16, 54#Int16, 55#Int16, 56#Int16, 57#Int16, 58#Int16, 59#Int16, 60#Int16, 61#Int16, 62#Int16, 63#Int16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X64 Int16 where
  indexByteArraySIMD# ba i = MkInt16X64WithVec512 (indexInt16ArrayAsInt16X32# ba i) (indexInt16ArrayAsInt16X32# ba (i +# 32#))
  readByteArraySIMD# mba i s0 = case readInt16ArrayAsInt16X32# mba i s0 of (# s1, v0 #) -> case readInt16ArrayAsInt16X32# mba (i +# 32#) s1 of (# s2, v1 #) -> (# s2, MkInt16X64WithVec512 v0 v1 #)
  writeByteArraySIMD# mba i (MkInt16X64WithVec512 v0 v1) s0 = case writeInt16ArrayAsInt16X32# mba i v0 s0 of s1 -> writeInt16ArrayAsInt16X32# mba (i +# 32#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X64 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt16OffAddrAsInt16X32# addr i s0 of (# s1, v0 #) -> case readInt16OffAddrAsInt16X32# addr (i +# 32#) s1 of (# s2, v1 #) -> (# s2, MkInt16X64WithVec512 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X64WithVec512 v0 v1) = IO (\s0 -> case writeInt16OffAddrAsInt16X32# addr i v0 s0 of s1 -> case writeInt16OffAddrAsInt16X32# addr (i +# 32#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X64 Int32 = MkInt32X64WithVec512 Int32X16# Int32X16# Int32X16# Int32X16#
instance PackX64 X64 Int32 where
  mkX64 (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) (I32# x8) (I32# x9) (I32# x10) (I32# x11) (I32# x12) (I32# x13) (I32# x14) (I32# x15) (I32# x16) (I32# x17) (I32# x18) (I32# x19) (I32# x20) (I32# x21) (I32# x22) (I32# x23) (I32# x24) (I32# x25) (I32# x26) (I32# x27) (I32# x28) (I32# x29) (I32# x30) (I32# x31) (I32# x32) (I32# x33) (I32# x34) (I32# x35) (I32# x36) (I32# x37) (I32# x38) (I32# x39) (I32# x40) (I32# x41) (I32# x42) (I32# x43) (I32# x44) (I32# x45) (I32# x46) (I32# x47) (I32# x48) (I32# x49) (I32# x50) (I32# x51) (I32# x52) (I32# x53) (I32# x54) (I32# x55) (I32# x56) (I32# x57) (I32# x58) (I32# x59) (I32# x60) (I32# x61) (I32# x62) (I32# x63) = MkInt32X64WithVec512 (packInt32X16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #)) (packInt32X16# (# x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #)) (packInt32X16# (# x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47 #)) (packInt32X16# (# x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #))
  unpackX64 (MkInt32X64WithVec512 v0 v1 v2 v3) = case unpackInt32X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackInt32X16# v1 of (# x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #) -> case unpackInt32X16# v2 of (# x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47 #) -> case unpackInt32X16# v3 of (# x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #) -> (I32# x0, I32# x1, I32# x2, I32# x3, I32# x4, I32# x5, I32# x6, I32# x7, I32# x8, I32# x9, I32# x10, I32# x11, I32# x12, I32# x13, I32# x14, I32# x15, I32# x16, I32# x17, I32# x18, I32# x19, I32# x20, I32# x21, I32# x22, I32# x23, I32# x24, I32# x25, I32# x26, I32# x27, I32# x28, I32# x29, I32# x30, I32# x31, I32# x32, I32# x33, I32# x34, I32# x35, I32# x36, I32# x37, I32# x38, I32# x39, I32# x40, I32# x41, I32# x42, I32# x43, I32# x44, I32# x45, I32# x46, I32# x47, I32# x48, I32# x49, I32# x50, I32# x51, I32# x52, I32# x53, I32# x54, I32# x55, I32# x56, I32# x57, I32# x58, I32# x59, I32# x60, I32# x61, I32# x62, I32# x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 Int32 where
  broadcast (I32# x) = let !v = broadcastInt32X16# x in MkInt32X64WithVec512 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X64 Int32 where
  selectF (MkBoolX64 !cond) (MkInt32X64WithVec512 x0 x1 x2 x3) (MkInt32X64WithVec512 y0 y1 y2 y3) = MkInt32X64WithVec512 (selectInt32X16# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt32X16# (fromIntegral $ cond `unsafeShiftR` 16) x1 y1) (selectInt32X16# (fromIntegral $ cond `unsafeShiftR` 32) x2 y2) (selectInt32X16# (fromIntegral $ cond `unsafeShiftR` 48) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, i32 < 64, i33 < 64, i34 < 64, i35 < 64, i36 < 64, i37 < 64, i38 < 64, i39 < 64, i40 < 64, i41 < 64, i42 < 64, i43 < 64, i44 < 64, i45 < 64, i46 < 64, i47 < 64, i48 < 64, i49 < 64, i50 < 64, i51 < 64, i52 < 64, i53 < 64, i54 < 64, i55 < 64, i56 < 64, i57 < 64, i58 < 64, i59 < 64, i60 < 64, i61 < 64, i62 < 64, i63 < 64, ShuffleMany Int32X16# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Int32X16# [i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Int32X16# [i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany Int32X16# [i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Int32 where
  unaryShuffle (MkInt32X64WithVec512 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt32X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 128, i1 < 128, i2 < 128, i3 < 128, i4 < 128, i5 < 128, i6 < 128, i7 < 128, i8 < 128, i9 < 128, i10 < 128, i11 < 128, i12 < 128, i13 < 128, i14 < 128, i15 < 128, i16 < 128, i17 < 128, i18 < 128, i19 < 128, i20 < 128, i21 < 128, i22 < 128, i23 < 128, i24 < 128, i25 < 128, i26 < 128, i27 < 128, i28 < 128, i29 < 128, i30 < 128, i31 < 128, i32 < 128, i33 < 128, i34 < 128, i35 < 128, i36 < 128, i37 < 128, i38 < 128, i39 < 128, i40 < 128, i41 < 128, i42 < 128, i43 < 128, i44 < 128, i45 < 128, i46 < 128, i47 < 128, i48 < 128, i49 < 128, i50 < 128, i51 < 128, i52 < 128, i53 < 128, i54 < 128, i55 < 128, i56 < 128, i57 < 128, i58 < 128, i59 < 128, i60 < 128, i61 < 128, i62 < 128, i63 < 128, ShuffleMany Int32X16# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Int32X16# [i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Int32X16# [i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany Int32X16# [i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Int32 where
  binaryShuffle (MkInt32X64WithVec512 x0 x1 x2 x3) (MkInt32X64WithVec512 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt32X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X64 Int32 where
  eqF (MkInt32X64WithVec512 u0 u1 u2 u3) (MkInt32X64WithVec512 v0 v1 v2 v3) = MkBoolX64 $ (fromIntegral (eqInt32X16# u0 v0)) .|. (fromIntegral (eqInt32X16# u1 v1) `unsafeShiftL` 16) .|. (fromIntegral (eqInt32X16# u2 v2) `unsafeShiftL` 32) .|. (fromIntegral (eqInt32X16# u3 v3) `unsafeShiftL` 48)
  {-# INLINE eqF #-}
instance OrderedF X64 Int32 where
  ltF (MkInt32X64WithVec512 u0 u1 u2 u3) (MkInt32X64WithVec512 v0 v1 v2 v3) = MkBoolX64 $ (fromIntegral (ltInt32X16# u0 v0)) .|. (fromIntegral (ltInt32X16# u1 v1) `unsafeShiftL` 16) .|. (fromIntegral (ltInt32X16# u2 v2) `unsafeShiftL` 32) .|. (fromIntegral (ltInt32X16# u3 v3) `unsafeShiftL` 48)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X64 Int32 where
  minF (MkInt32X64WithVec512 u0 u1 u2 u3) (MkInt32X64WithVec512 v0 v1 v2 v3) = MkInt32X64WithVec512 (minInt32X16# u0 v0) (minInt32X16# u1 v1) (minInt32X16# u2 v2) (minInt32X16# u3 v3)
  maxF (MkInt32X64WithVec512 u0 u1 u2 u3) (MkInt32X64WithVec512 v0 v1 v2 v3) = MkInt32X64WithVec512 (maxInt32X16# u0 v0) (maxInt32X16# u1 v1) (maxInt32X16# u2 v2) (maxInt32X16# u3 v3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X64 Int32 where
  plusF (MkInt32X64WithVec512 u0 u1 u2 u3) (MkInt32X64WithVec512 v0 v1 v2 v3) = MkInt32X64WithVec512 (plusInt32X16# u0 v0) (plusInt32X16# u1 v1) (plusInt32X16# u2 v2) (plusInt32X16# u3 v3)
  minusF (MkInt32X64WithVec512 u0 u1 u2 u3) (MkInt32X64WithVec512 v0 v1 v2 v3) = MkInt32X64WithVec512 (minusInt32X16# u0 v0) (minusInt32X16# u1 v1) (minusInt32X16# u2 v2) (minusInt32X16# u3 v3)
  timesF (MkInt32X64WithVec512 u0 u1 u2 u3) (MkInt32X64WithVec512 v0 v1 v2 v3) = MkInt32X64WithVec512 (timesInt32X16# u0 v0) (timesInt32X16# u1 v1) (timesInt32X16# u2 v2) (timesInt32X16# u3 v3)
  negateF (MkInt32X64WithVec512 u0 u1 u2 u3) = MkInt32X64WithVec512 (negateInt32X16# u0) (negateInt32X16# u1) (negateInt32X16# u2) (negateInt32X16# u3)
  absF (MkInt32X64WithVec512 u0 u1 u2 u3) = MkInt32X64WithVec512 (absInt32X16# u0) (absInt32X16# u1) (absInt32X16# u2) (absInt32X16# u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X64 Int32 where
  andF (MkInt32X64WithVec512 u0 u1 u2 u3) (MkInt32X64WithVec512 v0 v1 v2 v3) = MkInt32X64WithVec512 (andInt32X16# u0 v0) (andInt32X16# u1 v1) (andInt32X16# u2 v2) (andInt32X16# u3 v3)
  orF (MkInt32X64WithVec512 u0 u1 u2 u3) (MkInt32X64WithVec512 v0 v1 v2 v3) = MkInt32X64WithVec512 (orInt32X16# u0 v0) (orInt32X16# u1 v1) (orInt32X16# u2 v2) (orInt32X16# u3 v3)
  xorF (MkInt32X64WithVec512 u0 u1 u2 u3) (MkInt32X64WithVec512 v0 v1 v2 v3) = MkInt32X64WithVec512 (xorInt32X16# u0 v0) (xorInt32X16# u1 v1) (xorInt32X16# u2 v2) (xorInt32X16# u3 v3)
  complementF (MkInt32X64WithVec512 u0 u1 u2 u3) = MkInt32X64WithVec512 (complementInt32X16# u0) (complementInt32X16# u1) (complementInt32X16# u2) (complementInt32X16# u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X64 Int32 where
  shiftLF (MkInt32X64WithVec512 u0 u1 u2 u3) (I# i) = MkInt32X64WithVec512 (shiftLInt32X16# u0 i) (shiftLInt32X16# u1 i) (shiftLInt32X16# u2 i) (shiftLInt32X16# u3 i)
  shiftRF (MkInt32X64WithVec512 u0 u1 u2 u3) (I# i) = MkInt32X64WithVec512 (shiftRInt32X16# u0 i) (shiftRInt32X16# u1 i) (shiftRInt32X16# u2 i) (shiftRInt32X16# u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X64 Int32 where
  enumFromZero = MkInt32X64WithVec512 (packInt32X16# (# 0#Int32, 1#Int32, 2#Int32, 3#Int32, 4#Int32, 5#Int32, 6#Int32, 7#Int32, 8#Int32, 9#Int32, 10#Int32, 11#Int32, 12#Int32, 13#Int32, 14#Int32, 15#Int32 #)) (packInt32X16# (# 16#Int32, 17#Int32, 18#Int32, 19#Int32, 20#Int32, 21#Int32, 22#Int32, 23#Int32, 24#Int32, 25#Int32, 26#Int32, 27#Int32, 28#Int32, 29#Int32, 30#Int32, 31#Int32 #)) (packInt32X16# (# 32#Int32, 33#Int32, 34#Int32, 35#Int32, 36#Int32, 37#Int32, 38#Int32, 39#Int32, 40#Int32, 41#Int32, 42#Int32, 43#Int32, 44#Int32, 45#Int32, 46#Int32, 47#Int32 #)) (packInt32X16# (# 48#Int32, 49#Int32, 50#Int32, 51#Int32, 52#Int32, 53#Int32, 54#Int32, 55#Int32, 56#Int32, 57#Int32, 58#Int32, 59#Int32, 60#Int32, 61#Int32, 62#Int32, 63#Int32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X64 Int32 where
  indexByteArraySIMD# ba i = MkInt32X64WithVec512 (indexInt32ArrayAsInt32X16# ba i) (indexInt32ArrayAsInt32X16# ba (i +# 16#)) (indexInt32ArrayAsInt32X16# ba (i +# 32#)) (indexInt32ArrayAsInt32X16# ba (i +# 48#))
  readByteArraySIMD# mba i s0 = case readInt32ArrayAsInt32X16# mba i s0 of (# s1, v0 #) -> case readInt32ArrayAsInt32X16# mba (i +# 16#) s1 of (# s2, v1 #) -> case readInt32ArrayAsInt32X16# mba (i +# 32#) s2 of (# s3, v2 #) -> case readInt32ArrayAsInt32X16# mba (i +# 48#) s3 of (# s4, v3 #) -> (# s4, MkInt32X64WithVec512 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkInt32X64WithVec512 v0 v1 v2 v3) s0 = case writeInt32ArrayAsInt32X16# mba i v0 s0 of s1 -> case writeInt32ArrayAsInt32X16# mba (i +# 16#) v1 s1 of s2 -> case writeInt32ArrayAsInt32X16# mba (i +# 32#) v2 s2 of s3 -> writeInt32ArrayAsInt32X16# mba (i +# 48#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X64 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt32OffAddrAsInt32X16# addr i s0 of (# s1, v0 #) -> case readInt32OffAddrAsInt32X16# addr (i +# 16#) s1 of (# s2, v1 #) -> case readInt32OffAddrAsInt32X16# addr (i +# 32#) s2 of (# s3, v2 #) -> case readInt32OffAddrAsInt32X16# addr (i +# 48#) s3 of (# s4, v3 #) -> (# s4, MkInt32X64WithVec512 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X64WithVec512 v0 v1 v2 v3) = IO (\s0 -> case writeInt32OffAddrAsInt32X16# addr i v0 s0 of s1 -> case writeInt32OffAddrAsInt32X16# addr (i +# 16#) v1 s1 of s2 -> case writeInt32OffAddrAsInt32X16# addr (i +# 32#) v2 s2 of s3 -> case writeInt32OffAddrAsInt32X16# addr (i +# 48#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X64 Int64 = MkInt64X64WithVec512 Int64X8# Int64X8# Int64X8# Int64X8# Int64X8# Int64X8# Int64X8# Int64X8#
instance PackX64 X64 Int64 where
  mkX64 (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) (I64# x8) (I64# x9) (I64# x10) (I64# x11) (I64# x12) (I64# x13) (I64# x14) (I64# x15) (I64# x16) (I64# x17) (I64# x18) (I64# x19) (I64# x20) (I64# x21) (I64# x22) (I64# x23) (I64# x24) (I64# x25) (I64# x26) (I64# x27) (I64# x28) (I64# x29) (I64# x30) (I64# x31) (I64# x32) (I64# x33) (I64# x34) (I64# x35) (I64# x36) (I64# x37) (I64# x38) (I64# x39) (I64# x40) (I64# x41) (I64# x42) (I64# x43) (I64# x44) (I64# x45) (I64# x46) (I64# x47) (I64# x48) (I64# x49) (I64# x50) (I64# x51) (I64# x52) (I64# x53) (I64# x54) (I64# x55) (I64# x56) (I64# x57) (I64# x58) (I64# x59) (I64# x60) (I64# x61) (I64# x62) (I64# x63) = MkInt64X64WithVec512 (packInt64X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packInt64X8# (# x8, x9, x10, x11, x12, x13, x14, x15 #)) (packInt64X8# (# x16, x17, x18, x19, x20, x21, x22, x23 #)) (packInt64X8# (# x24, x25, x26, x27, x28, x29, x30, x31 #)) (packInt64X8# (# x32, x33, x34, x35, x36, x37, x38, x39 #)) (packInt64X8# (# x40, x41, x42, x43, x44, x45, x46, x47 #)) (packInt64X8# (# x48, x49, x50, x51, x52, x53, x54, x55 #)) (packInt64X8# (# x56, x57, x58, x59, x60, x61, x62, x63 #))
  unpackX64 (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = case unpackInt64X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackInt64X8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackInt64X8# v2 of (# x16, x17, x18, x19, x20, x21, x22, x23 #) -> case unpackInt64X8# v3 of (# x24, x25, x26, x27, x28, x29, x30, x31 #) -> case unpackInt64X8# v4 of (# x32, x33, x34, x35, x36, x37, x38, x39 #) -> case unpackInt64X8# v5 of (# x40, x41, x42, x43, x44, x45, x46, x47 #) -> case unpackInt64X8# v6 of (# x48, x49, x50, x51, x52, x53, x54, x55 #) -> case unpackInt64X8# v7 of (# x56, x57, x58, x59, x60, x61, x62, x63 #) -> (I64# x0, I64# x1, I64# x2, I64# x3, I64# x4, I64# x5, I64# x6, I64# x7, I64# x8, I64# x9, I64# x10, I64# x11, I64# x12, I64# x13, I64# x14, I64# x15, I64# x16, I64# x17, I64# x18, I64# x19, I64# x20, I64# x21, I64# x22, I64# x23, I64# x24, I64# x25, I64# x26, I64# x27, I64# x28, I64# x29, I64# x30, I64# x31, I64# x32, I64# x33, I64# x34, I64# x35, I64# x36, I64# x37, I64# x38, I64# x39, I64# x40, I64# x41, I64# x42, I64# x43, I64# x44, I64# x45, I64# x46, I64# x47, I64# x48, I64# x49, I64# x50, I64# x51, I64# x52, I64# x53, I64# x54, I64# x55, I64# x56, I64# x57, I64# x58, I64# x59, I64# x60, I64# x61, I64# x62, I64# x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 Int64 where
  broadcast (I64# x) = let !v = broadcastInt64X8# x in MkInt64X64WithVec512 v v v v v v v v
  {-# INLINE broadcast #-}
instance SelectableF X64 Int64 where
  selectF (MkBoolX64 !cond) (MkInt64X64WithVec512 x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X64WithVec512 y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X64WithVec512 (selectInt64X8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectInt64X8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1) (selectInt64X8# (fromIntegral $ cond `unsafeShiftR` 16) x2 y2) (selectInt64X8# (fromIntegral $ cond `unsafeShiftR` 24) x3 y3) (selectInt64X8# (fromIntegral $ cond `unsafeShiftR` 32) x4 y4) (selectInt64X8# (fromIntegral $ cond `unsafeShiftR` 40) x5 y5) (selectInt64X8# (fromIntegral $ cond `unsafeShiftR` 48) x6 y6) (selectInt64X8# (fromIntegral $ cond `unsafeShiftR` 56) x7 y7)
  {-# INLINE selectF #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, i32 < 64, i33 < 64, i34 < 64, i35 < 64, i36 < 64, i37 < 64, i38 < 64, i39 < 64, i40 < 64, i41 < 64, i42 < 64, i43 < 64, i44 < 64, i45 < 64, i46 < 64, i47 < 64, i48 < 64, i49 < 64, i50 < 64, i51 < 64, i52 < 64, i53 < 64, i54 < 64, i55 < 64, i56 < 64, i57 < 64, i58 < 64, i59 < 64, i60 < 64, i61 < 64, i62 < 64, i63 < 64, ShuffleMany Int64X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Int64X8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Int64X8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany Int64X8# [i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Int64X8# [i32, i33, i34, i35, i36, i37, i38, i39], ShuffleMany Int64X8# [i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany Int64X8# [i48, i49, i50, i51, i52, i53, i54, i55], ShuffleMany Int64X8# [i56, i57, i58, i59, i60, i61, i62, i63]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Int64 where
  unaryShuffle (MkInt64X64WithVec512 x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt64X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39] sources) (shuffleMany# @_ @_ @[i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55] sources) (shuffleMany# @_ @_ @[i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 128, i1 < 128, i2 < 128, i3 < 128, i4 < 128, i5 < 128, i6 < 128, i7 < 128, i8 < 128, i9 < 128, i10 < 128, i11 < 128, i12 < 128, i13 < 128, i14 < 128, i15 < 128, i16 < 128, i17 < 128, i18 < 128, i19 < 128, i20 < 128, i21 < 128, i22 < 128, i23 < 128, i24 < 128, i25 < 128, i26 < 128, i27 < 128, i28 < 128, i29 < 128, i30 < 128, i31 < 128, i32 < 128, i33 < 128, i34 < 128, i35 < 128, i36 < 128, i37 < 128, i38 < 128, i39 < 128, i40 < 128, i41 < 128, i42 < 128, i43 < 128, i44 < 128, i45 < 128, i46 < 128, i47 < 128, i48 < 128, i49 < 128, i50 < 128, i51 < 128, i52 < 128, i53 < 128, i54 < 128, i55 < 128, i56 < 128, i57 < 128, i58 < 128, i59 < 128, i60 < 128, i61 < 128, i62 < 128, i63 < 128, ShuffleMany Int64X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Int64X8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Int64X8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany Int64X8# [i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Int64X8# [i32, i33, i34, i35, i36, i37, i38, i39], ShuffleMany Int64X8# [i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany Int64X8# [i48, i49, i50, i51, i52, i53, i54, i55], ShuffleMany Int64X8# [i56, i57, i58, i59, i60, i61, i62, i63]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Int64 where
  binaryShuffle (MkInt64X64WithVec512 x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X64WithVec512 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt64X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39] sources) (shuffleMany# @_ @_ @[i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55] sources) (shuffleMany# @_ @_ @[i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X64 Int64 where
  eqF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX64 $ (fromIntegral (eqInt64X8# u0 v0)) .|. (fromIntegral (eqInt64X8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (eqInt64X8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (eqInt64X8# u3 v3) `unsafeShiftL` 24) .|. (fromIntegral (eqInt64X8# u4 v4) `unsafeShiftL` 32) .|. (fromIntegral (eqInt64X8# u5 v5) `unsafeShiftL` 40) .|. (fromIntegral (eqInt64X8# u6 v6) `unsafeShiftL` 48) .|. (fromIntegral (eqInt64X8# u7 v7) `unsafeShiftL` 56)
  {-# INLINE eqF #-}
instance OrderedF X64 Int64 where
  ltF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX64 $ (fromIntegral (ltInt64X8# u0 v0)) .|. (fromIntegral (ltInt64X8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (ltInt64X8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (ltInt64X8# u3 v3) `unsafeShiftL` 24) .|. (fromIntegral (ltInt64X8# u4 v4) `unsafeShiftL` 32) .|. (fromIntegral (ltInt64X8# u5 v5) `unsafeShiftL` 40) .|. (fromIntegral (ltInt64X8# u6 v6) `unsafeShiftL` 48) .|. (fromIntegral (ltInt64X8# u7 v7) `unsafeShiftL` 56)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X64 Int64 where
  minF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X64WithVec512 (minInt64X8# u0 v0) (minInt64X8# u1 v1) (minInt64X8# u2 v2) (minInt64X8# u3 v3) (minInt64X8# u4 v4) (minInt64X8# u5 v5) (minInt64X8# u6 v6) (minInt64X8# u7 v7)
  maxF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X64WithVec512 (maxInt64X8# u0 v0) (maxInt64X8# u1 v1) (maxInt64X8# u2 v2) (maxInt64X8# u3 v3) (maxInt64X8# u4 v4) (maxInt64X8# u5 v5) (maxInt64X8# u6 v6) (maxInt64X8# u7 v7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X64 Int64 where
  plusF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X64WithVec512 (plusInt64X8# u0 v0) (plusInt64X8# u1 v1) (plusInt64X8# u2 v2) (plusInt64X8# u3 v3) (plusInt64X8# u4 v4) (plusInt64X8# u5 v5) (plusInt64X8# u6 v6) (plusInt64X8# u7 v7)
  minusF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X64WithVec512 (minusInt64X8# u0 v0) (minusInt64X8# u1 v1) (minusInt64X8# u2 v2) (minusInt64X8# u3 v3) (minusInt64X8# u4 v4) (minusInt64X8# u5 v5) (minusInt64X8# u6 v6) (minusInt64X8# u7 v7)
  timesF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X64WithVec512 (timesInt64X8# u0 v0) (timesInt64X8# u1 v1) (timesInt64X8# u2 v2) (timesInt64X8# u3 v3) (timesInt64X8# u4 v4) (timesInt64X8# u5 v5) (timesInt64X8# u6 v6) (timesInt64X8# u7 v7)
  negateF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) = MkInt64X64WithVec512 (negateInt64X8# u0) (negateInt64X8# u1) (negateInt64X8# u2) (negateInt64X8# u3) (negateInt64X8# u4) (negateInt64X8# u5) (negateInt64X8# u6) (negateInt64X8# u7)
  absF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) = MkInt64X64WithVec512 (absInt64X8# u0) (absInt64X8# u1) (absInt64X8# u2) (absInt64X8# u3) (absInt64X8# u4) (absInt64X8# u5) (absInt64X8# u6) (absInt64X8# u7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X64 Int64 where
  andF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X64WithVec512 (andInt64X8# u0 v0) (andInt64X8# u1 v1) (andInt64X8# u2 v2) (andInt64X8# u3 v3) (andInt64X8# u4 v4) (andInt64X8# u5 v5) (andInt64X8# u6 v6) (andInt64X8# u7 v7)
  orF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X64WithVec512 (orInt64X8# u0 v0) (orInt64X8# u1 v1) (orInt64X8# u2 v2) (orInt64X8# u3 v3) (orInt64X8# u4 v4) (orInt64X8# u5 v5) (orInt64X8# u6 v6) (orInt64X8# u7 v7)
  xorF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkInt64X64WithVec512 (xorInt64X8# u0 v0) (xorInt64X8# u1 v1) (xorInt64X8# u2 v2) (xorInt64X8# u3 v3) (xorInt64X8# u4 v4) (xorInt64X8# u5 v5) (xorInt64X8# u6 v6) (xorInt64X8# u7 v7)
  complementF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) = MkInt64X64WithVec512 (complementInt64X8# u0) (complementInt64X8# u1) (complementInt64X8# u2) (complementInt64X8# u3) (complementInt64X8# u4) (complementInt64X8# u5) (complementInt64X8# u6) (complementInt64X8# u7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X64 Int64 where
  shiftLF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkInt64X64WithVec512 (shiftLInt64X8# u0 i) (shiftLInt64X8# u1 i) (shiftLInt64X8# u2 i) (shiftLInt64X8# u3 i) (shiftLInt64X8# u4 i) (shiftLInt64X8# u5 i) (shiftLInt64X8# u6 i) (shiftLInt64X8# u7 i)
  shiftRF (MkInt64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkInt64X64WithVec512 (shiftRInt64X8# u0 i) (shiftRInt64X8# u1 i) (shiftRInt64X8# u2 i) (shiftRInt64X8# u3 i) (shiftRInt64X8# u4 i) (shiftRInt64X8# u5 i) (shiftRInt64X8# u6 i) (shiftRInt64X8# u7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X64 Int64 where
  enumFromZero = MkInt64X64WithVec512 (packInt64X8# (# 0#Int64, 1#Int64, 2#Int64, 3#Int64, 4#Int64, 5#Int64, 6#Int64, 7#Int64 #)) (packInt64X8# (# 8#Int64, 9#Int64, 10#Int64, 11#Int64, 12#Int64, 13#Int64, 14#Int64, 15#Int64 #)) (packInt64X8# (# 16#Int64, 17#Int64, 18#Int64, 19#Int64, 20#Int64, 21#Int64, 22#Int64, 23#Int64 #)) (packInt64X8# (# 24#Int64, 25#Int64, 26#Int64, 27#Int64, 28#Int64, 29#Int64, 30#Int64, 31#Int64 #)) (packInt64X8# (# 32#Int64, 33#Int64, 34#Int64, 35#Int64, 36#Int64, 37#Int64, 38#Int64, 39#Int64 #)) (packInt64X8# (# 40#Int64, 41#Int64, 42#Int64, 43#Int64, 44#Int64, 45#Int64, 46#Int64, 47#Int64 #)) (packInt64X8# (# 48#Int64, 49#Int64, 50#Int64, 51#Int64, 52#Int64, 53#Int64, 54#Int64, 55#Int64 #)) (packInt64X8# (# 56#Int64, 57#Int64, 58#Int64, 59#Int64, 60#Int64, 61#Int64, 62#Int64, 63#Int64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X64 Int64 where
  indexByteArraySIMD# ba i = MkInt64X64WithVec512 (indexInt64ArrayAsInt64X8# ba i) (indexInt64ArrayAsInt64X8# ba (i +# 8#)) (indexInt64ArrayAsInt64X8# ba (i +# 16#)) (indexInt64ArrayAsInt64X8# ba (i +# 24#)) (indexInt64ArrayAsInt64X8# ba (i +# 32#)) (indexInt64ArrayAsInt64X8# ba (i +# 40#)) (indexInt64ArrayAsInt64X8# ba (i +# 48#)) (indexInt64ArrayAsInt64X8# ba (i +# 56#))
  readByteArraySIMD# mba i s0 = case readInt64ArrayAsInt64X8# mba i s0 of (# s1, v0 #) -> case readInt64ArrayAsInt64X8# mba (i +# 8#) s1 of (# s2, v1 #) -> case readInt64ArrayAsInt64X8# mba (i +# 16#) s2 of (# s3, v2 #) -> case readInt64ArrayAsInt64X8# mba (i +# 24#) s3 of (# s4, v3 #) -> case readInt64ArrayAsInt64X8# mba (i +# 32#) s4 of (# s5, v4 #) -> case readInt64ArrayAsInt64X8# mba (i +# 40#) s5 of (# s6, v5 #) -> case readInt64ArrayAsInt64X8# mba (i +# 48#) s6 of (# s7, v6 #) -> case readInt64ArrayAsInt64X8# mba (i +# 56#) s7 of (# s8, v7 #) -> (# s8, MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7 #)
  writeByteArraySIMD# mba i (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) s0 = case writeInt64ArrayAsInt64X8# mba i v0 s0 of s1 -> case writeInt64ArrayAsInt64X8# mba (i +# 8#) v1 s1 of s2 -> case writeInt64ArrayAsInt64X8# mba (i +# 16#) v2 s2 of s3 -> case writeInt64ArrayAsInt64X8# mba (i +# 24#) v3 s3 of s4 -> case writeInt64ArrayAsInt64X8# mba (i +# 32#) v4 s4 of s5 -> case writeInt64ArrayAsInt64X8# mba (i +# 40#) v5 s5 of s6 -> case writeInt64ArrayAsInt64X8# mba (i +# 48#) v6 s6 of s7 -> writeInt64ArrayAsInt64X8# mba (i +# 56#) v7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X64 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt64OffAddrAsInt64X8# addr i s0 of (# s1, v0 #) -> case readInt64OffAddrAsInt64X8# addr (i +# 8#) s1 of (# s2, v1 #) -> case readInt64OffAddrAsInt64X8# addr (i +# 16#) s2 of (# s3, v2 #) -> case readInt64OffAddrAsInt64X8# addr (i +# 24#) s3 of (# s4, v3 #) -> case readInt64OffAddrAsInt64X8# addr (i +# 32#) s4 of (# s5, v4 #) -> case readInt64OffAddrAsInt64X8# addr (i +# 40#) s5 of (# s6, v5 #) -> case readInt64OffAddrAsInt64X8# addr (i +# 48#) s6 of (# s7, v6 #) -> case readInt64OffAddrAsInt64X8# addr (i +# 56#) s7 of (# s8, v7 #) -> (# s8, MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = IO (\s0 -> case writeInt64OffAddrAsInt64X8# addr i v0 s0 of s1 -> case writeInt64OffAddrAsInt64X8# addr (i +# 8#) v1 s1 of s2 -> case writeInt64OffAddrAsInt64X8# addr (i +# 16#) v2 s2 of s3 -> case writeInt64OffAddrAsInt64X8# addr (i +# 24#) v3 s3 of s4 -> case writeInt64OffAddrAsInt64X8# addr (i +# 32#) v4 s4 of s5 -> case writeInt64OffAddrAsInt64X8# addr (i +# 40#) v5 s5 of s6 -> case writeInt64OffAddrAsInt64X8# addr (i +# 48#) v6 s6 of s7 -> case writeInt64OffAddrAsInt64X8# addr (i +# 56#) v7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X64 Word8 = MkWord8X64 Word8X64#
instance PackX64 X64 Word8 where
  mkX64 (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) (W8# x8) (W8# x9) (W8# x10) (W8# x11) (W8# x12) (W8# x13) (W8# x14) (W8# x15) (W8# x16) (W8# x17) (W8# x18) (W8# x19) (W8# x20) (W8# x21) (W8# x22) (W8# x23) (W8# x24) (W8# x25) (W8# x26) (W8# x27) (W8# x28) (W8# x29) (W8# x30) (W8# x31) (W8# x32) (W8# x33) (W8# x34) (W8# x35) (W8# x36) (W8# x37) (W8# x38) (W8# x39) (W8# x40) (W8# x41) (W8# x42) (W8# x43) (W8# x44) (W8# x45) (W8# x46) (W8# x47) (W8# x48) (W8# x49) (W8# x50) (W8# x51) (W8# x52) (W8# x53) (W8# x54) (W8# x55) (W8# x56) (W8# x57) (W8# x58) (W8# x59) (W8# x60) (W8# x61) (W8# x62) (W8# x63) = MkWord8X64 (packWord8X64# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #))
  unpackX64 (MkWord8X64 v0) = case unpackWord8X64# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #) -> (W8# x0, W8# x1, W8# x2, W8# x3, W8# x4, W8# x5, W8# x6, W8# x7, W8# x8, W8# x9, W8# x10, W8# x11, W8# x12, W8# x13, W8# x14, W8# x15, W8# x16, W8# x17, W8# x18, W8# x19, W8# x20, W8# x21, W8# x22, W8# x23, W8# x24, W8# x25, W8# x26, W8# x27, W8# x28, W8# x29, W8# x30, W8# x31, W8# x32, W8# x33, W8# x34, W8# x35, W8# x36, W8# x37, W8# x38, W8# x39, W8# x40, W8# x41, W8# x42, W8# x43, W8# x44, W8# x45, W8# x46, W8# x47, W8# x48, W8# x49, W8# x50, W8# x51, W8# x52, W8# x53, W8# x54, W8# x55, W8# x56, W8# x57, W8# x58, W8# x59, W8# x60, W8# x61, W8# x62, W8# x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 Word8 where
  broadcast (W8# x) = MkWord8X64 (broadcastWord8X64# x)
  {-# INLINE broadcast #-}
instance SelectableF X64 Word8 where
  selectF (MkBoolX64 !cond) (MkWord8X64 x0) (MkWord8X64 y0) = MkWord8X64 (selectWord8X64# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 64, ShuffleMany Word8X64# indices) => UnaryShuffleT indices X64 Word8 where
  unaryShuffle (MkWord8X64 x) = MkWord8X64 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 128, ShuffleMany Word8X64# indices) => BinaryShuffleT indices X64 Word8 where
  binaryShuffle (MkWord8X64 x0) (MkWord8X64 x1) = MkWord8X64 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X64 Word8 where
  eqF (MkWord8X64 u0) (MkWord8X64 v0) = MkBoolX64 $ (fromIntegral (eqWord8X64# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X64 Word8 where
  ltF (MkWord8X64 u0) (MkWord8X64 v0) = MkBoolX64 $ (fromIntegral (ltWord8X64# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X64 Word8 where
  minF (MkWord8X64 u0) (MkWord8X64 v0) = MkWord8X64 (minWord8X64# u0 v0)
  maxF (MkWord8X64 u0) (MkWord8X64 v0) = MkWord8X64 (maxWord8X64# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X64 Word8 where
  plusF (MkWord8X64 u0) (MkWord8X64 v0) = MkWord8X64 (plusWord8X64# u0 v0)
  minusF (MkWord8X64 u0) (MkWord8X64 v0) = MkWord8X64 (minusWord8X64# u0 v0)
  timesF (MkWord8X64 u0) (MkWord8X64 v0) = MkWord8X64 (timesWord8X64# u0 v0)
  -- Currently, there is no negateWord8X64#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X64 Word8 where
  andF (MkWord8X64 u0) (MkWord8X64 v0) = MkWord8X64 (andWord8X64# u0 v0)
  orF (MkWord8X64 u0) (MkWord8X64 v0) = MkWord8X64 (orWord8X64# u0 v0)
  xorF (MkWord8X64 u0) (MkWord8X64 v0) = MkWord8X64 (xorWord8X64# u0 v0)
  complementF (MkWord8X64 u0) = MkWord8X64 (complementWord8X64# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X64 Word8 where
  shiftLF (MkWord8X64 u0) (I# i) = MkWord8X64 (shiftLWord8X64# u0 i)
  shiftRF (MkWord8X64 u0) (I# i) = MkWord8X64 (shiftRWord8X64# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X64 Word8 where
  enumFromZero = MkWord8X64 (packWord8X64# (# 0#Word8, 1#Word8, 2#Word8, 3#Word8, 4#Word8, 5#Word8, 6#Word8, 7#Word8, 8#Word8, 9#Word8, 10#Word8, 11#Word8, 12#Word8, 13#Word8, 14#Word8, 15#Word8, 16#Word8, 17#Word8, 18#Word8, 19#Word8, 20#Word8, 21#Word8, 22#Word8, 23#Word8, 24#Word8, 25#Word8, 26#Word8, 27#Word8, 28#Word8, 29#Word8, 30#Word8, 31#Word8, 32#Word8, 33#Word8, 34#Word8, 35#Word8, 36#Word8, 37#Word8, 38#Word8, 39#Word8, 40#Word8, 41#Word8, 42#Word8, 43#Word8, 44#Word8, 45#Word8, 46#Word8, 47#Word8, 48#Word8, 49#Word8, 50#Word8, 51#Word8, 52#Word8, 53#Word8, 54#Word8, 55#Word8, 56#Word8, 57#Word8, 58#Word8, 59#Word8, 60#Word8, 61#Word8, 62#Word8, 63#Word8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X64 Word8 where
  indexByteArraySIMD# ba i = MkWord8X64 (indexWord8ArrayAsWord8X64# ba i)
  readByteArraySIMD# mba i s0 = case readWord8ArrayAsWord8X64# mba i s0 of (# s1, v0 #) -> (# s1, MkWord8X64 v0 #)
  writeByteArraySIMD# mba i (MkWord8X64 v0) s0 = writeWord8ArrayAsWord8X64# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X64 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord8OffAddrAsWord8X64# addr i s0 of (# s1, v0 #) -> (# s1, MkWord8X64 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X64 v0) = IO (\s0 -> case writeWord8OffAddrAsWord8X64# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X64 Word16 = MkWord16X64WithVec512 Word16X32# Word16X32#
instance PackX64 X64 Word16 where
  mkX64 (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) (W16# x8) (W16# x9) (W16# x10) (W16# x11) (W16# x12) (W16# x13) (W16# x14) (W16# x15) (W16# x16) (W16# x17) (W16# x18) (W16# x19) (W16# x20) (W16# x21) (W16# x22) (W16# x23) (W16# x24) (W16# x25) (W16# x26) (W16# x27) (W16# x28) (W16# x29) (W16# x30) (W16# x31) (W16# x32) (W16# x33) (W16# x34) (W16# x35) (W16# x36) (W16# x37) (W16# x38) (W16# x39) (W16# x40) (W16# x41) (W16# x42) (W16# x43) (W16# x44) (W16# x45) (W16# x46) (W16# x47) (W16# x48) (W16# x49) (W16# x50) (W16# x51) (W16# x52) (W16# x53) (W16# x54) (W16# x55) (W16# x56) (W16# x57) (W16# x58) (W16# x59) (W16# x60) (W16# x61) (W16# x62) (W16# x63) = MkWord16X64WithVec512 (packWord16X32# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #)) (packWord16X32# (# x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #))
  unpackX64 (MkWord16X64WithVec512 v0 v1) = case unpackWord16X32# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #) -> case unpackWord16X32# v1 of (# x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #) -> (W16# x0, W16# x1, W16# x2, W16# x3, W16# x4, W16# x5, W16# x6, W16# x7, W16# x8, W16# x9, W16# x10, W16# x11, W16# x12, W16# x13, W16# x14, W16# x15, W16# x16, W16# x17, W16# x18, W16# x19, W16# x20, W16# x21, W16# x22, W16# x23, W16# x24, W16# x25, W16# x26, W16# x27, W16# x28, W16# x29, W16# x30, W16# x31, W16# x32, W16# x33, W16# x34, W16# x35, W16# x36, W16# x37, W16# x38, W16# x39, W16# x40, W16# x41, W16# x42, W16# x43, W16# x44, W16# x45, W16# x46, W16# x47, W16# x48, W16# x49, W16# x50, W16# x51, W16# x52, W16# x53, W16# x54, W16# x55, W16# x56, W16# x57, W16# x58, W16# x59, W16# x60, W16# x61, W16# x62, W16# x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 Word16 where
  broadcast (W16# x) = let !v = broadcastWord16X32# x in MkWord16X64WithVec512 v v
  {-# INLINE broadcast #-}
instance SelectableF X64 Word16 where
  selectF (MkBoolX64 !cond) (MkWord16X64WithVec512 x0 x1) (MkWord16X64WithVec512 y0 y1) = MkWord16X64WithVec512 (selectWord16X32# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord16X32# (fromIntegral $ cond `unsafeShiftR` 32) x1 y1)
  {-# INLINE selectF #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, i32 < 64, i33 < 64, i34 < 64, i35 < 64, i36 < 64, i37 < 64, i38 < 64, i39 < 64, i40 < 64, i41 < 64, i42 < 64, i43 < 64, i44 < 64, i45 < 64, i46 < 64, i47 < 64, i48 < 64, i49 < 64, i50 < 64, i51 < 64, i52 < 64, i53 < 64, i54 < 64, i55 < 64, i56 < 64, i57 < 64, i58 < 64, i59 < 64, i60 < 64, i61 < 64, i62 < 64, i63 < 64, ShuffleMany Word16X32# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Word16X32# [i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Word16 where
  unaryShuffle (MkWord16X64WithVec512 x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord16X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 128, i1 < 128, i2 < 128, i3 < 128, i4 < 128, i5 < 128, i6 < 128, i7 < 128, i8 < 128, i9 < 128, i10 < 128, i11 < 128, i12 < 128, i13 < 128, i14 < 128, i15 < 128, i16 < 128, i17 < 128, i18 < 128, i19 < 128, i20 < 128, i21 < 128, i22 < 128, i23 < 128, i24 < 128, i25 < 128, i26 < 128, i27 < 128, i28 < 128, i29 < 128, i30 < 128, i31 < 128, i32 < 128, i33 < 128, i34 < 128, i35 < 128, i36 < 128, i37 < 128, i38 < 128, i39 < 128, i40 < 128, i41 < 128, i42 < 128, i43 < 128, i44 < 128, i45 < 128, i46 < 128, i47 < 128, i48 < 128, i49 < 128, i50 < 128, i51 < 128, i52 < 128, i53 < 128, i54 < 128, i55 < 128, i56 < 128, i57 < 128, i58 < 128, i59 < 128, i60 < 128, i61 < 128, i62 < 128, i63 < 128, ShuffleMany Word16X32# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Word16X32# [i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Word16 where
  binaryShuffle (MkWord16X64WithVec512 x0 x1) (MkWord16X64WithVec512 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord16X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X64 Word16 where
  eqF (MkWord16X64WithVec512 u0 u1) (MkWord16X64WithVec512 v0 v1) = MkBoolX64 $ (fromIntegral (eqWord16X32# u0 v0)) .|. (fromIntegral (eqWord16X32# u1 v1) `unsafeShiftL` 32)
  {-# INLINE eqF #-}
instance OrderedF X64 Word16 where
  ltF (MkWord16X64WithVec512 u0 u1) (MkWord16X64WithVec512 v0 v1) = MkBoolX64 $ (fromIntegral (ltWord16X32# u0 v0)) .|. (fromIntegral (ltWord16X32# u1 v1) `unsafeShiftL` 32)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X64 Word16 where
  minF (MkWord16X64WithVec512 u0 u1) (MkWord16X64WithVec512 v0 v1) = MkWord16X64WithVec512 (minWord16X32# u0 v0) (minWord16X32# u1 v1)
  maxF (MkWord16X64WithVec512 u0 u1) (MkWord16X64WithVec512 v0 v1) = MkWord16X64WithVec512 (maxWord16X32# u0 v0) (maxWord16X32# u1 v1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X64 Word16 where
  plusF (MkWord16X64WithVec512 u0 u1) (MkWord16X64WithVec512 v0 v1) = MkWord16X64WithVec512 (plusWord16X32# u0 v0) (plusWord16X32# u1 v1)
  minusF (MkWord16X64WithVec512 u0 u1) (MkWord16X64WithVec512 v0 v1) = MkWord16X64WithVec512 (minusWord16X32# u0 v0) (minusWord16X32# u1 v1)
  timesF (MkWord16X64WithVec512 u0 u1) (MkWord16X64WithVec512 v0 v1) = MkWord16X64WithVec512 (timesWord16X32# u0 v0) (timesWord16X32# u1 v1)
  -- Currently, there is no negateWord16X32#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X64 Word16 where
  andF (MkWord16X64WithVec512 u0 u1) (MkWord16X64WithVec512 v0 v1) = MkWord16X64WithVec512 (andWord16X32# u0 v0) (andWord16X32# u1 v1)
  orF (MkWord16X64WithVec512 u0 u1) (MkWord16X64WithVec512 v0 v1) = MkWord16X64WithVec512 (orWord16X32# u0 v0) (orWord16X32# u1 v1)
  xorF (MkWord16X64WithVec512 u0 u1) (MkWord16X64WithVec512 v0 v1) = MkWord16X64WithVec512 (xorWord16X32# u0 v0) (xorWord16X32# u1 v1)
  complementF (MkWord16X64WithVec512 u0 u1) = MkWord16X64WithVec512 (complementWord16X32# u0) (complementWord16X32# u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X64 Word16 where
  shiftLF (MkWord16X64WithVec512 u0 u1) (I# i) = MkWord16X64WithVec512 (shiftLWord16X32# u0 i) (shiftLWord16X32# u1 i)
  shiftRF (MkWord16X64WithVec512 u0 u1) (I# i) = MkWord16X64WithVec512 (shiftRWord16X32# u0 i) (shiftRWord16X32# u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X64 Word16 where
  enumFromZero = MkWord16X64WithVec512 (packWord16X32# (# 0#Word16, 1#Word16, 2#Word16, 3#Word16, 4#Word16, 5#Word16, 6#Word16, 7#Word16, 8#Word16, 9#Word16, 10#Word16, 11#Word16, 12#Word16, 13#Word16, 14#Word16, 15#Word16, 16#Word16, 17#Word16, 18#Word16, 19#Word16, 20#Word16, 21#Word16, 22#Word16, 23#Word16, 24#Word16, 25#Word16, 26#Word16, 27#Word16, 28#Word16, 29#Word16, 30#Word16, 31#Word16 #)) (packWord16X32# (# 32#Word16, 33#Word16, 34#Word16, 35#Word16, 36#Word16, 37#Word16, 38#Word16, 39#Word16, 40#Word16, 41#Word16, 42#Word16, 43#Word16, 44#Word16, 45#Word16, 46#Word16, 47#Word16, 48#Word16, 49#Word16, 50#Word16, 51#Word16, 52#Word16, 53#Word16, 54#Word16, 55#Word16, 56#Word16, 57#Word16, 58#Word16, 59#Word16, 60#Word16, 61#Word16, 62#Word16, 63#Word16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X64 Word16 where
  indexByteArraySIMD# ba i = MkWord16X64WithVec512 (indexWord16ArrayAsWord16X32# ba i) (indexWord16ArrayAsWord16X32# ba (i +# 32#))
  readByteArraySIMD# mba i s0 = case readWord16ArrayAsWord16X32# mba i s0 of (# s1, v0 #) -> case readWord16ArrayAsWord16X32# mba (i +# 32#) s1 of (# s2, v1 #) -> (# s2, MkWord16X64WithVec512 v0 v1 #)
  writeByteArraySIMD# mba i (MkWord16X64WithVec512 v0 v1) s0 = case writeWord16ArrayAsWord16X32# mba i v0 s0 of s1 -> writeWord16ArrayAsWord16X32# mba (i +# 32#) v1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X64 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord16OffAddrAsWord16X32# addr i s0 of (# s1, v0 #) -> case readWord16OffAddrAsWord16X32# addr (i +# 32#) s1 of (# s2, v1 #) -> (# s2, MkWord16X64WithVec512 v0 v1 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X64WithVec512 v0 v1) = IO (\s0 -> case writeWord16OffAddrAsWord16X32# addr i v0 s0 of s1 -> case writeWord16OffAddrAsWord16X32# addr (i +# 32#) v1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X64 Word32 = MkWord32X64WithVec512 Word32X16# Word32X16# Word32X16# Word32X16#
instance PackX64 X64 Word32 where
  mkX64 (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) (W32# x8) (W32# x9) (W32# x10) (W32# x11) (W32# x12) (W32# x13) (W32# x14) (W32# x15) (W32# x16) (W32# x17) (W32# x18) (W32# x19) (W32# x20) (W32# x21) (W32# x22) (W32# x23) (W32# x24) (W32# x25) (W32# x26) (W32# x27) (W32# x28) (W32# x29) (W32# x30) (W32# x31) (W32# x32) (W32# x33) (W32# x34) (W32# x35) (W32# x36) (W32# x37) (W32# x38) (W32# x39) (W32# x40) (W32# x41) (W32# x42) (W32# x43) (W32# x44) (W32# x45) (W32# x46) (W32# x47) (W32# x48) (W32# x49) (W32# x50) (W32# x51) (W32# x52) (W32# x53) (W32# x54) (W32# x55) (W32# x56) (W32# x57) (W32# x58) (W32# x59) (W32# x60) (W32# x61) (W32# x62) (W32# x63) = MkWord32X64WithVec512 (packWord32X16# (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #)) (packWord32X16# (# x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #)) (packWord32X16# (# x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47 #)) (packWord32X16# (# x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #))
  unpackX64 (MkWord32X64WithVec512 v0 v1 v2 v3) = case unpackWord32X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackWord32X16# v1 of (# x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31 #) -> case unpackWord32X16# v2 of (# x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47 #) -> case unpackWord32X16# v3 of (# x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63 #) -> (W32# x0, W32# x1, W32# x2, W32# x3, W32# x4, W32# x5, W32# x6, W32# x7, W32# x8, W32# x9, W32# x10, W32# x11, W32# x12, W32# x13, W32# x14, W32# x15, W32# x16, W32# x17, W32# x18, W32# x19, W32# x20, W32# x21, W32# x22, W32# x23, W32# x24, W32# x25, W32# x26, W32# x27, W32# x28, W32# x29, W32# x30, W32# x31, W32# x32, W32# x33, W32# x34, W32# x35, W32# x36, W32# x37, W32# x38, W32# x39, W32# x40, W32# x41, W32# x42, W32# x43, W32# x44, W32# x45, W32# x46, W32# x47, W32# x48, W32# x49, W32# x50, W32# x51, W32# x52, W32# x53, W32# x54, W32# x55, W32# x56, W32# x57, W32# x58, W32# x59, W32# x60, W32# x61, W32# x62, W32# x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 Word32 where
  broadcast (W32# x) = let !v = broadcastWord32X16# x in MkWord32X64WithVec512 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X64 Word32 where
  selectF (MkBoolX64 !cond) (MkWord32X64WithVec512 x0 x1 x2 x3) (MkWord32X64WithVec512 y0 y1 y2 y3) = MkWord32X64WithVec512 (selectWord32X16# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord32X16# (fromIntegral $ cond `unsafeShiftR` 16) x1 y1) (selectWord32X16# (fromIntegral $ cond `unsafeShiftR` 32) x2 y2) (selectWord32X16# (fromIntegral $ cond `unsafeShiftR` 48) x3 y3)
  {-# INLINE selectF #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, i32 < 64, i33 < 64, i34 < 64, i35 < 64, i36 < 64, i37 < 64, i38 < 64, i39 < 64, i40 < 64, i41 < 64, i42 < 64, i43 < 64, i44 < 64, i45 < 64, i46 < 64, i47 < 64, i48 < 64, i49 < 64, i50 < 64, i51 < 64, i52 < 64, i53 < 64, i54 < 64, i55 < 64, i56 < 64, i57 < 64, i58 < 64, i59 < 64, i60 < 64, i61 < 64, i62 < 64, i63 < 64, ShuffleMany Word32X16# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Word32X16# [i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Word32X16# [i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany Word32X16# [i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Word32 where
  unaryShuffle (MkWord32X64WithVec512 x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord32X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 128, i1 < 128, i2 < 128, i3 < 128, i4 < 128, i5 < 128, i6 < 128, i7 < 128, i8 < 128, i9 < 128, i10 < 128, i11 < 128, i12 < 128, i13 < 128, i14 < 128, i15 < 128, i16 < 128, i17 < 128, i18 < 128, i19 < 128, i20 < 128, i21 < 128, i22 < 128, i23 < 128, i24 < 128, i25 < 128, i26 < 128, i27 < 128, i28 < 128, i29 < 128, i30 < 128, i31 < 128, i32 < 128, i33 < 128, i34 < 128, i35 < 128, i36 < 128, i37 < 128, i38 < 128, i39 < 128, i40 < 128, i41 < 128, i42 < 128, i43 < 128, i44 < 128, i45 < 128, i46 < 128, i47 < 128, i48 < 128, i49 < 128, i50 < 128, i51 < 128, i52 < 128, i53 < 128, i54 < 128, i55 < 128, i56 < 128, i57 < 128, i58 < 128, i59 < 128, i60 < 128, i61 < 128, i62 < 128, i63 < 128, ShuffleMany Word32X16# [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Word32X16# [i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Word32X16# [i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany Word32X16# [i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Word32 where
  binaryShuffle (MkWord32X64WithVec512 x0 x1 x2 x3) (MkWord32X64WithVec512 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord32X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X64 Word32 where
  eqF (MkWord32X64WithVec512 u0 u1 u2 u3) (MkWord32X64WithVec512 v0 v1 v2 v3) = MkBoolX64 $ (fromIntegral (eqWord32X16# u0 v0)) .|. (fromIntegral (eqWord32X16# u1 v1) `unsafeShiftL` 16) .|. (fromIntegral (eqWord32X16# u2 v2) `unsafeShiftL` 32) .|. (fromIntegral (eqWord32X16# u3 v3) `unsafeShiftL` 48)
  {-# INLINE eqF #-}
instance OrderedF X64 Word32 where
  ltF (MkWord32X64WithVec512 u0 u1 u2 u3) (MkWord32X64WithVec512 v0 v1 v2 v3) = MkBoolX64 $ (fromIntegral (ltWord32X16# u0 v0)) .|. (fromIntegral (ltWord32X16# u1 v1) `unsafeShiftL` 16) .|. (fromIntegral (ltWord32X16# u2 v2) `unsafeShiftL` 32) .|. (fromIntegral (ltWord32X16# u3 v3) `unsafeShiftL` 48)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X64 Word32 where
  minF (MkWord32X64WithVec512 u0 u1 u2 u3) (MkWord32X64WithVec512 v0 v1 v2 v3) = MkWord32X64WithVec512 (minWord32X16# u0 v0) (minWord32X16# u1 v1) (minWord32X16# u2 v2) (minWord32X16# u3 v3)
  maxF (MkWord32X64WithVec512 u0 u1 u2 u3) (MkWord32X64WithVec512 v0 v1 v2 v3) = MkWord32X64WithVec512 (maxWord32X16# u0 v0) (maxWord32X16# u1 v1) (maxWord32X16# u2 v2) (maxWord32X16# u3 v3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X64 Word32 where
  plusF (MkWord32X64WithVec512 u0 u1 u2 u3) (MkWord32X64WithVec512 v0 v1 v2 v3) = MkWord32X64WithVec512 (plusWord32X16# u0 v0) (plusWord32X16# u1 v1) (plusWord32X16# u2 v2) (plusWord32X16# u3 v3)
  minusF (MkWord32X64WithVec512 u0 u1 u2 u3) (MkWord32X64WithVec512 v0 v1 v2 v3) = MkWord32X64WithVec512 (minusWord32X16# u0 v0) (minusWord32X16# u1 v1) (minusWord32X16# u2 v2) (minusWord32X16# u3 v3)
  timesF (MkWord32X64WithVec512 u0 u1 u2 u3) (MkWord32X64WithVec512 v0 v1 v2 v3) = MkWord32X64WithVec512 (timesWord32X16# u0 v0) (timesWord32X16# u1 v1) (timesWord32X16# u2 v2) (timesWord32X16# u3 v3)
  -- Currently, there is no negateWord32X16#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X64 Word32 where
  andF (MkWord32X64WithVec512 u0 u1 u2 u3) (MkWord32X64WithVec512 v0 v1 v2 v3) = MkWord32X64WithVec512 (andWord32X16# u0 v0) (andWord32X16# u1 v1) (andWord32X16# u2 v2) (andWord32X16# u3 v3)
  orF (MkWord32X64WithVec512 u0 u1 u2 u3) (MkWord32X64WithVec512 v0 v1 v2 v3) = MkWord32X64WithVec512 (orWord32X16# u0 v0) (orWord32X16# u1 v1) (orWord32X16# u2 v2) (orWord32X16# u3 v3)
  xorF (MkWord32X64WithVec512 u0 u1 u2 u3) (MkWord32X64WithVec512 v0 v1 v2 v3) = MkWord32X64WithVec512 (xorWord32X16# u0 v0) (xorWord32X16# u1 v1) (xorWord32X16# u2 v2) (xorWord32X16# u3 v3)
  complementF (MkWord32X64WithVec512 u0 u1 u2 u3) = MkWord32X64WithVec512 (complementWord32X16# u0) (complementWord32X16# u1) (complementWord32X16# u2) (complementWord32X16# u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X64 Word32 where
  shiftLF (MkWord32X64WithVec512 u0 u1 u2 u3) (I# i) = MkWord32X64WithVec512 (shiftLWord32X16# u0 i) (shiftLWord32X16# u1 i) (shiftLWord32X16# u2 i) (shiftLWord32X16# u3 i)
  shiftRF (MkWord32X64WithVec512 u0 u1 u2 u3) (I# i) = MkWord32X64WithVec512 (shiftRWord32X16# u0 i) (shiftRWord32X16# u1 i) (shiftRWord32X16# u2 i) (shiftRWord32X16# u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X64 Word32 where
  enumFromZero = MkWord32X64WithVec512 (packWord32X16# (# 0#Word32, 1#Word32, 2#Word32, 3#Word32, 4#Word32, 5#Word32, 6#Word32, 7#Word32, 8#Word32, 9#Word32, 10#Word32, 11#Word32, 12#Word32, 13#Word32, 14#Word32, 15#Word32 #)) (packWord32X16# (# 16#Word32, 17#Word32, 18#Word32, 19#Word32, 20#Word32, 21#Word32, 22#Word32, 23#Word32, 24#Word32, 25#Word32, 26#Word32, 27#Word32, 28#Word32, 29#Word32, 30#Word32, 31#Word32 #)) (packWord32X16# (# 32#Word32, 33#Word32, 34#Word32, 35#Word32, 36#Word32, 37#Word32, 38#Word32, 39#Word32, 40#Word32, 41#Word32, 42#Word32, 43#Word32, 44#Word32, 45#Word32, 46#Word32, 47#Word32 #)) (packWord32X16# (# 48#Word32, 49#Word32, 50#Word32, 51#Word32, 52#Word32, 53#Word32, 54#Word32, 55#Word32, 56#Word32, 57#Word32, 58#Word32, 59#Word32, 60#Word32, 61#Word32, 62#Word32, 63#Word32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X64 Word32 where
  indexByteArraySIMD# ba i = MkWord32X64WithVec512 (indexWord32ArrayAsWord32X16# ba i) (indexWord32ArrayAsWord32X16# ba (i +# 16#)) (indexWord32ArrayAsWord32X16# ba (i +# 32#)) (indexWord32ArrayAsWord32X16# ba (i +# 48#))
  readByteArraySIMD# mba i s0 = case readWord32ArrayAsWord32X16# mba i s0 of (# s1, v0 #) -> case readWord32ArrayAsWord32X16# mba (i +# 16#) s1 of (# s2, v1 #) -> case readWord32ArrayAsWord32X16# mba (i +# 32#) s2 of (# s3, v2 #) -> case readWord32ArrayAsWord32X16# mba (i +# 48#) s3 of (# s4, v3 #) -> (# s4, MkWord32X64WithVec512 v0 v1 v2 v3 #)
  writeByteArraySIMD# mba i (MkWord32X64WithVec512 v0 v1 v2 v3) s0 = case writeWord32ArrayAsWord32X16# mba i v0 s0 of s1 -> case writeWord32ArrayAsWord32X16# mba (i +# 16#) v1 s1 of s2 -> case writeWord32ArrayAsWord32X16# mba (i +# 32#) v2 s2 of s3 -> writeWord32ArrayAsWord32X16# mba (i +# 48#) v3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X64 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord32OffAddrAsWord32X16# addr i s0 of (# s1, v0 #) -> case readWord32OffAddrAsWord32X16# addr (i +# 16#) s1 of (# s2, v1 #) -> case readWord32OffAddrAsWord32X16# addr (i +# 32#) s2 of (# s3, v2 #) -> case readWord32OffAddrAsWord32X16# addr (i +# 48#) s3 of (# s4, v3 #) -> (# s4, MkWord32X64WithVec512 v0 v1 v2 v3 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X64WithVec512 v0 v1 v2 v3) = IO (\s0 -> case writeWord32OffAddrAsWord32X16# addr i v0 s0 of s1 -> case writeWord32OffAddrAsWord32X16# addr (i +# 16#) v1 s1 of s2 -> case writeWord32OffAddrAsWord32X16# addr (i +# 32#) v2 s2 of s3 -> case writeWord32OffAddrAsWord32X16# addr (i +# 48#) v3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X64 Word64 = MkWord64X64WithVec512 Word64X8# Word64X8# Word64X8# Word64X8# Word64X8# Word64X8# Word64X8# Word64X8#
instance PackX64 X64 Word64 where
  mkX64 (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) (W64# x8) (W64# x9) (W64# x10) (W64# x11) (W64# x12) (W64# x13) (W64# x14) (W64# x15) (W64# x16) (W64# x17) (W64# x18) (W64# x19) (W64# x20) (W64# x21) (W64# x22) (W64# x23) (W64# x24) (W64# x25) (W64# x26) (W64# x27) (W64# x28) (W64# x29) (W64# x30) (W64# x31) (W64# x32) (W64# x33) (W64# x34) (W64# x35) (W64# x36) (W64# x37) (W64# x38) (W64# x39) (W64# x40) (W64# x41) (W64# x42) (W64# x43) (W64# x44) (W64# x45) (W64# x46) (W64# x47) (W64# x48) (W64# x49) (W64# x50) (W64# x51) (W64# x52) (W64# x53) (W64# x54) (W64# x55) (W64# x56) (W64# x57) (W64# x58) (W64# x59) (W64# x60) (W64# x61) (W64# x62) (W64# x63) = MkWord64X64WithVec512 (packWord64X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #)) (packWord64X8# (# x8, x9, x10, x11, x12, x13, x14, x15 #)) (packWord64X8# (# x16, x17, x18, x19, x20, x21, x22, x23 #)) (packWord64X8# (# x24, x25, x26, x27, x28, x29, x30, x31 #)) (packWord64X8# (# x32, x33, x34, x35, x36, x37, x38, x39 #)) (packWord64X8# (# x40, x41, x42, x43, x44, x45, x46, x47 #)) (packWord64X8# (# x48, x49, x50, x51, x52, x53, x54, x55 #)) (packWord64X8# (# x56, x57, x58, x59, x60, x61, x62, x63 #))
  unpackX64 (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = case unpackWord64X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> case unpackWord64X8# v1 of (# x8, x9, x10, x11, x12, x13, x14, x15 #) -> case unpackWord64X8# v2 of (# x16, x17, x18, x19, x20, x21, x22, x23 #) -> case unpackWord64X8# v3 of (# x24, x25, x26, x27, x28, x29, x30, x31 #) -> case unpackWord64X8# v4 of (# x32, x33, x34, x35, x36, x37, x38, x39 #) -> case unpackWord64X8# v5 of (# x40, x41, x42, x43, x44, x45, x46, x47 #) -> case unpackWord64X8# v6 of (# x48, x49, x50, x51, x52, x53, x54, x55 #) -> case unpackWord64X8# v7 of (# x56, x57, x58, x59, x60, x61, x62, x63 #) -> (W64# x0, W64# x1, W64# x2, W64# x3, W64# x4, W64# x5, W64# x6, W64# x7, W64# x8, W64# x9, W64# x10, W64# x11, W64# x12, W64# x13, W64# x14, W64# x15, W64# x16, W64# x17, W64# x18, W64# x19, W64# x20, W64# x21, W64# x22, W64# x23, W64# x24, W64# x25, W64# x26, W64# x27, W64# x28, W64# x29, W64# x30, W64# x31, W64# x32, W64# x33, W64# x34, W64# x35, W64# x36, W64# x37, W64# x38, W64# x39, W64# x40, W64# x41, W64# x42, W64# x43, W64# x44, W64# x45, W64# x46, W64# x47, W64# x48, W64# x49, W64# x50, W64# x51, W64# x52, W64# x53, W64# x54, W64# x55, W64# x56, W64# x57, W64# x58, W64# x59, W64# x60, W64# x61, W64# x62, W64# x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 Word64 where
  broadcast (W64# x) = let !v = broadcastWord64X8# x in MkWord64X64WithVec512 v v v v v v v v
  {-# INLINE broadcast #-}
instance SelectableF X64 Word64 where
  selectF (MkBoolX64 !cond) (MkWord64X64WithVec512 x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X64WithVec512 y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X64WithVec512 (selectWord64X8# (fromIntegral $ cond `unsafeShiftR` 0) x0 y0) (selectWord64X8# (fromIntegral $ cond `unsafeShiftR` 8) x1 y1) (selectWord64X8# (fromIntegral $ cond `unsafeShiftR` 16) x2 y2) (selectWord64X8# (fromIntegral $ cond `unsafeShiftR` 24) x3 y3) (selectWord64X8# (fromIntegral $ cond `unsafeShiftR` 32) x4 y4) (selectWord64X8# (fromIntegral $ cond `unsafeShiftR` 40) x5 y5) (selectWord64X8# (fromIntegral $ cond `unsafeShiftR` 48) x6 y6) (selectWord64X8# (fromIntegral $ cond `unsafeShiftR` 56) x7 y7)
  {-# INLINE selectF #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, i32 < 64, i33 < 64, i34 < 64, i35 < 64, i36 < 64, i37 < 64, i38 < 64, i39 < 64, i40 < 64, i41 < 64, i42 < 64, i43 < 64, i44 < 64, i45 < 64, i46 < 64, i47 < 64, i48 < 64, i49 < 64, i50 < 64, i51 < 64, i52 < 64, i53 < 64, i54 < 64, i55 < 64, i56 < 64, i57 < 64, i58 < 64, i59 < 64, i60 < 64, i61 < 64, i62 < 64, i63 < 64, ShuffleMany Word64X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Word64X8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Word64X8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany Word64X8# [i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Word64X8# [i32, i33, i34, i35, i36, i37, i38, i39], ShuffleMany Word64X8# [i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany Word64X8# [i48, i49, i50, i51, i52, i53, i54, i55], ShuffleMany Word64X8# [i56, i57, i58, i59, i60, i61, i62, i63]) => UnaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Word64 where
  unaryShuffle (MkWord64X64WithVec512 x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord64X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39] sources) (shuffleMany# @_ @_ @[i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55] sources) (shuffleMany# @_ @_ @[i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 128, i1 < 128, i2 < 128, i3 < 128, i4 < 128, i5 < 128, i6 < 128, i7 < 128, i8 < 128, i9 < 128, i10 < 128, i11 < 128, i12 < 128, i13 < 128, i14 < 128, i15 < 128, i16 < 128, i17 < 128, i18 < 128, i19 < 128, i20 < 128, i21 < 128, i22 < 128, i23 < 128, i24 < 128, i25 < 128, i26 < 128, i27 < 128, i28 < 128, i29 < 128, i30 < 128, i31 < 128, i32 < 128, i33 < 128, i34 < 128, i35 < 128, i36 < 128, i37 < 128, i38 < 128, i39 < 128, i40 < 128, i41 < 128, i42 < 128, i43 < 128, i44 < 128, i45 < 128, i46 < 128, i47 < 128, i48 < 128, i49 < 128, i50 < 128, i51 < 128, i52 < 128, i53 < 128, i54 < 128, i55 < 128, i56 < 128, i57 < 128, i58 < 128, i59 < 128, i60 < 128, i61 < 128, i62 < 128, i63 < 128, ShuffleMany Word64X8# [i0, i1, i2, i3, i4, i5, i6, i7], ShuffleMany Word64X8# [i8, i9, i10, i11, i12, i13, i14, i15], ShuffleMany Word64X8# [i16, i17, i18, i19, i20, i21, i22, i23], ShuffleMany Word64X8# [i24, i25, i26, i27, i28, i29, i30, i31], ShuffleMany Word64X8# [i32, i33, i34, i35, i36, i37, i38, i39], ShuffleMany Word64X8# [i40, i41, i42, i43, i44, i45, i46, i47], ShuffleMany Word64X8# [i48, i49, i50, i51, i52, i53, i54, i55], ShuffleMany Word64X8# [i56, i57, i58, i59, i60, i61, i62, i63]) => BinaryShuffleT [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 Word64 where
  binaryShuffle (MkWord64X64WithVec512 x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X64WithVec512 x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord64X64WithVec512 (shuffleMany# @_ @_ @[i0, i1, i2, i3, i4, i5, i6, i7] sources) (shuffleMany# @_ @_ @[i8, i9, i10, i11, i12, i13, i14, i15] sources) (shuffleMany# @_ @_ @[i16, i17, i18, i19, i20, i21, i22, i23] sources) (shuffleMany# @_ @_ @[i24, i25, i26, i27, i28, i29, i30, i31] sources) (shuffleMany# @_ @_ @[i32, i33, i34, i35, i36, i37, i38, i39] sources) (shuffleMany# @_ @_ @[i40, i41, i42, i43, i44, i45, i46, i47] sources) (shuffleMany# @_ @_ @[i48, i49, i50, i51, i52, i53, i54, i55] sources) (shuffleMany# @_ @_ @[i56, i57, i58, i59, i60, i61, i62, i63] sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X64 Word64 where
  eqF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX64 $ (fromIntegral (eqWord64X8# u0 v0)) .|. (fromIntegral (eqWord64X8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (eqWord64X8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (eqWord64X8# u3 v3) `unsafeShiftL` 24) .|. (fromIntegral (eqWord64X8# u4 v4) `unsafeShiftL` 32) .|. (fromIntegral (eqWord64X8# u5 v5) `unsafeShiftL` 40) .|. (fromIntegral (eqWord64X8# u6 v6) `unsafeShiftL` 48) .|. (fromIntegral (eqWord64X8# u7 v7) `unsafeShiftL` 56)
  {-# INLINE eqF #-}
instance OrderedF X64 Word64 where
  ltF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkBoolX64 $ (fromIntegral (ltWord64X8# u0 v0)) .|. (fromIntegral (ltWord64X8# u1 v1) `unsafeShiftL` 8) .|. (fromIntegral (ltWord64X8# u2 v2) `unsafeShiftL` 16) .|. (fromIntegral (ltWord64X8# u3 v3) `unsafeShiftL` 24) .|. (fromIntegral (ltWord64X8# u4 v4) `unsafeShiftL` 32) .|. (fromIntegral (ltWord64X8# u5 v5) `unsafeShiftL` 40) .|. (fromIntegral (ltWord64X8# u6 v6) `unsafeShiftL` 48) .|. (fromIntegral (ltWord64X8# u7 v7) `unsafeShiftL` 56)
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X64 Word64 where
  minF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X64WithVec512 (minWord64X8# u0 v0) (minWord64X8# u1 v1) (minWord64X8# u2 v2) (minWord64X8# u3 v3) (minWord64X8# u4 v4) (minWord64X8# u5 v5) (minWord64X8# u6 v6) (minWord64X8# u7 v7)
  maxF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X64WithVec512 (maxWord64X8# u0 v0) (maxWord64X8# u1 v1) (maxWord64X8# u2 v2) (maxWord64X8# u3 v3) (maxWord64X8# u4 v4) (maxWord64X8# u5 v5) (maxWord64X8# u6 v6) (maxWord64X8# u7 v7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X64 Word64 where
  plusF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X64WithVec512 (plusWord64X8# u0 v0) (plusWord64X8# u1 v1) (plusWord64X8# u2 v2) (plusWord64X8# u3 v3) (plusWord64X8# u4 v4) (plusWord64X8# u5 v5) (plusWord64X8# u6 v6) (plusWord64X8# u7 v7)
  minusF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X64WithVec512 (minusWord64X8# u0 v0) (minusWord64X8# u1 v1) (minusWord64X8# u2 v2) (minusWord64X8# u3 v3) (minusWord64X8# u4 v4) (minusWord64X8# u5 v5) (minusWord64X8# u6 v6) (minusWord64X8# u7 v7)
  timesF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X64WithVec512 (timesWord64X8# u0 v0) (timesWord64X8# u1 v1) (timesWord64X8# u2 v2) (timesWord64X8# u3 v3) (timesWord64X8# u4 v4) (timesWord64X8# u5 v5) (timesWord64X8# u6 v6) (timesWord64X8# u7 v7)
  -- Currently, there is no negateWord64X8#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X64 Word64 where
  andF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X64WithVec512 (andWord64X8# u0 v0) (andWord64X8# u1 v1) (andWord64X8# u2 v2) (andWord64X8# u3 v3) (andWord64X8# u4 v4) (andWord64X8# u5 v5) (andWord64X8# u6 v6) (andWord64X8# u7 v7)
  orF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X64WithVec512 (orWord64X8# u0 v0) (orWord64X8# u1 v1) (orWord64X8# u2 v2) (orWord64X8# u3 v3) (orWord64X8# u4 v4) (orWord64X8# u5 v5) (orWord64X8# u6 v6) (orWord64X8# u7 v7)
  xorF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = MkWord64X64WithVec512 (xorWord64X8# u0 v0) (xorWord64X8# u1 v1) (xorWord64X8# u2 v2) (xorWord64X8# u3 v3) (xorWord64X8# u4 v4) (xorWord64X8# u5 v5) (xorWord64X8# u6 v6) (xorWord64X8# u7 v7)
  complementF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) = MkWord64X64WithVec512 (complementWord64X8# u0) (complementWord64X8# u1) (complementWord64X8# u2) (complementWord64X8# u3) (complementWord64X8# u4) (complementWord64X8# u5) (complementWord64X8# u6) (complementWord64X8# u7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X64 Word64 where
  shiftLF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkWord64X64WithVec512 (shiftLWord64X8# u0 i) (shiftLWord64X8# u1 i) (shiftLWord64X8# u2 i) (shiftLWord64X8# u3 i) (shiftLWord64X8# u4 i) (shiftLWord64X8# u5 i) (shiftLWord64X8# u6 i) (shiftLWord64X8# u7 i)
  shiftRF (MkWord64X64WithVec512 u0 u1 u2 u3 u4 u5 u6 u7) (I# i) = MkWord64X64WithVec512 (shiftRWord64X8# u0 i) (shiftRWord64X8# u1 i) (shiftRWord64X8# u2 i) (shiftRWord64X8# u3 i) (shiftRWord64X8# u4 i) (shiftRWord64X8# u5 i) (shiftRWord64X8# u6 i) (shiftRWord64X8# u7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X64 Word64 where
  enumFromZero = MkWord64X64WithVec512 (packWord64X8# (# 0#Word64, 1#Word64, 2#Word64, 3#Word64, 4#Word64, 5#Word64, 6#Word64, 7#Word64 #)) (packWord64X8# (# 8#Word64, 9#Word64, 10#Word64, 11#Word64, 12#Word64, 13#Word64, 14#Word64, 15#Word64 #)) (packWord64X8# (# 16#Word64, 17#Word64, 18#Word64, 19#Word64, 20#Word64, 21#Word64, 22#Word64, 23#Word64 #)) (packWord64X8# (# 24#Word64, 25#Word64, 26#Word64, 27#Word64, 28#Word64, 29#Word64, 30#Word64, 31#Word64 #)) (packWord64X8# (# 32#Word64, 33#Word64, 34#Word64, 35#Word64, 36#Word64, 37#Word64, 38#Word64, 39#Word64 #)) (packWord64X8# (# 40#Word64, 41#Word64, 42#Word64, 43#Word64, 44#Word64, 45#Word64, 46#Word64, 47#Word64 #)) (packWord64X8# (# 48#Word64, 49#Word64, 50#Word64, 51#Word64, 52#Word64, 53#Word64, 54#Word64, 55#Word64 #)) (packWord64X8# (# 56#Word64, 57#Word64, 58#Word64, 59#Word64, 60#Word64, 61#Word64, 62#Word64, 63#Word64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X64 Word64 where
  indexByteArraySIMD# ba i = MkWord64X64WithVec512 (indexWord64ArrayAsWord64X8# ba i) (indexWord64ArrayAsWord64X8# ba (i +# 8#)) (indexWord64ArrayAsWord64X8# ba (i +# 16#)) (indexWord64ArrayAsWord64X8# ba (i +# 24#)) (indexWord64ArrayAsWord64X8# ba (i +# 32#)) (indexWord64ArrayAsWord64X8# ba (i +# 40#)) (indexWord64ArrayAsWord64X8# ba (i +# 48#)) (indexWord64ArrayAsWord64X8# ba (i +# 56#))
  readByteArraySIMD# mba i s0 = case readWord64ArrayAsWord64X8# mba i s0 of (# s1, v0 #) -> case readWord64ArrayAsWord64X8# mba (i +# 8#) s1 of (# s2, v1 #) -> case readWord64ArrayAsWord64X8# mba (i +# 16#) s2 of (# s3, v2 #) -> case readWord64ArrayAsWord64X8# mba (i +# 24#) s3 of (# s4, v3 #) -> case readWord64ArrayAsWord64X8# mba (i +# 32#) s4 of (# s5, v4 #) -> case readWord64ArrayAsWord64X8# mba (i +# 40#) s5 of (# s6, v5 #) -> case readWord64ArrayAsWord64X8# mba (i +# 48#) s6 of (# s7, v6 #) -> case readWord64ArrayAsWord64X8# mba (i +# 56#) s7 of (# s8, v7 #) -> (# s8, MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7 #)
  writeByteArraySIMD# mba i (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) s0 = case writeWord64ArrayAsWord64X8# mba i v0 s0 of s1 -> case writeWord64ArrayAsWord64X8# mba (i +# 8#) v1 s1 of s2 -> case writeWord64ArrayAsWord64X8# mba (i +# 16#) v2 s2 of s3 -> case writeWord64ArrayAsWord64X8# mba (i +# 24#) v3 s3 of s4 -> case writeWord64ArrayAsWord64X8# mba (i +# 32#) v4 s4 of s5 -> case writeWord64ArrayAsWord64X8# mba (i +# 40#) v5 s5 of s6 -> case writeWord64ArrayAsWord64X8# mba (i +# 48#) v6 s6 of s7 -> writeWord64ArrayAsWord64X8# mba (i +# 56#) v7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X64 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord64OffAddrAsWord64X8# addr i s0 of (# s1, v0 #) -> case readWord64OffAddrAsWord64X8# addr (i +# 8#) s1 of (# s2, v1 #) -> case readWord64OffAddrAsWord64X8# addr (i +# 16#) s2 of (# s3, v2 #) -> case readWord64OffAddrAsWord64X8# addr (i +# 24#) s3 of (# s4, v3 #) -> case readWord64OffAddrAsWord64X8# addr (i +# 32#) s4 of (# s5, v4 #) -> case readWord64OffAddrAsWord64X8# addr (i +# 40#) s5 of (# s6, v5 #) -> case readWord64OffAddrAsWord64X8# addr (i +# 48#) s6 of (# s7, v6 #) -> case readWord64OffAddrAsWord64X8# addr (i +# 56#) s7 of (# s8, v7 #) -> (# s8, MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = IO (\s0 -> case writeWord64OffAddrAsWord64X8# addr i v0 s0 of s1 -> case writeWord64OffAddrAsWord64X8# addr (i +# 8#) v1 s1 of s2 -> case writeWord64OffAddrAsWord64X8# addr (i +# 16#) v2 s2 of s3 -> case writeWord64OffAddrAsWord64X8# addr (i +# 24#) v3 s3 of s4 -> case writeWord64OffAddrAsWord64X8# addr (i +# 32#) v4 s4 of s5 -> case writeWord64OffAddrAsWord64X8# addr (i +# 40#) v5 s5 of s6 -> case writeWord64OffAddrAsWord64X8# addr (i +# 48#) v6 s6 of s7 -> case writeWord64OffAddrAsWord64X8# addr (i +# 56#) v7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
newtype instance X64 (Sum a) = MkSumX64 (X64 a)
instance PackX64 X64 a => PackX64 X64 (Sum a) where
  mkX64 = coerce (mkX64 @X64 @a)
  unpackX64 = coerce (unpackX64 @X64 @a)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 a => Broadcast X64 (Sum a) where
  broadcast = coerce (broadcast @X64 @a)
  {-# INLINE broadcast #-}
instance SelectableF X64 a => SelectableF X64 (Sum a) where
  selectF = coerce (selectF @X64 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X64 a => UnaryShuffleT indices X64 (Sum a) where
  unaryShuffle = coerce (unaryShuffle @indices @X64 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X64 a => BinaryShuffleT indices X64 (Sum a) where
  binaryShuffle = coerce (binaryShuffle @indices @X64 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X64 (Product a) = MkProductX64 (X64 a)
instance PackX64 X64 a => PackX64 X64 (Product a) where
  mkX64 = coerce (mkX64 @X64 @a)
  unpackX64 = coerce (unpackX64 @X64 @a)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 a => Broadcast X64 (Product a) where
  broadcast = coerce (broadcast @X64 @a)
  {-# INLINE broadcast #-}
instance SelectableF X64 a => SelectableF X64 (Product a) where
  selectF = coerce (selectF @X64 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X64 a => UnaryShuffleT indices X64 (Product a) where
  unaryShuffle = coerce (unaryShuffle @indices @X64 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X64 a => BinaryShuffleT indices X64 (Product a) where
  binaryShuffle = coerce (binaryShuffle @indices @X64 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X64 (Min a) = MkMinX64 (X64 a)
instance PackX64 X64 a => PackX64 X64 (Min a) where
  mkX64 = coerce (mkX64 @X64 @a)
  unpackX64 = coerce (unpackX64 @X64 @a)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 a => Broadcast X64 (Min a) where
  broadcast = coerce (broadcast @X64 @a)
  {-# INLINE broadcast #-}
instance SelectableF X64 a => SelectableF X64 (Min a) where
  selectF = coerce (selectF @X64 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X64 a => UnaryShuffleT indices X64 (Min a) where
  unaryShuffle = coerce (unaryShuffle @indices @X64 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X64 a => BinaryShuffleT indices X64 (Min a) where
  binaryShuffle = coerce (binaryShuffle @indices @X64 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X64 (Max a) = MkMaxX64 (X64 a)
instance PackX64 X64 a => PackX64 X64 (Max a) where
  mkX64 = coerce (mkX64 @X64 @a)
  unpackX64 = coerce (unpackX64 @X64 @a)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 a => Broadcast X64 (Max a) where
  broadcast = coerce (broadcast @X64 @a)
  {-# INLINE broadcast #-}
instance SelectableF X64 a => SelectableF X64 (Max a) where
  selectF = coerce (selectF @X64 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X64 a => UnaryShuffleT indices X64 (Max a) where
  unaryShuffle = coerce (unaryShuffle @indices @X64 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X64 a => BinaryShuffleT indices X64 (Max a) where
  binaryShuffle = coerce (binaryShuffle @indices @X64 @a)
  {-# INLINE binaryShuffle #-}
data instance X64 (Complex a) = MkComplexX64 !(X64 a) !(X64 a)
instance PackX64 X64 a => PackX64 X64 (Complex a) where
  mkX64 (x0 :+ y0) (x1 :+ y1) (x2 :+ y2) (x3 :+ y3) (x4 :+ y4) (x5 :+ y5) (x6 :+ y6) (x7 :+ y7) (x8 :+ y8) (x9 :+ y9) (x10 :+ y10) (x11 :+ y11) (x12 :+ y12) (x13 :+ y13) (x14 :+ y14) (x15 :+ y15) (x16 :+ y16) (x17 :+ y17) (x18 :+ y18) (x19 :+ y19) (x20 :+ y20) (x21 :+ y21) (x22 :+ y22) (x23 :+ y23) (x24 :+ y24) (x25 :+ y25) (x26 :+ y26) (x27 :+ y27) (x28 :+ y28) (x29 :+ y29) (x30 :+ y30) (x31 :+ y31) (x32 :+ y32) (x33 :+ y33) (x34 :+ y34) (x35 :+ y35) (x36 :+ y36) (x37 :+ y37) (x38 :+ y38) (x39 :+ y39) (x40 :+ y40) (x41 :+ y41) (x42 :+ y42) (x43 :+ y43) (x44 :+ y44) (x45 :+ y45) (x46 :+ y46) (x47 :+ y47) (x48 :+ y48) (x49 :+ y49) (x50 :+ y50) (x51 :+ y51) (x52 :+ y52) (x53 :+ y53) (x54 :+ y54) (x55 :+ y55) (x56 :+ y56) (x57 :+ y57) (x58 :+ y58) (x59 :+ y59) (x60 :+ y60) (x61 :+ y61) (x62 :+ y62) (x63 :+ y63) = MkComplexX64 (mkX64 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) (mkX64 y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15 y16 y17 y18 y19 y20 y21 y22 y23 y24 y25 y26 y27 y28 y29 y30 y31 y32 y33 y34 y35 y36 y37 y38 y39 y40 y41 y42 y43 y44 y45 y46 y47 y48 y49 y50 y51 y52 y53 y54 y55 y56 y57 y58 y59 y60 y61 y62 y63)
  unpackX64 (MkComplexX64 s t) = case unpackX64 s of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) -> case unpackX64 t of (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29, y30, y31, y32, y33, y34, y35, y36, y37, y38, y39, y40, y41, y42, y43, y44, y45, y46, y47, y48, y49, y50, y51, y52, y53, y54, y55, y56, y57, y58, y59, y60, y61, y62, y63) -> (x0 :+ y0, x1 :+ y1, x2 :+ y2, x3 :+ y3, x4 :+ y4, x5 :+ y5, x6 :+ y6, x7 :+ y7, x8 :+ y8, x9 :+ y9, x10 :+ y10, x11 :+ y11, x12 :+ y12, x13 :+ y13, x14 :+ y14, x15 :+ y15, x16 :+ y16, x17 :+ y17, x18 :+ y18, x19 :+ y19, x20 :+ y20, x21 :+ y21, x22 :+ y22, x23 :+ y23, x24 :+ y24, x25 :+ y25, x26 :+ y26, x27 :+ y27, x28 :+ y28, x29 :+ y29, x30 :+ y30, x31 :+ y31, x32 :+ y32, x33 :+ y33, x34 :+ y34, x35 :+ y35, x36 :+ y36, x37 :+ y37, x38 :+ y38, x39 :+ y39, x40 :+ y40, x41 :+ y41, x42 :+ y42, x43 :+ y43, x44 :+ y44, x45 :+ y45, x46 :+ y46, x47 :+ y47, x48 :+ y48, x49 :+ y49, x50 :+ y50, x51 :+ y51, x52 :+ y52, x53 :+ y53, x54 :+ y54, x55 :+ y55, x56 :+ y56, x57 :+ y57, x58 :+ y58, x59 :+ y59, x60 :+ y60, x61 :+ y61, x62 :+ y62, x63 :+ y63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 a => Broadcast X64 (Complex a) where
  broadcast (x :+ y) = MkComplexX64 (broadcast x) (broadcast y)
  {-# INLINE broadcast #-}
instance SelectableF X64 a => SelectableF X64 (Complex a) where
  selectF !cond (MkComplexX64 x y) (MkComplexX64 x' y') = MkComplexX64 (selectF cond x x') (selectF cond y y')
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X64 a => UnaryShuffleT indices X64 (Complex a) where
  unaryShuffle (MkComplexX64 x y) = MkComplexX64 (unaryShuffle @indices x) (unaryShuffle @indices y)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X64 a => BinaryShuffleT indices X64 (Complex a) where
  binaryShuffle (MkComplexX64 x y) (MkComplexX64 u v) = MkComplexX64 (binaryShuffle @indices x u) (binaryShuffle @indices y v)
  {-# INLINE binaryShuffle #-}
data instance X64 () = MkUnitX64
instance PackX64 X64 () where
  mkX64 _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = MkUnitX64
  unpackX64 MkUnitX64 = ((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ())
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance Broadcast X64 () where
  broadcast _ = MkUnitX64
  {-# INLINE broadcast #-}
instance SelectableF X64 () where
  selectF _ _ _ = MkUnitX64
  {-# INLINE selectF #-}
instance (i0 < 64, i1 < 64, i2 < 64, i3 < 64, i4 < 64, i5 < 64, i6 < 64, i7 < 64, i8 < 64, i9 < 64, i10 < 64, i11 < 64, i12 < 64, i13 < 64, i14 < 64, i15 < 64, i16 < 64, i17 < 64, i18 < 64, i19 < 64, i20 < 64, i21 < 64, i22 < 64, i23 < 64, i24 < 64, i25 < 64, i26 < 64, i27 < 64, i28 < 64, i29 < 64, i30 < 64, i31 < 64, i32 < 64, i33 < 64, i34 < 64, i35 < 64, i36 < 64, i37 < 64, i38 < 64, i39 < 64, i40 < 64, i41 < 64, i42 < 64, i43 < 64, i44 < 64, i45 < 64, i46 < 64, i47 < 64, i48 < 64, i49 < 64, i50 < 64, i51 < 64, i52 < 64, i53 < 64, i54 < 64, i55 < 64, i56 < 64, i57 < 64, i58 < 64, i59 < 64, i60 < 64, i61 < 64, i62 < 64, i63 < 64) => UnaryShuffleT '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 () where
  unaryShuffle _ = MkUnitX64
  {-# INLINE unaryShuffle #-}
instance (i0 < 128, i1 < 128, i2 < 128, i3 < 128, i4 < 128, i5 < 128, i6 < 128, i7 < 128, i8 < 128, i9 < 128, i10 < 128, i11 < 128, i12 < 128, i13 < 128, i14 < 128, i15 < 128, i16 < 128, i17 < 128, i18 < 128, i19 < 128, i20 < 128, i21 < 128, i22 < 128, i23 < 128, i24 < 128, i25 < 128, i26 < 128, i27 < 128, i28 < 128, i29 < 128, i30 < 128, i31 < 128, i32 < 128, i33 < 128, i34 < 128, i35 < 128, i36 < 128, i37 < 128, i38 < 128, i39 < 128, i40 < 128, i41 < 128, i42 < 128, i43 < 128, i44 < 128, i45 < 128, i46 < 128, i47 < 128, i48 < 128, i49 < 128, i50 < 128, i51 < 128, i52 < 128, i53 < 128, i54 < 128, i55 < 128, i56 < 128, i57 < 128, i58 < 128, i59 < 128, i60 < 128, i61 < 128, i62 < 128, i63 < 128) => BinaryShuffleT '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 () where
  binaryShuffle _ _ = MkUnitX64
  {-# INLINE binaryShuffle #-}
data instance X64 (a0, a1) = MkTuple2X64 !(X64 a0) !(X64 a1)
instance (PackX64 X64 a0, PackX64 X64 a1) => PackX64 X64 (a0, a1) where
  mkX64 (x0_0, x0_1) (x1_0, x1_1) (x2_0, x2_1) (x3_0, x3_1) (x4_0, x4_1) (x5_0, x5_1) (x6_0, x6_1) (x7_0, x7_1) (x8_0, x8_1) (x9_0, x9_1) (x10_0, x10_1) (x11_0, x11_1) (x12_0, x12_1) (x13_0, x13_1) (x14_0, x14_1) (x15_0, x15_1) (x16_0, x16_1) (x17_0, x17_1) (x18_0, x18_1) (x19_0, x19_1) (x20_0, x20_1) (x21_0, x21_1) (x22_0, x22_1) (x23_0, x23_1) (x24_0, x24_1) (x25_0, x25_1) (x26_0, x26_1) (x27_0, x27_1) (x28_0, x28_1) (x29_0, x29_1) (x30_0, x30_1) (x31_0, x31_1) (x32_0, x32_1) (x33_0, x33_1) (x34_0, x34_1) (x35_0, x35_1) (x36_0, x36_1) (x37_0, x37_1) (x38_0, x38_1) (x39_0, x39_1) (x40_0, x40_1) (x41_0, x41_1) (x42_0, x42_1) (x43_0, x43_1) (x44_0, x44_1) (x45_0, x45_1) (x46_0, x46_1) (x47_0, x47_1) (x48_0, x48_1) (x49_0, x49_1) (x50_0, x50_1) (x51_0, x51_1) (x52_0, x52_1) (x53_0, x53_1) (x54_0, x54_1) (x55_0, x55_1) (x56_0, x56_1) (x57_0, x57_1) (x58_0, x58_1) (x59_0, x59_1) (x60_0, x60_1) (x61_0, x61_1) (x62_0, x62_1) (x63_0, x63_1) = MkTuple2X64 (mkX64 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0 x16_0 x17_0 x18_0 x19_0 x20_0 x21_0 x22_0 x23_0 x24_0 x25_0 x26_0 x27_0 x28_0 x29_0 x30_0 x31_0 x32_0 x33_0 x34_0 x35_0 x36_0 x37_0 x38_0 x39_0 x40_0 x41_0 x42_0 x43_0 x44_0 x45_0 x46_0 x47_0 x48_0 x49_0 x50_0 x51_0 x52_0 x53_0 x54_0 x55_0 x56_0 x57_0 x58_0 x59_0 x60_0 x61_0 x62_0 x63_0) (mkX64 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1 x16_1 x17_1 x18_1 x19_1 x20_1 x21_1 x22_1 x23_1 x24_1 x25_1 x26_1 x27_1 x28_1 x29_1 x30_1 x31_1 x32_1 x33_1 x34_1 x35_1 x36_1 x37_1 x38_1 x39_1 x40_1 x41_1 x42_1 x43_1 x44_1 x45_1 x46_1 x47_1 x48_1 x49_1 x50_1 x51_1 x52_1 x53_1 x54_1 x55_1 x56_1 x57_1 x58_1 x59_1 x60_1 x61_1 x62_1 x63_1)
  unpackX64 (MkTuple2X64 v0 v1) = case unpackX64 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0, x16_0, x17_0, x18_0, x19_0, x20_0, x21_0, x22_0, x23_0, x24_0, x25_0, x26_0, x27_0, x28_0, x29_0, x30_0, x31_0, x32_0, x33_0, x34_0, x35_0, x36_0, x37_0, x38_0, x39_0, x40_0, x41_0, x42_0, x43_0, x44_0, x45_0, x46_0, x47_0, x48_0, x49_0, x50_0, x51_0, x52_0, x53_0, x54_0, x55_0, x56_0, x57_0, x58_0, x59_0, x60_0, x61_0, x62_0, x63_0) -> case unpackX64 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1, x16_1, x17_1, x18_1, x19_1, x20_1, x21_1, x22_1, x23_1, x24_1, x25_1, x26_1, x27_1, x28_1, x29_1, x30_1, x31_1, x32_1, x33_1, x34_1, x35_1, x36_1, x37_1, x38_1, x39_1, x40_1, x41_1, x42_1, x43_1, x44_1, x45_1, x46_1, x47_1, x48_1, x49_1, x50_1, x51_1, x52_1, x53_1, x54_1, x55_1, x56_1, x57_1, x58_1, x59_1, x60_1, x61_1, x62_1, x63_1) -> ((x0_0, x0_1), (x1_0, x1_1), (x2_0, x2_1), (x3_0, x3_1), (x4_0, x4_1), (x5_0, x5_1), (x6_0, x6_1), (x7_0, x7_1), (x8_0, x8_1), (x9_0, x9_1), (x10_0, x10_1), (x11_0, x11_1), (x12_0, x12_1), (x13_0, x13_1), (x14_0, x14_1), (x15_0, x15_1), (x16_0, x16_1), (x17_0, x17_1), (x18_0, x18_1), (x19_0, x19_1), (x20_0, x20_1), (x21_0, x21_1), (x22_0, x22_1), (x23_0, x23_1), (x24_0, x24_1), (x25_0, x25_1), (x26_0, x26_1), (x27_0, x27_1), (x28_0, x28_1), (x29_0, x29_1), (x30_0, x30_1), (x31_0, x31_1), (x32_0, x32_1), (x33_0, x33_1), (x34_0, x34_1), (x35_0, x35_1), (x36_0, x36_1), (x37_0, x37_1), (x38_0, x38_1), (x39_0, x39_1), (x40_0, x40_1), (x41_0, x41_1), (x42_0, x42_1), (x43_0, x43_1), (x44_0, x44_1), (x45_0, x45_1), (x46_0, x46_1), (x47_0, x47_1), (x48_0, x48_1), (x49_0, x49_1), (x50_0, x50_1), (x51_0, x51_1), (x52_0, x52_1), (x53_0, x53_1), (x54_0, x54_1), (x55_0, x55_1), (x56_0, x56_1), (x57_0, x57_1), (x58_0, x58_1), (x59_0, x59_1), (x60_0, x60_1), (x61_0, x61_1), (x62_0, x62_1), (x63_0, x63_1))
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance (Broadcast X64 a0, Broadcast X64 a1) => Broadcast X64 (a0, a1) where
  broadcast (x0, x1) = MkTuple2X64 (broadcast x0) (broadcast x1)
  {-# INLINE broadcast #-}
instance (SelectableF X64 a0, SelectableF X64 a1) => SelectableF X64 (a0, a1) where
  selectF !cond (MkTuple2X64 x0 x1) (MkTuple2X64 y0 y1) = MkTuple2X64 (selectF cond x0 y0) (selectF cond x1 y1)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X64 a0, UnaryShuffleT indices X64 a1) => UnaryShuffleT indices X64 (a0, a1) where
  unaryShuffle (MkTuple2X64 x0 x1) = MkTuple2X64 (unaryShuffle @indices x0) (unaryShuffle @indices x1)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X64 a0, BinaryShuffleT indices X64 a1) => BinaryShuffleT indices X64 (a0, a1) where
  binaryShuffle (MkTuple2X64 x0 x1) (MkTuple2X64 y0 y1) = MkTuple2X64 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1)
  {-# INLINE binaryShuffle #-}
data instance X64 (a0, a1, a2) = MkTuple3X64 !(X64 a0) !(X64 a1) !(X64 a2)
instance (PackX64 X64 a0, PackX64 X64 a1, PackX64 X64 a2) => PackX64 X64 (a0, a1, a2) where
  mkX64 (x0_0, x0_1, x0_2) (x1_0, x1_1, x1_2) (x2_0, x2_1, x2_2) (x3_0, x3_1, x3_2) (x4_0, x4_1, x4_2) (x5_0, x5_1, x5_2) (x6_0, x6_1, x6_2) (x7_0, x7_1, x7_2) (x8_0, x8_1, x8_2) (x9_0, x9_1, x9_2) (x10_0, x10_1, x10_2) (x11_0, x11_1, x11_2) (x12_0, x12_1, x12_2) (x13_0, x13_1, x13_2) (x14_0, x14_1, x14_2) (x15_0, x15_1, x15_2) (x16_0, x16_1, x16_2) (x17_0, x17_1, x17_2) (x18_0, x18_1, x18_2) (x19_0, x19_1, x19_2) (x20_0, x20_1, x20_2) (x21_0, x21_1, x21_2) (x22_0, x22_1, x22_2) (x23_0, x23_1, x23_2) (x24_0, x24_1, x24_2) (x25_0, x25_1, x25_2) (x26_0, x26_1, x26_2) (x27_0, x27_1, x27_2) (x28_0, x28_1, x28_2) (x29_0, x29_1, x29_2) (x30_0, x30_1, x30_2) (x31_0, x31_1, x31_2) (x32_0, x32_1, x32_2) (x33_0, x33_1, x33_2) (x34_0, x34_1, x34_2) (x35_0, x35_1, x35_2) (x36_0, x36_1, x36_2) (x37_0, x37_1, x37_2) (x38_0, x38_1, x38_2) (x39_0, x39_1, x39_2) (x40_0, x40_1, x40_2) (x41_0, x41_1, x41_2) (x42_0, x42_1, x42_2) (x43_0, x43_1, x43_2) (x44_0, x44_1, x44_2) (x45_0, x45_1, x45_2) (x46_0, x46_1, x46_2) (x47_0, x47_1, x47_2) (x48_0, x48_1, x48_2) (x49_0, x49_1, x49_2) (x50_0, x50_1, x50_2) (x51_0, x51_1, x51_2) (x52_0, x52_1, x52_2) (x53_0, x53_1, x53_2) (x54_0, x54_1, x54_2) (x55_0, x55_1, x55_2) (x56_0, x56_1, x56_2) (x57_0, x57_1, x57_2) (x58_0, x58_1, x58_2) (x59_0, x59_1, x59_2) (x60_0, x60_1, x60_2) (x61_0, x61_1, x61_2) (x62_0, x62_1, x62_2) (x63_0, x63_1, x63_2) = MkTuple3X64 (mkX64 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0 x16_0 x17_0 x18_0 x19_0 x20_0 x21_0 x22_0 x23_0 x24_0 x25_0 x26_0 x27_0 x28_0 x29_0 x30_0 x31_0 x32_0 x33_0 x34_0 x35_0 x36_0 x37_0 x38_0 x39_0 x40_0 x41_0 x42_0 x43_0 x44_0 x45_0 x46_0 x47_0 x48_0 x49_0 x50_0 x51_0 x52_0 x53_0 x54_0 x55_0 x56_0 x57_0 x58_0 x59_0 x60_0 x61_0 x62_0 x63_0) (mkX64 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1 x16_1 x17_1 x18_1 x19_1 x20_1 x21_1 x22_1 x23_1 x24_1 x25_1 x26_1 x27_1 x28_1 x29_1 x30_1 x31_1 x32_1 x33_1 x34_1 x35_1 x36_1 x37_1 x38_1 x39_1 x40_1 x41_1 x42_1 x43_1 x44_1 x45_1 x46_1 x47_1 x48_1 x49_1 x50_1 x51_1 x52_1 x53_1 x54_1 x55_1 x56_1 x57_1 x58_1 x59_1 x60_1 x61_1 x62_1 x63_1) (mkX64 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2 x16_2 x17_2 x18_2 x19_2 x20_2 x21_2 x22_2 x23_2 x24_2 x25_2 x26_2 x27_2 x28_2 x29_2 x30_2 x31_2 x32_2 x33_2 x34_2 x35_2 x36_2 x37_2 x38_2 x39_2 x40_2 x41_2 x42_2 x43_2 x44_2 x45_2 x46_2 x47_2 x48_2 x49_2 x50_2 x51_2 x52_2 x53_2 x54_2 x55_2 x56_2 x57_2 x58_2 x59_2 x60_2 x61_2 x62_2 x63_2)
  unpackX64 (MkTuple3X64 v0 v1 v2) = case unpackX64 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0, x16_0, x17_0, x18_0, x19_0, x20_0, x21_0, x22_0, x23_0, x24_0, x25_0, x26_0, x27_0, x28_0, x29_0, x30_0, x31_0, x32_0, x33_0, x34_0, x35_0, x36_0, x37_0, x38_0, x39_0, x40_0, x41_0, x42_0, x43_0, x44_0, x45_0, x46_0, x47_0, x48_0, x49_0, x50_0, x51_0, x52_0, x53_0, x54_0, x55_0, x56_0, x57_0, x58_0, x59_0, x60_0, x61_0, x62_0, x63_0) -> case unpackX64 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1, x16_1, x17_1, x18_1, x19_1, x20_1, x21_1, x22_1, x23_1, x24_1, x25_1, x26_1, x27_1, x28_1, x29_1, x30_1, x31_1, x32_1, x33_1, x34_1, x35_1, x36_1, x37_1, x38_1, x39_1, x40_1, x41_1, x42_1, x43_1, x44_1, x45_1, x46_1, x47_1, x48_1, x49_1, x50_1, x51_1, x52_1, x53_1, x54_1, x55_1, x56_1, x57_1, x58_1, x59_1, x60_1, x61_1, x62_1, x63_1) -> case unpackX64 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2, x16_2, x17_2, x18_2, x19_2, x20_2, x21_2, x22_2, x23_2, x24_2, x25_2, x26_2, x27_2, x28_2, x29_2, x30_2, x31_2, x32_2, x33_2, x34_2, x35_2, x36_2, x37_2, x38_2, x39_2, x40_2, x41_2, x42_2, x43_2, x44_2, x45_2, x46_2, x47_2, x48_2, x49_2, x50_2, x51_2, x52_2, x53_2, x54_2, x55_2, x56_2, x57_2, x58_2, x59_2, x60_2, x61_2, x62_2, x63_2) -> ((x0_0, x0_1, x0_2), (x1_0, x1_1, x1_2), (x2_0, x2_1, x2_2), (x3_0, x3_1, x3_2), (x4_0, x4_1, x4_2), (x5_0, x5_1, x5_2), (x6_0, x6_1, x6_2), (x7_0, x7_1, x7_2), (x8_0, x8_1, x8_2), (x9_0, x9_1, x9_2), (x10_0, x10_1, x10_2), (x11_0, x11_1, x11_2), (x12_0, x12_1, x12_2), (x13_0, x13_1, x13_2), (x14_0, x14_1, x14_2), (x15_0, x15_1, x15_2), (x16_0, x16_1, x16_2), (x17_0, x17_1, x17_2), (x18_0, x18_1, x18_2), (x19_0, x19_1, x19_2), (x20_0, x20_1, x20_2), (x21_0, x21_1, x21_2), (x22_0, x22_1, x22_2), (x23_0, x23_1, x23_2), (x24_0, x24_1, x24_2), (x25_0, x25_1, x25_2), (x26_0, x26_1, x26_2), (x27_0, x27_1, x27_2), (x28_0, x28_1, x28_2), (x29_0, x29_1, x29_2), (x30_0, x30_1, x30_2), (x31_0, x31_1, x31_2), (x32_0, x32_1, x32_2), (x33_0, x33_1, x33_2), (x34_0, x34_1, x34_2), (x35_0, x35_1, x35_2), (x36_0, x36_1, x36_2), (x37_0, x37_1, x37_2), (x38_0, x38_1, x38_2), (x39_0, x39_1, x39_2), (x40_0, x40_1, x40_2), (x41_0, x41_1, x41_2), (x42_0, x42_1, x42_2), (x43_0, x43_1, x43_2), (x44_0, x44_1, x44_2), (x45_0, x45_1, x45_2), (x46_0, x46_1, x46_2), (x47_0, x47_1, x47_2), (x48_0, x48_1, x48_2), (x49_0, x49_1, x49_2), (x50_0, x50_1, x50_2), (x51_0, x51_1, x51_2), (x52_0, x52_1, x52_2), (x53_0, x53_1, x53_2), (x54_0, x54_1, x54_2), (x55_0, x55_1, x55_2), (x56_0, x56_1, x56_2), (x57_0, x57_1, x57_2), (x58_0, x58_1, x58_2), (x59_0, x59_1, x59_2), (x60_0, x60_1, x60_2), (x61_0, x61_1, x61_2), (x62_0, x62_1, x62_2), (x63_0, x63_1, x63_2))
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance (Broadcast X64 a0, Broadcast X64 a1, Broadcast X64 a2) => Broadcast X64 (a0, a1, a2) where
  broadcast (x0, x1, x2) = MkTuple3X64 (broadcast x0) (broadcast x1) (broadcast x2)
  {-# INLINE broadcast #-}
instance (SelectableF X64 a0, SelectableF X64 a1, SelectableF X64 a2) => SelectableF X64 (a0, a1, a2) where
  selectF !cond (MkTuple3X64 x0 x1 x2) (MkTuple3X64 y0 y1 y2) = MkTuple3X64 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X64 a0, UnaryShuffleT indices X64 a1, UnaryShuffleT indices X64 a2) => UnaryShuffleT indices X64 (a0, a1, a2) where
  unaryShuffle (MkTuple3X64 x0 x1 x2) = MkTuple3X64 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X64 a0, BinaryShuffleT indices X64 a1, BinaryShuffleT indices X64 a2) => BinaryShuffleT indices X64 (a0, a1, a2) where
  binaryShuffle (MkTuple3X64 x0 x1 x2) (MkTuple3X64 y0 y1 y2) = MkTuple3X64 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2)
  {-# INLINE binaryShuffle #-}
data instance X64 (a0, a1, a2, a3) = MkTuple4X64 !(X64 a0) !(X64 a1) !(X64 a2) !(X64 a3)
instance (PackX64 X64 a0, PackX64 X64 a1, PackX64 X64 a2, PackX64 X64 a3) => PackX64 X64 (a0, a1, a2, a3) where
  mkX64 (x0_0, x0_1, x0_2, x0_3) (x1_0, x1_1, x1_2, x1_3) (x2_0, x2_1, x2_2, x2_3) (x3_0, x3_1, x3_2, x3_3) (x4_0, x4_1, x4_2, x4_3) (x5_0, x5_1, x5_2, x5_3) (x6_0, x6_1, x6_2, x6_3) (x7_0, x7_1, x7_2, x7_3) (x8_0, x8_1, x8_2, x8_3) (x9_0, x9_1, x9_2, x9_3) (x10_0, x10_1, x10_2, x10_3) (x11_0, x11_1, x11_2, x11_3) (x12_0, x12_1, x12_2, x12_3) (x13_0, x13_1, x13_2, x13_3) (x14_0, x14_1, x14_2, x14_3) (x15_0, x15_1, x15_2, x15_3) (x16_0, x16_1, x16_2, x16_3) (x17_0, x17_1, x17_2, x17_3) (x18_0, x18_1, x18_2, x18_3) (x19_0, x19_1, x19_2, x19_3) (x20_0, x20_1, x20_2, x20_3) (x21_0, x21_1, x21_2, x21_3) (x22_0, x22_1, x22_2, x22_3) (x23_0, x23_1, x23_2, x23_3) (x24_0, x24_1, x24_2, x24_3) (x25_0, x25_1, x25_2, x25_3) (x26_0, x26_1, x26_2, x26_3) (x27_0, x27_1, x27_2, x27_3) (x28_0, x28_1, x28_2, x28_3) (x29_0, x29_1, x29_2, x29_3) (x30_0, x30_1, x30_2, x30_3) (x31_0, x31_1, x31_2, x31_3) (x32_0, x32_1, x32_2, x32_3) (x33_0, x33_1, x33_2, x33_3) (x34_0, x34_1, x34_2, x34_3) (x35_0, x35_1, x35_2, x35_3) (x36_0, x36_1, x36_2, x36_3) (x37_0, x37_1, x37_2, x37_3) (x38_0, x38_1, x38_2, x38_3) (x39_0, x39_1, x39_2, x39_3) (x40_0, x40_1, x40_2, x40_3) (x41_0, x41_1, x41_2, x41_3) (x42_0, x42_1, x42_2, x42_3) (x43_0, x43_1, x43_2, x43_3) (x44_0, x44_1, x44_2, x44_3) (x45_0, x45_1, x45_2, x45_3) (x46_0, x46_1, x46_2, x46_3) (x47_0, x47_1, x47_2, x47_3) (x48_0, x48_1, x48_2, x48_3) (x49_0, x49_1, x49_2, x49_3) (x50_0, x50_1, x50_2, x50_3) (x51_0, x51_1, x51_2, x51_3) (x52_0, x52_1, x52_2, x52_3) (x53_0, x53_1, x53_2, x53_3) (x54_0, x54_1, x54_2, x54_3) (x55_0, x55_1, x55_2, x55_3) (x56_0, x56_1, x56_2, x56_3) (x57_0, x57_1, x57_2, x57_3) (x58_0, x58_1, x58_2, x58_3) (x59_0, x59_1, x59_2, x59_3) (x60_0, x60_1, x60_2, x60_3) (x61_0, x61_1, x61_2, x61_3) (x62_0, x62_1, x62_2, x62_3) (x63_0, x63_1, x63_2, x63_3) = MkTuple4X64 (mkX64 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0 x16_0 x17_0 x18_0 x19_0 x20_0 x21_0 x22_0 x23_0 x24_0 x25_0 x26_0 x27_0 x28_0 x29_0 x30_0 x31_0 x32_0 x33_0 x34_0 x35_0 x36_0 x37_0 x38_0 x39_0 x40_0 x41_0 x42_0 x43_0 x44_0 x45_0 x46_0 x47_0 x48_0 x49_0 x50_0 x51_0 x52_0 x53_0 x54_0 x55_0 x56_0 x57_0 x58_0 x59_0 x60_0 x61_0 x62_0 x63_0) (mkX64 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1 x16_1 x17_1 x18_1 x19_1 x20_1 x21_1 x22_1 x23_1 x24_1 x25_1 x26_1 x27_1 x28_1 x29_1 x30_1 x31_1 x32_1 x33_1 x34_1 x35_1 x36_1 x37_1 x38_1 x39_1 x40_1 x41_1 x42_1 x43_1 x44_1 x45_1 x46_1 x47_1 x48_1 x49_1 x50_1 x51_1 x52_1 x53_1 x54_1 x55_1 x56_1 x57_1 x58_1 x59_1 x60_1 x61_1 x62_1 x63_1) (mkX64 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2 x16_2 x17_2 x18_2 x19_2 x20_2 x21_2 x22_2 x23_2 x24_2 x25_2 x26_2 x27_2 x28_2 x29_2 x30_2 x31_2 x32_2 x33_2 x34_2 x35_2 x36_2 x37_2 x38_2 x39_2 x40_2 x41_2 x42_2 x43_2 x44_2 x45_2 x46_2 x47_2 x48_2 x49_2 x50_2 x51_2 x52_2 x53_2 x54_2 x55_2 x56_2 x57_2 x58_2 x59_2 x60_2 x61_2 x62_2 x63_2) (mkX64 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3 x8_3 x9_3 x10_3 x11_3 x12_3 x13_3 x14_3 x15_3 x16_3 x17_3 x18_3 x19_3 x20_3 x21_3 x22_3 x23_3 x24_3 x25_3 x26_3 x27_3 x28_3 x29_3 x30_3 x31_3 x32_3 x33_3 x34_3 x35_3 x36_3 x37_3 x38_3 x39_3 x40_3 x41_3 x42_3 x43_3 x44_3 x45_3 x46_3 x47_3 x48_3 x49_3 x50_3 x51_3 x52_3 x53_3 x54_3 x55_3 x56_3 x57_3 x58_3 x59_3 x60_3 x61_3 x62_3 x63_3)
  unpackX64 (MkTuple4X64 v0 v1 v2 v3) = case unpackX64 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0, x16_0, x17_0, x18_0, x19_0, x20_0, x21_0, x22_0, x23_0, x24_0, x25_0, x26_0, x27_0, x28_0, x29_0, x30_0, x31_0, x32_0, x33_0, x34_0, x35_0, x36_0, x37_0, x38_0, x39_0, x40_0, x41_0, x42_0, x43_0, x44_0, x45_0, x46_0, x47_0, x48_0, x49_0, x50_0, x51_0, x52_0, x53_0, x54_0, x55_0, x56_0, x57_0, x58_0, x59_0, x60_0, x61_0, x62_0, x63_0) -> case unpackX64 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1, x16_1, x17_1, x18_1, x19_1, x20_1, x21_1, x22_1, x23_1, x24_1, x25_1, x26_1, x27_1, x28_1, x29_1, x30_1, x31_1, x32_1, x33_1, x34_1, x35_1, x36_1, x37_1, x38_1, x39_1, x40_1, x41_1, x42_1, x43_1, x44_1, x45_1, x46_1, x47_1, x48_1, x49_1, x50_1, x51_1, x52_1, x53_1, x54_1, x55_1, x56_1, x57_1, x58_1, x59_1, x60_1, x61_1, x62_1, x63_1) -> case unpackX64 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2, x16_2, x17_2, x18_2, x19_2, x20_2, x21_2, x22_2, x23_2, x24_2, x25_2, x26_2, x27_2, x28_2, x29_2, x30_2, x31_2, x32_2, x33_2, x34_2, x35_2, x36_2, x37_2, x38_2, x39_2, x40_2, x41_2, x42_2, x43_2, x44_2, x45_2, x46_2, x47_2, x48_2, x49_2, x50_2, x51_2, x52_2, x53_2, x54_2, x55_2, x56_2, x57_2, x58_2, x59_2, x60_2, x61_2, x62_2, x63_2) -> case unpackX64 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3, x8_3, x9_3, x10_3, x11_3, x12_3, x13_3, x14_3, x15_3, x16_3, x17_3, x18_3, x19_3, x20_3, x21_3, x22_3, x23_3, x24_3, x25_3, x26_3, x27_3, x28_3, x29_3, x30_3, x31_3, x32_3, x33_3, x34_3, x35_3, x36_3, x37_3, x38_3, x39_3, x40_3, x41_3, x42_3, x43_3, x44_3, x45_3, x46_3, x47_3, x48_3, x49_3, x50_3, x51_3, x52_3, x53_3, x54_3, x55_3, x56_3, x57_3, x58_3, x59_3, x60_3, x61_3, x62_3, x63_3) -> ((x0_0, x0_1, x0_2, x0_3), (x1_0, x1_1, x1_2, x1_3), (x2_0, x2_1, x2_2, x2_3), (x3_0, x3_1, x3_2, x3_3), (x4_0, x4_1, x4_2, x4_3), (x5_0, x5_1, x5_2, x5_3), (x6_0, x6_1, x6_2, x6_3), (x7_0, x7_1, x7_2, x7_3), (x8_0, x8_1, x8_2, x8_3), (x9_0, x9_1, x9_2, x9_3), (x10_0, x10_1, x10_2, x10_3), (x11_0, x11_1, x11_2, x11_3), (x12_0, x12_1, x12_2, x12_3), (x13_0, x13_1, x13_2, x13_3), (x14_0, x14_1, x14_2, x14_3), (x15_0, x15_1, x15_2, x15_3), (x16_0, x16_1, x16_2, x16_3), (x17_0, x17_1, x17_2, x17_3), (x18_0, x18_1, x18_2, x18_3), (x19_0, x19_1, x19_2, x19_3), (x20_0, x20_1, x20_2, x20_3), (x21_0, x21_1, x21_2, x21_3), (x22_0, x22_1, x22_2, x22_3), (x23_0, x23_1, x23_2, x23_3), (x24_0, x24_1, x24_2, x24_3), (x25_0, x25_1, x25_2, x25_3), (x26_0, x26_1, x26_2, x26_3), (x27_0, x27_1, x27_2, x27_3), (x28_0, x28_1, x28_2, x28_3), (x29_0, x29_1, x29_2, x29_3), (x30_0, x30_1, x30_2, x30_3), (x31_0, x31_1, x31_2, x31_3), (x32_0, x32_1, x32_2, x32_3), (x33_0, x33_1, x33_2, x33_3), (x34_0, x34_1, x34_2, x34_3), (x35_0, x35_1, x35_2, x35_3), (x36_0, x36_1, x36_2, x36_3), (x37_0, x37_1, x37_2, x37_3), (x38_0, x38_1, x38_2, x38_3), (x39_0, x39_1, x39_2, x39_3), (x40_0, x40_1, x40_2, x40_3), (x41_0, x41_1, x41_2, x41_3), (x42_0, x42_1, x42_2, x42_3), (x43_0, x43_1, x43_2, x43_3), (x44_0, x44_1, x44_2, x44_3), (x45_0, x45_1, x45_2, x45_3), (x46_0, x46_1, x46_2, x46_3), (x47_0, x47_1, x47_2, x47_3), (x48_0, x48_1, x48_2, x48_3), (x49_0, x49_1, x49_2, x49_3), (x50_0, x50_1, x50_2, x50_3), (x51_0, x51_1, x51_2, x51_3), (x52_0, x52_1, x52_2, x52_3), (x53_0, x53_1, x53_2, x53_3), (x54_0, x54_1, x54_2, x54_3), (x55_0, x55_1, x55_2, x55_3), (x56_0, x56_1, x56_2, x56_3), (x57_0, x57_1, x57_2, x57_3), (x58_0, x58_1, x58_2, x58_3), (x59_0, x59_1, x59_2, x59_3), (x60_0, x60_1, x60_2, x60_3), (x61_0, x61_1, x61_2, x61_3), (x62_0, x62_1, x62_2, x62_3), (x63_0, x63_1, x63_2, x63_3))
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance (Broadcast X64 a0, Broadcast X64 a1, Broadcast X64 a2, Broadcast X64 a3) => Broadcast X64 (a0, a1, a2, a3) where
  broadcast (x0, x1, x2, x3) = MkTuple4X64 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3)
  {-# INLINE broadcast #-}
instance (SelectableF X64 a0, SelectableF X64 a1, SelectableF X64 a2, SelectableF X64 a3) => SelectableF X64 (a0, a1, a2, a3) where
  selectF !cond (MkTuple4X64 x0 x1 x2 x3) (MkTuple4X64 y0 y1 y2 y3) = MkTuple4X64 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X64 a0, UnaryShuffleT indices X64 a1, UnaryShuffleT indices X64 a2, UnaryShuffleT indices X64 a3) => UnaryShuffleT indices X64 (a0, a1, a2, a3) where
  unaryShuffle (MkTuple4X64 x0 x1 x2 x3) = MkTuple4X64 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X64 a0, BinaryShuffleT indices X64 a1, BinaryShuffleT indices X64 a2, BinaryShuffleT indices X64 a3) => BinaryShuffleT indices X64 (a0, a1, a2, a3) where
  binaryShuffle (MkTuple4X64 x0 x1 x2 x3) (MkTuple4X64 y0 y1 y2 y3) = MkTuple4X64 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3)
  {-# INLINE binaryShuffle #-}
data instance X64 (a0, a1, a2, a3, a4) = MkTuple5X64 !(X64 a0) !(X64 a1) !(X64 a2) !(X64 a3) !(X64 a4)
instance (PackX64 X64 a0, PackX64 X64 a1, PackX64 X64 a2, PackX64 X64 a3, PackX64 X64 a4) => PackX64 X64 (a0, a1, a2, a3, a4) where
  mkX64 (x0_0, x0_1, x0_2, x0_3, x0_4) (x1_0, x1_1, x1_2, x1_3, x1_4) (x2_0, x2_1, x2_2, x2_3, x2_4) (x3_0, x3_1, x3_2, x3_3, x3_4) (x4_0, x4_1, x4_2, x4_3, x4_4) (x5_0, x5_1, x5_2, x5_3, x5_4) (x6_0, x6_1, x6_2, x6_3, x6_4) (x7_0, x7_1, x7_2, x7_3, x7_4) (x8_0, x8_1, x8_2, x8_3, x8_4) (x9_0, x9_1, x9_2, x9_3, x9_4) (x10_0, x10_1, x10_2, x10_3, x10_4) (x11_0, x11_1, x11_2, x11_3, x11_4) (x12_0, x12_1, x12_2, x12_3, x12_4) (x13_0, x13_1, x13_2, x13_3, x13_4) (x14_0, x14_1, x14_2, x14_3, x14_4) (x15_0, x15_1, x15_2, x15_3, x15_4) (x16_0, x16_1, x16_2, x16_3, x16_4) (x17_0, x17_1, x17_2, x17_3, x17_4) (x18_0, x18_1, x18_2, x18_3, x18_4) (x19_0, x19_1, x19_2, x19_3, x19_4) (x20_0, x20_1, x20_2, x20_3, x20_4) (x21_0, x21_1, x21_2, x21_3, x21_4) (x22_0, x22_1, x22_2, x22_3, x22_4) (x23_0, x23_1, x23_2, x23_3, x23_4) (x24_0, x24_1, x24_2, x24_3, x24_4) (x25_0, x25_1, x25_2, x25_3, x25_4) (x26_0, x26_1, x26_2, x26_3, x26_4) (x27_0, x27_1, x27_2, x27_3, x27_4) (x28_0, x28_1, x28_2, x28_3, x28_4) (x29_0, x29_1, x29_2, x29_3, x29_4) (x30_0, x30_1, x30_2, x30_3, x30_4) (x31_0, x31_1, x31_2, x31_3, x31_4) (x32_0, x32_1, x32_2, x32_3, x32_4) (x33_0, x33_1, x33_2, x33_3, x33_4) (x34_0, x34_1, x34_2, x34_3, x34_4) (x35_0, x35_1, x35_2, x35_3, x35_4) (x36_0, x36_1, x36_2, x36_3, x36_4) (x37_0, x37_1, x37_2, x37_3, x37_4) (x38_0, x38_1, x38_2, x38_3, x38_4) (x39_0, x39_1, x39_2, x39_3, x39_4) (x40_0, x40_1, x40_2, x40_3, x40_4) (x41_0, x41_1, x41_2, x41_3, x41_4) (x42_0, x42_1, x42_2, x42_3, x42_4) (x43_0, x43_1, x43_2, x43_3, x43_4) (x44_0, x44_1, x44_2, x44_3, x44_4) (x45_0, x45_1, x45_2, x45_3, x45_4) (x46_0, x46_1, x46_2, x46_3, x46_4) (x47_0, x47_1, x47_2, x47_3, x47_4) (x48_0, x48_1, x48_2, x48_3, x48_4) (x49_0, x49_1, x49_2, x49_3, x49_4) (x50_0, x50_1, x50_2, x50_3, x50_4) (x51_0, x51_1, x51_2, x51_3, x51_4) (x52_0, x52_1, x52_2, x52_3, x52_4) (x53_0, x53_1, x53_2, x53_3, x53_4) (x54_0, x54_1, x54_2, x54_3, x54_4) (x55_0, x55_1, x55_2, x55_3, x55_4) (x56_0, x56_1, x56_2, x56_3, x56_4) (x57_0, x57_1, x57_2, x57_3, x57_4) (x58_0, x58_1, x58_2, x58_3, x58_4) (x59_0, x59_1, x59_2, x59_3, x59_4) (x60_0, x60_1, x60_2, x60_3, x60_4) (x61_0, x61_1, x61_2, x61_3, x61_4) (x62_0, x62_1, x62_2, x62_3, x62_4) (x63_0, x63_1, x63_2, x63_3, x63_4) = MkTuple5X64 (mkX64 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0 x16_0 x17_0 x18_0 x19_0 x20_0 x21_0 x22_0 x23_0 x24_0 x25_0 x26_0 x27_0 x28_0 x29_0 x30_0 x31_0 x32_0 x33_0 x34_0 x35_0 x36_0 x37_0 x38_0 x39_0 x40_0 x41_0 x42_0 x43_0 x44_0 x45_0 x46_0 x47_0 x48_0 x49_0 x50_0 x51_0 x52_0 x53_0 x54_0 x55_0 x56_0 x57_0 x58_0 x59_0 x60_0 x61_0 x62_0 x63_0) (mkX64 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1 x16_1 x17_1 x18_1 x19_1 x20_1 x21_1 x22_1 x23_1 x24_1 x25_1 x26_1 x27_1 x28_1 x29_1 x30_1 x31_1 x32_1 x33_1 x34_1 x35_1 x36_1 x37_1 x38_1 x39_1 x40_1 x41_1 x42_1 x43_1 x44_1 x45_1 x46_1 x47_1 x48_1 x49_1 x50_1 x51_1 x52_1 x53_1 x54_1 x55_1 x56_1 x57_1 x58_1 x59_1 x60_1 x61_1 x62_1 x63_1) (mkX64 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2 x16_2 x17_2 x18_2 x19_2 x20_2 x21_2 x22_2 x23_2 x24_2 x25_2 x26_2 x27_2 x28_2 x29_2 x30_2 x31_2 x32_2 x33_2 x34_2 x35_2 x36_2 x37_2 x38_2 x39_2 x40_2 x41_2 x42_2 x43_2 x44_2 x45_2 x46_2 x47_2 x48_2 x49_2 x50_2 x51_2 x52_2 x53_2 x54_2 x55_2 x56_2 x57_2 x58_2 x59_2 x60_2 x61_2 x62_2 x63_2) (mkX64 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3 x8_3 x9_3 x10_3 x11_3 x12_3 x13_3 x14_3 x15_3 x16_3 x17_3 x18_3 x19_3 x20_3 x21_3 x22_3 x23_3 x24_3 x25_3 x26_3 x27_3 x28_3 x29_3 x30_3 x31_3 x32_3 x33_3 x34_3 x35_3 x36_3 x37_3 x38_3 x39_3 x40_3 x41_3 x42_3 x43_3 x44_3 x45_3 x46_3 x47_3 x48_3 x49_3 x50_3 x51_3 x52_3 x53_3 x54_3 x55_3 x56_3 x57_3 x58_3 x59_3 x60_3 x61_3 x62_3 x63_3) (mkX64 x0_4 x1_4 x2_4 x3_4 x4_4 x5_4 x6_4 x7_4 x8_4 x9_4 x10_4 x11_4 x12_4 x13_4 x14_4 x15_4 x16_4 x17_4 x18_4 x19_4 x20_4 x21_4 x22_4 x23_4 x24_4 x25_4 x26_4 x27_4 x28_4 x29_4 x30_4 x31_4 x32_4 x33_4 x34_4 x35_4 x36_4 x37_4 x38_4 x39_4 x40_4 x41_4 x42_4 x43_4 x44_4 x45_4 x46_4 x47_4 x48_4 x49_4 x50_4 x51_4 x52_4 x53_4 x54_4 x55_4 x56_4 x57_4 x58_4 x59_4 x60_4 x61_4 x62_4 x63_4)
  unpackX64 (MkTuple5X64 v0 v1 v2 v3 v4) = case unpackX64 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0, x16_0, x17_0, x18_0, x19_0, x20_0, x21_0, x22_0, x23_0, x24_0, x25_0, x26_0, x27_0, x28_0, x29_0, x30_0, x31_0, x32_0, x33_0, x34_0, x35_0, x36_0, x37_0, x38_0, x39_0, x40_0, x41_0, x42_0, x43_0, x44_0, x45_0, x46_0, x47_0, x48_0, x49_0, x50_0, x51_0, x52_0, x53_0, x54_0, x55_0, x56_0, x57_0, x58_0, x59_0, x60_0, x61_0, x62_0, x63_0) -> case unpackX64 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1, x16_1, x17_1, x18_1, x19_1, x20_1, x21_1, x22_1, x23_1, x24_1, x25_1, x26_1, x27_1, x28_1, x29_1, x30_1, x31_1, x32_1, x33_1, x34_1, x35_1, x36_1, x37_1, x38_1, x39_1, x40_1, x41_1, x42_1, x43_1, x44_1, x45_1, x46_1, x47_1, x48_1, x49_1, x50_1, x51_1, x52_1, x53_1, x54_1, x55_1, x56_1, x57_1, x58_1, x59_1, x60_1, x61_1, x62_1, x63_1) -> case unpackX64 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2, x16_2, x17_2, x18_2, x19_2, x20_2, x21_2, x22_2, x23_2, x24_2, x25_2, x26_2, x27_2, x28_2, x29_2, x30_2, x31_2, x32_2, x33_2, x34_2, x35_2, x36_2, x37_2, x38_2, x39_2, x40_2, x41_2, x42_2, x43_2, x44_2, x45_2, x46_2, x47_2, x48_2, x49_2, x50_2, x51_2, x52_2, x53_2, x54_2, x55_2, x56_2, x57_2, x58_2, x59_2, x60_2, x61_2, x62_2, x63_2) -> case unpackX64 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3, x8_3, x9_3, x10_3, x11_3, x12_3, x13_3, x14_3, x15_3, x16_3, x17_3, x18_3, x19_3, x20_3, x21_3, x22_3, x23_3, x24_3, x25_3, x26_3, x27_3, x28_3, x29_3, x30_3, x31_3, x32_3, x33_3, x34_3, x35_3, x36_3, x37_3, x38_3, x39_3, x40_3, x41_3, x42_3, x43_3, x44_3, x45_3, x46_3, x47_3, x48_3, x49_3, x50_3, x51_3, x52_3, x53_3, x54_3, x55_3, x56_3, x57_3, x58_3, x59_3, x60_3, x61_3, x62_3, x63_3) -> case unpackX64 v4 of (x0_4, x1_4, x2_4, x3_4, x4_4, x5_4, x6_4, x7_4, x8_4, x9_4, x10_4, x11_4, x12_4, x13_4, x14_4, x15_4, x16_4, x17_4, x18_4, x19_4, x20_4, x21_4, x22_4, x23_4, x24_4, x25_4, x26_4, x27_4, x28_4, x29_4, x30_4, x31_4, x32_4, x33_4, x34_4, x35_4, x36_4, x37_4, x38_4, x39_4, x40_4, x41_4, x42_4, x43_4, x44_4, x45_4, x46_4, x47_4, x48_4, x49_4, x50_4, x51_4, x52_4, x53_4, x54_4, x55_4, x56_4, x57_4, x58_4, x59_4, x60_4, x61_4, x62_4, x63_4) -> ((x0_0, x0_1, x0_2, x0_3, x0_4), (x1_0, x1_1, x1_2, x1_3, x1_4), (x2_0, x2_1, x2_2, x2_3, x2_4), (x3_0, x3_1, x3_2, x3_3, x3_4), (x4_0, x4_1, x4_2, x4_3, x4_4), (x5_0, x5_1, x5_2, x5_3, x5_4), (x6_0, x6_1, x6_2, x6_3, x6_4), (x7_0, x7_1, x7_2, x7_3, x7_4), (x8_0, x8_1, x8_2, x8_3, x8_4), (x9_0, x9_1, x9_2, x9_3, x9_4), (x10_0, x10_1, x10_2, x10_3, x10_4), (x11_0, x11_1, x11_2, x11_3, x11_4), (x12_0, x12_1, x12_2, x12_3, x12_4), (x13_0, x13_1, x13_2, x13_3, x13_4), (x14_0, x14_1, x14_2, x14_3, x14_4), (x15_0, x15_1, x15_2, x15_3, x15_4), (x16_0, x16_1, x16_2, x16_3, x16_4), (x17_0, x17_1, x17_2, x17_3, x17_4), (x18_0, x18_1, x18_2, x18_3, x18_4), (x19_0, x19_1, x19_2, x19_3, x19_4), (x20_0, x20_1, x20_2, x20_3, x20_4), (x21_0, x21_1, x21_2, x21_3, x21_4), (x22_0, x22_1, x22_2, x22_3, x22_4), (x23_0, x23_1, x23_2, x23_3, x23_4), (x24_0, x24_1, x24_2, x24_3, x24_4), (x25_0, x25_1, x25_2, x25_3, x25_4), (x26_0, x26_1, x26_2, x26_3, x26_4), (x27_0, x27_1, x27_2, x27_3, x27_4), (x28_0, x28_1, x28_2, x28_3, x28_4), (x29_0, x29_1, x29_2, x29_3, x29_4), (x30_0, x30_1, x30_2, x30_3, x30_4), (x31_0, x31_1, x31_2, x31_3, x31_4), (x32_0, x32_1, x32_2, x32_3, x32_4), (x33_0, x33_1, x33_2, x33_3, x33_4), (x34_0, x34_1, x34_2, x34_3, x34_4), (x35_0, x35_1, x35_2, x35_3, x35_4), (x36_0, x36_1, x36_2, x36_3, x36_4), (x37_0, x37_1, x37_2, x37_3, x37_4), (x38_0, x38_1, x38_2, x38_3, x38_4), (x39_0, x39_1, x39_2, x39_3, x39_4), (x40_0, x40_1, x40_2, x40_3, x40_4), (x41_0, x41_1, x41_2, x41_3, x41_4), (x42_0, x42_1, x42_2, x42_3, x42_4), (x43_0, x43_1, x43_2, x43_3, x43_4), (x44_0, x44_1, x44_2, x44_3, x44_4), (x45_0, x45_1, x45_2, x45_3, x45_4), (x46_0, x46_1, x46_2, x46_3, x46_4), (x47_0, x47_1, x47_2, x47_3, x47_4), (x48_0, x48_1, x48_2, x48_3, x48_4), (x49_0, x49_1, x49_2, x49_3, x49_4), (x50_0, x50_1, x50_2, x50_3, x50_4), (x51_0, x51_1, x51_2, x51_3, x51_4), (x52_0, x52_1, x52_2, x52_3, x52_4), (x53_0, x53_1, x53_2, x53_3, x53_4), (x54_0, x54_1, x54_2, x54_3, x54_4), (x55_0, x55_1, x55_2, x55_3, x55_4), (x56_0, x56_1, x56_2, x56_3, x56_4), (x57_0, x57_1, x57_2, x57_3, x57_4), (x58_0, x58_1, x58_2, x58_3, x58_4), (x59_0, x59_1, x59_2, x59_3, x59_4), (x60_0, x60_1, x60_2, x60_3, x60_4), (x61_0, x61_1, x61_2, x61_3, x61_4), (x62_0, x62_1, x62_2, x62_3, x62_4), (x63_0, x63_1, x63_2, x63_3, x63_4))
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance (Broadcast X64 a0, Broadcast X64 a1, Broadcast X64 a2, Broadcast X64 a3, Broadcast X64 a4) => Broadcast X64 (a0, a1, a2, a3, a4) where
  broadcast (x0, x1, x2, x3, x4) = MkTuple5X64 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4)
  {-# INLINE broadcast #-}
instance (SelectableF X64 a0, SelectableF X64 a1, SelectableF X64 a2, SelectableF X64 a3, SelectableF X64 a4) => SelectableF X64 (a0, a1, a2, a3, a4) where
  selectF !cond (MkTuple5X64 x0 x1 x2 x3 x4) (MkTuple5X64 y0 y1 y2 y3 y4) = MkTuple5X64 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X64 a0, UnaryShuffleT indices X64 a1, UnaryShuffleT indices X64 a2, UnaryShuffleT indices X64 a3, UnaryShuffleT indices X64 a4) => UnaryShuffleT indices X64 (a0, a1, a2, a3, a4) where
  unaryShuffle (MkTuple5X64 x0 x1 x2 x3 x4) = MkTuple5X64 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X64 a0, BinaryShuffleT indices X64 a1, BinaryShuffleT indices X64 a2, BinaryShuffleT indices X64 a3, BinaryShuffleT indices X64 a4) => BinaryShuffleT indices X64 (a0, a1, a2, a3, a4) where
  binaryShuffle (MkTuple5X64 x0 x1 x2 x3 x4) (MkTuple5X64 y0 y1 y2 y3 y4) = MkTuple5X64 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4)
  {-# INLINE binaryShuffle #-}
data instance X64 (a0, a1, a2, a3, a4, a5) = MkTuple6X64 !(X64 a0) !(X64 a1) !(X64 a2) !(X64 a3) !(X64 a4) !(X64 a5)
instance (PackX64 X64 a0, PackX64 X64 a1, PackX64 X64 a2, PackX64 X64 a3, PackX64 X64 a4, PackX64 X64 a5) => PackX64 X64 (a0, a1, a2, a3, a4, a5) where
  mkX64 (x0_0, x0_1, x0_2, x0_3, x0_4, x0_5) (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5) (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5) (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5) (x4_0, x4_1, x4_2, x4_3, x4_4, x4_5) (x5_0, x5_1, x5_2, x5_3, x5_4, x5_5) (x6_0, x6_1, x6_2, x6_3, x6_4, x6_5) (x7_0, x7_1, x7_2, x7_3, x7_4, x7_5) (x8_0, x8_1, x8_2, x8_3, x8_4, x8_5) (x9_0, x9_1, x9_2, x9_3, x9_4, x9_5) (x10_0, x10_1, x10_2, x10_3, x10_4, x10_5) (x11_0, x11_1, x11_2, x11_3, x11_4, x11_5) (x12_0, x12_1, x12_2, x12_3, x12_4, x12_5) (x13_0, x13_1, x13_2, x13_3, x13_4, x13_5) (x14_0, x14_1, x14_2, x14_3, x14_4, x14_5) (x15_0, x15_1, x15_2, x15_3, x15_4, x15_5) (x16_0, x16_1, x16_2, x16_3, x16_4, x16_5) (x17_0, x17_1, x17_2, x17_3, x17_4, x17_5) (x18_0, x18_1, x18_2, x18_3, x18_4, x18_5) (x19_0, x19_1, x19_2, x19_3, x19_4, x19_5) (x20_0, x20_1, x20_2, x20_3, x20_4, x20_5) (x21_0, x21_1, x21_2, x21_3, x21_4, x21_5) (x22_0, x22_1, x22_2, x22_3, x22_4, x22_5) (x23_0, x23_1, x23_2, x23_3, x23_4, x23_5) (x24_0, x24_1, x24_2, x24_3, x24_4, x24_5) (x25_0, x25_1, x25_2, x25_3, x25_4, x25_5) (x26_0, x26_1, x26_2, x26_3, x26_4, x26_5) (x27_0, x27_1, x27_2, x27_3, x27_4, x27_5) (x28_0, x28_1, x28_2, x28_3, x28_4, x28_5) (x29_0, x29_1, x29_2, x29_3, x29_4, x29_5) (x30_0, x30_1, x30_2, x30_3, x30_4, x30_5) (x31_0, x31_1, x31_2, x31_3, x31_4, x31_5) (x32_0, x32_1, x32_2, x32_3, x32_4, x32_5) (x33_0, x33_1, x33_2, x33_3, x33_4, x33_5) (x34_0, x34_1, x34_2, x34_3, x34_4, x34_5) (x35_0, x35_1, x35_2, x35_3, x35_4, x35_5) (x36_0, x36_1, x36_2, x36_3, x36_4, x36_5) (x37_0, x37_1, x37_2, x37_3, x37_4, x37_5) (x38_0, x38_1, x38_2, x38_3, x38_4, x38_5) (x39_0, x39_1, x39_2, x39_3, x39_4, x39_5) (x40_0, x40_1, x40_2, x40_3, x40_4, x40_5) (x41_0, x41_1, x41_2, x41_3, x41_4, x41_5) (x42_0, x42_1, x42_2, x42_3, x42_4, x42_5) (x43_0, x43_1, x43_2, x43_3, x43_4, x43_5) (x44_0, x44_1, x44_2, x44_3, x44_4, x44_5) (x45_0, x45_1, x45_2, x45_3, x45_4, x45_5) (x46_0, x46_1, x46_2, x46_3, x46_4, x46_5) (x47_0, x47_1, x47_2, x47_3, x47_4, x47_5) (x48_0, x48_1, x48_2, x48_3, x48_4, x48_5) (x49_0, x49_1, x49_2, x49_3, x49_4, x49_5) (x50_0, x50_1, x50_2, x50_3, x50_4, x50_5) (x51_0, x51_1, x51_2, x51_3, x51_4, x51_5) (x52_0, x52_1, x52_2, x52_3, x52_4, x52_5) (x53_0, x53_1, x53_2, x53_3, x53_4, x53_5) (x54_0, x54_1, x54_2, x54_3, x54_4, x54_5) (x55_0, x55_1, x55_2, x55_3, x55_4, x55_5) (x56_0, x56_1, x56_2, x56_3, x56_4, x56_5) (x57_0, x57_1, x57_2, x57_3, x57_4, x57_5) (x58_0, x58_1, x58_2, x58_3, x58_4, x58_5) (x59_0, x59_1, x59_2, x59_3, x59_4, x59_5) (x60_0, x60_1, x60_2, x60_3, x60_4, x60_5) (x61_0, x61_1, x61_2, x61_3, x61_4, x61_5) (x62_0, x62_1, x62_2, x62_3, x62_4, x62_5) (x63_0, x63_1, x63_2, x63_3, x63_4, x63_5) = MkTuple6X64 (mkX64 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0 x8_0 x9_0 x10_0 x11_0 x12_0 x13_0 x14_0 x15_0 x16_0 x17_0 x18_0 x19_0 x20_0 x21_0 x22_0 x23_0 x24_0 x25_0 x26_0 x27_0 x28_0 x29_0 x30_0 x31_0 x32_0 x33_0 x34_0 x35_0 x36_0 x37_0 x38_0 x39_0 x40_0 x41_0 x42_0 x43_0 x44_0 x45_0 x46_0 x47_0 x48_0 x49_0 x50_0 x51_0 x52_0 x53_0 x54_0 x55_0 x56_0 x57_0 x58_0 x59_0 x60_0 x61_0 x62_0 x63_0) (mkX64 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1 x8_1 x9_1 x10_1 x11_1 x12_1 x13_1 x14_1 x15_1 x16_1 x17_1 x18_1 x19_1 x20_1 x21_1 x22_1 x23_1 x24_1 x25_1 x26_1 x27_1 x28_1 x29_1 x30_1 x31_1 x32_1 x33_1 x34_1 x35_1 x36_1 x37_1 x38_1 x39_1 x40_1 x41_1 x42_1 x43_1 x44_1 x45_1 x46_1 x47_1 x48_1 x49_1 x50_1 x51_1 x52_1 x53_1 x54_1 x55_1 x56_1 x57_1 x58_1 x59_1 x60_1 x61_1 x62_1 x63_1) (mkX64 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2 x8_2 x9_2 x10_2 x11_2 x12_2 x13_2 x14_2 x15_2 x16_2 x17_2 x18_2 x19_2 x20_2 x21_2 x22_2 x23_2 x24_2 x25_2 x26_2 x27_2 x28_2 x29_2 x30_2 x31_2 x32_2 x33_2 x34_2 x35_2 x36_2 x37_2 x38_2 x39_2 x40_2 x41_2 x42_2 x43_2 x44_2 x45_2 x46_2 x47_2 x48_2 x49_2 x50_2 x51_2 x52_2 x53_2 x54_2 x55_2 x56_2 x57_2 x58_2 x59_2 x60_2 x61_2 x62_2 x63_2) (mkX64 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3 x8_3 x9_3 x10_3 x11_3 x12_3 x13_3 x14_3 x15_3 x16_3 x17_3 x18_3 x19_3 x20_3 x21_3 x22_3 x23_3 x24_3 x25_3 x26_3 x27_3 x28_3 x29_3 x30_3 x31_3 x32_3 x33_3 x34_3 x35_3 x36_3 x37_3 x38_3 x39_3 x40_3 x41_3 x42_3 x43_3 x44_3 x45_3 x46_3 x47_3 x48_3 x49_3 x50_3 x51_3 x52_3 x53_3 x54_3 x55_3 x56_3 x57_3 x58_3 x59_3 x60_3 x61_3 x62_3 x63_3) (mkX64 x0_4 x1_4 x2_4 x3_4 x4_4 x5_4 x6_4 x7_4 x8_4 x9_4 x10_4 x11_4 x12_4 x13_4 x14_4 x15_4 x16_4 x17_4 x18_4 x19_4 x20_4 x21_4 x22_4 x23_4 x24_4 x25_4 x26_4 x27_4 x28_4 x29_4 x30_4 x31_4 x32_4 x33_4 x34_4 x35_4 x36_4 x37_4 x38_4 x39_4 x40_4 x41_4 x42_4 x43_4 x44_4 x45_4 x46_4 x47_4 x48_4 x49_4 x50_4 x51_4 x52_4 x53_4 x54_4 x55_4 x56_4 x57_4 x58_4 x59_4 x60_4 x61_4 x62_4 x63_4) (mkX64 x0_5 x1_5 x2_5 x3_5 x4_5 x5_5 x6_5 x7_5 x8_5 x9_5 x10_5 x11_5 x12_5 x13_5 x14_5 x15_5 x16_5 x17_5 x18_5 x19_5 x20_5 x21_5 x22_5 x23_5 x24_5 x25_5 x26_5 x27_5 x28_5 x29_5 x30_5 x31_5 x32_5 x33_5 x34_5 x35_5 x36_5 x37_5 x38_5 x39_5 x40_5 x41_5 x42_5 x43_5 x44_5 x45_5 x46_5 x47_5 x48_5 x49_5 x50_5 x51_5 x52_5 x53_5 x54_5 x55_5 x56_5 x57_5 x58_5 x59_5 x60_5 x61_5 x62_5 x63_5)
  unpackX64 (MkTuple6X64 v0 v1 v2 v3 v4 v5) = case unpackX64 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0, x8_0, x9_0, x10_0, x11_0, x12_0, x13_0, x14_0, x15_0, x16_0, x17_0, x18_0, x19_0, x20_0, x21_0, x22_0, x23_0, x24_0, x25_0, x26_0, x27_0, x28_0, x29_0, x30_0, x31_0, x32_0, x33_0, x34_0, x35_0, x36_0, x37_0, x38_0, x39_0, x40_0, x41_0, x42_0, x43_0, x44_0, x45_0, x46_0, x47_0, x48_0, x49_0, x50_0, x51_0, x52_0, x53_0, x54_0, x55_0, x56_0, x57_0, x58_0, x59_0, x60_0, x61_0, x62_0, x63_0) -> case unpackX64 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1, x8_1, x9_1, x10_1, x11_1, x12_1, x13_1, x14_1, x15_1, x16_1, x17_1, x18_1, x19_1, x20_1, x21_1, x22_1, x23_1, x24_1, x25_1, x26_1, x27_1, x28_1, x29_1, x30_1, x31_1, x32_1, x33_1, x34_1, x35_1, x36_1, x37_1, x38_1, x39_1, x40_1, x41_1, x42_1, x43_1, x44_1, x45_1, x46_1, x47_1, x48_1, x49_1, x50_1, x51_1, x52_1, x53_1, x54_1, x55_1, x56_1, x57_1, x58_1, x59_1, x60_1, x61_1, x62_1, x63_1) -> case unpackX64 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2, x8_2, x9_2, x10_2, x11_2, x12_2, x13_2, x14_2, x15_2, x16_2, x17_2, x18_2, x19_2, x20_2, x21_2, x22_2, x23_2, x24_2, x25_2, x26_2, x27_2, x28_2, x29_2, x30_2, x31_2, x32_2, x33_2, x34_2, x35_2, x36_2, x37_2, x38_2, x39_2, x40_2, x41_2, x42_2, x43_2, x44_2, x45_2, x46_2, x47_2, x48_2, x49_2, x50_2, x51_2, x52_2, x53_2, x54_2, x55_2, x56_2, x57_2, x58_2, x59_2, x60_2, x61_2, x62_2, x63_2) -> case unpackX64 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3, x8_3, x9_3, x10_3, x11_3, x12_3, x13_3, x14_3, x15_3, x16_3, x17_3, x18_3, x19_3, x20_3, x21_3, x22_3, x23_3, x24_3, x25_3, x26_3, x27_3, x28_3, x29_3, x30_3, x31_3, x32_3, x33_3, x34_3, x35_3, x36_3, x37_3, x38_3, x39_3, x40_3, x41_3, x42_3, x43_3, x44_3, x45_3, x46_3, x47_3, x48_3, x49_3, x50_3, x51_3, x52_3, x53_3, x54_3, x55_3, x56_3, x57_3, x58_3, x59_3, x60_3, x61_3, x62_3, x63_3) -> case unpackX64 v4 of (x0_4, x1_4, x2_4, x3_4, x4_4, x5_4, x6_4, x7_4, x8_4, x9_4, x10_4, x11_4, x12_4, x13_4, x14_4, x15_4, x16_4, x17_4, x18_4, x19_4, x20_4, x21_4, x22_4, x23_4, x24_4, x25_4, x26_4, x27_4, x28_4, x29_4, x30_4, x31_4, x32_4, x33_4, x34_4, x35_4, x36_4, x37_4, x38_4, x39_4, x40_4, x41_4, x42_4, x43_4, x44_4, x45_4, x46_4, x47_4, x48_4, x49_4, x50_4, x51_4, x52_4, x53_4, x54_4, x55_4, x56_4, x57_4, x58_4, x59_4, x60_4, x61_4, x62_4, x63_4) -> case unpackX64 v5 of (x0_5, x1_5, x2_5, x3_5, x4_5, x5_5, x6_5, x7_5, x8_5, x9_5, x10_5, x11_5, x12_5, x13_5, x14_5, x15_5, x16_5, x17_5, x18_5, x19_5, x20_5, x21_5, x22_5, x23_5, x24_5, x25_5, x26_5, x27_5, x28_5, x29_5, x30_5, x31_5, x32_5, x33_5, x34_5, x35_5, x36_5, x37_5, x38_5, x39_5, x40_5, x41_5, x42_5, x43_5, x44_5, x45_5, x46_5, x47_5, x48_5, x49_5, x50_5, x51_5, x52_5, x53_5, x54_5, x55_5, x56_5, x57_5, x58_5, x59_5, x60_5, x61_5, x62_5, x63_5) -> ((x0_0, x0_1, x0_2, x0_3, x0_4, x0_5), (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5), (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5), (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5), (x4_0, x4_1, x4_2, x4_3, x4_4, x4_5), (x5_0, x5_1, x5_2, x5_3, x5_4, x5_5), (x6_0, x6_1, x6_2, x6_3, x6_4, x6_5), (x7_0, x7_1, x7_2, x7_3, x7_4, x7_5), (x8_0, x8_1, x8_2, x8_3, x8_4, x8_5), (x9_0, x9_1, x9_2, x9_3, x9_4, x9_5), (x10_0, x10_1, x10_2, x10_3, x10_4, x10_5), (x11_0, x11_1, x11_2, x11_3, x11_4, x11_5), (x12_0, x12_1, x12_2, x12_3, x12_4, x12_5), (x13_0, x13_1, x13_2, x13_3, x13_4, x13_5), (x14_0, x14_1, x14_2, x14_3, x14_4, x14_5), (x15_0, x15_1, x15_2, x15_3, x15_4, x15_5), (x16_0, x16_1, x16_2, x16_3, x16_4, x16_5), (x17_0, x17_1, x17_2, x17_3, x17_4, x17_5), (x18_0, x18_1, x18_2, x18_3, x18_4, x18_5), (x19_0, x19_1, x19_2, x19_3, x19_4, x19_5), (x20_0, x20_1, x20_2, x20_3, x20_4, x20_5), (x21_0, x21_1, x21_2, x21_3, x21_4, x21_5), (x22_0, x22_1, x22_2, x22_3, x22_4, x22_5), (x23_0, x23_1, x23_2, x23_3, x23_4, x23_5), (x24_0, x24_1, x24_2, x24_3, x24_4, x24_5), (x25_0, x25_1, x25_2, x25_3, x25_4, x25_5), (x26_0, x26_1, x26_2, x26_3, x26_4, x26_5), (x27_0, x27_1, x27_2, x27_3, x27_4, x27_5), (x28_0, x28_1, x28_2, x28_3, x28_4, x28_5), (x29_0, x29_1, x29_2, x29_3, x29_4, x29_5), (x30_0, x30_1, x30_2, x30_3, x30_4, x30_5), (x31_0, x31_1, x31_2, x31_3, x31_4, x31_5), (x32_0, x32_1, x32_2, x32_3, x32_4, x32_5), (x33_0, x33_1, x33_2, x33_3, x33_4, x33_5), (x34_0, x34_1, x34_2, x34_3, x34_4, x34_5), (x35_0, x35_1, x35_2, x35_3, x35_4, x35_5), (x36_0, x36_1, x36_2, x36_3, x36_4, x36_5), (x37_0, x37_1, x37_2, x37_3, x37_4, x37_5), (x38_0, x38_1, x38_2, x38_3, x38_4, x38_5), (x39_0, x39_1, x39_2, x39_3, x39_4, x39_5), (x40_0, x40_1, x40_2, x40_3, x40_4, x40_5), (x41_0, x41_1, x41_2, x41_3, x41_4, x41_5), (x42_0, x42_1, x42_2, x42_3, x42_4, x42_5), (x43_0, x43_1, x43_2, x43_3, x43_4, x43_5), (x44_0, x44_1, x44_2, x44_3, x44_4, x44_5), (x45_0, x45_1, x45_2, x45_3, x45_4, x45_5), (x46_0, x46_1, x46_2, x46_3, x46_4, x46_5), (x47_0, x47_1, x47_2, x47_3, x47_4, x47_5), (x48_0, x48_1, x48_2, x48_3, x48_4, x48_5), (x49_0, x49_1, x49_2, x49_3, x49_4, x49_5), (x50_0, x50_1, x50_2, x50_3, x50_4, x50_5), (x51_0, x51_1, x51_2, x51_3, x51_4, x51_5), (x52_0, x52_1, x52_2, x52_3, x52_4, x52_5), (x53_0, x53_1, x53_2, x53_3, x53_4, x53_5), (x54_0, x54_1, x54_2, x54_3, x54_4, x54_5), (x55_0, x55_1, x55_2, x55_3, x55_4, x55_5), (x56_0, x56_1, x56_2, x56_3, x56_4, x56_5), (x57_0, x57_1, x57_2, x57_3, x57_4, x57_5), (x58_0, x58_1, x58_2, x58_3, x58_4, x58_5), (x59_0, x59_1, x59_2, x59_3, x59_4, x59_5), (x60_0, x60_1, x60_2, x60_3, x60_4, x60_5), (x61_0, x61_1, x61_2, x61_3, x61_4, x61_5), (x62_0, x62_1, x62_2, x62_3, x62_4, x62_5), (x63_0, x63_1, x63_2, x63_3, x63_4, x63_5))
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance (Broadcast X64 a0, Broadcast X64 a1, Broadcast X64 a2, Broadcast X64 a3, Broadcast X64 a4, Broadcast X64 a5) => Broadcast X64 (a0, a1, a2, a3, a4, a5) where
  broadcast (x0, x1, x2, x3, x4, x5) = MkTuple6X64 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4) (broadcast x5)
  {-# INLINE broadcast #-}
instance (SelectableF X64 a0, SelectableF X64 a1, SelectableF X64 a2, SelectableF X64 a3, SelectableF X64 a4, SelectableF X64 a5) => SelectableF X64 (a0, a1, a2, a3, a4, a5) where
  selectF !cond (MkTuple6X64 x0 x1 x2 x3 x4 x5) (MkTuple6X64 y0 y1 y2 y3 y4 y5) = MkTuple6X64 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4) (selectF cond x5 y5)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X64 a0, UnaryShuffleT indices X64 a1, UnaryShuffleT indices X64 a2, UnaryShuffleT indices X64 a3, UnaryShuffleT indices X64 a4, UnaryShuffleT indices X64 a5) => UnaryShuffleT indices X64 (a0, a1, a2, a3, a4, a5) where
  unaryShuffle (MkTuple6X64 x0 x1 x2 x3 x4 x5) = MkTuple6X64 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4) (unaryShuffle @indices x5)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X64 a0, BinaryShuffleT indices X64 a1, BinaryShuffleT indices X64 a2, BinaryShuffleT indices X64 a3, BinaryShuffleT indices X64 a4, BinaryShuffleT indices X64 a5) => BinaryShuffleT indices X64 (a0, a1, a2, a3, a4, a5) where
  binaryShuffle (MkTuple6X64 x0 x1 x2 x3 x4 x5) (MkTuple6X64 y0 y1 y2 y3 y4 y5) = MkTuple6X64 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4) (binaryShuffle @indices x5 y5)
  {-# INLINE binaryShuffle #-}
instance (PackX64 X64 a, PackX64 X64 b) => LiftSIMD X64 a b where
  liftSIMD f !v = case unpackX64 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) -> mkX64 (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9) (f x10) (f x11) (f x12) (f x13) (f x14) (f x15) (f x16) (f x17) (f x18) (f x19) (f x20) (f x21) (f x22) (f x23) (f x24) (f x25) (f x26) (f x27) (f x28) (f x29) (f x30) (f x31) (f x32) (f x33) (f x34) (f x35) (f x36) (f x37) (f x38) (f x39) (f x40) (f x41) (f x42) (f x43) (f x44) (f x45) (f x46) (f x47) (f x48) (f x49) (f x50) (f x51) (f x52) (f x53) (f x54) (f x55) (f x56) (f x57) (f x58) (f x59) (f x60) (f x61) (f x62) (f x63)
  {-# INLINE liftSIMD #-}
instance (PackX64 X64 a, PackX64 X64 b, PackX64 X64 c) => LiftSIMD2 X64 a b c where
  liftSIMD2 f !u !v = case unpackX64 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) -> case unpackX64 v of (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29, y30, y31, y32, y33, y34, y35, y36, y37, y38, y39, y40, y41, y42, y43, y44, y45, y46, y47, y48, y49, y50, y51, y52, y53, y54, y55, y56, y57, y58, y59, y60, y61, y62, y63) -> mkX64 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4) (f x5 y5) (f x6 y6) (f x7 y7) (f x8 y8) (f x9 y9) (f x10 y10) (f x11 y11) (f x12 y12) (f x13 y13) (f x14 y14) (f x15 y15) (f x16 y16) (f x17 y17) (f x18 y18) (f x19 y19) (f x20 y20) (f x21 y21) (f x22 y22) (f x23 y23) (f x24 y24) (f x25 y25) (f x26 y26) (f x27 y27) (f x28 y28) (f x29 y29) (f x30 y30) (f x31 y31) (f x32 y32) (f x33 y33) (f x34 y34) (f x35 y35) (f x36 y36) (f x37 y37) (f x38 y38) (f x39 y39) (f x40 y40) (f x41 y41) (f x42 y42) (f x43 y43) (f x44 y44) (f x45 y45) (f x46 y46) (f x47 y47) (f x48 y48) (f x49 y49) (f x50 y50) (f x51 y51) (f x52 y52) (f x53 y53) (f x54 y54) (f x55 y55) (f x56 y56) (f x57 y57) (f x58 y58) (f x59 y59) (f x60 y60) (f x61 y61) (f x62 y62) (f x63 y63)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X64 where
  mkTuple2 = MkTuple2X64
  mkTuple3 = MkTuple3X64
  mkTuple4 = MkTuple4X64
  mkTuple5 = MkTuple5X64
  mkTuple6 = MkTuple6X64
  deconstructTuple2 (MkTuple2X64 v0 v1) = (v0, v1)
  deconstructTuple3 (MkTuple3X64 v0 v1 v2) = (v0, v1, v2)
  deconstructTuple4 (MkTuple4X64 v0 v1 v2 v3) = (v0, v1, v2, v3)
  deconstructTuple5 (MkTuple5X64 v0 v1 v2 v3 v4) = (v0, v1, v2, v3, v4)
  deconstructTuple6 (MkTuple6X64 v0 v1 v2 v3 v4 v5) = (v0, v1, v2, v3, v4, v5)
  mkSum = coerce
  getSum' = coerce
  mkProduct = coerce
  getProduct' = coerce
  mkMin = coerce
  getMin' = coerce
  mkMax = coerce
  getMax' = coerce
  mkComplex = MkComplexX64
  deconstructComplex (MkComplexX64 x y) = (x, y)
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
deriving via WrappedMulti X64 a instance SelectableF X64 a => Selectable (X64 a)
deriving via WrappedMulti X64 a instance NumF X64 a => Num (X64 a)
deriving via WrappedMulti X64 a instance FractionalF X64 a => Fractional (X64 a)
deriving via WrappedMulti X64 a instance FloatingF X64 a => Floating (X64 a)
deriving via WrappedMulti X64 a instance BooleanF X64 a => Boolean (X64 a)
deriving via WrappedMulti X64 a instance BitShiftF X64 a => BitShift (X64 a)
deriving via WrappedMulti X64 a instance MinMaxF X64 a => MinMax (X64 a)
deriving via WrappedMulti X64 a instance FusedMultiplyAddF X64 a => FusedMultiplyAdd (X64 a)
