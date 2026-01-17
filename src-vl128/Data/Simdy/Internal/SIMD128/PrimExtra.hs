{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Simdy.Internal.SIMD128.PrimExtra where
import Data.Word
import GHC.Exts

-- GHC bug: https://gitlab.haskell.org/ghc/ghc/-/issues/25730
#if 0
foreign import ccall unsafe "hs_simdy_pack_mask8x16"
  packMask8X16 :: Int8X16# -> Word16

foreign import ccall unsafe "hs_simdy_pack_mask16x8"
  packMask16X8 :: Int16X8# -> Word8

foreign import ccall unsafe "hs_simdy_pack_mask32x4"
  packMask32X4 :: Int32X4# -> Word8

foreign import ccall unsafe "hs_simdy_pack_mask64x2"
  packMask64X2 :: Int64X2# -> Word8

foreign import ccall unsafe "hs_simdy_mask16x8x2_to_mask8x16"
  mask16X8X2ToMask8X16 :: Int16X8# -> Int16X8# -> Int8X16#

foreign import ccall unsafe "hs_simdy_mask32x4x2_to_mask16x8"
  mask32X4X2ToMask16X8 :: Int32X4# -> Int32X4# -> Int16X8#

foreign import ccall unsafe "hs_simdy_mask64x2x2_to_mask32x4"
  mask64X2X2ToMask32X4 :: Int64X2# -> Int64X2# -> Int32X4#

foreign import ccall unsafe "hs_simdy_unpack_mask8x16"
  unpackMask8X16 :: Word16 -> Int8X16#

foreign import ccall unsafe "hs_simdy_unpack_mask16x8"
  unpackMask16X8 :: Word8 -> Int16X8#

foreign import ccall unsafe "hs_simdy_unpack_mask32x4"
  unpackMask32X4 :: Word8 -> Int32X4#

foreign import ccall unsafe "hs_simdy_unpack_mask64x2"
  unpackMask64X2 :: Word8 -> Int64X2#

foreign import ccall unsafe "hs_simdy_select_int128"
  selectInt8X16Mask# :: Int8X16# -> Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_select_int128"
  selectInt16X8Mask# :: Int16X8# -> Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_select_int128"
  selectInt32X4Mask# :: Int32X4# -> Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_select_int128"
  selectInt64X2Mask# :: Int64X2# -> Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_select_int128"
  selectWord8X16Mask# :: Int8X16# -> Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_select_int128"
  selectWord16X8Mask# :: Int16X8# -> Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_select_int128"
  selectWord32X4Mask# :: Int32X4# -> Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_select_int128"
  selectWord64X2Mask# :: Int64X2# -> Word64X2# -> Word64X2# -> Word64X2#

foreign import ccall unsafe "hs_simdy_select_floatx4"
  selectFloatX4Mask# :: Int32X4# -> FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_select_doublex2"
  selectDoubleX2Mask# :: Int64X2# -> DoubleX2# -> DoubleX2# -> DoubleX2#

selectInt8X16# :: Word16 -> Int8X16# -> Int8X16# -> Int8X16#
selectInt8X16# !x = selectInt8X16Mask# (unpackMask8X16 x)
{-# INLINE selectInt8X16# #-}

selectInt16X8# :: Word8 -> Int16X8# -> Int16X8# -> Int16X8#
selectInt16X8# !x = selectInt16X8Mask# (unpackMask16X8 x)
{-# INLINE selectInt16X8# #-}

selectInt32X4# :: Word8 -> Int32X4# -> Int32X4# -> Int32X4#
selectInt32X4# !x = selectInt32X4Mask# (unpackMask32X4 x)
{-# INLINE selectInt32X4# #-}

selectInt64X2# :: Word8 -> Int64X2# -> Int64X2# -> Int64X2#
selectInt64X2# !x = selectInt64X2Mask# (unpackMask64X2 x)
{-# INLINE selectInt64X2# #-}

selectWord8X16# :: Word16 -> Word8X16# -> Word8X16# -> Word8X16#
selectWord8X16# !x = selectWord8X16Mask# (unpackMask8X16 x)
{-# INLINE selectWord8X16# #-}

selectWord16X8# :: Word8 -> Word16X8# -> Word16X8# -> Word16X8#
selectWord16X8# !x = selectWord16X8Mask# (unpackMask16X8 x)
{-# INLINE selectWord16X8# #-}

selectWord32X4# :: Word8 -> Word32X4# -> Word32X4# -> Word32X4#
selectWord32X4# !x = selectWord32X4Mask# (unpackMask32X4 x)
{-# INLINE selectWord32X4# #-}

selectWord64X2# :: Word8 -> Word64X2# -> Word64X2# -> Word64X2#
selectWord64X2# !x = selectWord64X2Mask# (unpackMask64X2 x)
{-# INLINE selectWord64X2# #-}

selectFloatX4# :: Word8 -> FloatX4# -> FloatX4# -> FloatX4#
selectFloatX4# !x = selectFloatX4Mask# (unpackMask32X4 x)
{-# INLINE selectFloatX4# #-}

selectDoubleX2# :: Word8 -> DoubleX2# -> DoubleX2# -> DoubleX2#
selectDoubleX2# !x = selectDoubleX2Mask# (unpackMask64X2 x)
{-# INLINE selectDoubleX2# #-}
#endif

foreign import ccall unsafe "hs_simdy_select_int8x16_densemask"
  selectInt8X16# :: Word16 -> Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_select_int16x8_densemask"
  selectInt16X8# :: Word8 -> Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_select_int32x4_densemask"
  selectInt32X4# :: Word8 -> Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_select_int64x2_densemask"
  selectInt64X2# :: Word8 -> Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_select_int8x16_densemask"
  selectWord8X16# :: Word16 -> Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_select_int16x8_densemask"
  selectWord16X8# :: Word8 -> Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_select_int32x4_densemask"
  selectWord32X4# :: Word8 -> Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_select_int64x2_densemask"
  selectWord64X2# :: Word8 -> Word64X2# -> Word64X2# -> Word64X2#

foreign import ccall unsafe "hs_simdy_select_floatx4_densemask"
  selectFloatX4# :: Word8 -> FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_select_doublex2_densemask"
  selectDoubleX2# :: Word8 -> DoubleX2# -> DoubleX2# -> DoubleX2#

#if defined(x86_64_HOST_ARCH)

--
-- Complement (bitwise not)
--

foreign import ccall unsafe "hs_simdy_complement_int128"
  complementInt8X16# :: Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_complement_int128"
  complementInt16X8# :: Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_complement_int128"
  complementInt32X4# :: Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_complement_int128"
  complementInt64X2# :: Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_complement_int128"
  complementWord8X16# :: Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_complement_int128"
  complementWord16X8# :: Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_complement_int128"
  complementWord32X4# :: Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_complement_int128"
  complementWord64X2# :: Word64X2# -> Word64X2#

--
-- Bitwise AND
--

foreign import ccall unsafe "hs_simdy_and_int128"
  andInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_and_int128"
  andInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_and_int128"
  andInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_and_int128"
  andInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_and_int128"
  andWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_and_int128"
  andWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_and_int128"
  andWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_and_int128"
  andWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#

--
-- Bitwise OR
--

foreign import ccall unsafe "hs_simdy_or_int128"
  orInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_or_int128"
  orInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_or_int128"
  orInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_or_int128"
  orInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_or_int128"
  orWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_or_int128"
  orWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_or_int128"
  orWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_or_int128"
  orWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#

--
-- Bitwise XOR
--

foreign import ccall unsafe "hs_simdy_xor_int128"
  xorInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_xor_int128"
  xorInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_xor_int128"
  xorInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_xor_int128"
  xorInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_xor_int128"
  xorWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_xor_int128"
  xorWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_xor_int128"
  xorWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_xor_int128"
  xorWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#

#else

--
-- Complement (bitwise not)
--

foreign import ccall unsafe "hs_simdy_complement_int8x16"
  complementInt8X16# :: Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_complement_int16x8"
  complementInt16X8# :: Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_complement_int32x4"
  complementInt32X4# :: Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_complement_int64x2"
  complementInt64X2# :: Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_complement_word8x16"
  complementWord8X16# :: Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_complement_word16x8"
  complementWord16X8# :: Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_complement_word32x4"
  complementWord32X4# :: Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_complement_word64x2"
  complementWord64X2# :: Word64X2# -> Word64X2#

--
-- Bitwise AND
--

foreign import ccall unsafe "hs_simdy_and_int8x16"
  andInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_and_int16x8"
  andInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_and_int32x4"
  andInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_and_int64x2"
  andInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_and_word8x16"
  andWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_and_word16x8"
  andWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_and_word32x4"
  andWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_and_word64x2"
  andWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#

--
-- Bitwise OR
--

foreign import ccall unsafe "hs_simdy_or_int8x16"
  orInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_or_int16x8"
  orInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_or_int32x4"
  orInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_or_int64x2"
  orInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_or_word8x16"
  orWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_or_word16x8"
  orWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_or_word32x4"
  orWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_or_word64x2"
  orWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#

--
-- Bitwise XOR
--

foreign import ccall unsafe "hs_simdy_xor_int8x16"
  xorInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_xor_int16x8"
  xorInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_xor_int32x4"
  xorInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_xor_int64x2"
  xorInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_xor_word8x16"
  xorWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_xor_word16x8"
  xorWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_xor_word32x4"
  xorWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_xor_word64x2"
  xorWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#

#endif

--
-- Left Shift
--

foreign import ccall unsafe "hs_simdy_shiftL_int8x16"
  shiftLInt8X16# :: Int8X16# -> Int# -> Int8X16#

foreign import ccall unsafe "hs_simdy_shiftL_int16x8"
  shiftLInt16X8# :: Int16X8# -> Int# -> Int16X8#

foreign import ccall unsafe "hs_simdy_shiftL_int32x4"
  shiftLInt32X4# :: Int32X4# -> Int# -> Int32X4#

foreign import ccall unsafe "hs_simdy_shiftL_int64x2"
  shiftLInt64X2# :: Int64X2# -> Int# -> Int64X2#

foreign import ccall unsafe "hs_simdy_shiftL_word8x16"
  shiftLWord8X16# :: Word8X16# -> Int# -> Word8X16#

foreign import ccall unsafe "hs_simdy_shiftL_word16x8"
  shiftLWord16X8# :: Word16X8# -> Int# -> Word16X8#

foreign import ccall unsafe "hs_simdy_shiftL_word32x4"
  shiftLWord32X4# :: Word32X4# -> Int# -> Word32X4#

foreign import ccall unsafe "hs_simdy_shiftL_word64x2"
  shiftLWord64X2# :: Word64X2# -> Int# -> Word64X2#

--
-- Right Shift (Arithmetic / Logical)
--

foreign import ccall unsafe "hs_simdy_shiftR_int8x16"
  shiftRInt8X16# :: Int8X16# -> Int# -> Int8X16#

foreign import ccall unsafe "hs_simdy_shiftR_int16x8"
  shiftRInt16X8# :: Int16X8# -> Int# -> Int16X8#

foreign import ccall unsafe "hs_simdy_shiftR_int32x4"
  shiftRInt32X4# :: Int32X4# -> Int# -> Int32X4#

foreign import ccall unsafe "hs_simdy_shiftR_int64x2"
  shiftRInt64X2# :: Int64X2# -> Int# -> Int64X2#

foreign import ccall unsafe "hs_simdy_shiftR_word8x16"
  shiftRWord8X16# :: Word8X16# -> Int# -> Word8X16#

foreign import ccall unsafe "hs_simdy_shiftR_word16x8"
  shiftRWord16X8# :: Word16X8# -> Int# -> Word16X8#

foreign import ccall unsafe "hs_simdy_shiftR_word32x4"
  shiftRWord32X4# :: Word32X4# -> Int# -> Word32X4#

foreign import ccall unsafe "hs_simdy_shiftR_word64x2"
  shiftRWord64X2# :: Word64X2# -> Int# -> Word64X2#

#if !MIN_VERSION_ghc_prim(0, 13, 0)
--
-- Integer minimum/maximum
--

foreign import ccall unsafe "hs_simdy_minInt8X16"
  minInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_minInt16X8"
  minInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_minInt32X4"
  minInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_minInt64X2"
  minInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_minWord8X16"
  minWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_minWord16X8"
  minWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_minWord32X4"
  minWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_minWord64X2"
  minWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#

foreign import ccall unsafe "hs_simdy_maxInt8X16"
  maxInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_maxInt16X8"
  maxInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_maxInt32X4"
  maxInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_maxInt64X2"
  maxInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_maxWord8X16"
  maxWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_maxWord16X8"
  maxWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_maxWord32X4"
  maxWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_maxWord64X2"
  maxWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
#endif

--
-- Floating-point minimum/maximum (IEEE compliant)
--

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_minimum_floatx4_avx512"
  minimumFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_maximum_floatx4_avx512"
  maximumFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_minimumNumber_floatx4_avx512"
  minimumNumberFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_maximumNumber_floatx4_avx512"
  maximumNumberFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_minimum_doublex2_avx512"
  minimumDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_maximum_doublex2_avx512"
  maximumDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_minimumNumber_doublex2_avx512"
  minimumNumberDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_maximumNumber_doublex2_avx512"
  maximumNumberDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

#else

foreign import ccall unsafe "hs_simdy_minimum_floatx4"
  minimumFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_maximum_floatx4"
  maximumFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_minimumNumber_floatx4"
  minimumNumberFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_maximumNumber_floatx4"
  maximumNumberFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_minimum_doublex2"
  minimumDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_maximum_doublex2"
  maximumDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_minimumNumber_doublex2"
  minimumNumberDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_maximumNumber_doublex2"
  maximumNumberDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

#endif

--
-- Equality
--

foreign import ccall unsafe "hs_simdy_int8x16_eq_densemask"
  eqInt8X16# :: Int8X16# -> Int8X16# -> Word16

foreign import ccall unsafe "hs_simdy_int16x8_eq_densemask"
  eqInt16X8# :: Int16X8# -> Int16X8# -> Word8

foreign import ccall unsafe "hs_simdy_int32x4_eq_densemask"
  eqInt32X4# :: Int32X4# -> Int32X4# -> Word8

foreign import ccall unsafe "hs_simdy_int64x2_eq_densemask"
  eqInt64X2# :: Int64X2# -> Int64X2# -> Word8

foreign import ccall unsafe "hs_simdy_int8x16_eq_densemask"
  eqWord8X16# :: Word8X16# -> Word8X16# -> Word16

foreign import ccall unsafe "hs_simdy_int16x8_eq_densemask"
  eqWord16X8# :: Word16X8# -> Word16X8# -> Word8

foreign import ccall unsafe "hs_simdy_int32x4_eq_densemask"
  eqWord32X4# :: Word32X4# -> Word32X4# -> Word8

foreign import ccall unsafe "hs_simdy_int64x2_eq_densemask"
  eqWord64X2# :: Word64X2# -> Word64X2# -> Word8

foreign import ccall unsafe "hs_simdy_floatx4_eq_densemask"
  eqFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_doublex2_eq_densemask"
  eqDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

--
-- Less than
--

foreign import ccall unsafe "hs_simdy_int8x16_lt_densemask"
  ltInt8X16# :: Int8X16# -> Int8X16# -> Word16

foreign import ccall unsafe "hs_simdy_int16x8_lt_densemask"
  ltInt16X8# :: Int16X8# -> Int16X8# -> Word8

foreign import ccall unsafe "hs_simdy_int32x4_lt_densemask"
  ltInt32X4# :: Int32X4# -> Int32X4# -> Word8

foreign import ccall unsafe "hs_simdy_int64x2_lt_densemask"
  ltInt64X2# :: Int64X2# -> Int64X2# -> Word8

foreign import ccall unsafe "hs_simdy_word8x16_lt_densemask"
  ltWord8X16# :: Word8X16# -> Word8X16# -> Word16

foreign import ccall unsafe "hs_simdy_word16x8_lt_densemask"
  ltWord16X8# :: Word16X8# -> Word16X8# -> Word8

foreign import ccall unsafe "hs_simdy_word32x4_lt_densemask"
  ltWord32X4# :: Word32X4# -> Word32X4# -> Word8

foreign import ccall unsafe "hs_simdy_word64x2_lt_densemask"
  ltWord64X2# :: Word64X2# -> Word64X2# -> Word8

foreign import ccall unsafe "hs_simdy_floatx4_lt_densemask"
  ltFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_doublex2_lt_densemask"
  ltDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

--
-- Less than or equal to
--

foreign import ccall unsafe "hs_simdy_floatx4_le_densemask"
  leFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_doublex2_le_densemask"
  leDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

--
-- Greater than
--

foreign import ccall unsafe "hs_simdy_floatx4_gt_densemask"
  gtFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_doublex2_gt_densemask"
  gtDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

--
-- Greater than or equal to
--

foreign import ccall unsafe "hs_simdy_floatx4_ge_densemask"
  geFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_doublex2_ge_densemask"
  geDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

{-
--
-- Unordered
--

foreign import ccall unsafe "hs_simdy_floatx4_unord_densemask"
  unordFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_doublex2_unord_densemask"
  unordDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8
-}

--
-- FMA
--
#if !MIN_VERSION_ghc_prim(0, 13, 0)
#if defined(USE_FMA) && MIN_VERSION_base(4, 19, 0)
-- GHC 9.8 or later
-- Let's hope LLVM's optimizer does a good job!

fmaddFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4#
fmaddFloatX4# x y z = case unpackFloatX4# x of
  (# x0, x1, x2, x3 #) ->
    case unpackFloatX4# y of
      (# y0, y1, y2, y3 #) ->
        case unpackFloatX4# z of
          (# z0, z1, z2, z3 #) ->
            packFloatX4#
              (# fmaddFloat# x0 y0 z0
               , fmaddFloat# x1 y1 z1
               , fmaddFloat# x2 y2 z2
               , fmaddFloat# x3 y3 z3
               #)
{-# INLINE fmaddFloatX4# #-}

fmaddDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#
fmaddDoubleX2# x y z = case unpackDoubleX2# x of
  (# x0, x1 #) ->
    case unpackDoubleX2# y of
      (# y0, y1 #) ->
        case unpackDoubleX2# z of
          (# z0, z1 #) ->
            packDoubleX2#
              (# fmaddDouble# x0 y0 z0
               , fmaddDouble# x1 y1 z1
               #)
{-# INLINE fmaddDoubleX2# #-}

#else

foreign import ccall unsafe "hs_simdy_fmaddFloatX4"
  fmaddFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_fmaddDoubleX2"
  fmaddDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2# -> DoubleX2#

#endif
#endif
