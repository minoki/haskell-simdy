{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Simdy.Internal.SIMD256.PrimExtra where
import Data.Word
import GHC.Exts

-- GHC bug: https://gitlab.haskell.org/ghc/ghc/-/issues/25730
#if 0
foreign import ccall unsafe "hs_simdy_pack_mask8x32"
  packMask8X32 :: Int8X32# -> Word32

foreign import ccall unsafe "hs_simdy_pack_mask16x16"
  packMask16X16 :: Int16X16# -> Word16

foreign import ccall unsafe "hs_simdy_pack_mask32x8"
  packMask32X8 :: Int32X8# -> Word8

foreign import ccall unsafe "hs_simdy_pack_mask64x4"
  packMask64X4 :: Int64X4# -> Word8

foreign import ccall unsafe "hs_simdy_unpack_mask8x32"
  unpackMask8X32 :: Word32 -> Int8X32#

foreign import ccall unsafe "hs_simdy_unpack_mask16x16"
  unpackMask16X16 :: Word16 -> Int16X16#

foreign import ccall unsafe "hs_simdy_unpack_mask32x8"
  unpackMask32X8 :: Word8 -> Int32X8#

foreign import ccall unsafe "hs_simdy_unpack_mask64x4"
  unpackMask64X4 :: Word8 -> Int64X4#

foreign import ccall unsafe "hs_simdy_select_int256"
  selectInt8X32Mask# :: Int8X32# -> Int8X32# -> Int8X32# -> Int8X32#

foreign import ccall unsafe "hs_simdy_select_int256"
  selectInt16X16Mask# :: Int16X16# -> Int16X16# -> Int16X16# -> Int16X16#

foreign import ccall unsafe "hs_simdy_select_int256"
  selectInt32X8Mask# :: Int32X8# -> Int32X8# -> Int32X8# -> Int32X8#

foreign import ccall unsafe "hs_simdy_select_int256"
  selectInt64X4Mask# :: Int64X4# -> Int64X4# -> Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_select_int256"
  selectWord8X32Mask# :: Int8X32# -> Word8X32# -> Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_select_int256"
  selectWord16X16Mask# :: Int16X16# -> Word16X16# -> Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_select_int256"
  selectWord32X8Mask# :: Int32X8# -> Word32X8# -> Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_select_int256"
  selectWord64X4Mask# :: Int64X4# -> Word64X4# -> Word64X4# -> Word64X4#

foreign import ccall unsafe "hs_simdy_select_floatx8"
  selectFloatX8Mask# :: Int32X8# -> FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_select_doublex4"
  selectDoubleX4Mask# :: Int64X4# -> DoubleX4# -> DoubleX4# -> DoubleX4#

selectInt8X32# :: Word32 -> Int8X32# -> Int8X32# -> Int8X32#
selectInt8X32# !x = selectInt8X32Mask# (unpackMask8X32 x)
{-# INLINE selectInt8X32# #-}

selectInt16X16# :: Word16 -> Int16X16# -> Int16X16# -> Int16X16#
selectInt16X16# !x = selectInt16X16Mask# (unpackMask16X16 x)
{-# INLINE selectInt16X16# #-}

selectInt32X8# :: Word8 -> Int32X8# -> Int32X8# -> Int32X8#
selectInt32X8# !x = selectInt32X8Mask# (unpackMask32X8 x)
{-# INLINE selectInt32X8# #-}

selectInt64X4# :: Word8 -> Int64X4# -> Int64X4# -> Int64X4#
selectInt64X4# !x = selectInt64X4Mask# (unpackMask64X4 x)
{-# INLINE selectInt64X4# #-}

selectWord8X32# :: Word32 -> Word8X32# -> Word8X32# -> Word8X32#
selectWord8X32# !x = selectWord8X32Mask# (unpackMask8X32 x)
{-# INLINE selectWord8X32# #-}

selectWord16X16# :: Word16 -> Word16X16# -> Word16X16# -> Word16X16#
selectWord16X16# !x = selectWord16X16Mask# (unpackMask16X16 x)
{-# INLINE selectWord16X16# #-}

selectWord32X8# :: Word8 -> Word32X8# -> Word32X8# -> Word32X8#
selectWord32X8# !x = selectWord32X8Mask# (unpackMask32X8 x)
{-# INLINE selectWord32X8# #-}

selectWord64X4# :: Word8 -> Word64X4# -> Word64X4# -> Word64X4#
selectWord64X4# !x = selectWord64X4Mask# (unpackMask64X4 x)
{-# INLINE selectWord64X4# #-}

selectFloatX8# :: Word8 -> FloatX8# -> FloatX8# -> FloatX8#
selectFloatX8# !x = selectFloatX8Mask# (unpackMask32X8 x)
{-# INLINE selectFloatX8# #-}

selectDoubleX4# :: Word8 -> DoubleX4# -> DoubleX4# -> DoubleX4#
selectDoubleX4# !x = selectDoubleX4Mask# (unpackMask64X4 x)
{-# INLINE selectDoubleX4# #-}
#endif

foreign import ccall unsafe "hs_simdy_select_int8x32_densemask"
  selectInt8X32# :: Word32 -> Int8X32# -> Int8X32# -> Int8X32#

foreign import ccall unsafe "hs_simdy_select_int16x16_densemask"
  selectInt16X16# :: Word16 -> Int16X16# -> Int16X16# -> Int16X16#

foreign import ccall unsafe "hs_simdy_select_int32x8_densemask"
  selectInt32X8# :: Word8 -> Int32X8# -> Int32X8# -> Int32X8#

foreign import ccall unsafe "hs_simdy_select_int64x2_densemask"
  selectInt64X4# :: Word8 -> Int64X4# -> Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_select_int8x32_densemask"
  selectWord8X32# :: Word32 -> Word8X32# -> Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_select_int16x16_densemask"
  selectWord16X16# :: Word16 -> Word16X16# -> Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_select_int32x8_densemask"
  selectWord32X8# :: Word8 -> Word32X8# -> Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_select_int64x2_densemask"
  selectWord64X4# :: Word8 -> Word64X4# -> Word64X4# -> Word64X4#

foreign import ccall unsafe "hs_simdy_select_floatx8"
  selectFloatX8# :: Word8 -> FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_select_doublex4"
  selectDoubleX4# :: Word8 -> DoubleX4# -> DoubleX4# -> DoubleX4#

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_minimum_floatx8_avx512"
  minimumFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_maximum_floatx8_avx512"
  maximumFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_minimumNumber_floatx8_avx512"
  minimumNumberFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_maximumNumber_floatx8_avx512"
  maximumNumberFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_minimum_doublex4_avx512"
  minimumDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_maximum_doublex4_avx512"
  maximumDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_minimumNumber_doublex4_avx512"
  minimumNumberDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_maximumNumber_doublex4_avx512"
  maximumNumberDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

#else

foreign import ccall unsafe "hs_simdy_minimum_floatx8"
  minimumFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_maximum_floatx8"
  maximumFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_minimumNumber_floatx8"
  minimumNumberFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_maximumNumber_floatx8"
  maximumNumberFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_minimum_doublex4"
  minimumDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_maximum_doublex4"
  maximumDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_minimumNumber_doublex4"
  minimumNumberDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_maximumNumber_doublex4"
  maximumNumberDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

#endif
