{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
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
