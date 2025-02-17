{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Simdy.Internal.PrimExtra where
import Data.Word
import GHC.Exts

#if defined(USE_SIMD128)

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

#endif

#if defined(USE_SIMD256)

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

#endif

#if defined(USE_SIMD512)

foreign import ccall unsafe "hs_simdy_select_int8x64_densemask"
  selectInt8X64# :: Word64 -> Int8X64# -> Int8X64# -> Int8X64#

foreign import ccall unsafe "hs_simdy_select_int16x32_densemask"
  selectInt16X32# :: Word32 -> Int16X32# -> Int16X32# -> Int16X32#

foreign import ccall unsafe "hs_simdy_select_int32x16_densemask"
  selectInt32X16# :: Word16 -> Int32X16# -> Int32X16# -> Int32X16#

foreign import ccall unsafe "hs_simdy_select_int64x8_densemask"
  selectInt64X8# :: Word8 -> Int64X8# -> Int64X8# -> Int64X8#

foreign import ccall unsafe "hs_simdy_select_int8x64_densemask"
  selectWord8X64# :: Word64 -> Word8X64# -> Word8X64# -> Word8X64#

foreign import ccall unsafe "hs_simdy_select_int16x32_densemask"
  selectWord16X32# :: Word32 -> Word16X32# -> Word16X32# -> Word16X32#

foreign import ccall unsafe "hs_simdy_select_int32x16_densemask"
  selectWord32X16# :: Word16 -> Word32X16# -> Word32X16# -> Word32X16#

foreign import ccall unsafe "hs_simdy_select_int64x8_densemask"
  selectWord64X8# :: Word8 -> Word64X8# -> Word64X8# -> Word64X8#

foreign import ccall unsafe "hs_simdy_select_floatx16_densemask"
  selectFloatX16# :: Word16 -> FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_select_doublex8_densemask"
  selectDoubleX8# :: Word8 -> DoubleX8# -> DoubleX8# -> DoubleX8#

#endif

foreign import ccall unsafe "hs_simdy_minimum_float"
  minimumFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_maximum_float"
  maximumFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_minimumNumber_float"
  minimumNumberFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_maximumNumber_float"
  maximumNumberFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_minimum_double"
  minimumDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_maximum_double"
  maximumDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_minimumNumber_double"
  minimumNumberDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_maximumNumber_double"
  maximumNumberDouble :: Double -> Double -> Double

#if defined(USE_SIMD128)

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

#endif

#if defined(USE_SIMD256)

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

#endif

#if defined(USE_SIMD512)

foreign import ccall unsafe "hs_simdy_minimum_floatx16"
  minimumFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_maximum_floatx16"
  maximumFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_minimumNumber_floatx16"
  minimumNumberFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_maximumNumber_floatx16"
  maximumNumberFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_minimum_doublex8"
  minimumDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#

foreign import ccall unsafe "hs_simdy_maximum_doublex8"
  maximumDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#

foreign import ccall unsafe "hs_simdy_minimumNumber_doublex8"
  minimumNumberDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#

foreign import ccall unsafe "hs_simdy_maximumNumber_doublex8"
  maximumNumberDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#

#endif
