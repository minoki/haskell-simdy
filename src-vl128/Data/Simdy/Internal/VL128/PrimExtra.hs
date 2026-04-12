{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Simdy.Internal.VL128.PrimExtra where
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

foreign import ccall unsafe "hs_simdy_selectInt128"
  selectInt8X16Mask# :: Int8X16# -> Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_selectInt128"
  selectInt16X8Mask# :: Int16X8# -> Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_selectInt128"
  selectInt32X4Mask# :: Int32X4# -> Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_selectInt128"
  selectInt64X2Mask# :: Int64X2# -> Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_selectInt128"
  selectWord8X16Mask# :: Int8X16# -> Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_selectInt128"
  selectWord16X8Mask# :: Int16X8# -> Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_selectInt128"
  selectWord32X4Mask# :: Int32X4# -> Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_selectInt128"
  selectWord64X2Mask# :: Int64X2# -> Word64X2# -> Word64X2# -> Word64X2#

foreign import ccall unsafe "hs_simdy_selectFloatX4"
  selectFloatX4Mask# :: Int32X4# -> FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_selectDoubleX2"
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

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_selectInt8X16_densemask_avx512"
  selectInt8X16# :: Word16 -> Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_selectInt16X8_densemask_avx512"
  selectInt16X8# :: Word8 -> Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_selectInt32X4_densemask_avx512"
  selectInt32X4# :: Word8 -> Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_selectInt64X2_densemask_avx512"
  selectInt64X2# :: Word8 -> Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_selectInt8X16_densemask_avx512"
  selectWord8X16# :: Word16 -> Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_selectInt16X8_densemask_avx512"
  selectWord16X8# :: Word8 -> Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_selectInt32X4_densemask_avx512"
  selectWord32X4# :: Word8 -> Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_selectInt64X2_densemask_avx512"
  selectWord64X2# :: Word8 -> Word64X2# -> Word64X2# -> Word64X2#

foreign import ccall unsafe "hs_simdy_selectFloatX4_densemask_avx512"
  selectFloatX4# :: Word8 -> FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_selectDoubleX2_densemask_avx512"
  selectDoubleX2# :: Word8 -> DoubleX2# -> DoubleX2# -> DoubleX2#

#elif defined(__SSE4_1__)

foreign import ccall unsafe "hs_simdy_selectInt8X16_densemask_sse41"
  selectInt8X16# :: Word16 -> Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_selectInt16X8_densemask_sse41"
  selectInt16X8# :: Word8 -> Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_selectInt32X4_densemask_sse41"
  selectInt32X4# :: Word8 -> Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_selectInt64X2_densemask_sse41"
  selectInt64X2# :: Word8 -> Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_selectInt8X16_densemask_sse41"
  selectWord8X16# :: Word16 -> Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_selectInt16X8_densemask_sse41"
  selectWord16X8# :: Word8 -> Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_selectInt32X4_densemask_sse41"
  selectWord32X4# :: Word8 -> Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_selectInt64X2_densemask_sse41"
  selectWord64X2# :: Word8 -> Word64X2# -> Word64X2# -> Word64X2#

foreign import ccall unsafe "hs_simdy_selectFloatX4_densemask_sse41"
  selectFloatX4# :: Word8 -> FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_selectDoubleX2_densemask_sse41"
  selectDoubleX2# :: Word8 -> DoubleX2# -> DoubleX2# -> DoubleX2#

#else

foreign import ccall unsafe "hs_simdy_selectInt8X16_densemask"
  selectInt8X16# :: Word16 -> Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_selectInt16X8_densemask"
  selectInt16X8# :: Word8 -> Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_selectInt32X4_densemask"
  selectInt32X4# :: Word8 -> Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_selectInt64X2_densemask"
  selectInt64X2# :: Word8 -> Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_selectInt8X16_densemask"
  selectWord8X16# :: Word16 -> Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_selectInt16X8_densemask"
  selectWord16X8# :: Word8 -> Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_selectInt32X4_densemask"
  selectWord32X4# :: Word8 -> Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_selectInt64X2_densemask"
  selectWord64X2# :: Word8 -> Word64X2# -> Word64X2# -> Word64X2#

foreign import ccall unsafe "hs_simdy_selectFloatX4_densemask"
  selectFloatX4# :: Word8 -> FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_selectDoubleX2_densemask"
  selectDoubleX2# :: Word8 -> DoubleX2# -> DoubleX2# -> DoubleX2#

#endif

--
-- Left Shift
--

foreign import ccall unsafe "hs_simdy_shiftLInt8X16"
  shiftLInt8X16# :: Int8X16# -> Int# -> Int8X16#

foreign import ccall unsafe "hs_simdy_shiftLInt16X8"
  shiftLInt16X8# :: Int16X8# -> Int# -> Int16X8#

foreign import ccall unsafe "hs_simdy_shiftLInt32X4"
  shiftLInt32X4# :: Int32X4# -> Int# -> Int32X4#

foreign import ccall unsafe "hs_simdy_shiftLInt64X2"
  shiftLInt64X2# :: Int64X2# -> Int# -> Int64X2#

foreign import ccall unsafe "hs_simdy_shiftLWord8X16"
  shiftLWord8X16# :: Word8X16# -> Int# -> Word8X16#

foreign import ccall unsafe "hs_simdy_shiftLWord16X8"
  shiftLWord16X8# :: Word16X8# -> Int# -> Word16X8#

foreign import ccall unsafe "hs_simdy_shiftLWord32X4"
  shiftLWord32X4# :: Word32X4# -> Int# -> Word32X4#

foreign import ccall unsafe "hs_simdy_shiftLWord64X2"
  shiftLWord64X2# :: Word64X2# -> Int# -> Word64X2#

--
-- Right Shift (Arithmetic / Logical)
--

foreign import ccall unsafe "hs_simdy_shiftRInt8X16"
  shiftRInt8X16# :: Int8X16# -> Int# -> Int8X16#

foreign import ccall unsafe "hs_simdy_shiftRInt16X8"
  shiftRInt16X8# :: Int16X8# -> Int# -> Int16X8#

foreign import ccall unsafe "hs_simdy_shiftRInt32X4"
  shiftRInt32X4# :: Int32X4# -> Int# -> Int32X4#

#if defined(USE_AVX512)
foreign import ccall unsafe "hs_simdy_shiftRInt64X2_avx512"
  shiftRInt64X2# :: Int64X2# -> Int# -> Int64X2#
#else
foreign import ccall unsafe "hs_simdy_shiftRInt64X2"
  shiftRInt64X2# :: Int64X2# -> Int# -> Int64X2#
#endif

foreign import ccall unsafe "hs_simdy_shiftRWord8X16"
  shiftRWord8X16# :: Word8X16# -> Int# -> Word8X16#

foreign import ccall unsafe "hs_simdy_shiftRWord16X8"
  shiftRWord16X8# :: Word16X8# -> Int# -> Word16X8#

foreign import ccall unsafe "hs_simdy_shiftRWord32X4"
  shiftRWord32X4# :: Word32X4# -> Int# -> Word32X4#

foreign import ccall unsafe "hs_simdy_shiftRWord64X2"
  shiftRWord64X2# :: Word64X2# -> Int# -> Word64X2#

--
-- Floating-point minimum/maximum (IEEE compliant)
--

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_minimumFloatX4_avx512"
  minimumFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_maximumFloatX4_avx512"
  maximumFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_minimumNumberFloatX4_avx512"
  minimumNumberFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_maximumNumberFloatX4_avx512"
  maximumNumberFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_minimumDoubleX2_avx512"
  minimumDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_maximumDoubleX2_avx512"
  maximumDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_minimumNumberDoubleX2_avx512"
  minimumNumberDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_maximumNumberDoubleX2_avx512"
  maximumNumberDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

#else

foreign import ccall unsafe "hs_simdy_minimumFloatX4"
  minimumFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_maximumFloatX4"
  maximumFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_minimumNumberFloatX4"
  minimumNumberFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_maximumNumberFloatX4"
  maximumNumberFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

foreign import ccall unsafe "hs_simdy_minimumDoubleX2"
  minimumDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_maximumDoubleX2"
  maximumDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_minimumNumberDoubleX2"
  minimumNumberDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

foreign import ccall unsafe "hs_simdy_maximumNumberDoubleX2"
  maximumNumberDoubleX2# :: DoubleX2# -> DoubleX2# -> DoubleX2#

#endif

--
-- Equality
--

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_eqInt8X16_densemask_avx512"
  eqInt8X16# :: Int8X16# -> Int8X16# -> Word16

foreign import ccall unsafe "hs_simdy_eqInt16X8_densemask_avx512"
  eqInt16X8# :: Int16X8# -> Int16X8# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt32X4_densemask_avx512"
  eqInt32X4# :: Int32X4# -> Int32X4# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt64X2_densemask_avx512"
  eqInt64X2# :: Int64X2# -> Int64X2# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt8X16_densemask_avx512"
  eqWord8X16# :: Word8X16# -> Word8X16# -> Word16

foreign import ccall unsafe "hs_simdy_eqInt16X8_densemask_avx512"
  eqWord16X8# :: Word16X8# -> Word16X8# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt32X4_densemask_avx512"
  eqWord32X4# :: Word32X4# -> Word32X4# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt64X2_densemask_avx512"
  eqWord64X2# :: Word64X2# -> Word64X2# -> Word8

foreign import ccall unsafe "hs_simdy_eqFloatX4_densemask_avx512"
  eqFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_eqDoubleX2_densemask_avx512"
  eqDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#else

foreign import ccall unsafe "hs_simdy_eqInt8X16_densemask"
  eqInt8X16# :: Int8X16# -> Int8X16# -> Word16

foreign import ccall unsafe "hs_simdy_eqInt16X8_densemask"
  eqInt16X8# :: Int16X8# -> Int16X8# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt32X4_densemask"
  eqInt32X4# :: Int32X4# -> Int32X4# -> Word8

#if defined(__SSE4_1__)
foreign import ccall unsafe "hs_simdy_eqInt64X2_densemask_sse41"
  eqInt64X2# :: Int64X2# -> Int64X2# -> Word8
#else
foreign import ccall unsafe "hs_simdy_eqInt64X2_densemask"
  eqInt64X2# :: Int64X2# -> Int64X2# -> Word8
#endif

foreign import ccall unsafe "hs_simdy_eqInt8X16_densemask"
  eqWord8X16# :: Word8X16# -> Word8X16# -> Word16

foreign import ccall unsafe "hs_simdy_eqInt16X8_densemask"
  eqWord16X8# :: Word16X8# -> Word16X8# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt32X4_densemask"
  eqWord32X4# :: Word32X4# -> Word32X4# -> Word8

#if defined(__SSE4_1__)
foreign import ccall unsafe "hs_simdy_eqInt64X2_densemask_sse41"
  eqWord64X2# :: Word64X2# -> Word64X2# -> Word8
#else
foreign import ccall unsafe "hs_simdy_eqInt64X2_densemask"
  eqWord64X2# :: Word64X2# -> Word64X2# -> Word8
#endif

foreign import ccall unsafe "hs_simdy_eqFloatX4_densemask"
  eqFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_eqDoubleX2_densemask"
  eqDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#endif

--
-- Less than
--

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_ltInt8X16_densemask_avx512"
  ltInt8X16# :: Int8X16# -> Int8X16# -> Word16

foreign import ccall unsafe "hs_simdy_ltInt16X8_densemask_avx512"
  ltInt16X8# :: Int16X8# -> Int16X8# -> Word8

foreign import ccall unsafe "hs_simdy_ltInt32X4_densemask_avx512"
  ltInt32X4# :: Int32X4# -> Int32X4# -> Word8

foreign import ccall unsafe "hs_simdy_ltInt64X2_densemask_avx512"
  ltInt64X2# :: Int64X2# -> Int64X2# -> Word8

foreign import ccall unsafe "hs_simdy_ltWord8X16_densemask_avx512"
  ltWord8X16# :: Word8X16# -> Word8X16# -> Word16

foreign import ccall unsafe "hs_simdy_ltWord16X8_densemask_avx512"
  ltWord16X8# :: Word16X8# -> Word16X8# -> Word8

foreign import ccall unsafe "hs_simdy_ltWord32X4_densemask_avx512"
  ltWord32X4# :: Word32X4# -> Word32X4# -> Word8

foreign import ccall unsafe "hs_simdy_ltWord64X2_densemask_avx512"
  ltWord64X2# :: Word64X2# -> Word64X2# -> Word8

foreign import ccall unsafe "hs_simdy_ltFloatX4_densemask_avx512"
  ltFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_ltDoubleX2_densemask_avx512"
  ltDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#else

foreign import ccall unsafe "hs_simdy_ltInt8X16_densemask"
  ltInt8X16# :: Int8X16# -> Int8X16# -> Word16

foreign import ccall unsafe "hs_simdy_ltInt16X8_densemask"
  ltInt16X8# :: Int16X8# -> Int16X8# -> Word8

foreign import ccall unsafe "hs_simdy_ltInt32X4_densemask"
  ltInt32X4# :: Int32X4# -> Int32X4# -> Word8

#if defined(__SSE4_2__)
foreign import ccall unsafe "hs_simdy_ltInt64X2_densemask_sse42"
  ltInt64X2# :: Int64X2# -> Int64X2# -> Word8
#else
foreign import ccall unsafe "hs_simdy_ltInt64X2_densemask"
  ltInt64X2# :: Int64X2# -> Int64X2# -> Word8
#endif

foreign import ccall unsafe "hs_simdy_ltWord8X16_densemask"
  ltWord8X16# :: Word8X16# -> Word8X16# -> Word16

foreign import ccall unsafe "hs_simdy_ltWord16X8_densemask"
  ltWord16X8# :: Word16X8# -> Word16X8# -> Word8

foreign import ccall unsafe "hs_simdy_ltWord32X4_densemask"
  ltWord32X4# :: Word32X4# -> Word32X4# -> Word8

#if defined(__SSE4_2__)
foreign import ccall unsafe "hs_simdy_ltWord64X2_densemask_sse42"
  ltWord64X2# :: Word64X2# -> Word64X2# -> Word8
#else
foreign import ccall unsafe "hs_simdy_ltWord64X2_densemask"
  ltWord64X2# :: Word64X2# -> Word64X2# -> Word8
#endif

foreign import ccall unsafe "hs_simdy_ltFloatX4_densemask"
  ltFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_ltDoubleX2_densemask"
  ltDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#endif

--
-- Less than or equal to
--

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_leFloatX4_densemask_avx512"
  leFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_leDoubleX2_densemask_avx512"
  leDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#else

foreign import ccall unsafe "hs_simdy_leFloatX4_densemask"
  leFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_leDoubleX2_densemask"
  leDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#endif

--
-- Greater than
--

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_gtFloatX4_densemask_avx512"
  gtFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_gtDoubleX2_densemask_avx512"
  gtDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#else

foreign import ccall unsafe "hs_simdy_gtFloatX4_densemask"
  gtFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_gtDoubleX2_densemask"
  gtDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#endif

--
-- Greater than or equal to
--

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_geFloatX4_densemask_avx512"
  geFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_geDoubleX2_densemask_avx512"
  geDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#else

foreign import ccall unsafe "hs_simdy_geFloatX4_densemask"
  geFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_geDoubleX2_densemask"
  geDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#endif

{-
--
-- Unordered
--

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_unordFloatX4_densemask_avx512"
  unordFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_unordDoubleX2_densemask_avx512"
  unordDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#else

foreign import ccall unsafe "hs_simdy_unordFloatX4_densemask"
  unordFloatX4# :: FloatX4# -> FloatX4# -> Word8

foreign import ccall unsafe "hs_simdy_unordDoubleX2_densemask"
  unordDoubleX2# :: DoubleX2# -> DoubleX2# -> Word8

#endif

-}

--
-- FMA
--
#if !MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
#if MIN_VERSION_base(4, 19, 0)
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
