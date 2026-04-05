{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
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

foreign import ccall unsafe "hs_simdy_selectInt256"
  selectInt8X32Mask# :: Int8X32# -> Int8X32# -> Int8X32# -> Int8X32#

foreign import ccall unsafe "hs_simdy_selectInt256"
  selectInt16X16Mask# :: Int16X16# -> Int16X16# -> Int16X16# -> Int16X16#

foreign import ccall unsafe "hs_simdy_selectInt256"
  selectInt32X8Mask# :: Int32X8# -> Int32X8# -> Int32X8# -> Int32X8#

foreign import ccall unsafe "hs_simdy_selectInt256"
  selectInt64X4Mask# :: Int64X4# -> Int64X4# -> Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_selectInt256"
  selectWord8X32Mask# :: Int8X32# -> Word8X32# -> Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_selectInt256"
  selectWord16X16Mask# :: Int16X16# -> Word16X16# -> Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_selectInt256"
  selectWord32X8Mask# :: Int32X8# -> Word32X8# -> Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_selectInt256"
  selectWord64X4Mask# :: Int64X4# -> Word64X4# -> Word64X4# -> Word64X4#

foreign import ccall unsafe "hs_simdy_selectFloatX8"
  selectFloatX8Mask# :: Int32X8# -> FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_selectDoubleX4"
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

foreign import ccall unsafe "hs_simdy_selectInt8X32_densemask"
  selectInt8X32# :: Word32 -> Int8X32# -> Int8X32# -> Int8X32#

foreign import ccall unsafe "hs_simdy_selectInt16X16_densemask"
  selectInt16X16# :: Word16 -> Int16X16# -> Int16X16# -> Int16X16#

foreign import ccall unsafe "hs_simdy_selectInt32X8_densemask"
  selectInt32X8# :: Word8 -> Int32X8# -> Int32X8# -> Int32X8#

foreign import ccall unsafe "hs_simdy_selectInt64X4_densemask"
  selectInt64X4# :: Word8 -> Int64X4# -> Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_selectInt8X32_densemask"
  selectWord8X32# :: Word32 -> Word8X32# -> Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_selectInt16X16_densemask"
  selectWord16X16# :: Word16 -> Word16X16# -> Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_selectInt32X8_densemask"
  selectWord32X8# :: Word8 -> Word32X8# -> Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_selectInt64X4_densemask"
  selectWord64X4# :: Word8 -> Word64X4# -> Word64X4# -> Word64X4#

foreign import ccall unsafe "hs_simdy_selectFloatX8_densemask"
  selectFloatX8# :: Word8 -> FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_selectDoubleX4_densemask"
  selectDoubleX4# :: Word8 -> DoubleX4# -> DoubleX4# -> DoubleX4#

--
-- Left Shift
--

foreign import ccall unsafe "hs_simdy_shiftLInt8X32"
  shiftLInt8X32# :: Int8X32# -> Int# -> Int8X32#

foreign import ccall unsafe "hs_simdy_shiftLInt16X16"
  shiftLInt16X16# :: Int16X16# -> Int# -> Int16X16#

foreign import ccall unsafe "hs_simdy_shiftLInt32X8"
  shiftLInt32X8# :: Int32X8# -> Int# -> Int32X8#

foreign import ccall unsafe "hs_simdy_shiftLInt64X4"
  shiftLInt64X4# :: Int64X4# -> Int# -> Int64X4#

foreign import ccall unsafe "hs_simdy_shiftLWord8X32"
  shiftLWord8X32# :: Word8X32# -> Int# -> Word8X32#

foreign import ccall unsafe "hs_simdy_shiftLWord16X16"
  shiftLWord16X16# :: Word16X16# -> Int# -> Word16X16#

foreign import ccall unsafe "hs_simdy_shiftLWord32X8"
  shiftLWord32X8# :: Word32X8# -> Int# -> Word32X8#

foreign import ccall unsafe "hs_simdy_shiftLWord64X4"
  shiftLWord64X4# :: Word64X4# -> Int# -> Word64X4#

--
-- Right Shift (Arithmetic / Logical)
--

foreign import ccall unsafe "hs_simdy_shiftRInt8X32"
  shiftRInt8X32# :: Int8X32# -> Int# -> Int8X32#

foreign import ccall unsafe "hs_simdy_shiftRInt16X16"
  shiftRInt16X16# :: Int16X16# -> Int# -> Int16X16#

foreign import ccall unsafe "hs_simdy_shiftRInt32X8"
  shiftRInt32X8# :: Int32X8# -> Int# -> Int32X8#

#if defined(USE_AVX512)
foreign import ccall unsafe "hs_simdy_shiftRInt64X4_avx512"
  shiftRInt64X4# :: Int64X4# -> Int# -> Int64X4#
#else
foreign import ccall unsafe "hs_simdy_shiftRInt64X4"
  shiftRInt64X4# :: Int64X4# -> Int# -> Int64X4#
#endif

foreign import ccall unsafe "hs_simdy_shiftRWord8X32"
  shiftRWord8X32# :: Word8X32# -> Int# -> Word8X32#

foreign import ccall unsafe "hs_simdy_shiftRWord16X16"
  shiftRWord16X16# :: Word16X16# -> Int# -> Word16X16#

foreign import ccall unsafe "hs_simdy_shiftRWord32X8"
  shiftRWord32X8# :: Word32X8# -> Int# -> Word32X8#

foreign import ccall unsafe "hs_simdy_shiftRWord64X4"
  shiftRWord64X4# :: Word64X4# -> Int# -> Word64X4#

--
-- Floating-point minimum/maximum (IEEE compliant)
--

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_minimumFloatX8_avx512"
  minimumFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_maximumFloatX8_avx512"
  maximumFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_minimumNumberFloatX8_avx512"
  minimumNumberFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_maximumNumberFloatX8_avx512"
  maximumNumberFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_minimumDoubleX4_avx512"
  minimumDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_maximumDoubleX4_avx512"
  maximumDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_minimumNumberDoubleX4_avx512"
  minimumNumberDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_maximumNumberDoubleX4_avx512"
  maximumNumberDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

#else

foreign import ccall unsafe "hs_simdy_minimumFloatX8"
  minimumFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_maximumFloatX8"
  maximumFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_minimumNumberFloatX8"
  minimumNumberFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_maximumNumberFloatX8"
  maximumNumberFloatX8# :: FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_minimumDoubleX4"
  minimumDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_maximumDoubleX4"
  maximumDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_minimumNumberDoubleX4"
  minimumNumberDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

foreign import ccall unsafe "hs_simdy_maximumNumberDoubleX4"
  maximumNumberDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4#

#endif

--
-- Equality
--

#if defined(USE_AVX512)

foreign import ccall unsafe "hs_simdy_eqInt8X32_densemask_avx512"
  eqInt8X32# :: Int8X32# -> Int8X32# -> Word32

foreign import ccall unsafe "hs_simdy_eqInt16X16_densemask_avx512"
  eqInt16X16# :: Int16X16# -> Int16X16# -> Word16

foreign import ccall unsafe "hs_simdy_eqInt32X8_densemask_avx512"
  eqInt32X8# :: Int32X8# -> Int32X8# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt64X4_densemask_avx512"
  eqInt64X4# :: Int64X4# -> Int64X4# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt8X32_densemask_avx512"
  eqWord8X32# :: Word8X32# -> Word8X32# -> Word32

foreign import ccall unsafe "hs_simdy_eqInt16X16_densemask_avx512"
  eqWord16X16# :: Word16X16# -> Word16X16# -> Word16

foreign import ccall unsafe "hs_simdy_eqInt32X8_densemask_avx512"
  eqWord32X8# :: Word32X8# -> Word32X8# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt64X4_densemask_avx512"
  eqWord64X4# :: Word64X4# -> Word64X4# -> Word8

foreign import ccall unsafe "hs_simdy_eqFloatX8_densemask_avx512"
  eqFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_eqDoubleX4_densemask_avx512"
  eqDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

#else

foreign import ccall unsafe "hs_simdy_eqInt8X32_densemask"
  eqInt8X32# :: Int8X32# -> Int8X32# -> Word32

foreign import ccall unsafe "hs_simdy_eqInt16X16_densemask"
  eqInt16X16# :: Int16X16# -> Int16X16# -> Word16

foreign import ccall unsafe "hs_simdy_eqInt32X8_densemask"
  eqInt32X8# :: Int32X8# -> Int32X8# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt64X4_densemask"
  eqInt64X4# :: Int64X4# -> Int64X4# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt8X32_densemask"
  eqWord8X32# :: Word8X32# -> Word8X32# -> Word32

foreign import ccall unsafe "hs_simdy_eqInt16X16_densemask"
  eqWord16X16# :: Word16X16# -> Word16X16# -> Word16

foreign import ccall unsafe "hs_simdy_eqInt32X8_densemask"
  eqWord32X8# :: Word32X8# -> Word32X8# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt64X4_densemask"
  eqWord64X4# :: Word64X4# -> Word64X4# -> Word8

foreign import ccall unsafe "hs_simdy_eqFloatX8_densemask"
  eqFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_eqDoubleX4_densemask"
  eqDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

#endif

--
-- Less than
--

foreign import ccall unsafe "hs_simdy_ltInt8X32_densemask"
  ltInt8X32# :: Int8X32# -> Int8X32# -> Word32

foreign import ccall unsafe "hs_simdy_ltInt16X16_densemask"
  ltInt16X16# :: Int16X16# -> Int16X16# -> Word16

foreign import ccall unsafe "hs_simdy_ltInt32X8_densemask"
  ltInt32X8# :: Int32X8# -> Int32X8# -> Word8

foreign import ccall unsafe "hs_simdy_ltInt64X4_densemask"
  ltInt64X4# :: Int64X4# -> Int64X4# -> Word8

foreign import ccall unsafe "hs_simdy_ltWord8X32_densemask"
  ltWord8X32# :: Word8X32# -> Word8X32# -> Word32

foreign import ccall unsafe "hs_simdy_ltWord16X16_densemask"
  ltWord16X16# :: Word16X16# -> Word16X16# -> Word16

foreign import ccall unsafe "hs_simdy_ltWord32X8_densemask"
  ltWord32X8# :: Word32X8# -> Word32X8# -> Word8

foreign import ccall unsafe "hs_simdy_ltWord64X4_densemask"
  ltWord64X4# :: Word64X4# -> Word64X4# -> Word8

foreign import ccall unsafe "hs_simdy_ltFloatX8_densemask"
  ltFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_ltDoubleX4_densemask"
  ltDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

--
-- Less than or equal to
--

foreign import ccall unsafe "hs_simdy_leFloatX8_densemask"
  leFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_leDoubleX4_densemask"
  leDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

--
-- Greater than
--

foreign import ccall unsafe "hs_simdy_gtFloatX8_densemask"
  gtFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_gtDoubleX4_densemask"
  gtDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

--
-- Greater than or equal to
--

foreign import ccall unsafe "hs_simdy_geFloatX8_densemask"
  geFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_geDoubleX4_densemask"
  geDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

{-
--
-- Unordered
--

foreign import ccall unsafe "hs_simdy_unordFloatX8_densemask"
  unordFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_unordDoubleX4_densemask"
  unordDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8
-}

--
-- FMA
--
#if !MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
#if MIN_VERSION_base(4, 19, 0)
-- GHC 9.8 or later
-- Let's hope LLVM's optimizer does a good job!

fmaddFloatX8# :: FloatX8# -> FloatX8# -> FloatX8# -> FloatX8#
fmaddFloatX8# x y z = case unpackFloatX8# x of
  (# x0, x1, x2, x3, x4, x5, x6, x7 #) ->
    case unpackFloatX8# y of
      (# y0, y1, y2, y3, y4, y5, y6, y7 #) ->
        case unpackFloatX8# z of
          (# z0, z1, z2, z3, z4, z5, z6, z7 #) ->
            packFloatX8#
              (# fmaddFloat# x0 y0 z0
               , fmaddFloat# x1 y1 z1
               , fmaddFloat# x2 y2 z2
               , fmaddFloat# x3 y3 z3
               , fmaddFloat# x4 y4 z4
               , fmaddFloat# x5 y5 z5
               , fmaddFloat# x6 y6 z6
               , fmaddFloat# x7 y7 z7
               #)
{-# INLINE fmaddFloatX8# #-}

fmaddDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4# -> DoubleX4#
fmaddDoubleX4# x y z = case unpackDoubleX4# x of
  (# x0, x1, x2, x3 #) ->
    case unpackDoubleX4# y of
      (# y0, y1, y2, y3 #) ->
        case unpackDoubleX4# z of
          (# z0, z1, z2, z3 #) ->
            packDoubleX4#
              (# fmaddDouble# x0 y0 z0
               , fmaddDouble# x1 y1 z1
               , fmaddDouble# x2 y2 z2
               , fmaddDouble# x3 y3 z3
               #)
{-# INLINE fmaddDoubleX4# #-}

#else

foreign import ccall unsafe "hs_simdy_fmaddFloatX8"
  fmaddFloatX8# :: FloatX8# -> FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_fmaddDoubleX4"
  fmaddDoubleX4# :: DoubleX4# -> DoubleX4# -> DoubleX4# -> DoubleX4#

#endif
#endif
