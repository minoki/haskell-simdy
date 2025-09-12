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

foreign import ccall unsafe "hs_simdy_select_int64x4_densemask"
  selectInt64X4# :: Word8 -> Int64X4# -> Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_select_int8x32_densemask"
  selectWord8X32# :: Word32 -> Word8X32# -> Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_select_int16x16_densemask"
  selectWord16X16# :: Word16 -> Word16X16# -> Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_select_int32x8_densemask"
  selectWord32X8# :: Word8 -> Word32X8# -> Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_select_int64x4_densemask"
  selectWord64X4# :: Word8 -> Word64X4# -> Word64X4# -> Word64X4#

foreign import ccall unsafe "hs_simdy_select_floatx8_densemask"
  selectFloatX8# :: Word8 -> FloatX8# -> FloatX8# -> FloatX8#

foreign import ccall unsafe "hs_simdy_select_doublex4_densemask"
  selectDoubleX4# :: Word8 -> DoubleX4# -> DoubleX4# -> DoubleX4#

--
-- Complement (bitwise not)
--

foreign import ccall unsafe "hs_simdy_complement_int256"
  complementInt8X32# :: Int8X32# -> Int8X32#

foreign import ccall unsafe "hs_simdy_complement_int256"
  complementInt16X16# :: Int16X16# -> Int16X16#

foreign import ccall unsafe "hs_simdy_complement_int256"
  complementInt32X8# :: Int32X8# -> Int32X8#

foreign import ccall unsafe "hs_simdy_complement_int256"
  complementInt64X4# :: Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_complement_int256"
  complementWord8X32# :: Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_complement_int256"
  complementWord16X16# :: Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_complement_int256"
  complementWord32X8# :: Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_complement_int256"
  complementWord64X4# :: Word64X4# -> Word64X4#

--
-- Bitwise AND
--

foreign import ccall unsafe "hs_simdy_and_int256"
  andInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#

foreign import ccall unsafe "hs_simdy_and_int256"
  andInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#

foreign import ccall unsafe "hs_simdy_and_int256"
  andInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#

foreign import ccall unsafe "hs_simdy_and_int256"
  andInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_and_int256"
  andWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_and_int256"
  andWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_and_int256"
  andWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_and_int256"
  andWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#

--
-- Bitwise OR
--

foreign import ccall unsafe "hs_simdy_or_int256"
  orInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#

foreign import ccall unsafe "hs_simdy_or_int256"
  orInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#

foreign import ccall unsafe "hs_simdy_or_int256"
  orInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#

foreign import ccall unsafe "hs_simdy_or_int256"
  orInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_or_int256"
  orWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_or_int256"
  orWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_or_int256"
  orWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_or_int256"
  orWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#

--
-- Bitwise XOR
--

foreign import ccall unsafe "hs_simdy_xor_int256"
  xorInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#

foreign import ccall unsafe "hs_simdy_xor_int256"
  xorInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#

foreign import ccall unsafe "hs_simdy_xor_int256"
  xorInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#

foreign import ccall unsafe "hs_simdy_xor_int256"
  xorInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_xor_int256"
  xorWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_xor_int256"
  xorWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_xor_int256"
  xorWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_xor_int256"
  xorWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#

--
-- Left Shift
--

foreign import ccall unsafe "hs_simdy_shiftL_int8x32"
  shiftLInt8X32# :: Int8X32# -> Int# -> Int8X32#

foreign import ccall unsafe "hs_simdy_shiftL_int16x16"
  shiftLInt16X16# :: Int16X16# -> Int# -> Int16X16#

foreign import ccall unsafe "hs_simdy_shiftL_int32x8"
  shiftLInt32X8# :: Int32X8# -> Int# -> Int32X8#

foreign import ccall unsafe "hs_simdy_shiftL_int64x4"
  shiftLInt64X4# :: Int64X4# -> Int# -> Int64X4#

foreign import ccall unsafe "hs_simdy_shiftL_word8x32"
  shiftLWord8X32# :: Word8X32# -> Int# -> Word8X32#

foreign import ccall unsafe "hs_simdy_shiftL_word16x16"
  shiftLWord16X16# :: Word16X16# -> Int# -> Word16X16#

foreign import ccall unsafe "hs_simdy_shiftL_word32x8"
  shiftLWord32X8# :: Word32X8# -> Int# -> Word32X8#

foreign import ccall unsafe "hs_simdy_shiftL_word64x4"
  shiftLWord64X4# :: Word64X4# -> Int# -> Word64X4#

--
-- Right Shift (Arithmetic / Logical)
--

foreign import ccall unsafe "hs_simdy_shiftR_int8x32"
  shiftRInt8X32# :: Int8X32# -> Int# -> Int8X32#

foreign import ccall unsafe "hs_simdy_shiftR_int16x16"
  shiftRInt16X16# :: Int16X16# -> Int# -> Int16X16#

foreign import ccall unsafe "hs_simdy_shiftR_int32x8"
  shiftRInt32X8# :: Int32X8# -> Int# -> Int32X8#

foreign import ccall unsafe "hs_simdy_shiftR_int64x4"
  shiftRInt64X4# :: Int64X4# -> Int# -> Int64X4#

foreign import ccall unsafe "hs_simdy_shiftR_word8x32"
  shiftRWord8X32# :: Word8X32# -> Int# -> Word8X32#

foreign import ccall unsafe "hs_simdy_shiftR_word16x16"
  shiftRWord16X16# :: Word16X16# -> Int# -> Word16X16#

foreign import ccall unsafe "hs_simdy_shiftR_word32x8"
  shiftRWord32X8# :: Word32X8# -> Int# -> Word32X8#

foreign import ccall unsafe "hs_simdy_shiftR_word64x4"
  shiftRWord64X4# :: Word64X4# -> Int# -> Word64X4#

-- Workaround a bug in LLVM
-- #if !MIN_VERSION_ghc_prim(0, 13, 0) || (defined(USE_AVX512) && defined(__GLASGOW_HASKELL_LLVM__) && __GLASGOW_HASKELL_LLVM__ < 2200)
#if !MIN_VERSION_ghc_prim(0, 13, 0) || defined(USE_AVX512)
--
-- Integer minimum/maximum
--

foreign import ccall unsafe "hs_simdy_minInt8X32"
  minInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#

foreign import ccall unsafe "hs_simdy_minInt16X16"
  minInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#

foreign import ccall unsafe "hs_simdy_minInt32X8"
  minInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#

foreign import ccall unsafe "hs_simdy_minInt64X4"
  minInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_minWord8X32"
  minWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_minWord16X16"
  minWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_minWord32X8"
  minWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_minWord64X4"
  minWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#

foreign import ccall unsafe "hs_simdy_maxInt8X32"
  maxInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#

foreign import ccall unsafe "hs_simdy_maxInt16X16"
  maxInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#

foreign import ccall unsafe "hs_simdy_maxInt32X8"
  maxInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#

foreign import ccall unsafe "hs_simdy_maxInt64X4"
  maxInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#

foreign import ccall unsafe "hs_simdy_maxWord8X32"
  maxWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#

foreign import ccall unsafe "hs_simdy_maxWord16X16"
  maxWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#

foreign import ccall unsafe "hs_simdy_maxWord32X8"
  maxWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#

foreign import ccall unsafe "hs_simdy_maxWord64X4"
  maxWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
#endif

--
-- Floating-point minimum/maximum (IEEE compliant)
--

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

--
-- Equality
--

foreign import ccall unsafe "hs_simdy_int8x32_eq_densemask"
  eqInt8X32# :: Int8X32# -> Int8X32# -> Word32

foreign import ccall unsafe "hs_simdy_int16x16_eq_densemask"
  eqInt16X16# :: Int16X16# -> Int16X16# -> Word16

foreign import ccall unsafe "hs_simdy_int32x8_eq_densemask"
  eqInt32X8# :: Int32X8# -> Int32X8# -> Word8

foreign import ccall unsafe "hs_simdy_int64x4_eq_densemask"
  eqInt64X4# :: Int64X4# -> Int64X4# -> Word8

foreign import ccall unsafe "hs_simdy_int8x32_eq_densemask"
  eqWord8X32# :: Word8X32# -> Word8X32# -> Word32

foreign import ccall unsafe "hs_simdy_int16x16_eq_densemask"
  eqWord16X16# :: Word16X16# -> Word16X16# -> Word16

foreign import ccall unsafe "hs_simdy_int32x8_eq_densemask"
  eqWord32X8# :: Word32X8# -> Word32X8# -> Word8

foreign import ccall unsafe "hs_simdy_int64x4_eq_densemask"
  eqWord64X4# :: Word64X4# -> Word64X4# -> Word8

foreign import ccall unsafe "hs_simdy_floatx8_eq_densemask"
  eqFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_doublex4_eq_densemask"
  eqDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

--
-- Less than
--

foreign import ccall unsafe "hs_simdy_int8x32_lt_densemask"
  ltInt8X32# :: Int8X32# -> Int8X32# -> Word32

foreign import ccall unsafe "hs_simdy_int16x16_lt_densemask"
  ltInt16X16# :: Int16X16# -> Int16X16# -> Word16

foreign import ccall unsafe "hs_simdy_int32x8_lt_densemask"
  ltInt32X8# :: Int32X8# -> Int32X8# -> Word8

foreign import ccall unsafe "hs_simdy_int64x4_lt_densemask"
  ltInt64X4# :: Int64X4# -> Int64X4# -> Word8

foreign import ccall unsafe "hs_simdy_word8x32_lt_densemask"
  ltWord8X32# :: Word8X32# -> Word8X32# -> Word32

foreign import ccall unsafe "hs_simdy_word16x16_lt_densemask"
  ltWord16X16# :: Word16X16# -> Word16X16# -> Word16

foreign import ccall unsafe "hs_simdy_word32x8_lt_densemask"
  ltWord32X8# :: Word32X8# -> Word32X8# -> Word8

foreign import ccall unsafe "hs_simdy_word64x4_lt_densemask"
  ltWord64X4# :: Word64X4# -> Word64X4# -> Word8

foreign import ccall unsafe "hs_simdy_floatx8_lt_densemask"
  ltFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_doublex4_lt_densemask"
  ltDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

--
-- Less than or equal to
--

foreign import ccall unsafe "hs_simdy_floatx8_le_densemask"
  leFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_doublex4_le_densemask"
  leDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

--
-- Greater than
--

foreign import ccall unsafe "hs_simdy_floatx8_gt_densemask"
  gtFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_doublex4_gt_densemask"
  gtDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

--
-- Greater than or equal to
--

foreign import ccall unsafe "hs_simdy_floatx8_ge_densemask"
  geFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_doublex4_ge_densemask"
  geDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8

{-
--
-- Unordered
--

foreign import ccall unsafe "hs_simdy_floatx8_unord_densemask"
  unordFloatX8# :: FloatX8# -> FloatX8# -> Word8

foreign import ccall unsafe "hs_simdy_doublex4_unord_densemask"
  unordDoubleX4# :: DoubleX4# -> DoubleX4# -> Word8
-}

--
-- FMA
--
#if !MIN_VERSION_ghc_prim(0, 13, 0)
#if defined(USE_FMA) && MIN_VERSION_base(4, 19, 0)
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
