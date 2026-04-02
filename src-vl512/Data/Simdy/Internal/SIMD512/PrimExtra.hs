{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Simdy.Internal.SIMD512.PrimExtra where
import Data.Word
import GHC.Exts

foreign import ccall unsafe "hs_simdy_selectInt8X64_densemask"
  selectInt8X64# :: Word64 -> Int8X64# -> Int8X64# -> Int8X64#

foreign import ccall unsafe "hs_simdy_selectInt16X32_densemask"
  selectInt16X32# :: Word32 -> Int16X32# -> Int16X32# -> Int16X32#

foreign import ccall unsafe "hs_simdy_selectInt32X16_densemask"
  selectInt32X16# :: Word16 -> Int32X16# -> Int32X16# -> Int32X16#

foreign import ccall unsafe "hs_simdy_selectInt64X8_densemask"
  selectInt64X8# :: Word8 -> Int64X8# -> Int64X8# -> Int64X8#

foreign import ccall unsafe "hs_simdy_selectInt8X64_densemask"
  selectWord8X64# :: Word64 -> Word8X64# -> Word8X64# -> Word8X64#

foreign import ccall unsafe "hs_simdy_selectInt16X32_densemask"
  selectWord16X32# :: Word32 -> Word16X32# -> Word16X32# -> Word16X32#

foreign import ccall unsafe "hs_simdy_selectInt32X16_densemask"
  selectWord32X16# :: Word16 -> Word32X16# -> Word32X16# -> Word32X16#

foreign import ccall unsafe "hs_simdy_selectInt64X8_densemask"
  selectWord64X8# :: Word8 -> Word64X8# -> Word64X8# -> Word64X8#

foreign import ccall unsafe "hs_simdy_selectFloatX16_densemask"
  selectFloatX16# :: Word16 -> FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_selectDoubleX8_densemask"
  selectDoubleX8# :: Word8 -> DoubleX8# -> DoubleX8# -> DoubleX8#

--
-- Left Shift
--

foreign import ccall unsafe "hs_simdy_shiftLInt8X64"
  shiftLInt8X64# :: Int8X64# -> Int# -> Int8X64#

foreign import ccall unsafe "hs_simdy_shiftLInt16X32"
  shiftLInt16X32# :: Int16X32# -> Int# -> Int16X32#

foreign import ccall unsafe "hs_simdy_shiftLInt32X16"
  shiftLInt32X16# :: Int32X16# -> Int# -> Int32X16#

foreign import ccall unsafe "hs_simdy_shiftLInt64X8"
  shiftLInt64X8# :: Int64X8# -> Int# -> Int64X8#

foreign import ccall unsafe "hs_simdy_shiftLWord8X64"
  shiftLWord8X64# :: Word8X64# -> Int# -> Word8X64#

foreign import ccall unsafe "hs_simdy_shiftLWord16X32"
  shiftLWord16X32# :: Word16X32# -> Int# -> Word16X32#

foreign import ccall unsafe "hs_simdy_shiftLWord32X16"
  shiftLWord32X16# :: Word32X16# -> Int# -> Word32X16#

foreign import ccall unsafe "hs_simdy_shiftLWord64X8"
  shiftLWord64X8# :: Word64X8# -> Int# -> Word64X8#

--
-- Right Shift (Arithmetic / Logical)
--

foreign import ccall unsafe "hs_simdy_shiftRInt8X64"
  shiftRInt8X64# :: Int8X64# -> Int# -> Int8X64#

foreign import ccall unsafe "hs_simdy_shiftRInt16X32"
  shiftRInt16X32# :: Int16X32# -> Int# -> Int16X32#

foreign import ccall unsafe "hs_simdy_shiftRInt32X16"
  shiftRInt32X16# :: Int32X16# -> Int# -> Int32X16#

foreign import ccall unsafe "hs_simdy_shiftRInt64X8"
  shiftRInt64X8# :: Int64X8# -> Int# -> Int64X8#

foreign import ccall unsafe "hs_simdy_shiftRWord8X64"
  shiftRWord8X64# :: Word8X64# -> Int# -> Word8X64#

foreign import ccall unsafe "hs_simdy_shiftRWord16X32"
  shiftRWord16X32# :: Word16X32# -> Int# -> Word16X32#

foreign import ccall unsafe "hs_simdy_shiftRWord32X16"
  shiftRWord32X16# :: Word32X16# -> Int# -> Word32X16#

foreign import ccall unsafe "hs_simdy_shiftRWord64X8"
  shiftRWord64X8# :: Word64X8# -> Int# -> Word64X8#

--
-- Floating-point minimum/maximum (IEEE compliant)
--

foreign import ccall unsafe "hs_simdy_minimumFloatX16"
  minimumFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_maximumFloatX16"
  maximumFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_minimumNumberFloatX16"
  minimumNumberFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_maximumNumberFloatX16"
  maximumNumberFloatX16# :: FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_minimumDoubleX8"
  minimumDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#

foreign import ccall unsafe "hs_simdy_maximumDoubleX8"
  maximumDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#

foreign import ccall unsafe "hs_simdy_minimumNumberDoubleX8"
  minimumNumberDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#

foreign import ccall unsafe "hs_simdy_maximumNumberDoubleX8"
  maximumNumberDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8#

--
-- Equality
--

foreign import ccall unsafe "hs_simdy_eqInt8X64_densemask"
  eqInt8X64# :: Int8X64# -> Int8X64# -> Word64

foreign import ccall unsafe "hs_simdy_eqInt16X32_densemask"
  eqInt16X32# :: Int16X32# -> Int16X32# -> Word32

foreign import ccall unsafe "hs_simdy_eqInt32X16_densemask"
  eqInt32X16# :: Int32X16# -> Int32X16# -> Word16

foreign import ccall unsafe "hs_simdy_eqInt64X8_densemask"
  eqInt64X8# :: Int64X8# -> Int64X8# -> Word8

foreign import ccall unsafe "hs_simdy_eqInt8X64_densemask"
  eqWord8X64# :: Word8X64# -> Word8X64# -> Word64

foreign import ccall unsafe "hs_simdy_eqInt16X32_densemask"
  eqWord16X32# :: Word16X32# -> Word16X32# -> Word32

foreign import ccall unsafe "hs_simdy_eqInt32X16_densemask"
  eqWord32X16# :: Word32X16# -> Word32X16# -> Word16

foreign import ccall unsafe "hs_simdy_eqInt64X8_densemask"
  eqWord64X8# :: Word64X8# -> Word64X8# -> Word8

foreign import ccall unsafe "hs_simdy_eqFloatX16_densemask"
  eqFloatX16# :: FloatX16# -> FloatX16# -> Word16

foreign import ccall unsafe "hs_simdy_eqDoubleX8_densemask"
  eqDoubleX8# :: DoubleX8# -> DoubleX8# -> Word8

--
-- Less than
--

foreign import ccall unsafe "hs_simdy_ltInt8X64_densemask"
  ltInt8X64# :: Int8X64# -> Int8X64# -> Word64

foreign import ccall unsafe "hs_simdy_ltInt16X32_densemask"
  ltInt16X32# :: Int16X32# -> Int16X32# -> Word32

foreign import ccall unsafe "hs_simdy_ltInt32X16_densemask"
  ltInt32X16# :: Int32X16# -> Int32X16# -> Word16

foreign import ccall unsafe "hs_simdy_ltInt64X8_densemask"
  ltInt64X8# :: Int64X8# -> Int64X8# -> Word8

foreign import ccall unsafe "hs_simdy_ltWord8X64_densemask"
  ltWord8X64# :: Word8X64# -> Word8X64# -> Word64

foreign import ccall unsafe "hs_simdy_ltWord16X32_densemask"
  ltWord16X32# :: Word16X32# -> Word16X32# -> Word32

foreign import ccall unsafe "hs_simdy_ltWord32X16_densemask"
  ltWord32X16# :: Word32X16# -> Word32X16# -> Word16

foreign import ccall unsafe "hs_simdy_ltWord64X8_densemask"
  ltWord64X8# :: Word64X8# -> Word64X8# -> Word8

foreign import ccall unsafe "hs_simdy_ltFloatX16_densemask"
  ltFloatX16# :: FloatX16# -> FloatX16# -> Word16

foreign import ccall unsafe "hs_simdy_ltDoubleX8_densemask"
  ltDoubleX8# :: DoubleX8# -> DoubleX8# -> Word8

--
-- Less than or equal to
--

foreign import ccall unsafe "hs_simdy_leFloatX16_densemask"
  leFloatX16# :: FloatX16# -> FloatX16# -> Word16

foreign import ccall unsafe "hs_simdy_leDoubleX8_densemask"
  leDoubleX8# :: DoubleX8# -> DoubleX8# -> Word8

--
-- Greater than
--

foreign import ccall unsafe "hs_simdy_gtFloatX16_densemask"
  gtFloatX16# :: FloatX16# -> FloatX16# -> Word16

foreign import ccall unsafe "hs_simdy_gtDoubleX8_densemask"
  gtDoubleX8# :: DoubleX8# -> DoubleX8# -> Word8

--
-- Greater than or equal to
--

foreign import ccall unsafe "hs_simdy_geFloatX16_densemask"
  geFloatX16# :: FloatX16# -> FloatX16# -> Word16

foreign import ccall unsafe "hs_simdy_geDoubleX8_densemask"
  geDoubleX8# :: DoubleX8# -> DoubleX8# -> Word8

{-
--
-- Unordered
--

foreign import ccall unsafe "hs_simdy_unordFloatX16_densemask"
  unordFloatX16# :: FloatX16# -> FloatX16# -> Word16

foreign import ccall unsafe "hs_simdy_unordDoubleX8_densemask"
  unordDoubleX8# :: DoubleX8# -> DoubleX8# -> Word8
-}

--
-- FMA
--
#if !MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
#if MIN_VERSION_base(4, 19, 0)
-- GHC 9.8 or later
-- Let's hope LLVM's optimizer does a good job!

fmaddFloatX16# :: FloatX16# -> FloatX16# -> FloatX16# -> FloatX16#
fmaddFloatX16# x y z = case unpackFloatX16# x of
  (# x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15 #) ->
    case unpackFloatX16# y of
      (# y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15 #) ->
        case unpackFloatX16# z of
          (# z0, z1, z2, z3, z4, z5, z6, z7, z8, z9, z10, z11, z12, z13, z14, z15 #) ->
            packFloatX16#
              (# fmaddFloat# x0 y0 z0
               , fmaddFloat# x1 y1 z1
               , fmaddFloat# x2 y2 z2
               , fmaddFloat# x3 y3 z3
               , fmaddFloat# x4 y4 z4
               , fmaddFloat# x5 y5 z5
               , fmaddFloat# x6 y6 z6
               , fmaddFloat# x7 y7 z7
               , fmaddFloat# x8 y8 z8
               , fmaddFloat# x9 y9 z9
               , fmaddFloat# x10 y10 z10
               , fmaddFloat# x11 y11 z11
               , fmaddFloat# x12 y12 z12
               , fmaddFloat# x13 y13 z13
               , fmaddFloat# x14 y14 z14
               , fmaddFloat# x15 y15 z15
               #)
{-# INLINE fmaddFloatX16# #-}

fmaddDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8# -> DoubleX8#
fmaddDoubleX8# x y z = case unpackDoubleX8# x of
  (# x0, x1, x2, x3, x4, x5, x6, x7 #) ->
    case unpackDoubleX8# y of
      (# y0, y1, y2, y3, y4, y5, y6, y7 #) ->
        case unpackDoubleX8# z of
          (# z0, z1, z2, z3, z4, z5, z6, z7 #) ->
            packDoubleX8#
              (# fmaddDouble# x0 y0 z0
               , fmaddDouble# x1 y1 z1
               , fmaddDouble# x2 y2 z2
               , fmaddDouble# x3 y3 z3
               , fmaddDouble# x4 y4 z4
               , fmaddDouble# x5 y5 z5
               , fmaddDouble# x6 y6 z6
               , fmaddDouble# x7 y7 z7
               #)
{-# INLINE fmaddDoubleX8# #-}

#else

foreign import ccall unsafe "hs_simdy_fmaddFloatX16"
  fmaddFloatX16# :: FloatX16# -> FloatX16# -> FloatX16# -> FloatX16#

foreign import ccall unsafe "hs_simdy_fmaddDoubleX8"
  fmaddDoubleX8# :: DoubleX8# -> DoubleX8# -> DoubleX8# -> DoubleX8#

#endif
#endif
