{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Simdy.Internal.SIMD512.PrimExtra where
import Data.Word
import GHC.Exts

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

--
-- Complement (bitwise not)
--

foreign import ccall unsafe "hs_simdy_complement_int512"
  complementInt8X64# :: Int8X64# -> Int8X64#

foreign import ccall unsafe "hs_simdy_complement_int512"
  complementInt16X32# :: Int16X32# -> Int16X32#

foreign import ccall unsafe "hs_simdy_complement_int512"
  complementInt32X16# :: Int32X16# -> Int32X16#

foreign import ccall unsafe "hs_simdy_complement_int512"
  complementInt64X8# :: Int64X8# -> Int64X8#

foreign import ccall unsafe "hs_simdy_complement_int512"
  complementWord8X64# :: Word8X64# -> Word8X64#

foreign import ccall unsafe "hs_simdy_complement_int512"
  complementWord16X32# :: Word16X32# -> Word16X32#

foreign import ccall unsafe "hs_simdy_complement_int512"
  complementWord32X16# :: Word32X16# -> Word32X16#

foreign import ccall unsafe "hs_simdy_complement_int512"
  complementWord64X8# :: Word64X8# -> Word64X8#

--
-- Bitwise AND
--

foreign import ccall unsafe "hs_simdy_and_int512"
  andInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#

foreign import ccall unsafe "hs_simdy_and_int512"
  andInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#

foreign import ccall unsafe "hs_simdy_and_int512"
  andInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#

foreign import ccall unsafe "hs_simdy_and_int512"
  andInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#

foreign import ccall unsafe "hs_simdy_and_int512"
  andWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#

foreign import ccall unsafe "hs_simdy_and_int512"
  andWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#

foreign import ccall unsafe "hs_simdy_and_int512"
  andWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#

foreign import ccall unsafe "hs_simdy_and_int512"
  andWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#

--
-- Bitwise OR
--

foreign import ccall unsafe "hs_simdy_or_int512"
  orInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#

foreign import ccall unsafe "hs_simdy_or_int512"
  orInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#

foreign import ccall unsafe "hs_simdy_or_int512"
  orInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#

foreign import ccall unsafe "hs_simdy_or_int512"
  orInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#

foreign import ccall unsafe "hs_simdy_or_int512"
  orWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#

foreign import ccall unsafe "hs_simdy_or_int512"
  orWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#

foreign import ccall unsafe "hs_simdy_or_int512"
  orWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#

foreign import ccall unsafe "hs_simdy_or_int512"
  orWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#

--
-- Bitwise XOR
--

foreign import ccall unsafe "hs_simdy_xor_int512"
  xorInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#

foreign import ccall unsafe "hs_simdy_xor_int512"
  xorInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#

foreign import ccall unsafe "hs_simdy_xor_int512"
  xorInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#

foreign import ccall unsafe "hs_simdy_xor_int512"
  xorInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#

foreign import ccall unsafe "hs_simdy_xor_int512"
  xorWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#

foreign import ccall unsafe "hs_simdy_xor_int512"
  xorWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#

foreign import ccall unsafe "hs_simdy_xor_int512"
  xorWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#

foreign import ccall unsafe "hs_simdy_xor_int512"
  xorWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#

--
-- Left Shift
--

foreign import ccall unsafe "hs_simdy_shiftL_int8x64"
  shiftLInt8X64# :: Int8X64# -> Int# -> Int8X64#

foreign import ccall unsafe "hs_simdy_shiftL_int16x32"
  shiftLInt16X32# :: Int16X32# -> Int# -> Int16X32#

foreign import ccall unsafe "hs_simdy_shiftL_int32x16"
  shiftLInt32X16# :: Int32X16# -> Int# -> Int32X16#

foreign import ccall unsafe "hs_simdy_shiftL_int64x8"
  shiftLInt64X8# :: Int64X8# -> Int# -> Int64X8#

foreign import ccall unsafe "hs_simdy_shiftL_word8x64"
  shiftLWord8X64# :: Word8X64# -> Int# -> Word8X64#

foreign import ccall unsafe "hs_simdy_shiftL_word16x32"
  shiftLWord16X32# :: Word16X32# -> Int# -> Word16X32#

foreign import ccall unsafe "hs_simdy_shiftL_word32x16"
  shiftLWord32X16# :: Word32X16# -> Int# -> Word32X16#

foreign import ccall unsafe "hs_simdy_shiftL_word64x8"
  shiftLWord64X8# :: Word64X8# -> Int# -> Word64X8#

--
-- Right Shift (Arithmetic / Logical)
--

foreign import ccall unsafe "hs_simdy_shiftR_int8x64"
  shiftRInt8X64# :: Int8X64# -> Int# -> Int8X64#

foreign import ccall unsafe "hs_simdy_shiftR_int16x32"
  shiftRInt16X32# :: Int16X32# -> Int# -> Int16X32#

foreign import ccall unsafe "hs_simdy_shiftR_int32x16"
  shiftRInt32X16# :: Int32X16# -> Int# -> Int32X16#

foreign import ccall unsafe "hs_simdy_shiftR_int64x8"
  shiftRInt64X8# :: Int64X8# -> Int# -> Int64X8#

foreign import ccall unsafe "hs_simdy_shiftR_word8x64"
  shiftRWord8X64# :: Word8X64# -> Int# -> Word8X64#

foreign import ccall unsafe "hs_simdy_shiftR_word16x32"
  shiftRWord16X32# :: Word16X32# -> Int# -> Word16X32#

foreign import ccall unsafe "hs_simdy_shiftR_word32x16"
  shiftRWord32X16# :: Word32X16# -> Int# -> Word32X16#

foreign import ccall unsafe "hs_simdy_shiftR_word64x8"
  shiftRWord64X8# :: Word64X8# -> Int# -> Word64X8#

--
-- Floating-point minimum/maximum (IEEE compliant)
--

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

--
-- Equality
--

foreign import ccall unsafe "hs_simdy_int8x64_eq_densemask"
  eqInt8X64# :: Int8X64# -> Int8X64# -> Word64

foreign import ccall unsafe "hs_simdy_int16x32_eq_densemask"
  eqInt16X32# :: Int16X32# -> Int16X32# -> Word32

foreign import ccall unsafe "hs_simdy_int32x16_eq_densemask"
  eqInt32X16# :: Int32X16# -> Int32X16# -> Word16

foreign import ccall unsafe "hs_simdy_int64x8_eq_densemask"
  eqInt64X8# :: Int64X8# -> Int64X8# -> Word8

foreign import ccall unsafe "hs_simdy_int8x64_eq_densemask"
  eqWord8X64# :: Word8X64# -> Word8X64# -> Word64

foreign import ccall unsafe "hs_simdy_int16x32_eq_densemask"
  eqWord16X32# :: Word16X32# -> Word16X32# -> Word32

foreign import ccall unsafe "hs_simdy_int32x16_eq_densemask"
  eqWord32X16# :: Word32X16# -> Word32X16# -> Word16

foreign import ccall unsafe "hs_simdy_int64x8_eq_densemask"
  eqWord64X8# :: Word64X8# -> Word64X8# -> Word8

--
-- Less than
--

foreign import ccall unsafe "hs_simdy_int8x64_lt_densemask"
  ltInt8X64# :: Int8X64# -> Int8X64# -> Word64

foreign import ccall unsafe "hs_simdy_int16x32_lt_densemask"
  ltInt16X32# :: Int16X32# -> Int16X32# -> Word32

foreign import ccall unsafe "hs_simdy_int32x16_lt_densemask"
  ltInt32X16# :: Int32X16# -> Int32X16# -> Word16

foreign import ccall unsafe "hs_simdy_int64x8_lt_densemask"
  ltInt64X8# :: Int64X8# -> Int64X8# -> Word8

foreign import ccall unsafe "hs_simdy_word8x64_lt_densemask"
  ltWord8X64# :: Word8X64# -> Word8X64# -> Word64

foreign import ccall unsafe "hs_simdy_word16x32_lt_densemask"
  ltWord16X32# :: Word16X32# -> Word16X32# -> Word32

foreign import ccall unsafe "hs_simdy_word32x16_lt_densemask"
  ltWord32X16# :: Word32X16# -> Word32X16# -> Word16

foreign import ccall unsafe "hs_simdy_word64x8_lt_densemask"
  ltWord64X8# :: Word64X8# -> Word64X8# -> Word8
