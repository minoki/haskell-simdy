{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Simdy.Internal.PrimExtra where
import GHC.Exts

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
