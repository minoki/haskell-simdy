{-# LANGUAGE CPP #-}
module Data.Simdy
  ( -- * Classes
    SIMD
  , SIMDElement
  , broadcast
  , liftSIMD
  , liftSIMD2
  , SIMDNum
  , SIMDFractional
  , SIMDFloating
  , SIMDPrim
  , SIMDUnbox
  , SIMDStorable
  , HalfVector
  , SplitShortVector (splitShortVector, joinShortVector)
  , KnownSIMDLength (SIMDLength, simdLength)
    -- * SIMD vector types
  , X2
  , X4
  , X8
  , X16
  , X32
  , Identity (Identity)
  ) where
import           Data.Functor.Identity (Identity (Identity))
import           Data.Simdy.Internal.Class hiding (broadcast)
#if defined(USE_SIMD512)
import           Data.Simdy.Internal.SIMD512
#elif defined(USE_SIMD256)
import           Data.Simdy.Internal.SIMD256
#elif defined(USE_SIMD128)
import           Data.Simdy.Internal.SIMD128
#else
import           Data.Simdy.Internal.NoSIMD
#endif
