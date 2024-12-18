{-# LANGUAGE CPP #-}
module Data.Simdy
  ( -- * Classes
    SIMD
  , SIMDElement
  , liftSIMD
  , liftSIMD2
  , MultiNum
  , MultiFractional
  , MultiFloating
  , MultiPrim
  , MultiUnbox
  , MultiStorable
  , HalfVector
  , SplitShortVector (splitShortVector, joinShortVector)
  , Broadcast (broadcast)
  , ShortVectorLength (shortVectorLength)
    -- * SIMD vector types
  , X2
  , X4
  , X8
  , X16
  , X32
  , Identity (Identity)
  ) where
import           Data.Functor.Identity (Identity (Identity))
import           Data.Simdy.Class
#if defined(USE_SIMD512)
import           Data.Simdy.Internal.SIMD512
#elif defined(USE_SIMD256)
import           Data.Simdy.Internal.SIMD256
#elif defined(USE_SIMD128)
import           Data.Simdy.Internal.SIMD128
#else
import           Data.Simdy.Internal.NoSIMD
#endif
