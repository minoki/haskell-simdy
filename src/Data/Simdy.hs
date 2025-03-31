module Data.Simdy
  ( -- * Classes
    SIMD
  , SIMDElement
  , broadcast
  , liftSIMD
  , liftSIMD2
  , SIMDEq
  , SIMDOrd
  , SIMDNum
  , SIMDFractional
  , SIMDFloating
  , SIMDPrim
  -- , SIMDUnbox
  , SIMDStorable
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
import           Data.Simdy.Internal.Class hiding (broadcast, liftSIMD, liftSIMD2)
import           Data.Simdy.Internal.Default
