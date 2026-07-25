-- |
-- The main entry point for the simdy library.
--
-- This module re-exports the core SIMD type classes and vector types.
-- The concrete SIMD backend (128-bit, 256-bit, 512-bit, or scalar fallback)
-- is selected at compile time based on cabal flags.
--
-- = Quick start
--
-- > import Data.Simdy
-- >
-- > -- Vector operations work polymorphically over all widths:
-- > addVectors :: (SIMD f, SIMDNum a) => f a -> f a -> f a
-- > addVectors x y = x + y
--
-- = Available vector widths
--
-- * 'X2' &#x2014; 2 lanes
-- * 'X4' &#x2014; 4 lanes
-- * 'X8' &#x2014; 8 lanes
-- * 'X16' &#x2014; 16 lanes
-- * 'X32' &#x2014; 32 lanes
-- * 'X64' &#x2014; 64 lanes
-- * t'Identity' &#x2014; 1 lane (scalar, always available)
module Data.Simdy
  ( -- * Classes
    SIMD
  , SIMDElement
  , broadcast
  , liftSIMD
  , liftSIMD2
  , selectSIMD
  , SIMDEq
  , (==^)
  , (/=^)
  , SIMDOrd
  , (<^)
  , (<=^)
  , (>^)
  , (>=^)
  , SIMDNum
  , SIMDFractional
  , SIMDFloating
  , SIMDBoolean
  , SIMDBits
  , SIMDMinMax
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
  , X64
  , Identity (Identity)
  ) where
import           Data.Functor.Identity (Identity (Identity))
import           Data.Simdy.Internal.Class hiding (broadcast, liftSIMD, liftSIMD2)
import           Data.Simdy.Internal.Default
