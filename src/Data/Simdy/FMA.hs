-- |
-- Fused multiply-add (FMA) operations for SIMD vectors and scalars.
--
-- FMA computes @x * y + z@ in a single operation with only one rounding step,
-- which is both faster and more accurate than separate multiply and add.
--
-- The 'HasFMA' class and 'isFMAAvailable' allow runtime detection of FMA support.
-- To enable automatic fusion of @a * b + c@ expressions, import "Data.Simdy.Fusible"
-- instead of 'Prelude' arithmetic.
module Data.Simdy.FMA
  ( -- * FMA class
    FusedMultiplyAdd (..)
    -- * SIMD FMA constraint
  , SIMDFMA
    -- * FMA availability
  , HasFMA
  , FMAWitness (..)
  , isFMAAvailable
  ) where
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default
