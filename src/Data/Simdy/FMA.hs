-- |
-- Fused multiply-add (FMA) operations for SIMD vectors and scalars.
--
-- FMA computes @x * y + z@ in a single operation with only one rounding step,
-- which is both faster and more accurate than separate multiply and add.
--
-- The 'HasFMA' class and 'isFMAAvailable' reflect whether FMA support was enabled
-- at compile time via Cabal package flags (@haswell@, @avx512@) or target architecture
-- (@aarch64@).  They do __not__ perform runtime CPUID detection.
--
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
