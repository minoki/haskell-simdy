-- |
-- Comparison and masking operations for SIMD vectors.
--
-- SIMD comparisons return a 'Mask' (a vector of booleans) rather than a single 'Bool'.
-- Use 'selectSIMD' to choose lanes based on a mask, or 'Boolean' operators to combine masks.
--
-- The @(==^)@, @(/=^)@, @(\<^)@, @(\<=^)@, @(>^)@, @(>=^)@ operators are
-- convenience wrappers that avoid name clashes with "Prelude".
module Data.Simdy.Mask
  ( -- * Boolean algebra on masks
    Boolean (..)
    -- * Mask type
  , Mask
    -- * Conditional selection
  , selectSIMD
  , Selectable (..)
    -- * Equality comparison
  , Equatable (..)
  , (==^)
  , (/=^)
    -- * Ordering comparison
  , Ordered (..)
  , (<^)
  , (<=^)
  , (>^)
  , (>=^)
  ) where
import           Data.Simdy.Internal.Bits
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default
