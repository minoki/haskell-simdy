-- |
-- Lane-wise minimum and maximum operations.
--
-- 'MinMax' provides IEEE 754-2019 compliant min\/max for floating-point types:
--
-- * 'min' \/ 'max' propagate NaN.
-- * 'minimumNumber' \/ 'maximumNumber' prefer numeric values over NaN.
module Data.Simdy.MinMax
  ( -- | Lane-wise minimum and maximum. For floating-point types, 'min'\/'max'
    -- follow IEEE 754-2019 semantics (propagating NaN), while
    -- 'minimumNumber'\/'maximumNumber' prefer numeric values over NaN.
    MinMax (..)
    -- | Constraint for element types that support lane-wise min\/max in SIMD vectors.
  , SIMDMinMax
  ) where
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default (SIMDMinMax)
