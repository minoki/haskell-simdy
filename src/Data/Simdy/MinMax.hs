-- |
-- Lane-wise minimum and maximum operations.
--
-- 'MinMax' provides IEEE 754-2019 compliant min\/max for floating-point types:
--
-- * 'min' \/ 'max' propagate NaN.
-- * 'minimumNumber' \/ 'maximumNumber' prefer numeric values over NaN.
module Data.Simdy.MinMax
  ( MinMax (..)
  , SIMDMinMax
  ) where
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default (SIMDMinMax)
