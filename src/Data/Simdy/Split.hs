-- |
-- Split and join SIMD vectors by halves.
--
-- 'HalfVector' maps a vector type to its half-width counterpart (e.g. @HalfVector X8 = X4@).
-- 'SplitShortVector' provides 'splitShortVector' and 'joinShortVector' for
-- decomposing and recomposing vectors. This is used internally by horizontal
-- reductions ('Data.Simdy.Horizontal.horizontalFold').
module Data.Simdy.Split
  ( HalfVector
  , SplitShortVector (splitShortVector, joinShortVector)
  ) where
import           Data.Simdy.Internal.Class
