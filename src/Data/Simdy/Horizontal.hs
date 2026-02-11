-- |
-- Horizontal (cross-lane) reductions on SIMD vectors.
--
-- These functions reduce all lanes of a SIMD vector to a single scalar value.
-- Internally they use recursive halving via 'Data.Simdy.Split.SplitShortVector'.
module Data.Simdy.Horizontal
  ( -- * General fold
    horizontalFold
    -- * Arithmetic reductions
  , horizontalSum
  , horizontalProduct
    -- * Min\/max reductions
  , horizontalMin
  , horizontalMax
  , horizontalMinimumNumber
  , horizontalMaximumNumber
    -- * Bitwise reductions
  , horizontalAnd
  , horizontalOr
  , horizontalXor
  ) where
import           Data.Simdy.Class.Bits
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default
import           Prelude hiding (min, max)

-- | Sum all lanes of a vector.
horizontalSum :: (SIMD f, SIMDNum a) => f a -> a
horizontalSum = horizontalFold (+)
{-# INLINE horizontalSum #-}

-- | Multiply all lanes of a vector.
horizontalProduct :: (SIMD f, SIMDNum a) => f a -> a
horizontalProduct = horizontalFold (*)
{-# INLINE horizontalProduct #-}

-- | Minimum of all lanes (IEEE 754-2019: NaN-propagating).
horizontalMin :: (SIMD f, SIMDMinMax a) => f a -> a
horizontalMin = horizontalFold min
{-# INLINE horizontalMin #-}

-- | Maximum of all lanes (IEEE 754-2019: NaN-propagating).
horizontalMax :: (SIMD f, SIMDMinMax a) => f a -> a
horizontalMax = horizontalFold max
{-# INLINE horizontalMax #-}

-- | Minimum of all lanes, preferring numeric values over NaN.
horizontalMinimumNumber :: (SIMD f, SIMDMinMax a) => f a -> a
horizontalMinimumNumber = horizontalFold minimumNumber
{-# INLINE horizontalMinimumNumber #-}

-- | Maximum of all lanes, preferring numeric values over NaN.
horizontalMaximumNumber :: (SIMD f, SIMDMinMax a) => f a -> a
horizontalMaximumNumber = horizontalFold maximumNumber
{-# INLINE horizontalMaximumNumber #-}

-- | Bitwise AND of all lanes.
horizontalAnd :: (SIMD f, SIMDBoolean a) => f a -> a
horizontalAnd = horizontalFold (.&.)
{-# INLINE horizontalAnd #-}

-- | Bitwise OR of all lanes.
horizontalOr :: (SIMD f, SIMDBoolean a) => f a -> a
horizontalOr = horizontalFold (.|.)
{-# INLINE horizontalOr #-}

-- | Bitwise XOR of all lanes.
horizontalXor :: (SIMD f, SIMDBoolean a) => f a -> a
horizontalXor = horizontalFold xor
{-# INLINE horizontalXor #-}
