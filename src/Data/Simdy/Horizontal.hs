module Data.Simdy.Horizontal
  ( horizontalFold
  , horizontalSum
  , horizontalProduct
  , horizontalMin
  , horizontalMax
  , horizontalMinimumNumber
  , horizontalMaximumNumber
  , horizontalAnd
  , horizontalOr
  , horizontalXor
  ) where
import           Data.Simdy.Class.Bits
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default
import           Prelude hiding (min, max)

horizontalSum :: (SIMD f, SIMDNum a) => f a -> a
horizontalSum = horizontalFold (+)
{-# INLINE horizontalSum #-}

horizontalProduct :: (SIMD f, SIMDNum a) => f a -> a
horizontalProduct = horizontalFold (*)
{-# INLINE horizontalProduct #-}

horizontalMin :: (SIMD f, SIMDMinMax a) => f a -> a
horizontalMin = horizontalFold min
{-# INLINE horizontalMin #-}

horizontalMax :: (SIMD f, SIMDMinMax a) => f a -> a
horizontalMax = horizontalFold max
{-# INLINE horizontalMax #-}

horizontalMinimumNumber :: (SIMD f, SIMDMinMax a) => f a -> a
horizontalMinimumNumber = horizontalFold minimumNumber
{-# INLINE horizontalMinimumNumber #-}

horizontalMaximumNumber :: (SIMD f, SIMDMinMax a) => f a -> a
horizontalMaximumNumber = horizontalFold maximumNumber
{-# INLINE horizontalMaximumNumber #-}

horizontalAnd :: (SIMD f, SIMDBoolean a) => f a -> a
horizontalAnd = horizontalFold (.&.)
{-# INLINE horizontalAnd #-}

horizontalOr :: (SIMD f, SIMDBoolean a) => f a -> a
horizontalOr = horizontalFold (.|.)
{-# INLINE horizontalOr #-}

horizontalXor :: (SIMD f, SIMDBoolean a) => f a -> a
horizontalXor = horizontalFold xor
{-# INLINE horizontalXor #-}
