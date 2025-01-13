module Data.Simdy.Horizontal
  ( horizontalFold
  , horizontalSum
  , horizontalProduct
  ) where
import           Data.Simdy.Internal.Default

horizontalSum :: (SIMD f, SIMDNum a) => f a -> a
horizontalSum = horizontalFold (+)
{-# INLINE horizontalSum #-}

horizontalProduct :: (SIMD f, SIMDNum a) => f a -> a
horizontalProduct = horizontalFold (*)
{-# INLINE horizontalProduct #-}

-- TODO: min, max, and, or, bitand, bitor, bitxor
