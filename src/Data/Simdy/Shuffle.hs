{-# LANGUAGE CPP #-}
-- |
-- Lane-reordering (shuffle) operations on SIMD vectors.
--
-- Shuffles rearrange vector lanes according to compile-time index lists.
-- Two families of operations are provided:
--
-- * @unaryShuffleX/N/@ \/ @binaryShuffleX/N/@ (GHC 9.10+): Use @RequiredTypeArguments@
--   to pass indices as type-level tuples.
-- * @unaryShuffleWithX/N/@ \/ @binaryShuffleWithX/N/@: Use proxy-based API that works
--   on all supported GHC versions.
--
-- A compiler plugin ("Data.Simdy.Shuffle.Plugin") is required to solve
-- the underlying 'ShuffleMany' constraints.
module Data.Simdy.Shuffle
  (
#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 0, 0)
    -- * Unary shuffle (RequiredTypeArguments)
    -- | Rearrange lanes of a single vector. Pass indices as a type-level tuple:
    --
    -- @unaryShuffleX4 vec (3, 2, 1, 0)  -- reverse lanes@
    unaryShuffleX2
  , unaryShuffleX4
  , unaryShuffleX8
  , unaryShuffleX16
  , unaryShuffleX32
  , unaryShuffleX64
    -- * Binary shuffle (RequiredTypeArguments)
    -- | Select lanes from two concatenated vectors. Pass indices as a type-level tuple:
    --
    -- @binaryShuffleX4 u v (0, 4, 1, 5)  -- interleave lanes@
  , binaryShuffleX2
  , binaryShuffleX4
  , binaryShuffleX8
  , binaryShuffleX16
  , binaryShuffleX32
  , binaryShuffleX64
  ,
#endif
    -- * Unary shuffle (proxy-based)
    -- | Rearrange lanes of a single vector using a proxy to specify indices.
    -- Works on all supported GHC versions.
    unaryShuffleWithX2
  , unaryShuffleWithX4
  , unaryShuffleWithX8
  , unaryShuffleWithX16
  , unaryShuffleWithX32
  , unaryShuffleWithX64
    -- * Binary shuffle (proxy-based)
    -- | Select lanes from two concatenated vectors using a proxy to specify indices.
  , binaryShuffleWithX2
  , binaryShuffleWithX4
  , binaryShuffleWithX8
  , binaryShuffleWithX16
  , binaryShuffleWithX32
  , binaryShuffleWithX64
    -- * Shuffle classes
    -- | Rearrange lanes of a single vector according to compile-time indices.
  , UnaryShuffle (..)
    -- | Select lanes from two concatenated vectors according to compile-time indices.
    -- Indices @0..n-1@ select from the first vector, @n..2n-1@ from the second.
  , BinaryShuffle (..)
  ) where
import           Data.Simdy.Internal.Default
import           Data.Simdy.Internal.Shuffle
