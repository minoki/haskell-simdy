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
    --
    -- The index function receives a tuple of proxies representing lane positions
    -- and returns a tuple selecting which lane to place at each output position.
    --
    -- @
    -- -- Reverse the 4 lanes of an X4 vector:
    -- --   [a, b, c, d] -> [d, c, b, a]
    -- unaryShuffleWithX4 (\\(x0, x1, x2, x3) -> (x3, x2, x1, x0)) vec
    --
    -- -- Broadcast lane 0 to all positions:
    -- --   [a, b, c, d] -> [a, a, a, a]
    -- unaryShuffleWithX4 (\\(x0, _x1, _x2, _x3) -> (x0, x0, x0, x0)) vec
    --
    -- -- Swap adjacent pairs:
    -- --   [a, b, c, d] -> [b, a, d, c]
    -- unaryShuffleWithX4 (\\(x0, x1, x2, x3) -> (x1, x0, x3, x2)) vec
    -- @
    unaryShuffleWithX2
  , unaryShuffleWithX4
  , unaryShuffleWithX8
  , unaryShuffleWithX16
  , unaryShuffleWithX32
  , unaryShuffleWithX64
    -- * Binary shuffle (proxy-based)
    -- | Select lanes from two concatenated vectors using a proxy to specify indices.
    -- The index function receives two tuples of proxies: the first represents lanes
    -- of the first vector (@0..n-1@), and the second represents lanes of the second
    -- vector (@n..2n-1@). It returns a tuple selecting which lanes to place at each
    -- output position.
    --
    -- @
    -- -- Interleave lanes from two X4 vectors:
    -- --   [a, b, c, d] [e, f, g, h] -> [a, e, b, f]
    -- binaryShuffleWithX4
    --   (\\(x0, x1, _x2, _x3) (x4, x5, _x6, _x7) -> (x0, x4, x1, x5)) u v
    -- @
    --
    -- Note: for X4, the first tuple @(Proxy 0, Proxy 1, Proxy 2, Proxy 3)@
    -- represents lanes of the first vector and the second tuple
    -- @(Proxy 4, Proxy 5, Proxy 6, Proxy 7)@ represents lanes of the second.
    --
    -- @
    -- -- Interleave even-indexed lanes:
    -- --   [a, b, c, d] [e, f, g, h] -> [a, e, c, g]
    -- binaryShuffleWithX4
    --   (\\(x0, _x1, x2, _x3) (x4, _x5, x6, _x7) -> (x0, x4, x2, x6)) u v
    --
    -- -- Concatenate the second halves:
    -- --   [a, b, c, d] [e, f, g, h] -> [c, d, g, h]
    -- binaryShuffleWithX4
    --   (\\(_x0, _x1, x2, x3) (_x4, _x5, x6, x7) -> (x2, x3, x6, x7)) u v
    -- @
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
