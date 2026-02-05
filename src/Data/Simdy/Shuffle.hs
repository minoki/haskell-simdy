{-# LANGUAGE CPP #-}
module Data.Simdy.Shuffle
  (
#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 0, 0)
    unaryShuffleX2
  , unaryShuffleX4
  , unaryShuffleX8
  , unaryShuffleX16
  , unaryShuffleX32
  , unaryShuffleX64
  , binaryShuffleX2
  , binaryShuffleX4
  , binaryShuffleX8
  , binaryShuffleX16
  , binaryShuffleX32
  , binaryShuffleX64
  ,
#endif
    unaryShuffleWithX2
  , unaryShuffleWithX4
  , unaryShuffleWithX8
  , unaryShuffleWithX16
  , unaryShuffleWithX32
  , unaryShuffleWithX64
  , binaryShuffleWithX2
  , binaryShuffleWithX4
  , binaryShuffleWithX8
  , binaryShuffleWithX16
  , binaryShuffleWithX32
  , binaryShuffleWithX64
  , UnaryShuffle (..)
  , BinaryShuffle (..)
  ) where
import           Data.Simdy.Internal.Default
import           Data.Simdy.Internal.Shuffle
