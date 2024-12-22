{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Data.Simdy.Internal.SIMD256.Prim
  ( module M
  , module Data.Simdy.Internal.SIMD256.Prim
  ) where
#if __GLASGOW_HASKELL__ == 912 && !MIN_VERSION_GLASGOW_HASKELL(9, 12, 2, 0)
-- GHC 9.12.1 has a bug with broadcast: https://gitlab.haskell.org/ghc/ghc/-/issues/25561

import           GHC.Exts as M hiding (broadcastFloatX8#, broadcastDoubleX4#, broadcastInt8X32#, broadcastInt16X16#, broadcastInt32X8#, broadcastInt64X4#, broadcastWord8X32#, broadcastWord16X16#, broadcastWord32X8#, broadcastWord64X4#)
import qualified GHC.Exts

broadcastFloatX8# :: Float# -> FloatX8#
broadcastFloatX8# = GHC.Exts.broadcastFloatX8#
{-# NOINLINE broadcastFloatX8# #-}

broadcastDoubleX4# :: Double# -> DoubleX4#
broadcastDoubleX4# = GHC.Exts.broadcastDoubleX4#
{-# NOINLINE broadcastDoubleX4# #-}

broadcastInt8X32# :: Int8# -> Int8X32#
broadcastInt8X32# = GHC.Exts.broadcastInt8X32#
{-# NOINLINE broadcastInt8X32# #-}

broadcastInt16X16# :: Int16# -> Int16X16#
broadcastInt16X16# = GHC.Exts.broadcastInt16X16#
{-# NOINLINE broadcastInt16X16# #-}

broadcastInt32X8# :: Int32# -> Int32X8#
broadcastInt32X8# = GHC.Exts.broadcastInt32X8#
{-# NOINLINE broadcastInt32X8# #-}

broadcastInt64X4# :: Int64# -> Int64X4#
broadcastInt64X4# = GHC.Exts.broadcastInt64X4#
{-# NOINLINE broadcastInt64X4# #-}

broadcastWord8X32# :: Word8# -> Word8X32#
broadcastWord8X32# = GHC.Exts.broadcastWord8X32#
{-# NOINLINE broadcastWord8X32# #-}

broadcastWord16X16# :: Word16# -> Word16X16#
broadcastWord16X16# = GHC.Exts.broadcastWord16X16#
{-# NOINLINE broadcastWord16X16# #-}

broadcastWord32X8# :: Word32# -> Word32X8#
broadcastWord32X8# = GHC.Exts.broadcastWord32X8#
{-# NOINLINE broadcastWord32X8# #-}

broadcastWord64X4# :: Word64# -> Word64X4#
broadcastWord64X4# = GHC.Exts.broadcastWord64X4#
{-# NOINLINE broadcastWord64X4# #-}

#else

import           GHC.Exts as M

#endif
