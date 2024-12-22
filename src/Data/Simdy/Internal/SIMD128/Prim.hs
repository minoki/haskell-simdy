{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Data.Simdy.Internal.SIMD128.Prim
  ( module M
  , module Data.Simdy.Internal.SIMD128.Prim
  ) where
#if __GLASGOW_HASKELL__ == 912 && !MIN_VERSION_GLASGOW_HASKELL(9, 12, 2, 0)
-- GHC 9.12.1 has a bug with broadcast: https://gitlab.haskell.org/ghc/ghc/-/issues/25561

import           GHC.Exts as M hiding (broadcastFloatX4#, broadcastDoubleX2#, broadcastInt8X16#, broadcastInt16X8#, broadcastInt32X4#, broadcastInt64X2#, broadcastWord8X16#, broadcastWord16X8#, broadcastWord32X4#, broadcastWord64X2#)
import qualified GHC.Exts

broadcastFloatX4# :: Float# -> FloatX4#
broadcastFloatX4# = GHC.Exts.broadcastFloatX4#
{-# NOINLINE broadcastFloatX4# #-}

broadcastDoubleX2# :: Double# -> DoubleX2#
broadcastDoubleX2# = GHC.Exts.broadcastDoubleX2#
{-# NOINLINE broadcastDoubleX2# #-}

broadcastInt8X16# :: Int8# -> Int8X16#
broadcastInt8X16# = GHC.Exts.broadcastInt8X16#
{-# NOINLINE broadcastInt8X16# #-}

broadcastInt16X8# :: Int16# -> Int16X8#
broadcastInt16X8# = GHC.Exts.broadcastInt16X8#
{-# NOINLINE broadcastInt16X8# #-}

broadcastInt32X4# :: Int32# -> Int32X4#
broadcastInt32X4# = GHC.Exts.broadcastInt32X4#
{-# NOINLINE broadcastInt32X4# #-}

broadcastInt64X2# :: Int64# -> Int64X2#
broadcastInt64X2# = GHC.Exts.broadcastInt64X2#
{-# NOINLINE broadcastInt64X2# #-}

broadcastWord8X16# :: Word8# -> Word8X16#
broadcastWord8X16# = GHC.Exts.broadcastWord8X16#
{-# NOINLINE broadcastWord8X16# #-}

broadcastWord16X8# :: Word16# -> Word16X8#
broadcastWord16X8# = GHC.Exts.broadcastWord16X8#
{-# NOINLINE broadcastWord16X8# #-}

broadcastWord32X4# :: Word32# -> Word32X4#
broadcastWord32X4# = GHC.Exts.broadcastWord32X4#
{-# NOINLINE broadcastWord32X4# #-}

broadcastWord64X2# :: Word64# -> Word64X2#
broadcastWord64X2# = GHC.Exts.broadcastWord64X2#
{-# NOINLINE broadcastWord64X2# #-}

#else

import           GHC.Exts as M

#endif
