{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-dodgy-exports #-}
module Data.Simdy.Internal.SIMD512.Prim
  ( module M
  , module Data.Simdy.Internal.SIMD512.Prim
  ) where
#if __GLASGOW_HASKELL__ == 912 && !MIN_VERSION_GLASGOW_HASKELL(9, 12, 2, 0) && defined(USE_LLVM_BACKEND)
-- GHC 9.12.1 has a bug with broadcast: https://gitlab.haskell.org/ghc/ghc/-/issues/25561

import           GHC.Exts as M hiding (broadcastFloatX16#, broadcastDoubleX8#, broadcastInt8X64#, broadcastInt16X32#, broadcastInt32X16#, broadcastInt64X8#, broadcastWord8X64#, broadcastWord16X32#, broadcastWord32X16#, broadcastWord64X8#)
import qualified GHC.Exts

broadcastFloatX16# :: Float# -> FloatX16#
broadcastFloatX16# = GHC.Exts.broadcastFloatX16#
{-# NOINLINE broadcastFloatX16# #-}

broadcastDoubleX8# :: Double# -> DoubleX8#
broadcastDoubleX8# = GHC.Exts.broadcastDoubleX8#
{-# NOINLINE broadcastDoubleX8# #-}

broadcastInt8X64# :: Int8# -> Int8X64#
broadcastInt8X64# = GHC.Exts.broadcastInt8X64#
{-# NOINLINE broadcastInt8X64# #-}

broadcastInt16X32# :: Int16# -> Int16X32#
broadcastInt16X32# = GHC.Exts.broadcastInt16X32#
{-# NOINLINE broadcastInt16X32# #-}

broadcastInt32X16# :: Int32# -> Int32X16#
broadcastInt32X16# = GHC.Exts.broadcastInt32X16#
{-# NOINLINE broadcastInt32X16# #-}

broadcastInt64X8# :: Int64# -> Int64X8#
broadcastInt64X8# = GHC.Exts.broadcastInt64X8#
{-# NOINLINE broadcastInt64X8# #-}

broadcastWord8X64# :: Word8# -> Word8X64#
broadcastWord8X64# = GHC.Exts.broadcastWord8X64#
{-# NOINLINE broadcastWord8X64# #-}

broadcastWord16X32# :: Word16# -> Word16X32#
broadcastWord16X32# = GHC.Exts.broadcastWord16X32#
{-# NOINLINE broadcastWord16X32# #-}

broadcastWord32X16# :: Word32# -> Word32X16#
broadcastWord32X16# = GHC.Exts.broadcastWord32X16#
{-# NOINLINE broadcastWord32X16# #-}

broadcastWord64X8# :: Word64# -> Word64X8#
broadcastWord64X8# = GHC.Exts.broadcastWord64X8#
{-# NOINLINE broadcastWord64X8# #-}

#else

import           GHC.Exts as M

#endif
