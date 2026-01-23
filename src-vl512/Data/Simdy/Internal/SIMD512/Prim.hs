{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Simdy.Internal.SIMD512.Prim
  ( M.Int8X64#
  , M.Int16X32#
  , M.Int32X16#
  , M.Int64X8#
  , M.Word8X64#
  , M.Word16X32#
  , M.Word32X16#
  , M.Word64X8#
  , M.FloatX16#
  , M.DoubleX8#
  , broadcastInt8X64#
  , broadcastInt16X32#
  , broadcastInt32X16#
  , broadcastInt64X8#
  , broadcastWord8X64#
  , broadcastWord16X32#
  , broadcastWord32X16#
  , broadcastWord64X8#
  , broadcastFloatX16#
  , broadcastDoubleX8#
  , M.packInt8X64#
  , M.packInt16X32#
  , M.packInt32X16#
  , M.packInt64X8#
  , M.packWord8X64#
  , M.packWord16X32#
  , M.packWord32X16#
  , M.packWord64X8#
  , M.packFloatX16#
  , M.packDoubleX8#
  , M.unpackInt8X64#
  , M.unpackInt16X32#
  , M.unpackInt32X16#
  , M.unpackInt64X8#
  , M.unpackWord8X64#
  , M.unpackWord16X32#
  , M.unpackWord32X16#
  , M.unpackWord64X8#
  , M.unpackFloatX16#
  , M.unpackDoubleX8#
  , M.insertInt8X64#
  , M.insertInt16X32#
  , M.insertInt32X16#
  , M.insertInt64X8#
  , M.insertWord8X64#
  , M.insertWord16X32#
  , M.insertWord32X16#
  , M.insertWord64X8#
  , M.insertFloatX16#
  , M.insertDoubleX8#
  , M.plusInt8X64#
  , M.plusInt16X32#
  , M.plusInt32X16#
  , M.plusInt64X8#
  , M.plusWord8X64#
  , M.plusWord16X32#
  , M.plusWord32X16#
  , M.plusWord64X8#
  , M.plusFloatX16#
  , M.plusDoubleX8#
  , M.minusInt8X64#
  , M.minusInt16X32#
  , M.minusInt32X16#
  , M.minusInt64X8#
  , M.minusWord8X64#
  , M.minusWord16X32#
  , M.minusWord32X16#
  , M.minusWord64X8#
  , M.minusFloatX16#
  , M.minusDoubleX8#
  , M.timesInt8X64#
  , M.timesInt16X32#
  , M.timesInt32X16#
  , M.timesInt64X8#
  , M.timesWord8X64#
  , M.timesWord16X32#
  , M.timesWord32X16#
  , M.timesWord64X8#
  , M.timesFloatX16#
  , M.timesDoubleX8#
  , M.divideFloatX16#
  , M.divideDoubleX8#
  , M.quotInt8X64#
  , M.quotInt16X32#
  , M.quotInt32X16#
  , M.quotInt64X8#
  , M.quotWord8X64#
  , M.quotWord16X32#
  , M.quotWord32X16#
  , M.quotWord64X8#
  , M.remInt8X64#
  , M.remInt16X32#
  , M.remInt32X16#
  , M.remInt64X8#
  , M.remWord8X64#
  , M.remWord16X32#
  , M.remWord32X16#
  , M.remWord64X8#
  , M.negateInt8X64#
  , M.negateInt16X32#
  , M.negateInt32X16#
  , M.negateInt64X8#
  , M.negateFloatX16#
  , M.negateDoubleX8#
  , M.indexInt8X64Array#
  , M.indexInt16X32Array#
  , M.indexInt32X16Array#
  , M.indexInt64X8Array#
  , M.indexWord8X64Array#
  , M.indexWord16X32Array#
  , M.indexWord32X16Array#
  , M.indexWord64X8Array#
  , M.indexFloatX16Array#
  , M.indexDoubleX8Array#
  , M.readInt8X64Array#
  , M.readInt16X32Array#
  , M.readInt32X16Array#
  , M.readInt64X8Array#
  , M.readWord8X64Array#
  , M.readWord16X32Array#
  , M.readWord32X16Array#
  , M.readWord64X8Array#
  , M.readFloatX16Array#
  , M.readDoubleX8Array#
  , M.writeInt8X64Array#
  , M.writeInt16X32Array#
  , M.writeInt32X16Array#
  , M.writeInt64X8Array#
  , M.writeWord8X64Array#
  , M.writeWord16X32Array#
  , M.writeWord32X16Array#
  , M.writeWord64X8Array#
  , M.writeFloatX16Array#
  , M.writeDoubleX8Array#
  , M.indexInt8X64OffAddr#
  , M.indexInt16X32OffAddr#
  , M.indexInt32X16OffAddr#
  , M.indexInt64X8OffAddr#
  , M.indexWord8X64OffAddr#
  , M.indexWord16X32OffAddr#
  , M.indexWord32X16OffAddr#
  , M.indexWord64X8OffAddr#
  , M.indexFloatX16OffAddr#
  , M.indexDoubleX8OffAddr#
  , M.readInt8X64OffAddr#
  , M.readInt16X32OffAddr#
  , M.readInt32X16OffAddr#
  , M.readInt64X8OffAddr#
  , M.readWord8X64OffAddr#
  , M.readWord16X32OffAddr#
  , M.readWord32X16OffAddr#
  , M.readWord64X8OffAddr#
  , M.readFloatX16OffAddr#
  , M.readDoubleX8OffAddr#
  , M.writeInt8X64OffAddr#
  , M.writeInt16X32OffAddr#
  , M.writeInt32X16OffAddr#
  , M.writeInt64X8OffAddr#
  , M.writeWord8X64OffAddr#
  , M.writeWord16X32OffAddr#
  , M.writeWord32X16OffAddr#
  , M.writeWord64X8OffAddr#
  , M.writeFloatX16OffAddr#
  , M.writeDoubleX8OffAddr#
  , M.indexInt8ArrayAsInt8X64#
  , M.indexInt16ArrayAsInt16X32#
  , M.indexInt32ArrayAsInt32X16#
  , M.indexInt64ArrayAsInt64X8#
  , M.indexWord8ArrayAsWord8X64#
  , M.indexWord16ArrayAsWord16X32#
  , M.indexWord32ArrayAsWord32X16#
  , M.indexWord64ArrayAsWord64X8#
  , M.indexFloatArrayAsFloatX16#
  , M.indexDoubleArrayAsDoubleX8#
  , M.readInt8ArrayAsInt8X64#
  , M.readInt16ArrayAsInt16X32#
  , M.readInt32ArrayAsInt32X16#
  , M.readInt64ArrayAsInt64X8#
  , M.readWord8ArrayAsWord8X64#
  , M.readWord16ArrayAsWord16X32#
  , M.readWord32ArrayAsWord32X16#
  , M.readWord64ArrayAsWord64X8#
  , M.readFloatArrayAsFloatX16#
  , M.readDoubleArrayAsDoubleX8#
  , M.writeInt8ArrayAsInt8X64#
  , M.writeInt16ArrayAsInt16X32#
  , M.writeInt32ArrayAsInt32X16#
  , M.writeInt64ArrayAsInt64X8#
  , M.writeWord8ArrayAsWord8X64#
  , M.writeWord16ArrayAsWord16X32#
  , M.writeWord32ArrayAsWord32X16#
  , M.writeWord64ArrayAsWord64X8#
  , M.writeFloatArrayAsFloatX16#
  , M.writeDoubleArrayAsDoubleX8#
  , M.indexInt8OffAddrAsInt8X64#
  , M.indexInt16OffAddrAsInt16X32#
  , M.indexInt32OffAddrAsInt32X16#
  , M.indexInt64OffAddrAsInt64X8#
  , M.indexWord8OffAddrAsWord8X64#
  , M.indexWord16OffAddrAsWord16X32#
  , M.indexWord32OffAddrAsWord32X16#
  , M.indexWord64OffAddrAsWord64X8#
  , M.indexFloatOffAddrAsFloatX16#
  , M.indexDoubleOffAddrAsDoubleX8#
  , M.readInt8OffAddrAsInt8X64#
  , M.readInt16OffAddrAsInt16X32#
  , M.readInt32OffAddrAsInt32X16#
  , M.readInt64OffAddrAsInt64X8#
  , M.readWord8OffAddrAsWord8X64#
  , M.readWord16OffAddrAsWord16X32#
  , M.readWord32OffAddrAsWord32X16#
  , M.readWord64OffAddrAsWord64X8#
  , M.readFloatOffAddrAsFloatX16#
  , M.readDoubleOffAddrAsDoubleX8#
  , M.writeInt8OffAddrAsInt8X64#
  , M.writeInt16OffAddrAsInt16X32#
  , M.writeInt32OffAddrAsInt32X16#
  , M.writeInt64OffAddrAsInt64X8#
  , M.writeWord8OffAddrAsWord8X64#
  , M.writeWord16OffAddrAsWord16X32#
  , M.writeWord32OffAddrAsWord32X16#
  , M.writeWord64OffAddrAsWord64X8#
  , M.writeFloatOffAddrAsFloatX16#
  , M.writeDoubleOffAddrAsDoubleX8#
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
  , M.fmaddFloatX16#
  , M.fmaddDoubleX8#
  , M.fmsubFloatX16#
  , M.fmsubDoubleX8#
  , M.fnmaddFloatX16#
  , M.fnmaddDoubleX8#
  , M.fnmsubFloatX16#
  , M.fnmsubDoubleX8#
  , M.shuffleInt8X64#
  , M.shuffleInt16X32#
  , M.shuffleInt32X16#
  , M.shuffleInt64X8#
  , M.shuffleWord8X64#
  , M.shuffleWord16X32#
  , M.shuffleWord32X16#
  , M.shuffleWord64X8#
  , M.shuffleFloatX16#
  , M.shuffleDoubleX8#
  , M.minInt8X64#
  , M.minInt16X32#
  , M.minInt32X16#
  , M.minInt64X8#
  , M.minWord8X64#
  , M.minWord16X32#
  , M.minWord32X16#
  , M.minWord64X8#
  , M.minFloatX16#
  , M.minDoubleX8#
  , M.maxInt8X64#
  , M.maxInt16X32#
  , M.maxInt32X16#
  , M.maxInt64X8#
  , M.maxWord8X64#
  , M.maxWord16X32#
  , M.maxWord32X16#
  , M.maxWord64X8#
  , M.maxFloatX16#
  , M.maxDoubleX8#
#endif
  , module Data.Simdy.Internal.SIMD256.Prim
  ) where
import           Data.Simdy.Internal.SIMD256.Prim

#if defined(BROADCAST_IS_BROKEN)
-- The LLVM backend of GHC 9.12.{1,2} has a bug with broadcast: https://gitlab.haskell.org/ghc/ghc/-/issues/25561

import qualified GHC.PrimOps as M
import           GHC.PrimOps (Float#, Double#, Int8#, Int16#, Int32#, Int64#, Word8#, Word16#, Word32#, Word64#, FloatX16#, DoubleX8#, Int8X64#, Int16X32#, Int32X16#, Int64X8#, Word8X64#, Word16X32#, Word32X16#, Word64X8#)

broadcastFloatX16# :: Float# -> FloatX16#
broadcastFloatX16# x = M.packFloatX16# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastFloatX16# #-}

broadcastDoubleX8# :: Double# -> DoubleX8#
broadcastDoubleX8# x = M.packDoubleX8# (# x, x, x, x, x, x, x, x #)
{-# INLINE broadcastDoubleX8# #-}

broadcastInt8X64# :: Int8# -> Int8X64#
broadcastInt8X64# x = M.packInt8X64# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastInt8X64# #-}

broadcastInt16X32# :: Int16# -> Int16X32#
broadcastInt16X32# x = M.packInt16X32# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastInt16X32# #-}

broadcastInt32X16# :: Int32# -> Int32X16#
broadcastInt32X16# x = M.packInt32X16# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastInt32X16# #-}

broadcastInt64X8# :: Int64# -> Int64X8#
broadcastInt64X8# x = M.packInt64X8# (# x, x, x, x, x, x, x, x #)
{-# INLINE broadcastInt64X8# #-}

broadcastWord8X64# :: Word8# -> Word8X64#
broadcastWord8X64# x = M.packWord8X64# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastWord8X64# #-}

broadcastWord16X32# :: Word16# -> Word16X32#
broadcastWord16X32# x = M.packWord16X32# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastWord16X32# #-}

broadcastWord32X16# :: Word32# -> Word32X16#
broadcastWord32X16# x = M.packWord32X16# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastWord32X16# #-}

broadcastWord64X8# :: Word64# -> Word64X8#
broadcastWord64X8# x = M.packWord64X8# (# x, x, x, x, x, x, x, x #)
{-# INLINE broadcastWord64X8# #-}

#elif MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)

-- from ghc-experimental
import           GHC.PrimOps as M

#else

import           GHC.Exts as M

#endif
