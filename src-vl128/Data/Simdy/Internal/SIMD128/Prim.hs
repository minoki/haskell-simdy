{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Simdy.Internal.SIMD128.Prim
  ( M.Int8X16#
  , M.Int16X8#
  , M.Int32X4#
  , M.Int64X2#
  , M.Word8X16#
  , M.Word16X8#
  , M.Word32X4#
  , M.Word64X2#
  , M.FloatX4#
  , M.DoubleX2#
  , broadcastInt8X16#
  , broadcastInt16X8#
  , broadcastInt32X4#
  , broadcastInt64X2#
  , broadcastWord8X16#
  , broadcastWord16X8#
  , broadcastWord32X4#
  , broadcastWord64X2#
  , broadcastFloatX4#
  , broadcastDoubleX2#
  , M.packInt8X16#
  , M.packInt16X8#
  , M.packInt32X4#
  , M.packInt64X2#
  , M.packWord8X16#
  , M.packWord16X8#
  , M.packWord32X4#
  , M.packWord64X2#
  , M.packFloatX4#
  , M.packDoubleX2#
  , M.unpackInt8X16#
  , M.unpackInt16X8#
  , M.unpackInt32X4#
  , M.unpackInt64X2#
  , M.unpackWord8X16#
  , M.unpackWord16X8#
  , M.unpackWord32X4#
  , M.unpackWord64X2#
  , M.unpackFloatX4#
  , M.unpackDoubleX2#
  , M.insertInt8X16#
  , M.insertInt16X8#
  , M.insertInt32X4#
  , M.insertInt64X2#
  , M.insertWord8X16#
  , M.insertWord16X8#
  , M.insertWord32X4#
  , M.insertWord64X2#
  , M.insertFloatX4#
  , M.insertDoubleX2#
  , M.plusInt8X16#
  , M.plusInt16X8#
  , M.plusInt32X4#
  , M.plusInt64X2#
  , M.plusWord8X16#
  , M.plusWord16X8#
  , M.plusWord32X4#
  , M.plusWord64X2#
  , M.plusFloatX4#
  , M.plusDoubleX2#
  , M.minusInt8X16#
  , M.minusInt16X8#
  , M.minusInt32X4#
  , M.minusInt64X2#
  , M.minusWord8X16#
  , M.minusWord16X8#
  , M.minusWord32X4#
  , M.minusWord64X2#
  , M.minusFloatX4#
  , M.minusDoubleX2#
  , M.timesInt8X16#
  , M.timesInt16X8#
  , M.timesInt32X4#
  , M.timesInt64X2#
  , M.timesWord8X16#
  , M.timesWord16X8#
  , M.timesWord32X4#
  , M.timesWord64X2#
  , M.timesFloatX4#
  , M.timesDoubleX2#
  , M.divideFloatX4#
  , M.divideDoubleX2#
  , M.quotInt8X16#
  , M.quotInt16X8#
  , M.quotInt32X4#
  , M.quotInt64X2#
  , M.quotWord8X16#
  , M.quotWord16X8#
  , M.quotWord32X4#
  , M.quotWord64X2#
  , M.remInt8X16#
  , M.remInt16X8#
  , M.remInt32X4#
  , M.remInt64X2#
  , M.remWord8X16#
  , M.remWord16X8#
  , M.remWord32X4#
  , M.remWord64X2#
  , M.negateInt8X16#
  , M.negateInt16X8#
  , M.negateInt32X4#
  , M.negateInt64X2#
  , M.negateFloatX4#
  , M.negateDoubleX2#
  , M.indexInt8X16Array#
  , M.indexInt16X8Array#
  , M.indexInt32X4Array#
  , M.indexInt64X2Array#
  , M.indexWord8X16Array#
  , M.indexWord16X8Array#
  , M.indexWord32X4Array#
  , M.indexWord64X2Array#
  , M.indexFloatX4Array#
  , M.indexDoubleX2Array#
  , M.readInt8X16Array#
  , M.readInt16X8Array#
  , M.readInt32X4Array#
  , M.readInt64X2Array#
  , M.readWord8X16Array#
  , M.readWord16X8Array#
  , M.readWord32X4Array#
  , M.readWord64X2Array#
  , M.readFloatX4Array#
  , M.readDoubleX2Array#
  , M.writeInt8X16Array#
  , M.writeInt16X8Array#
  , M.writeInt32X4Array#
  , M.writeInt64X2Array#
  , M.writeWord8X16Array#
  , M.writeWord16X8Array#
  , M.writeWord32X4Array#
  , M.writeWord64X2Array#
  , M.writeFloatX4Array#
  , M.writeDoubleX2Array#
  , M.indexInt8X16OffAddr#
  , M.indexInt16X8OffAddr#
  , M.indexInt32X4OffAddr#
  , M.indexInt64X2OffAddr#
  , M.indexWord8X16OffAddr#
  , M.indexWord16X8OffAddr#
  , M.indexWord32X4OffAddr#
  , M.indexWord64X2OffAddr#
  , M.indexFloatX4OffAddr#
  , M.indexDoubleX2OffAddr#
  , M.readInt8X16OffAddr#
  , M.readInt16X8OffAddr#
  , M.readInt32X4OffAddr#
  , M.readInt64X2OffAddr#
  , M.readWord8X16OffAddr#
  , M.readWord16X8OffAddr#
  , M.readWord32X4OffAddr#
  , M.readWord64X2OffAddr#
  , M.readFloatX4OffAddr#
  , M.readDoubleX2OffAddr#
  , M.writeInt8X16OffAddr#
  , M.writeInt16X8OffAddr#
  , M.writeInt32X4OffAddr#
  , M.writeInt64X2OffAddr#
  , M.writeWord8X16OffAddr#
  , M.writeWord16X8OffAddr#
  , M.writeWord32X4OffAddr#
  , M.writeWord64X2OffAddr#
  , M.writeFloatX4OffAddr#
  , M.writeDoubleX2OffAddr#
  , M.indexInt8ArrayAsInt8X16#
  , M.indexInt16ArrayAsInt16X8#
  , M.indexInt32ArrayAsInt32X4#
  , M.indexInt64ArrayAsInt64X2#
  , M.indexWord8ArrayAsWord8X16#
  , M.indexWord16ArrayAsWord16X8#
  , M.indexWord32ArrayAsWord32X4#
  , M.indexWord64ArrayAsWord64X2#
  , M.indexFloatArrayAsFloatX4#
  , M.indexDoubleArrayAsDoubleX2#
  , M.readInt8ArrayAsInt8X16#
  , M.readInt16ArrayAsInt16X8#
  , M.readInt32ArrayAsInt32X4#
  , M.readInt64ArrayAsInt64X2#
  , M.readWord8ArrayAsWord8X16#
  , M.readWord16ArrayAsWord16X8#
  , M.readWord32ArrayAsWord32X4#
  , M.readWord64ArrayAsWord64X2#
  , M.readFloatArrayAsFloatX4#
  , M.readDoubleArrayAsDoubleX2#
  , M.writeInt8ArrayAsInt8X16#
  , M.writeInt16ArrayAsInt16X8#
  , M.writeInt32ArrayAsInt32X4#
  , M.writeInt64ArrayAsInt64X2#
  , M.writeWord8ArrayAsWord8X16#
  , M.writeWord16ArrayAsWord16X8#
  , M.writeWord32ArrayAsWord32X4#
  , M.writeWord64ArrayAsWord64X2#
  , M.writeFloatArrayAsFloatX4#
  , M.writeDoubleArrayAsDoubleX2#
  , M.indexInt8OffAddrAsInt8X16#
  , M.indexInt16OffAddrAsInt16X8#
  , M.indexInt32OffAddrAsInt32X4#
  , M.indexInt64OffAddrAsInt64X2#
  , M.indexWord8OffAddrAsWord8X16#
  , M.indexWord16OffAddrAsWord16X8#
  , M.indexWord32OffAddrAsWord32X4#
  , M.indexWord64OffAddrAsWord64X2#
  , M.indexFloatOffAddrAsFloatX4#
  , M.indexDoubleOffAddrAsDoubleX2#
  , M.readInt8OffAddrAsInt8X16#
  , M.readInt16OffAddrAsInt16X8#
  , M.readInt32OffAddrAsInt32X4#
  , M.readInt64OffAddrAsInt64X2#
  , M.readWord8OffAddrAsWord8X16#
  , M.readWord16OffAddrAsWord16X8#
  , M.readWord32OffAddrAsWord32X4#
  , M.readWord64OffAddrAsWord64X2#
  , M.readFloatOffAddrAsFloatX4#
  , M.readDoubleOffAddrAsDoubleX2#
  , M.writeInt8OffAddrAsInt8X16#
  , M.writeInt16OffAddrAsInt16X8#
  , M.writeInt32OffAddrAsInt32X4#
  , M.writeInt64OffAddrAsInt64X2#
  , M.writeWord8OffAddrAsWord8X16#
  , M.writeWord16OffAddrAsWord16X8#
  , M.writeWord32OffAddrAsWord32X4#
  , M.writeWord64OffAddrAsWord64X2#
  , M.writeFloatOffAddrAsFloatX4#
  , M.writeDoubleOffAddrAsDoubleX2#
#if MIN_VERSION_ghc_prim(0, 13, 0)
  , M.fmaddFloatX4#
  , M.fmaddDoubleX2#
  , M.fmsubFloatX4#
  , M.fmsubDoubleX2#
  , M.fnmaddFloatX4#
  , M.fnmaddDoubleX2#
  , M.fnmsubFloatX4#
  , M.fnmsubDoubleX2#
  , M.shuffleInt8X16#
  , M.shuffleInt16X8#
  , M.shuffleInt32X4#
  , M.shuffleInt64X2#
  , M.shuffleWord8X16#
  , M.shuffleWord16X8#
  , M.shuffleWord32X4#
  , M.shuffleWord64X2#
  , M.shuffleFloatX4#
  , M.shuffleDoubleX2#
  , M.minInt8X16#
  , M.minInt16X8#
  , M.minInt32X4#
  , M.minInt64X2#
  , M.minWord8X16#
  , M.minWord16X8#
  , M.minWord32X4#
  , M.minWord64X2#
  , M.minFloatX4#
  , M.minDoubleX2#
  , M.maxInt8X16#
  , M.maxInt16X8#
  , M.maxInt32X4#
  , M.maxInt64X2#
  , M.maxWord8X16#
  , M.maxWord16X8#
  , M.maxWord32X4#
  , M.maxWord64X2#
  , M.maxFloatX4#
  , M.maxDoubleX2#
#endif
  ) where

#if defined(BROADCAST_IS_BROKEN)
-- The LLVM backend of GHC 9.12.{1,2} has a bug with broadcast: https://gitlab.haskell.org/ghc/ghc/-/issues/25561

import qualified GHC.Prim as M
import           GHC.Prim (Float#, Double#, Int8#, Int16#, Int32#, Int64#, Word8#, Word16#, Word32#, Word64#, FloatX4#, DoubleX2#, Int8X16#, Int16X8#, Int32X4#, Int64X2#, Word8X16#, Word16X8#, Word32X4#, Word64X2#)

broadcastFloatX4# :: Float# -> FloatX4#
broadcastFloatX4# x = M.packFloatX4# (# x, x, x, x #)
{-# INLINE broadcastFloatX4# #-}

broadcastDoubleX2# :: Double# -> DoubleX2#
broadcastDoubleX2# x = M.packDoubleX2# (# x, x #)
{-# INLINE broadcastDoubleX2# #-}

broadcastInt8X16# :: Int8# -> Int8X16#
broadcastInt8X16# x = M.packInt8X16# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastInt8X16# #-}

broadcastInt16X8# :: Int16# -> Int16X8#
broadcastInt16X8# x = M.packInt16X8# (# x, x, x, x, x, x, x, x #)
{-# INLINE broadcastInt16X8# #-}

broadcastInt32X4# :: Int32# -> Int32X4#
broadcastInt32X4# x = M.packInt32X4# (# x, x, x, x #)
{-# INLINE broadcastInt32X4# #-}

broadcastInt64X2# :: Int64# -> Int64X2#
broadcastInt64X2# x = M.packInt64X2# (# x, x #)
{-# INLINE broadcastInt64X2# #-}

broadcastWord8X16# :: Word8# -> Word8X16#
broadcastWord8X16# x = M.packWord8X16# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastWord8X16# #-}

broadcastWord16X8# :: Word16# -> Word16X8#
broadcastWord16X8# x = M.packWord16X8# (# x, x, x, x, x, x, x, x #)
{-# INLINE broadcastWord16X8# #-}

broadcastWord32X4# :: Word32# -> Word32X4#
broadcastWord32X4# x = M.packWord32X4# (# x, x, x, x #)
{-# INLINE broadcastWord32X4# #-}

broadcastWord64X2# :: Word64# -> Word64X2#
broadcastWord64X2# x = M.packWord64X2# (# x, x #)
{-# INLINE broadcastWord64X2# #-}

#else

import           GHC.Prim as M

#endif
