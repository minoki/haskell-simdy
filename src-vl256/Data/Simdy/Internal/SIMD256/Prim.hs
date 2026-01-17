{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Simdy.Internal.SIMD256.Prim
  ( M.Int8X32#
  , M.Int16X16#
  , M.Int32X8#
  , M.Int64X4#
  , M.Word8X32#
  , M.Word16X16#
  , M.Word32X8#
  , M.Word64X4#
  , M.FloatX8#
  , M.DoubleX4#
  , broadcastInt8X32#
  , broadcastInt16X16#
  , broadcastInt32X8#
  , broadcastInt64X4#
  , broadcastWord8X32#
  , broadcastWord16X16#
  , broadcastWord32X8#
  , broadcastWord64X4#
  , broadcastFloatX8#
  , broadcastDoubleX4#
  , M.packInt8X32#
  , M.packInt16X16#
  , M.packInt32X8#
  , M.packInt64X4#
  , M.packWord8X32#
  , M.packWord16X16#
  , M.packWord32X8#
  , M.packWord64X4#
  , M.packFloatX8#
  , M.packDoubleX4#
  , M.unpackInt8X32#
  , M.unpackInt16X16#
  , M.unpackInt32X8#
  , M.unpackInt64X4#
  , M.unpackWord8X32#
  , M.unpackWord16X16#
  , M.unpackWord32X8#
  , M.unpackWord64X4#
  , M.unpackFloatX8#
  , M.unpackDoubleX4#
  , M.insertInt8X32#
  , M.insertInt16X16#
  , M.insertInt32X8#
  , M.insertInt64X4#
  , M.insertWord8X32#
  , M.insertWord16X16#
  , M.insertWord32X8#
  , M.insertWord64X4#
  , M.insertFloatX8#
  , M.insertDoubleX4#
  , M.plusInt8X32#
  , M.plusInt16X16#
  , M.plusInt32X8#
  , M.plusInt64X4#
  , M.plusWord8X32#
  , M.plusWord16X16#
  , M.plusWord32X8#
  , M.plusWord64X4#
  , M.plusFloatX8#
  , M.plusDoubleX4#
  , M.minusInt8X32#
  , M.minusInt16X16#
  , M.minusInt32X8#
  , M.minusInt64X4#
  , M.minusWord8X32#
  , M.minusWord16X16#
  , M.minusWord32X8#
  , M.minusWord64X4#
  , M.minusFloatX8#
  , M.minusDoubleX4#
  , M.timesInt8X32#
  , M.timesInt16X16#
  , M.timesInt32X8#
  , M.timesInt64X4#
  , M.timesWord8X32#
  , M.timesWord16X16#
  , M.timesWord32X8#
  , M.timesWord64X4#
  , M.timesFloatX8#
  , M.timesDoubleX4#
  , M.divideFloatX8#
  , M.divideDoubleX4#
  , M.quotInt8X32#
  , M.quotInt16X16#
  , M.quotInt32X8#
  , M.quotInt64X4#
  , M.quotWord8X32#
  , M.quotWord16X16#
  , M.quotWord32X8#
  , M.quotWord64X4#
  , M.remInt8X32#
  , M.remInt16X16#
  , M.remInt32X8#
  , M.remInt64X4#
  , M.remWord8X32#
  , M.remWord16X16#
  , M.remWord32X8#
  , M.remWord64X4#
  , M.negateInt8X32#
  , M.negateInt16X16#
  , M.negateInt32X8#
  , M.negateInt64X4#
  , M.negateFloatX8#
  , M.negateDoubleX4#
  , M.indexInt8X32Array#
  , M.indexInt16X16Array#
  , M.indexInt32X8Array#
  , M.indexInt64X4Array#
  , M.indexWord8X32Array#
  , M.indexWord16X16Array#
  , M.indexWord32X8Array#
  , M.indexWord64X4Array#
  , M.indexFloatX8Array#
  , M.indexDoubleX4Array#
  , M.readInt8X32Array#
  , M.readInt16X16Array#
  , M.readInt32X8Array#
  , M.readInt64X4Array#
  , M.readWord8X32Array#
  , M.readWord16X16Array#
  , M.readWord32X8Array#
  , M.readWord64X4Array#
  , M.readFloatX8Array#
  , M.readDoubleX4Array#
  , M.writeInt8X32Array#
  , M.writeInt16X16Array#
  , M.writeInt32X8Array#
  , M.writeInt64X4Array#
  , M.writeWord8X32Array#
  , M.writeWord16X16Array#
  , M.writeWord32X8Array#
  , M.writeWord64X4Array#
  , M.writeFloatX8Array#
  , M.writeDoubleX4Array#
  , M.indexInt8X32OffAddr#
  , M.indexInt16X16OffAddr#
  , M.indexInt32X8OffAddr#
  , M.indexInt64X4OffAddr#
  , M.indexWord8X32OffAddr#
  , M.indexWord16X16OffAddr#
  , M.indexWord32X8OffAddr#
  , M.indexWord64X4OffAddr#
  , M.indexFloatX8OffAddr#
  , M.indexDoubleX4OffAddr#
  , M.readInt8X32OffAddr#
  , M.readInt16X16OffAddr#
  , M.readInt32X8OffAddr#
  , M.readInt64X4OffAddr#
  , M.readWord8X32OffAddr#
  , M.readWord16X16OffAddr#
  , M.readWord32X8OffAddr#
  , M.readWord64X4OffAddr#
  , M.readFloatX8OffAddr#
  , M.readDoubleX4OffAddr#
  , M.writeInt8X32OffAddr#
  , M.writeInt16X16OffAddr#
  , M.writeInt32X8OffAddr#
  , M.writeInt64X4OffAddr#
  , M.writeWord8X32OffAddr#
  , M.writeWord16X16OffAddr#
  , M.writeWord32X8OffAddr#
  , M.writeWord64X4OffAddr#
  , M.writeFloatX8OffAddr#
  , M.writeDoubleX4OffAddr#
  , M.indexInt8ArrayAsInt8X32#
  , M.indexInt16ArrayAsInt16X16#
  , M.indexInt32ArrayAsInt32X8#
  , M.indexInt64ArrayAsInt64X4#
  , M.indexWord8ArrayAsWord8X32#
  , M.indexWord16ArrayAsWord16X16#
  , M.indexWord32ArrayAsWord32X8#
  , M.indexWord64ArrayAsWord64X4#
  , M.indexFloatArrayAsFloatX8#
  , M.indexDoubleArrayAsDoubleX4#
  , M.readInt8ArrayAsInt8X32#
  , M.readInt16ArrayAsInt16X16#
  , M.readInt32ArrayAsInt32X8#
  , M.readInt64ArrayAsInt64X4#
  , M.readWord8ArrayAsWord8X32#
  , M.readWord16ArrayAsWord16X16#
  , M.readWord32ArrayAsWord32X8#
  , M.readWord64ArrayAsWord64X4#
  , M.readFloatArrayAsFloatX8#
  , M.readDoubleArrayAsDoubleX4#
  , M.writeInt8ArrayAsInt8X32#
  , M.writeInt16ArrayAsInt16X16#
  , M.writeInt32ArrayAsInt32X8#
  , M.writeInt64ArrayAsInt64X4#
  , M.writeWord8ArrayAsWord8X32#
  , M.writeWord16ArrayAsWord16X16#
  , M.writeWord32ArrayAsWord32X8#
  , M.writeWord64ArrayAsWord64X4#
  , M.writeFloatArrayAsFloatX8#
  , M.writeDoubleArrayAsDoubleX4#
  , M.indexInt8OffAddrAsInt8X32#
  , M.indexInt16OffAddrAsInt16X16#
  , M.indexInt32OffAddrAsInt32X8#
  , M.indexInt64OffAddrAsInt64X4#
  , M.indexWord8OffAddrAsWord8X32#
  , M.indexWord16OffAddrAsWord16X16#
  , M.indexWord32OffAddrAsWord32X8#
  , M.indexWord64OffAddrAsWord64X4#
  , M.indexFloatOffAddrAsFloatX8#
  , M.indexDoubleOffAddrAsDoubleX4#
  , M.readInt8OffAddrAsInt8X32#
  , M.readInt16OffAddrAsInt16X16#
  , M.readInt32OffAddrAsInt32X8#
  , M.readInt64OffAddrAsInt64X4#
  , M.readWord8OffAddrAsWord8X32#
  , M.readWord16OffAddrAsWord16X16#
  , M.readWord32OffAddrAsWord32X8#
  , M.readWord64OffAddrAsWord64X4#
  , M.readFloatOffAddrAsFloatX8#
  , M.readDoubleOffAddrAsDoubleX4#
  , M.writeInt8OffAddrAsInt8X32#
  , M.writeInt16OffAddrAsInt16X16#
  , M.writeInt32OffAddrAsInt32X8#
  , M.writeInt64OffAddrAsInt64X4#
  , M.writeWord8OffAddrAsWord8X32#
  , M.writeWord16OffAddrAsWord16X16#
  , M.writeWord32OffAddrAsWord32X8#
  , M.writeWord64OffAddrAsWord64X4#
  , M.writeFloatOffAddrAsFloatX8#
  , M.writeDoubleOffAddrAsDoubleX4#
#if MIN_VERSION_ghc_prim(0, 13, 0)
  , M.fmaddFloatX8#
  , M.fmaddDoubleX4#
  , M.fmsubFloatX8#
  , M.fmsubDoubleX4#
  , M.fnmaddFloatX8#
  , M.fnmaddDoubleX4#
  , M.fnmsubFloatX8#
  , M.fnmsubDoubleX4#
  , M.shuffleInt8X32#
  , M.shuffleInt16X16#
  , M.shuffleInt32X8#
  , M.shuffleInt64X4#
  , M.shuffleWord8X32#
  , M.shuffleWord16X16#
  , M.shuffleWord32X8#
  , M.shuffleWord64X4#
  , M.shuffleFloatX8#
  , M.shuffleDoubleX4#
  , M.minInt8X32#
  , M.minInt16X16#
  , M.minInt32X8#
  , M.minInt64X4#
  , M.minWord8X32#
  , M.minWord16X16#
  , M.minWord32X8#
  , M.minWord64X4#
  , M.minFloatX8#
  , M.minDoubleX4#
  , M.maxInt8X32#
  , M.maxInt16X16#
  , M.maxInt32X8#
  , M.maxInt64X4#
  , M.maxWord8X32#
  , M.maxWord16X16#
  , M.maxWord32X8#
  , M.maxWord64X4#
  , M.maxFloatX8#
  , M.maxDoubleX4#
#endif
  , module Data.Simdy.Internal.SIMD128.Prim
  ) where
import           Data.Simdy.Internal.SIMD128.Prim

#if __GLASGOW_HASKELL__ == 912 && !MIN_VERSION_GLASGOW_HASKELL(9, 12, 3, 0) && defined(USE_LLVM_BACKEND)
-- The LLVM backend of GHC 9.12.{1,2} has a bug with broadcast: https://gitlab.haskell.org/ghc/ghc/-/issues/25561

import qualified GHC.Prim as M
import           GHC.Prim (Float#, Double#, Int8#, Int16#, Int32#, Int64#, Word8#, Word16#, Word32#, Word64#, FloatX8#, DoubleX4#, Int8X32#, Int16X16#, Int32X8#, Int64X4#, Word8X32#, Word16X16#, Word32X8#, Word64X4#)

broadcastFloatX8# :: Float# -> FloatX8#
broadcastFloatX8# x = M.packFloatX8# (# x, x, x, x, x, x, x, x #)
{-# INLINE broadcastFloatX8# #-}

broadcastDoubleX4# :: Double# -> DoubleX4#
broadcastDoubleX4# x = M.packDoubleX4# (# x, x, x, x #)
{-# INLINE broadcastDoubleX4# #-}

broadcastInt8X32# :: Int8# -> Int8X32#
broadcastInt8X32# x = M.packInt8X32# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastInt8X32# #-}

broadcastInt16X16# :: Int16# -> Int16X16#
broadcastInt16X16# x = M.packInt16X16# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastInt16X16# #-}

broadcastInt32X8# :: Int32# -> Int32X8#
broadcastInt32X8# x = M.packInt32X8# (# x, x, x, x, x, x, x, x #)
{-# INLINE broadcastInt32X8# #-}

broadcastInt64X4# :: Int64# -> Int64X4#
broadcastInt64X4# x = M.packInt64X4# (# x, x, x, x #)
{-# INLINE broadcastInt64X4# #-}

broadcastWord8X32# :: Word8# -> Word8X32#
broadcastWord8X32# x = M.packWord8X32# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastWord8X32# #-}

broadcastWord16X16# :: Word16# -> Word16X16#
broadcastWord16X16# x = M.packWord16X16# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE broadcastWord16X16# #-}

broadcastWord32X8# :: Word32# -> Word32X8#
broadcastWord32X8# x = M.packWord32X8# (# x, x, x, x, x, x, x, x #)
{-# INLINE broadcastWord32X8# #-}

broadcastWord64X4# :: Word64# -> Word64X4#
broadcastWord64X4# x = M.packWord64X4# (# x, x, x, x #)
{-# INLINE broadcastWord64X4# #-}

#else

import           GHC.Prim as M

#endif
