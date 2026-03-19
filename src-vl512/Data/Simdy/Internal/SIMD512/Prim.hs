-- This file was created by script/GenPrim.hs. Do not edit by hand!
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.Simdy.Internal.SIMD512.Prim
  ( Int8X64#
  , Int16X32#
  , Int32X16#
  , Int64X8#
  , Word8X64#
  , Word16X32#
  , Word32X16#
  , Word64X8#
  , FloatX16#
  , DoubleX8#
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
  , packInt8X64#
  , packInt16X32#
  , packInt32X16#
  , packInt64X8#
  , packWord8X64#
  , packWord16X32#
  , packWord32X16#
  , packWord64X8#
  , packFloatX16#
  , packDoubleX8#
  , unpackInt8X64#
  , unpackInt16X32#
  , unpackInt32X16#
  , unpackInt64X8#
  , unpackWord8X64#
  , unpackWord16X32#
  , unpackWord32X16#
  , unpackWord64X8#
  , unpackFloatX16#
  , unpackDoubleX8#
  , insertInt8X64#
  , insertInt16X32#
  , insertInt32X16#
  , insertInt64X8#
  , insertWord8X64#
  , insertWord16X32#
  , insertWord32X16#
  , insertWord64X8#
  , insertFloatX16#
  , insertDoubleX8#
  , plusInt8X64#
  , plusInt16X32#
  , plusInt32X16#
  , plusInt64X8#
  , plusWord8X64#
  , plusWord16X32#
  , plusWord32X16#
  , plusWord64X8#
  , plusFloatX16#
  , plusDoubleX8#
  , minusInt8X64#
  , minusInt16X32#
  , minusInt32X16#
  , minusInt64X8#
  , minusWord8X64#
  , minusWord16X32#
  , minusWord32X16#
  , minusWord64X8#
  , minusFloatX16#
  , minusDoubleX8#
  , timesInt8X64#
  , timesInt16X32#
  , timesInt32X16#
  , timesInt64X8#
  , timesWord8X64#
  , timesWord16X32#
  , timesWord32X16#
  , timesWord64X8#
  , timesFloatX16#
  , timesDoubleX8#
  , divideFloatX16#
  , divideDoubleX8#
  , quotInt8X64#
  , quotInt16X32#
  , quotInt32X16#
  , quotInt64X8#
  , quotWord8X64#
  , quotWord16X32#
  , quotWord32X16#
  , quotWord64X8#
  , remInt8X64#
  , remInt16X32#
  , remInt32X16#
  , remInt64X8#
  , remWord8X64#
  , remWord16X32#
  , remWord32X16#
  , remWord64X8#
  , negateInt8X64#
  , negateInt16X32#
  , negateInt32X16#
  , negateInt64X8#
  , negateFloatX16#
  , negateDoubleX8#
  , indexInt8X64Array#
  , indexInt16X32Array#
  , indexInt32X16Array#
  , indexInt64X8Array#
  , indexWord8X64Array#
  , indexWord16X32Array#
  , indexWord32X16Array#
  , indexWord64X8Array#
  , indexFloatX16Array#
  , indexDoubleX8Array#
  , readInt8X64Array#
  , readInt16X32Array#
  , readInt32X16Array#
  , readInt64X8Array#
  , readWord8X64Array#
  , readWord16X32Array#
  , readWord32X16Array#
  , readWord64X8Array#
  , readFloatX16Array#
  , readDoubleX8Array#
  , writeInt8X64Array#
  , writeInt16X32Array#
  , writeInt32X16Array#
  , writeInt64X8Array#
  , writeWord8X64Array#
  , writeWord16X32Array#
  , writeWord32X16Array#
  , writeWord64X8Array#
  , writeFloatX16Array#
  , writeDoubleX8Array#
  , indexInt8X64OffAddr#
  , indexInt16X32OffAddr#
  , indexInt32X16OffAddr#
  , indexInt64X8OffAddr#
  , indexWord8X64OffAddr#
  , indexWord16X32OffAddr#
  , indexWord32X16OffAddr#
  , indexWord64X8OffAddr#
  , indexFloatX16OffAddr#
  , indexDoubleX8OffAddr#
  , readInt8X64OffAddr#
  , readInt16X32OffAddr#
  , readInt32X16OffAddr#
  , readInt64X8OffAddr#
  , readWord8X64OffAddr#
  , readWord16X32OffAddr#
  , readWord32X16OffAddr#
  , readWord64X8OffAddr#
  , readFloatX16OffAddr#
  , readDoubleX8OffAddr#
  , writeInt8X64OffAddr#
  , writeInt16X32OffAddr#
  , writeInt32X16OffAddr#
  , writeInt64X8OffAddr#
  , writeWord8X64OffAddr#
  , writeWord16X32OffAddr#
  , writeWord32X16OffAddr#
  , writeWord64X8OffAddr#
  , writeFloatX16OffAddr#
  , writeDoubleX8OffAddr#
  , indexInt8ArrayAsInt8X64#
  , indexInt16ArrayAsInt16X32#
  , indexInt32ArrayAsInt32X16#
  , indexInt64ArrayAsInt64X8#
  , indexWord8ArrayAsWord8X64#
  , indexWord16ArrayAsWord16X32#
  , indexWord32ArrayAsWord32X16#
  , indexWord64ArrayAsWord64X8#
  , indexFloatArrayAsFloatX16#
  , indexDoubleArrayAsDoubleX8#
  , readInt8ArrayAsInt8X64#
  , readInt16ArrayAsInt16X32#
  , readInt32ArrayAsInt32X16#
  , readInt64ArrayAsInt64X8#
  , readWord8ArrayAsWord8X64#
  , readWord16ArrayAsWord16X32#
  , readWord32ArrayAsWord32X16#
  , readWord64ArrayAsWord64X8#
  , readFloatArrayAsFloatX16#
  , readDoubleArrayAsDoubleX8#
  , writeInt8ArrayAsInt8X64#
  , writeInt16ArrayAsInt16X32#
  , writeInt32ArrayAsInt32X16#
  , writeInt64ArrayAsInt64X8#
  , writeWord8ArrayAsWord8X64#
  , writeWord16ArrayAsWord16X32#
  , writeWord32ArrayAsWord32X16#
  , writeWord64ArrayAsWord64X8#
  , writeFloatArrayAsFloatX16#
  , writeDoubleArrayAsDoubleX8#
  , indexInt8OffAddrAsInt8X64#
  , indexInt16OffAddrAsInt16X32#
  , indexInt32OffAddrAsInt32X16#
  , indexInt64OffAddrAsInt64X8#
  , indexWord8OffAddrAsWord8X64#
  , indexWord16OffAddrAsWord16X32#
  , indexWord32OffAddrAsWord32X16#
  , indexWord64OffAddrAsWord64X8#
  , indexFloatOffAddrAsFloatX16#
  , indexDoubleOffAddrAsDoubleX8#
  , readInt8OffAddrAsInt8X64#
  , readInt16OffAddrAsInt16X32#
  , readInt32OffAddrAsInt32X16#
  , readInt64OffAddrAsInt64X8#
  , readWord8OffAddrAsWord8X64#
  , readWord16OffAddrAsWord16X32#
  , readWord32OffAddrAsWord32X16#
  , readWord64OffAddrAsWord64X8#
  , readFloatOffAddrAsFloatX16#
  , readDoubleOffAddrAsDoubleX8#
  , writeInt8OffAddrAsInt8X64#
  , writeInt16OffAddrAsInt16X32#
  , writeInt32OffAddrAsInt32X16#
  , writeInt64OffAddrAsInt64X8#
  , writeWord8OffAddrAsWord8X64#
  , writeWord16OffAddrAsWord16X32#
  , writeWord32OffAddrAsWord32X16#
  , writeWord64OffAddrAsWord64X8#
  , writeFloatOffAddrAsFloatX16#
  , writeDoubleOffAddrAsDoubleX8#
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
  , fmaddFloatX16#
  , fmaddDoubleX8#
  , fmsubFloatX16#
  , fmsubDoubleX8#
  , fnmaddFloatX16#
  , fnmaddDoubleX8#
  , fnmsubFloatX16#
  , fnmsubDoubleX8#
  , shuffleInt8X64#
  , shuffleInt16X32#
  , shuffleInt32X16#
  , shuffleInt64X8#
  , shuffleWord8X64#
  , shuffleWord16X32#
  , shuffleWord32X16#
  , shuffleWord64X8#
  , shuffleFloatX16#
  , shuffleDoubleX8#
  , minFloatX16#
  , minDoubleX8#
  , maxFloatX16#
  , maxDoubleX8#
#endif
  , minInt8X64#
  , minInt16X32#
  , minInt32X16#
  , minInt64X8#
  , minWord8X64#
  , minWord16X32#
  , minWord32X16#
  , minWord64X8#
  , maxInt8X64#
  , maxInt16X32#
  , maxInt32X16#
  , maxInt64X8#
  , maxWord8X64#
  , maxWord16X32#
  , maxWord32X16#
  , maxWord64X8#
  , andInt8X64#
  , andInt16X32#
  , andInt32X16#
  , andInt64X8#
  , andWord8X64#
  , andWord16X32#
  , andWord32X16#
  , andWord64X8#
  , orInt8X64#
  , orInt16X32#
  , orInt32X16#
  , orInt64X8#
  , orWord8X64#
  , orWord16X32#
  , orWord32X16#
  , orWord64X8#
  , xorInt8X64#
  , xorInt16X32#
  , xorInt32X16#
  , xorInt64X8#
  , xorWord8X64#
  , xorWord16X32#
  , xorWord32X16#
  , xorWord64X8#
#if MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)
  , andFloatX16#
  , andDoubleX8#
  , orFloatX16#
  , orDoubleX8#
  , xorFloatX16#
  , xorDoubleX8#
#endif
  , absInt8X64#
  , absInt16X32#
  , absInt32X16#
  , absInt64X8#
  , absFloatX16#
  , absDoubleX8#
  , sqrtFloatX16#
  , sqrtDoubleX8#
  , complementInt8X64#
  , complementInt16X32#
  , complementInt32X16#
  , complementInt64X8#
  , complementWord8X64#
  , complementWord16X32#
  , complementWord32X16#
  , complementWord64X8#
  , module Data.Simdy.Internal.SIMD256.Prim
  ) where
import           Data.Simdy.Internal.SIMD256.Prim

#if defined(BROADCAST_IS_BROKEN)
-- The LLVM backend of GHC 9.12.{1,2} has a bug with broadcast: https://gitlab.haskell.org/ghc/ghc/-/issues/25561
import           GHC.PrimOps hiding (broadcastInt8X64#, broadcastInt16X32#, broadcastInt32X16#, broadcastInt64X8#, broadcastWord8X64#, broadcastWord16X32#, broadcastWord32X16#, broadcastWord64X8#, broadcastFloatX16#, broadcastDoubleX8#)

broadcastInt8X64# :: Int8# -> Int8X64#
broadcastInt8X64# x = packInt8X64# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE [0] broadcastInt8X64# #-}

broadcastInt16X32# :: Int16# -> Int16X32#
broadcastInt16X32# x = packInt16X32# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE [0] broadcastInt16X32# #-}

broadcastInt32X16# :: Int32# -> Int32X16#
broadcastInt32X16# x = packInt32X16# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE [0] broadcastInt32X16# #-}

broadcastInt64X8# :: Int64# -> Int64X8#
broadcastInt64X8# x = packInt64X8# (# x, x, x, x, x, x, x, x #)
{-# INLINE [0] broadcastInt64X8# #-}

broadcastWord8X64# :: Word8# -> Word8X64#
broadcastWord8X64# x = packWord8X64# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE [0] broadcastWord8X64# #-}

broadcastWord16X32# :: Word16# -> Word16X32#
broadcastWord16X32# x = packWord16X32# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE [0] broadcastWord16X32# #-}

broadcastWord32X16# :: Word32# -> Word32X16#
broadcastWord32X16# x = packWord32X16# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE [0] broadcastWord32X16# #-}

broadcastWord64X8# :: Word64# -> Word64X8#
broadcastWord64X8# x = packWord64X8# (# x, x, x, x, x, x, x, x #)
{-# INLINE [0] broadcastWord64X8# #-}

broadcastFloatX16# :: Float# -> FloatX16#
broadcastFloatX16# x = packFloatX16# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #)
{-# INLINE [0] broadcastFloatX16# #-}

broadcastDoubleX8# :: Double# -> DoubleX8#
broadcastDoubleX8# x = packDoubleX8# (# x, x, x, x, x, x, x, x #)
{-# INLINE [0] broadcastDoubleX8# #-}

#elif MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)

-- from ghc-experimental
import           GHC.PrimOps

#else

import           GHC.Exts

#endif

complementInt8X64# :: Int8X64# -> Int8X64#
complementInt8X64# u = case unpackInt8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> packInt8X64# (# intToInt8# (notI# (int8ToInt# u0)), intToInt8# (notI# (int8ToInt# u1)), intToInt8# (notI# (int8ToInt# u2)), intToInt8# (notI# (int8ToInt# u3)), intToInt8# (notI# (int8ToInt# u4)), intToInt8# (notI# (int8ToInt# u5)), intToInt8# (notI# (int8ToInt# u6)), intToInt8# (notI# (int8ToInt# u7)), intToInt8# (notI# (int8ToInt# u8)), intToInt8# (notI# (int8ToInt# u9)), intToInt8# (notI# (int8ToInt# u10)), intToInt8# (notI# (int8ToInt# u11)), intToInt8# (notI# (int8ToInt# u12)), intToInt8# (notI# (int8ToInt# u13)), intToInt8# (notI# (int8ToInt# u14)), intToInt8# (notI# (int8ToInt# u15)), intToInt8# (notI# (int8ToInt# u16)), intToInt8# (notI# (int8ToInt# u17)), intToInt8# (notI# (int8ToInt# u18)), intToInt8# (notI# (int8ToInt# u19)), intToInt8# (notI# (int8ToInt# u20)), intToInt8# (notI# (int8ToInt# u21)), intToInt8# (notI# (int8ToInt# u22)), intToInt8# (notI# (int8ToInt# u23)), intToInt8# (notI# (int8ToInt# u24)), intToInt8# (notI# (int8ToInt# u25)), intToInt8# (notI# (int8ToInt# u26)), intToInt8# (notI# (int8ToInt# u27)), intToInt8# (notI# (int8ToInt# u28)), intToInt8# (notI# (int8ToInt# u29)), intToInt8# (notI# (int8ToInt# u30)), intToInt8# (notI# (int8ToInt# u31)), intToInt8# (notI# (int8ToInt# u32)), intToInt8# (notI# (int8ToInt# u33)), intToInt8# (notI# (int8ToInt# u34)), intToInt8# (notI# (int8ToInt# u35)), intToInt8# (notI# (int8ToInt# u36)), intToInt8# (notI# (int8ToInt# u37)), intToInt8# (notI# (int8ToInt# u38)), intToInt8# (notI# (int8ToInt# u39)), intToInt8# (notI# (int8ToInt# u40)), intToInt8# (notI# (int8ToInt# u41)), intToInt8# (notI# (int8ToInt# u42)), intToInt8# (notI# (int8ToInt# u43)), intToInt8# (notI# (int8ToInt# u44)), intToInt8# (notI# (int8ToInt# u45)), intToInt8# (notI# (int8ToInt# u46)), intToInt8# (notI# (int8ToInt# u47)), intToInt8# (notI# (int8ToInt# u48)), intToInt8# (notI# (int8ToInt# u49)), intToInt8# (notI# (int8ToInt# u50)), intToInt8# (notI# (int8ToInt# u51)), intToInt8# (notI# (int8ToInt# u52)), intToInt8# (notI# (int8ToInt# u53)), intToInt8# (notI# (int8ToInt# u54)), intToInt8# (notI# (int8ToInt# u55)), intToInt8# (notI# (int8ToInt# u56)), intToInt8# (notI# (int8ToInt# u57)), intToInt8# (notI# (int8ToInt# u58)), intToInt8# (notI# (int8ToInt# u59)), intToInt8# (notI# (int8ToInt# u60)), intToInt8# (notI# (int8ToInt# u61)), intToInt8# (notI# (int8ToInt# u62)), intToInt8# (notI# (int8ToInt# u63)) #)
{-# INLINE [0] complementInt8X64# #-}

complementInt16X32# :: Int16X32# -> Int16X32#
complementInt16X32# u = case unpackInt16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> packInt16X32# (# intToInt16# (notI# (int16ToInt# u0)), intToInt16# (notI# (int16ToInt# u1)), intToInt16# (notI# (int16ToInt# u2)), intToInt16# (notI# (int16ToInt# u3)), intToInt16# (notI# (int16ToInt# u4)), intToInt16# (notI# (int16ToInt# u5)), intToInt16# (notI# (int16ToInt# u6)), intToInt16# (notI# (int16ToInt# u7)), intToInt16# (notI# (int16ToInt# u8)), intToInt16# (notI# (int16ToInt# u9)), intToInt16# (notI# (int16ToInt# u10)), intToInt16# (notI# (int16ToInt# u11)), intToInt16# (notI# (int16ToInt# u12)), intToInt16# (notI# (int16ToInt# u13)), intToInt16# (notI# (int16ToInt# u14)), intToInt16# (notI# (int16ToInt# u15)), intToInt16# (notI# (int16ToInt# u16)), intToInt16# (notI# (int16ToInt# u17)), intToInt16# (notI# (int16ToInt# u18)), intToInt16# (notI# (int16ToInt# u19)), intToInt16# (notI# (int16ToInt# u20)), intToInt16# (notI# (int16ToInt# u21)), intToInt16# (notI# (int16ToInt# u22)), intToInt16# (notI# (int16ToInt# u23)), intToInt16# (notI# (int16ToInt# u24)), intToInt16# (notI# (int16ToInt# u25)), intToInt16# (notI# (int16ToInt# u26)), intToInt16# (notI# (int16ToInt# u27)), intToInt16# (notI# (int16ToInt# u28)), intToInt16# (notI# (int16ToInt# u29)), intToInt16# (notI# (int16ToInt# u30)), intToInt16# (notI# (int16ToInt# u31)) #)
{-# INLINE [0] complementInt16X32# #-}

complementInt32X16# :: Int32X16# -> Int32X16#
complementInt32X16# u = case unpackInt32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packInt32X16# (# intToInt32# (notI# (int32ToInt# u0)), intToInt32# (notI# (int32ToInt# u1)), intToInt32# (notI# (int32ToInt# u2)), intToInt32# (notI# (int32ToInt# u3)), intToInt32# (notI# (int32ToInt# u4)), intToInt32# (notI# (int32ToInt# u5)), intToInt32# (notI# (int32ToInt# u6)), intToInt32# (notI# (int32ToInt# u7)), intToInt32# (notI# (int32ToInt# u8)), intToInt32# (notI# (int32ToInt# u9)), intToInt32# (notI# (int32ToInt# u10)), intToInt32# (notI# (int32ToInt# u11)), intToInt32# (notI# (int32ToInt# u12)), intToInt32# (notI# (int32ToInt# u13)), intToInt32# (notI# (int32ToInt# u14)), intToInt32# (notI# (int32ToInt# u15)) #)
{-# INLINE [0] complementInt32X16# #-}

complementInt64X8# :: Int64X8# -> Int64X8#
complementInt64X8# u = case unpackInt64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packInt64X8# (# word64ToInt64# (not64# (int64ToWord64# u0)), word64ToInt64# (not64# (int64ToWord64# u1)), word64ToInt64# (not64# (int64ToWord64# u2)), word64ToInt64# (not64# (int64ToWord64# u3)), word64ToInt64# (not64# (int64ToWord64# u4)), word64ToInt64# (not64# (int64ToWord64# u5)), word64ToInt64# (not64# (int64ToWord64# u6)), word64ToInt64# (not64# (int64ToWord64# u7)) #)
{-# INLINE [0] complementInt64X8# #-}

complementWord8X64# :: Word8X64# -> Word8X64#
complementWord8X64# u = case unpackWord8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> packWord8X64# (# wordToWord8# (not# (word8ToWord# u0)), wordToWord8# (not# (word8ToWord# u1)), wordToWord8# (not# (word8ToWord# u2)), wordToWord8# (not# (word8ToWord# u3)), wordToWord8# (not# (word8ToWord# u4)), wordToWord8# (not# (word8ToWord# u5)), wordToWord8# (not# (word8ToWord# u6)), wordToWord8# (not# (word8ToWord# u7)), wordToWord8# (not# (word8ToWord# u8)), wordToWord8# (not# (word8ToWord# u9)), wordToWord8# (not# (word8ToWord# u10)), wordToWord8# (not# (word8ToWord# u11)), wordToWord8# (not# (word8ToWord# u12)), wordToWord8# (not# (word8ToWord# u13)), wordToWord8# (not# (word8ToWord# u14)), wordToWord8# (not# (word8ToWord# u15)), wordToWord8# (not# (word8ToWord# u16)), wordToWord8# (not# (word8ToWord# u17)), wordToWord8# (not# (word8ToWord# u18)), wordToWord8# (not# (word8ToWord# u19)), wordToWord8# (not# (word8ToWord# u20)), wordToWord8# (not# (word8ToWord# u21)), wordToWord8# (not# (word8ToWord# u22)), wordToWord8# (not# (word8ToWord# u23)), wordToWord8# (not# (word8ToWord# u24)), wordToWord8# (not# (word8ToWord# u25)), wordToWord8# (not# (word8ToWord# u26)), wordToWord8# (not# (word8ToWord# u27)), wordToWord8# (not# (word8ToWord# u28)), wordToWord8# (not# (word8ToWord# u29)), wordToWord8# (not# (word8ToWord# u30)), wordToWord8# (not# (word8ToWord# u31)), wordToWord8# (not# (word8ToWord# u32)), wordToWord8# (not# (word8ToWord# u33)), wordToWord8# (not# (word8ToWord# u34)), wordToWord8# (not# (word8ToWord# u35)), wordToWord8# (not# (word8ToWord# u36)), wordToWord8# (not# (word8ToWord# u37)), wordToWord8# (not# (word8ToWord# u38)), wordToWord8# (not# (word8ToWord# u39)), wordToWord8# (not# (word8ToWord# u40)), wordToWord8# (not# (word8ToWord# u41)), wordToWord8# (not# (word8ToWord# u42)), wordToWord8# (not# (word8ToWord# u43)), wordToWord8# (not# (word8ToWord# u44)), wordToWord8# (not# (word8ToWord# u45)), wordToWord8# (not# (word8ToWord# u46)), wordToWord8# (not# (word8ToWord# u47)), wordToWord8# (not# (word8ToWord# u48)), wordToWord8# (not# (word8ToWord# u49)), wordToWord8# (not# (word8ToWord# u50)), wordToWord8# (not# (word8ToWord# u51)), wordToWord8# (not# (word8ToWord# u52)), wordToWord8# (not# (word8ToWord# u53)), wordToWord8# (not# (word8ToWord# u54)), wordToWord8# (not# (word8ToWord# u55)), wordToWord8# (not# (word8ToWord# u56)), wordToWord8# (not# (word8ToWord# u57)), wordToWord8# (not# (word8ToWord# u58)), wordToWord8# (not# (word8ToWord# u59)), wordToWord8# (not# (word8ToWord# u60)), wordToWord8# (not# (word8ToWord# u61)), wordToWord8# (not# (word8ToWord# u62)), wordToWord8# (not# (word8ToWord# u63)) #)
{-# INLINE [0] complementWord8X64# #-}

complementWord16X32# :: Word16X32# -> Word16X32#
complementWord16X32# u = case unpackWord16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> packWord16X32# (# wordToWord16# (not# (word16ToWord# u0)), wordToWord16# (not# (word16ToWord# u1)), wordToWord16# (not# (word16ToWord# u2)), wordToWord16# (not# (word16ToWord# u3)), wordToWord16# (not# (word16ToWord# u4)), wordToWord16# (not# (word16ToWord# u5)), wordToWord16# (not# (word16ToWord# u6)), wordToWord16# (not# (word16ToWord# u7)), wordToWord16# (not# (word16ToWord# u8)), wordToWord16# (not# (word16ToWord# u9)), wordToWord16# (not# (word16ToWord# u10)), wordToWord16# (not# (word16ToWord# u11)), wordToWord16# (not# (word16ToWord# u12)), wordToWord16# (not# (word16ToWord# u13)), wordToWord16# (not# (word16ToWord# u14)), wordToWord16# (not# (word16ToWord# u15)), wordToWord16# (not# (word16ToWord# u16)), wordToWord16# (not# (word16ToWord# u17)), wordToWord16# (not# (word16ToWord# u18)), wordToWord16# (not# (word16ToWord# u19)), wordToWord16# (not# (word16ToWord# u20)), wordToWord16# (not# (word16ToWord# u21)), wordToWord16# (not# (word16ToWord# u22)), wordToWord16# (not# (word16ToWord# u23)), wordToWord16# (not# (word16ToWord# u24)), wordToWord16# (not# (word16ToWord# u25)), wordToWord16# (not# (word16ToWord# u26)), wordToWord16# (not# (word16ToWord# u27)), wordToWord16# (not# (word16ToWord# u28)), wordToWord16# (not# (word16ToWord# u29)), wordToWord16# (not# (word16ToWord# u30)), wordToWord16# (not# (word16ToWord# u31)) #)
{-# INLINE [0] complementWord16X32# #-}

complementWord32X16# :: Word32X16# -> Word32X16#
complementWord32X16# u = case unpackWord32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packWord32X16# (# wordToWord32# (not# (word32ToWord# u0)), wordToWord32# (not# (word32ToWord# u1)), wordToWord32# (not# (word32ToWord# u2)), wordToWord32# (not# (word32ToWord# u3)), wordToWord32# (not# (word32ToWord# u4)), wordToWord32# (not# (word32ToWord# u5)), wordToWord32# (not# (word32ToWord# u6)), wordToWord32# (not# (word32ToWord# u7)), wordToWord32# (not# (word32ToWord# u8)), wordToWord32# (not# (word32ToWord# u9)), wordToWord32# (not# (word32ToWord# u10)), wordToWord32# (not# (word32ToWord# u11)), wordToWord32# (not# (word32ToWord# u12)), wordToWord32# (not# (word32ToWord# u13)), wordToWord32# (not# (word32ToWord# u14)), wordToWord32# (not# (word32ToWord# u15)) #)
{-# INLINE [0] complementWord32X16# #-}

complementWord64X8# :: Word64X8# -> Word64X8#
complementWord64X8# u = case unpackWord64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packWord64X8# (# not64# u0, not64# u1, not64# u2, not64# u3, not64# u4, not64# u5, not64# u6, not64# u7 #)
{-# INLINE [0] complementWord64X8# #-}

#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)

andInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#
andInt8X64# u v = case unpackInt8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> case unpackInt8X64# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60, v61, v62, v63 #) -> packInt8X64# (# intToInt8# (int8ToInt# u0 `andI#` int8ToInt# v0), intToInt8# (int8ToInt# u1 `andI#` int8ToInt# v1), intToInt8# (int8ToInt# u2 `andI#` int8ToInt# v2), intToInt8# (int8ToInt# u3 `andI#` int8ToInt# v3), intToInt8# (int8ToInt# u4 `andI#` int8ToInt# v4), intToInt8# (int8ToInt# u5 `andI#` int8ToInt# v5), intToInt8# (int8ToInt# u6 `andI#` int8ToInt# v6), intToInt8# (int8ToInt# u7 `andI#` int8ToInt# v7), intToInt8# (int8ToInt# u8 `andI#` int8ToInt# v8), intToInt8# (int8ToInt# u9 `andI#` int8ToInt# v9), intToInt8# (int8ToInt# u10 `andI#` int8ToInt# v10), intToInt8# (int8ToInt# u11 `andI#` int8ToInt# v11), intToInt8# (int8ToInt# u12 `andI#` int8ToInt# v12), intToInt8# (int8ToInt# u13 `andI#` int8ToInt# v13), intToInt8# (int8ToInt# u14 `andI#` int8ToInt# v14), intToInt8# (int8ToInt# u15 `andI#` int8ToInt# v15), intToInt8# (int8ToInt# u16 `andI#` int8ToInt# v16), intToInt8# (int8ToInt# u17 `andI#` int8ToInt# v17), intToInt8# (int8ToInt# u18 `andI#` int8ToInt# v18), intToInt8# (int8ToInt# u19 `andI#` int8ToInt# v19), intToInt8# (int8ToInt# u20 `andI#` int8ToInt# v20), intToInt8# (int8ToInt# u21 `andI#` int8ToInt# v21), intToInt8# (int8ToInt# u22 `andI#` int8ToInt# v22), intToInt8# (int8ToInt# u23 `andI#` int8ToInt# v23), intToInt8# (int8ToInt# u24 `andI#` int8ToInt# v24), intToInt8# (int8ToInt# u25 `andI#` int8ToInt# v25), intToInt8# (int8ToInt# u26 `andI#` int8ToInt# v26), intToInt8# (int8ToInt# u27 `andI#` int8ToInt# v27), intToInt8# (int8ToInt# u28 `andI#` int8ToInt# v28), intToInt8# (int8ToInt# u29 `andI#` int8ToInt# v29), intToInt8# (int8ToInt# u30 `andI#` int8ToInt# v30), intToInt8# (int8ToInt# u31 `andI#` int8ToInt# v31), intToInt8# (int8ToInt# u32 `andI#` int8ToInt# v32), intToInt8# (int8ToInt# u33 `andI#` int8ToInt# v33), intToInt8# (int8ToInt# u34 `andI#` int8ToInt# v34), intToInt8# (int8ToInt# u35 `andI#` int8ToInt# v35), intToInt8# (int8ToInt# u36 `andI#` int8ToInt# v36), intToInt8# (int8ToInt# u37 `andI#` int8ToInt# v37), intToInt8# (int8ToInt# u38 `andI#` int8ToInt# v38), intToInt8# (int8ToInt# u39 `andI#` int8ToInt# v39), intToInt8# (int8ToInt# u40 `andI#` int8ToInt# v40), intToInt8# (int8ToInt# u41 `andI#` int8ToInt# v41), intToInt8# (int8ToInt# u42 `andI#` int8ToInt# v42), intToInt8# (int8ToInt# u43 `andI#` int8ToInt# v43), intToInt8# (int8ToInt# u44 `andI#` int8ToInt# v44), intToInt8# (int8ToInt# u45 `andI#` int8ToInt# v45), intToInt8# (int8ToInt# u46 `andI#` int8ToInt# v46), intToInt8# (int8ToInt# u47 `andI#` int8ToInt# v47), intToInt8# (int8ToInt# u48 `andI#` int8ToInt# v48), intToInt8# (int8ToInt# u49 `andI#` int8ToInt# v49), intToInt8# (int8ToInt# u50 `andI#` int8ToInt# v50), intToInt8# (int8ToInt# u51 `andI#` int8ToInt# v51), intToInt8# (int8ToInt# u52 `andI#` int8ToInt# v52), intToInt8# (int8ToInt# u53 `andI#` int8ToInt# v53), intToInt8# (int8ToInt# u54 `andI#` int8ToInt# v54), intToInt8# (int8ToInt# u55 `andI#` int8ToInt# v55), intToInt8# (int8ToInt# u56 `andI#` int8ToInt# v56), intToInt8# (int8ToInt# u57 `andI#` int8ToInt# v57), intToInt8# (int8ToInt# u58 `andI#` int8ToInt# v58), intToInt8# (int8ToInt# u59 `andI#` int8ToInt# v59), intToInt8# (int8ToInt# u60 `andI#` int8ToInt# v60), intToInt8# (int8ToInt# u61 `andI#` int8ToInt# v61), intToInt8# (int8ToInt# u62 `andI#` int8ToInt# v62), intToInt8# (int8ToInt# u63 `andI#` int8ToInt# v63) #)
{-# INLINE [0] andInt8X64# #-}

andInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#
andInt16X32# u v = case unpackInt16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackInt16X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packInt16X32# (# intToInt16# (int16ToInt# u0 `andI#` int16ToInt# v0), intToInt16# (int16ToInt# u1 `andI#` int16ToInt# v1), intToInt16# (int16ToInt# u2 `andI#` int16ToInt# v2), intToInt16# (int16ToInt# u3 `andI#` int16ToInt# v3), intToInt16# (int16ToInt# u4 `andI#` int16ToInt# v4), intToInt16# (int16ToInt# u5 `andI#` int16ToInt# v5), intToInt16# (int16ToInt# u6 `andI#` int16ToInt# v6), intToInt16# (int16ToInt# u7 `andI#` int16ToInt# v7), intToInt16# (int16ToInt# u8 `andI#` int16ToInt# v8), intToInt16# (int16ToInt# u9 `andI#` int16ToInt# v9), intToInt16# (int16ToInt# u10 `andI#` int16ToInt# v10), intToInt16# (int16ToInt# u11 `andI#` int16ToInt# v11), intToInt16# (int16ToInt# u12 `andI#` int16ToInt# v12), intToInt16# (int16ToInt# u13 `andI#` int16ToInt# v13), intToInt16# (int16ToInt# u14 `andI#` int16ToInt# v14), intToInt16# (int16ToInt# u15 `andI#` int16ToInt# v15), intToInt16# (int16ToInt# u16 `andI#` int16ToInt# v16), intToInt16# (int16ToInt# u17 `andI#` int16ToInt# v17), intToInt16# (int16ToInt# u18 `andI#` int16ToInt# v18), intToInt16# (int16ToInt# u19 `andI#` int16ToInt# v19), intToInt16# (int16ToInt# u20 `andI#` int16ToInt# v20), intToInt16# (int16ToInt# u21 `andI#` int16ToInt# v21), intToInt16# (int16ToInt# u22 `andI#` int16ToInt# v22), intToInt16# (int16ToInt# u23 `andI#` int16ToInt# v23), intToInt16# (int16ToInt# u24 `andI#` int16ToInt# v24), intToInt16# (int16ToInt# u25 `andI#` int16ToInt# v25), intToInt16# (int16ToInt# u26 `andI#` int16ToInt# v26), intToInt16# (int16ToInt# u27 `andI#` int16ToInt# v27), intToInt16# (int16ToInt# u28 `andI#` int16ToInt# v28), intToInt16# (int16ToInt# u29 `andI#` int16ToInt# v29), intToInt16# (int16ToInt# u30 `andI#` int16ToInt# v30), intToInt16# (int16ToInt# u31 `andI#` int16ToInt# v31) #)
{-# INLINE [0] andInt16X32# #-}

andInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#
andInt32X16# u v = case unpackInt32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt32X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt32X16# (# intToInt32# (int32ToInt# u0 `andI#` int32ToInt# v0), intToInt32# (int32ToInt# u1 `andI#` int32ToInt# v1), intToInt32# (int32ToInt# u2 `andI#` int32ToInt# v2), intToInt32# (int32ToInt# u3 `andI#` int32ToInt# v3), intToInt32# (int32ToInt# u4 `andI#` int32ToInt# v4), intToInt32# (int32ToInt# u5 `andI#` int32ToInt# v5), intToInt32# (int32ToInt# u6 `andI#` int32ToInt# v6), intToInt32# (int32ToInt# u7 `andI#` int32ToInt# v7), intToInt32# (int32ToInt# u8 `andI#` int32ToInt# v8), intToInt32# (int32ToInt# u9 `andI#` int32ToInt# v9), intToInt32# (int32ToInt# u10 `andI#` int32ToInt# v10), intToInt32# (int32ToInt# u11 `andI#` int32ToInt# v11), intToInt32# (int32ToInt# u12 `andI#` int32ToInt# v12), intToInt32# (int32ToInt# u13 `andI#` int32ToInt# v13), intToInt32# (int32ToInt# u14 `andI#` int32ToInt# v14), intToInt32# (int32ToInt# u15 `andI#` int32ToInt# v15) #)
{-# INLINE [0] andInt32X16# #-}

andInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#
andInt64X8# u v = case unpackInt64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt64X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt64X8# (# word64ToInt64# (int64ToWord64# u0 `and64#` int64ToWord64# v0), word64ToInt64# (int64ToWord64# u1 `and64#` int64ToWord64# v1), word64ToInt64# (int64ToWord64# u2 `and64#` int64ToWord64# v2), word64ToInt64# (int64ToWord64# u3 `and64#` int64ToWord64# v3), word64ToInt64# (int64ToWord64# u4 `and64#` int64ToWord64# v4), word64ToInt64# (int64ToWord64# u5 `and64#` int64ToWord64# v5), word64ToInt64# (int64ToWord64# u6 `and64#` int64ToWord64# v6), word64ToInt64# (int64ToWord64# u7 `and64#` int64ToWord64# v7) #)
{-# INLINE [0] andInt64X8# #-}

andWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#
andWord8X64# u v = case unpackWord8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> case unpackWord8X64# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60, v61, v62, v63 #) -> packWord8X64# (# wordToWord8# (word8ToWord# u0 `and#` word8ToWord# v0), wordToWord8# (word8ToWord# u1 `and#` word8ToWord# v1), wordToWord8# (word8ToWord# u2 `and#` word8ToWord# v2), wordToWord8# (word8ToWord# u3 `and#` word8ToWord# v3), wordToWord8# (word8ToWord# u4 `and#` word8ToWord# v4), wordToWord8# (word8ToWord# u5 `and#` word8ToWord# v5), wordToWord8# (word8ToWord# u6 `and#` word8ToWord# v6), wordToWord8# (word8ToWord# u7 `and#` word8ToWord# v7), wordToWord8# (word8ToWord# u8 `and#` word8ToWord# v8), wordToWord8# (word8ToWord# u9 `and#` word8ToWord# v9), wordToWord8# (word8ToWord# u10 `and#` word8ToWord# v10), wordToWord8# (word8ToWord# u11 `and#` word8ToWord# v11), wordToWord8# (word8ToWord# u12 `and#` word8ToWord# v12), wordToWord8# (word8ToWord# u13 `and#` word8ToWord# v13), wordToWord8# (word8ToWord# u14 `and#` word8ToWord# v14), wordToWord8# (word8ToWord# u15 `and#` word8ToWord# v15), wordToWord8# (word8ToWord# u16 `and#` word8ToWord# v16), wordToWord8# (word8ToWord# u17 `and#` word8ToWord# v17), wordToWord8# (word8ToWord# u18 `and#` word8ToWord# v18), wordToWord8# (word8ToWord# u19 `and#` word8ToWord# v19), wordToWord8# (word8ToWord# u20 `and#` word8ToWord# v20), wordToWord8# (word8ToWord# u21 `and#` word8ToWord# v21), wordToWord8# (word8ToWord# u22 `and#` word8ToWord# v22), wordToWord8# (word8ToWord# u23 `and#` word8ToWord# v23), wordToWord8# (word8ToWord# u24 `and#` word8ToWord# v24), wordToWord8# (word8ToWord# u25 `and#` word8ToWord# v25), wordToWord8# (word8ToWord# u26 `and#` word8ToWord# v26), wordToWord8# (word8ToWord# u27 `and#` word8ToWord# v27), wordToWord8# (word8ToWord# u28 `and#` word8ToWord# v28), wordToWord8# (word8ToWord# u29 `and#` word8ToWord# v29), wordToWord8# (word8ToWord# u30 `and#` word8ToWord# v30), wordToWord8# (word8ToWord# u31 `and#` word8ToWord# v31), wordToWord8# (word8ToWord# u32 `and#` word8ToWord# v32), wordToWord8# (word8ToWord# u33 `and#` word8ToWord# v33), wordToWord8# (word8ToWord# u34 `and#` word8ToWord# v34), wordToWord8# (word8ToWord# u35 `and#` word8ToWord# v35), wordToWord8# (word8ToWord# u36 `and#` word8ToWord# v36), wordToWord8# (word8ToWord# u37 `and#` word8ToWord# v37), wordToWord8# (word8ToWord# u38 `and#` word8ToWord# v38), wordToWord8# (word8ToWord# u39 `and#` word8ToWord# v39), wordToWord8# (word8ToWord# u40 `and#` word8ToWord# v40), wordToWord8# (word8ToWord# u41 `and#` word8ToWord# v41), wordToWord8# (word8ToWord# u42 `and#` word8ToWord# v42), wordToWord8# (word8ToWord# u43 `and#` word8ToWord# v43), wordToWord8# (word8ToWord# u44 `and#` word8ToWord# v44), wordToWord8# (word8ToWord# u45 `and#` word8ToWord# v45), wordToWord8# (word8ToWord# u46 `and#` word8ToWord# v46), wordToWord8# (word8ToWord# u47 `and#` word8ToWord# v47), wordToWord8# (word8ToWord# u48 `and#` word8ToWord# v48), wordToWord8# (word8ToWord# u49 `and#` word8ToWord# v49), wordToWord8# (word8ToWord# u50 `and#` word8ToWord# v50), wordToWord8# (word8ToWord# u51 `and#` word8ToWord# v51), wordToWord8# (word8ToWord# u52 `and#` word8ToWord# v52), wordToWord8# (word8ToWord# u53 `and#` word8ToWord# v53), wordToWord8# (word8ToWord# u54 `and#` word8ToWord# v54), wordToWord8# (word8ToWord# u55 `and#` word8ToWord# v55), wordToWord8# (word8ToWord# u56 `and#` word8ToWord# v56), wordToWord8# (word8ToWord# u57 `and#` word8ToWord# v57), wordToWord8# (word8ToWord# u58 `and#` word8ToWord# v58), wordToWord8# (word8ToWord# u59 `and#` word8ToWord# v59), wordToWord8# (word8ToWord# u60 `and#` word8ToWord# v60), wordToWord8# (word8ToWord# u61 `and#` word8ToWord# v61), wordToWord8# (word8ToWord# u62 `and#` word8ToWord# v62), wordToWord8# (word8ToWord# u63 `and#` word8ToWord# v63) #)
{-# INLINE [0] andWord8X64# #-}

andWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#
andWord16X32# u v = case unpackWord16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackWord16X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packWord16X32# (# wordToWord16# (word16ToWord# u0 `and#` word16ToWord# v0), wordToWord16# (word16ToWord# u1 `and#` word16ToWord# v1), wordToWord16# (word16ToWord# u2 `and#` word16ToWord# v2), wordToWord16# (word16ToWord# u3 `and#` word16ToWord# v3), wordToWord16# (word16ToWord# u4 `and#` word16ToWord# v4), wordToWord16# (word16ToWord# u5 `and#` word16ToWord# v5), wordToWord16# (word16ToWord# u6 `and#` word16ToWord# v6), wordToWord16# (word16ToWord# u7 `and#` word16ToWord# v7), wordToWord16# (word16ToWord# u8 `and#` word16ToWord# v8), wordToWord16# (word16ToWord# u9 `and#` word16ToWord# v9), wordToWord16# (word16ToWord# u10 `and#` word16ToWord# v10), wordToWord16# (word16ToWord# u11 `and#` word16ToWord# v11), wordToWord16# (word16ToWord# u12 `and#` word16ToWord# v12), wordToWord16# (word16ToWord# u13 `and#` word16ToWord# v13), wordToWord16# (word16ToWord# u14 `and#` word16ToWord# v14), wordToWord16# (word16ToWord# u15 `and#` word16ToWord# v15), wordToWord16# (word16ToWord# u16 `and#` word16ToWord# v16), wordToWord16# (word16ToWord# u17 `and#` word16ToWord# v17), wordToWord16# (word16ToWord# u18 `and#` word16ToWord# v18), wordToWord16# (word16ToWord# u19 `and#` word16ToWord# v19), wordToWord16# (word16ToWord# u20 `and#` word16ToWord# v20), wordToWord16# (word16ToWord# u21 `and#` word16ToWord# v21), wordToWord16# (word16ToWord# u22 `and#` word16ToWord# v22), wordToWord16# (word16ToWord# u23 `and#` word16ToWord# v23), wordToWord16# (word16ToWord# u24 `and#` word16ToWord# v24), wordToWord16# (word16ToWord# u25 `and#` word16ToWord# v25), wordToWord16# (word16ToWord# u26 `and#` word16ToWord# v26), wordToWord16# (word16ToWord# u27 `and#` word16ToWord# v27), wordToWord16# (word16ToWord# u28 `and#` word16ToWord# v28), wordToWord16# (word16ToWord# u29 `and#` word16ToWord# v29), wordToWord16# (word16ToWord# u30 `and#` word16ToWord# v30), wordToWord16# (word16ToWord# u31 `and#` word16ToWord# v31) #)
{-# INLINE [0] andWord16X32# #-}

andWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#
andWord32X16# u v = case unpackWord32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord32X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord32X16# (# wordToWord32# (word32ToWord# u0 `and#` word32ToWord# v0), wordToWord32# (word32ToWord# u1 `and#` word32ToWord# v1), wordToWord32# (word32ToWord# u2 `and#` word32ToWord# v2), wordToWord32# (word32ToWord# u3 `and#` word32ToWord# v3), wordToWord32# (word32ToWord# u4 `and#` word32ToWord# v4), wordToWord32# (word32ToWord# u5 `and#` word32ToWord# v5), wordToWord32# (word32ToWord# u6 `and#` word32ToWord# v6), wordToWord32# (word32ToWord# u7 `and#` word32ToWord# v7), wordToWord32# (word32ToWord# u8 `and#` word32ToWord# v8), wordToWord32# (word32ToWord# u9 `and#` word32ToWord# v9), wordToWord32# (word32ToWord# u10 `and#` word32ToWord# v10), wordToWord32# (word32ToWord# u11 `and#` word32ToWord# v11), wordToWord32# (word32ToWord# u12 `and#` word32ToWord# v12), wordToWord32# (word32ToWord# u13 `and#` word32ToWord# v13), wordToWord32# (word32ToWord# u14 `and#` word32ToWord# v14), wordToWord32# (word32ToWord# u15 `and#` word32ToWord# v15) #)
{-# INLINE [0] andWord32X16# #-}

andWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#
andWord64X8# u v = case unpackWord64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord64X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord64X8# (# and64# u0 v0, and64# u1 v1, and64# u2 v2, and64# u3 v3, and64# u4 v4, and64# u5 v5, and64# u6 v6, and64# u7 v7 #)
{-# INLINE [0] andWord64X8# #-}

orInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#
orInt8X64# u v = case unpackInt8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> case unpackInt8X64# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60, v61, v62, v63 #) -> packInt8X64# (# intToInt8# (int8ToInt# u0 `orI#` int8ToInt# v0), intToInt8# (int8ToInt# u1 `orI#` int8ToInt# v1), intToInt8# (int8ToInt# u2 `orI#` int8ToInt# v2), intToInt8# (int8ToInt# u3 `orI#` int8ToInt# v3), intToInt8# (int8ToInt# u4 `orI#` int8ToInt# v4), intToInt8# (int8ToInt# u5 `orI#` int8ToInt# v5), intToInt8# (int8ToInt# u6 `orI#` int8ToInt# v6), intToInt8# (int8ToInt# u7 `orI#` int8ToInt# v7), intToInt8# (int8ToInt# u8 `orI#` int8ToInt# v8), intToInt8# (int8ToInt# u9 `orI#` int8ToInt# v9), intToInt8# (int8ToInt# u10 `orI#` int8ToInt# v10), intToInt8# (int8ToInt# u11 `orI#` int8ToInt# v11), intToInt8# (int8ToInt# u12 `orI#` int8ToInt# v12), intToInt8# (int8ToInt# u13 `orI#` int8ToInt# v13), intToInt8# (int8ToInt# u14 `orI#` int8ToInt# v14), intToInt8# (int8ToInt# u15 `orI#` int8ToInt# v15), intToInt8# (int8ToInt# u16 `orI#` int8ToInt# v16), intToInt8# (int8ToInt# u17 `orI#` int8ToInt# v17), intToInt8# (int8ToInt# u18 `orI#` int8ToInt# v18), intToInt8# (int8ToInt# u19 `orI#` int8ToInt# v19), intToInt8# (int8ToInt# u20 `orI#` int8ToInt# v20), intToInt8# (int8ToInt# u21 `orI#` int8ToInt# v21), intToInt8# (int8ToInt# u22 `orI#` int8ToInt# v22), intToInt8# (int8ToInt# u23 `orI#` int8ToInt# v23), intToInt8# (int8ToInt# u24 `orI#` int8ToInt# v24), intToInt8# (int8ToInt# u25 `orI#` int8ToInt# v25), intToInt8# (int8ToInt# u26 `orI#` int8ToInt# v26), intToInt8# (int8ToInt# u27 `orI#` int8ToInt# v27), intToInt8# (int8ToInt# u28 `orI#` int8ToInt# v28), intToInt8# (int8ToInt# u29 `orI#` int8ToInt# v29), intToInt8# (int8ToInt# u30 `orI#` int8ToInt# v30), intToInt8# (int8ToInt# u31 `orI#` int8ToInt# v31), intToInt8# (int8ToInt# u32 `orI#` int8ToInt# v32), intToInt8# (int8ToInt# u33 `orI#` int8ToInt# v33), intToInt8# (int8ToInt# u34 `orI#` int8ToInt# v34), intToInt8# (int8ToInt# u35 `orI#` int8ToInt# v35), intToInt8# (int8ToInt# u36 `orI#` int8ToInt# v36), intToInt8# (int8ToInt# u37 `orI#` int8ToInt# v37), intToInt8# (int8ToInt# u38 `orI#` int8ToInt# v38), intToInt8# (int8ToInt# u39 `orI#` int8ToInt# v39), intToInt8# (int8ToInt# u40 `orI#` int8ToInt# v40), intToInt8# (int8ToInt# u41 `orI#` int8ToInt# v41), intToInt8# (int8ToInt# u42 `orI#` int8ToInt# v42), intToInt8# (int8ToInt# u43 `orI#` int8ToInt# v43), intToInt8# (int8ToInt# u44 `orI#` int8ToInt# v44), intToInt8# (int8ToInt# u45 `orI#` int8ToInt# v45), intToInt8# (int8ToInt# u46 `orI#` int8ToInt# v46), intToInt8# (int8ToInt# u47 `orI#` int8ToInt# v47), intToInt8# (int8ToInt# u48 `orI#` int8ToInt# v48), intToInt8# (int8ToInt# u49 `orI#` int8ToInt# v49), intToInt8# (int8ToInt# u50 `orI#` int8ToInt# v50), intToInt8# (int8ToInt# u51 `orI#` int8ToInt# v51), intToInt8# (int8ToInt# u52 `orI#` int8ToInt# v52), intToInt8# (int8ToInt# u53 `orI#` int8ToInt# v53), intToInt8# (int8ToInt# u54 `orI#` int8ToInt# v54), intToInt8# (int8ToInt# u55 `orI#` int8ToInt# v55), intToInt8# (int8ToInt# u56 `orI#` int8ToInt# v56), intToInt8# (int8ToInt# u57 `orI#` int8ToInt# v57), intToInt8# (int8ToInt# u58 `orI#` int8ToInt# v58), intToInt8# (int8ToInt# u59 `orI#` int8ToInt# v59), intToInt8# (int8ToInt# u60 `orI#` int8ToInt# v60), intToInt8# (int8ToInt# u61 `orI#` int8ToInt# v61), intToInt8# (int8ToInt# u62 `orI#` int8ToInt# v62), intToInt8# (int8ToInt# u63 `orI#` int8ToInt# v63) #)
{-# INLINE [0] orInt8X64# #-}

orInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#
orInt16X32# u v = case unpackInt16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackInt16X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packInt16X32# (# intToInt16# (int16ToInt# u0 `orI#` int16ToInt# v0), intToInt16# (int16ToInt# u1 `orI#` int16ToInt# v1), intToInt16# (int16ToInt# u2 `orI#` int16ToInt# v2), intToInt16# (int16ToInt# u3 `orI#` int16ToInt# v3), intToInt16# (int16ToInt# u4 `orI#` int16ToInt# v4), intToInt16# (int16ToInt# u5 `orI#` int16ToInt# v5), intToInt16# (int16ToInt# u6 `orI#` int16ToInt# v6), intToInt16# (int16ToInt# u7 `orI#` int16ToInt# v7), intToInt16# (int16ToInt# u8 `orI#` int16ToInt# v8), intToInt16# (int16ToInt# u9 `orI#` int16ToInt# v9), intToInt16# (int16ToInt# u10 `orI#` int16ToInt# v10), intToInt16# (int16ToInt# u11 `orI#` int16ToInt# v11), intToInt16# (int16ToInt# u12 `orI#` int16ToInt# v12), intToInt16# (int16ToInt# u13 `orI#` int16ToInt# v13), intToInt16# (int16ToInt# u14 `orI#` int16ToInt# v14), intToInt16# (int16ToInt# u15 `orI#` int16ToInt# v15), intToInt16# (int16ToInt# u16 `orI#` int16ToInt# v16), intToInt16# (int16ToInt# u17 `orI#` int16ToInt# v17), intToInt16# (int16ToInt# u18 `orI#` int16ToInt# v18), intToInt16# (int16ToInt# u19 `orI#` int16ToInt# v19), intToInt16# (int16ToInt# u20 `orI#` int16ToInt# v20), intToInt16# (int16ToInt# u21 `orI#` int16ToInt# v21), intToInt16# (int16ToInt# u22 `orI#` int16ToInt# v22), intToInt16# (int16ToInt# u23 `orI#` int16ToInt# v23), intToInt16# (int16ToInt# u24 `orI#` int16ToInt# v24), intToInt16# (int16ToInt# u25 `orI#` int16ToInt# v25), intToInt16# (int16ToInt# u26 `orI#` int16ToInt# v26), intToInt16# (int16ToInt# u27 `orI#` int16ToInt# v27), intToInt16# (int16ToInt# u28 `orI#` int16ToInt# v28), intToInt16# (int16ToInt# u29 `orI#` int16ToInt# v29), intToInt16# (int16ToInt# u30 `orI#` int16ToInt# v30), intToInt16# (int16ToInt# u31 `orI#` int16ToInt# v31) #)
{-# INLINE [0] orInt16X32# #-}

orInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#
orInt32X16# u v = case unpackInt32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt32X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt32X16# (# intToInt32# (int32ToInt# u0 `orI#` int32ToInt# v0), intToInt32# (int32ToInt# u1 `orI#` int32ToInt# v1), intToInt32# (int32ToInt# u2 `orI#` int32ToInt# v2), intToInt32# (int32ToInt# u3 `orI#` int32ToInt# v3), intToInt32# (int32ToInt# u4 `orI#` int32ToInt# v4), intToInt32# (int32ToInt# u5 `orI#` int32ToInt# v5), intToInt32# (int32ToInt# u6 `orI#` int32ToInt# v6), intToInt32# (int32ToInt# u7 `orI#` int32ToInt# v7), intToInt32# (int32ToInt# u8 `orI#` int32ToInt# v8), intToInt32# (int32ToInt# u9 `orI#` int32ToInt# v9), intToInt32# (int32ToInt# u10 `orI#` int32ToInt# v10), intToInt32# (int32ToInt# u11 `orI#` int32ToInt# v11), intToInt32# (int32ToInt# u12 `orI#` int32ToInt# v12), intToInt32# (int32ToInt# u13 `orI#` int32ToInt# v13), intToInt32# (int32ToInt# u14 `orI#` int32ToInt# v14), intToInt32# (int32ToInt# u15 `orI#` int32ToInt# v15) #)
{-# INLINE [0] orInt32X16# #-}

orInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#
orInt64X8# u v = case unpackInt64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt64X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt64X8# (# word64ToInt64# (int64ToWord64# u0 `or64#` int64ToWord64# v0), word64ToInt64# (int64ToWord64# u1 `or64#` int64ToWord64# v1), word64ToInt64# (int64ToWord64# u2 `or64#` int64ToWord64# v2), word64ToInt64# (int64ToWord64# u3 `or64#` int64ToWord64# v3), word64ToInt64# (int64ToWord64# u4 `or64#` int64ToWord64# v4), word64ToInt64# (int64ToWord64# u5 `or64#` int64ToWord64# v5), word64ToInt64# (int64ToWord64# u6 `or64#` int64ToWord64# v6), word64ToInt64# (int64ToWord64# u7 `or64#` int64ToWord64# v7) #)
{-# INLINE [0] orInt64X8# #-}

orWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#
orWord8X64# u v = case unpackWord8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> case unpackWord8X64# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60, v61, v62, v63 #) -> packWord8X64# (# wordToWord8# (word8ToWord# u0 `or#` word8ToWord# v0), wordToWord8# (word8ToWord# u1 `or#` word8ToWord# v1), wordToWord8# (word8ToWord# u2 `or#` word8ToWord# v2), wordToWord8# (word8ToWord# u3 `or#` word8ToWord# v3), wordToWord8# (word8ToWord# u4 `or#` word8ToWord# v4), wordToWord8# (word8ToWord# u5 `or#` word8ToWord# v5), wordToWord8# (word8ToWord# u6 `or#` word8ToWord# v6), wordToWord8# (word8ToWord# u7 `or#` word8ToWord# v7), wordToWord8# (word8ToWord# u8 `or#` word8ToWord# v8), wordToWord8# (word8ToWord# u9 `or#` word8ToWord# v9), wordToWord8# (word8ToWord# u10 `or#` word8ToWord# v10), wordToWord8# (word8ToWord# u11 `or#` word8ToWord# v11), wordToWord8# (word8ToWord# u12 `or#` word8ToWord# v12), wordToWord8# (word8ToWord# u13 `or#` word8ToWord# v13), wordToWord8# (word8ToWord# u14 `or#` word8ToWord# v14), wordToWord8# (word8ToWord# u15 `or#` word8ToWord# v15), wordToWord8# (word8ToWord# u16 `or#` word8ToWord# v16), wordToWord8# (word8ToWord# u17 `or#` word8ToWord# v17), wordToWord8# (word8ToWord# u18 `or#` word8ToWord# v18), wordToWord8# (word8ToWord# u19 `or#` word8ToWord# v19), wordToWord8# (word8ToWord# u20 `or#` word8ToWord# v20), wordToWord8# (word8ToWord# u21 `or#` word8ToWord# v21), wordToWord8# (word8ToWord# u22 `or#` word8ToWord# v22), wordToWord8# (word8ToWord# u23 `or#` word8ToWord# v23), wordToWord8# (word8ToWord# u24 `or#` word8ToWord# v24), wordToWord8# (word8ToWord# u25 `or#` word8ToWord# v25), wordToWord8# (word8ToWord# u26 `or#` word8ToWord# v26), wordToWord8# (word8ToWord# u27 `or#` word8ToWord# v27), wordToWord8# (word8ToWord# u28 `or#` word8ToWord# v28), wordToWord8# (word8ToWord# u29 `or#` word8ToWord# v29), wordToWord8# (word8ToWord# u30 `or#` word8ToWord# v30), wordToWord8# (word8ToWord# u31 `or#` word8ToWord# v31), wordToWord8# (word8ToWord# u32 `or#` word8ToWord# v32), wordToWord8# (word8ToWord# u33 `or#` word8ToWord# v33), wordToWord8# (word8ToWord# u34 `or#` word8ToWord# v34), wordToWord8# (word8ToWord# u35 `or#` word8ToWord# v35), wordToWord8# (word8ToWord# u36 `or#` word8ToWord# v36), wordToWord8# (word8ToWord# u37 `or#` word8ToWord# v37), wordToWord8# (word8ToWord# u38 `or#` word8ToWord# v38), wordToWord8# (word8ToWord# u39 `or#` word8ToWord# v39), wordToWord8# (word8ToWord# u40 `or#` word8ToWord# v40), wordToWord8# (word8ToWord# u41 `or#` word8ToWord# v41), wordToWord8# (word8ToWord# u42 `or#` word8ToWord# v42), wordToWord8# (word8ToWord# u43 `or#` word8ToWord# v43), wordToWord8# (word8ToWord# u44 `or#` word8ToWord# v44), wordToWord8# (word8ToWord# u45 `or#` word8ToWord# v45), wordToWord8# (word8ToWord# u46 `or#` word8ToWord# v46), wordToWord8# (word8ToWord# u47 `or#` word8ToWord# v47), wordToWord8# (word8ToWord# u48 `or#` word8ToWord# v48), wordToWord8# (word8ToWord# u49 `or#` word8ToWord# v49), wordToWord8# (word8ToWord# u50 `or#` word8ToWord# v50), wordToWord8# (word8ToWord# u51 `or#` word8ToWord# v51), wordToWord8# (word8ToWord# u52 `or#` word8ToWord# v52), wordToWord8# (word8ToWord# u53 `or#` word8ToWord# v53), wordToWord8# (word8ToWord# u54 `or#` word8ToWord# v54), wordToWord8# (word8ToWord# u55 `or#` word8ToWord# v55), wordToWord8# (word8ToWord# u56 `or#` word8ToWord# v56), wordToWord8# (word8ToWord# u57 `or#` word8ToWord# v57), wordToWord8# (word8ToWord# u58 `or#` word8ToWord# v58), wordToWord8# (word8ToWord# u59 `or#` word8ToWord# v59), wordToWord8# (word8ToWord# u60 `or#` word8ToWord# v60), wordToWord8# (word8ToWord# u61 `or#` word8ToWord# v61), wordToWord8# (word8ToWord# u62 `or#` word8ToWord# v62), wordToWord8# (word8ToWord# u63 `or#` word8ToWord# v63) #)
{-# INLINE [0] orWord8X64# #-}

orWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#
orWord16X32# u v = case unpackWord16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackWord16X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packWord16X32# (# wordToWord16# (word16ToWord# u0 `or#` word16ToWord# v0), wordToWord16# (word16ToWord# u1 `or#` word16ToWord# v1), wordToWord16# (word16ToWord# u2 `or#` word16ToWord# v2), wordToWord16# (word16ToWord# u3 `or#` word16ToWord# v3), wordToWord16# (word16ToWord# u4 `or#` word16ToWord# v4), wordToWord16# (word16ToWord# u5 `or#` word16ToWord# v5), wordToWord16# (word16ToWord# u6 `or#` word16ToWord# v6), wordToWord16# (word16ToWord# u7 `or#` word16ToWord# v7), wordToWord16# (word16ToWord# u8 `or#` word16ToWord# v8), wordToWord16# (word16ToWord# u9 `or#` word16ToWord# v9), wordToWord16# (word16ToWord# u10 `or#` word16ToWord# v10), wordToWord16# (word16ToWord# u11 `or#` word16ToWord# v11), wordToWord16# (word16ToWord# u12 `or#` word16ToWord# v12), wordToWord16# (word16ToWord# u13 `or#` word16ToWord# v13), wordToWord16# (word16ToWord# u14 `or#` word16ToWord# v14), wordToWord16# (word16ToWord# u15 `or#` word16ToWord# v15), wordToWord16# (word16ToWord# u16 `or#` word16ToWord# v16), wordToWord16# (word16ToWord# u17 `or#` word16ToWord# v17), wordToWord16# (word16ToWord# u18 `or#` word16ToWord# v18), wordToWord16# (word16ToWord# u19 `or#` word16ToWord# v19), wordToWord16# (word16ToWord# u20 `or#` word16ToWord# v20), wordToWord16# (word16ToWord# u21 `or#` word16ToWord# v21), wordToWord16# (word16ToWord# u22 `or#` word16ToWord# v22), wordToWord16# (word16ToWord# u23 `or#` word16ToWord# v23), wordToWord16# (word16ToWord# u24 `or#` word16ToWord# v24), wordToWord16# (word16ToWord# u25 `or#` word16ToWord# v25), wordToWord16# (word16ToWord# u26 `or#` word16ToWord# v26), wordToWord16# (word16ToWord# u27 `or#` word16ToWord# v27), wordToWord16# (word16ToWord# u28 `or#` word16ToWord# v28), wordToWord16# (word16ToWord# u29 `or#` word16ToWord# v29), wordToWord16# (word16ToWord# u30 `or#` word16ToWord# v30), wordToWord16# (word16ToWord# u31 `or#` word16ToWord# v31) #)
{-# INLINE [0] orWord16X32# #-}

orWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#
orWord32X16# u v = case unpackWord32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord32X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord32X16# (# wordToWord32# (word32ToWord# u0 `or#` word32ToWord# v0), wordToWord32# (word32ToWord# u1 `or#` word32ToWord# v1), wordToWord32# (word32ToWord# u2 `or#` word32ToWord# v2), wordToWord32# (word32ToWord# u3 `or#` word32ToWord# v3), wordToWord32# (word32ToWord# u4 `or#` word32ToWord# v4), wordToWord32# (word32ToWord# u5 `or#` word32ToWord# v5), wordToWord32# (word32ToWord# u6 `or#` word32ToWord# v6), wordToWord32# (word32ToWord# u7 `or#` word32ToWord# v7), wordToWord32# (word32ToWord# u8 `or#` word32ToWord# v8), wordToWord32# (word32ToWord# u9 `or#` word32ToWord# v9), wordToWord32# (word32ToWord# u10 `or#` word32ToWord# v10), wordToWord32# (word32ToWord# u11 `or#` word32ToWord# v11), wordToWord32# (word32ToWord# u12 `or#` word32ToWord# v12), wordToWord32# (word32ToWord# u13 `or#` word32ToWord# v13), wordToWord32# (word32ToWord# u14 `or#` word32ToWord# v14), wordToWord32# (word32ToWord# u15 `or#` word32ToWord# v15) #)
{-# INLINE [0] orWord32X16# #-}

orWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#
orWord64X8# u v = case unpackWord64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord64X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord64X8# (# or64# u0 v0, or64# u1 v1, or64# u2 v2, or64# u3 v3, or64# u4 v4, or64# u5 v5, or64# u6 v6, or64# u7 v7 #)
{-# INLINE [0] orWord64X8# #-}

xorInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#
xorInt8X64# u v = case unpackInt8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> case unpackInt8X64# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60, v61, v62, v63 #) -> packInt8X64# (# intToInt8# (int8ToInt# u0 `xorI#` int8ToInt# v0), intToInt8# (int8ToInt# u1 `xorI#` int8ToInt# v1), intToInt8# (int8ToInt# u2 `xorI#` int8ToInt# v2), intToInt8# (int8ToInt# u3 `xorI#` int8ToInt# v3), intToInt8# (int8ToInt# u4 `xorI#` int8ToInt# v4), intToInt8# (int8ToInt# u5 `xorI#` int8ToInt# v5), intToInt8# (int8ToInt# u6 `xorI#` int8ToInt# v6), intToInt8# (int8ToInt# u7 `xorI#` int8ToInt# v7), intToInt8# (int8ToInt# u8 `xorI#` int8ToInt# v8), intToInt8# (int8ToInt# u9 `xorI#` int8ToInt# v9), intToInt8# (int8ToInt# u10 `xorI#` int8ToInt# v10), intToInt8# (int8ToInt# u11 `xorI#` int8ToInt# v11), intToInt8# (int8ToInt# u12 `xorI#` int8ToInt# v12), intToInt8# (int8ToInt# u13 `xorI#` int8ToInt# v13), intToInt8# (int8ToInt# u14 `xorI#` int8ToInt# v14), intToInt8# (int8ToInt# u15 `xorI#` int8ToInt# v15), intToInt8# (int8ToInt# u16 `xorI#` int8ToInt# v16), intToInt8# (int8ToInt# u17 `xorI#` int8ToInt# v17), intToInt8# (int8ToInt# u18 `xorI#` int8ToInt# v18), intToInt8# (int8ToInt# u19 `xorI#` int8ToInt# v19), intToInt8# (int8ToInt# u20 `xorI#` int8ToInt# v20), intToInt8# (int8ToInt# u21 `xorI#` int8ToInt# v21), intToInt8# (int8ToInt# u22 `xorI#` int8ToInt# v22), intToInt8# (int8ToInt# u23 `xorI#` int8ToInt# v23), intToInt8# (int8ToInt# u24 `xorI#` int8ToInt# v24), intToInt8# (int8ToInt# u25 `xorI#` int8ToInt# v25), intToInt8# (int8ToInt# u26 `xorI#` int8ToInt# v26), intToInt8# (int8ToInt# u27 `xorI#` int8ToInt# v27), intToInt8# (int8ToInt# u28 `xorI#` int8ToInt# v28), intToInt8# (int8ToInt# u29 `xorI#` int8ToInt# v29), intToInt8# (int8ToInt# u30 `xorI#` int8ToInt# v30), intToInt8# (int8ToInt# u31 `xorI#` int8ToInt# v31), intToInt8# (int8ToInt# u32 `xorI#` int8ToInt# v32), intToInt8# (int8ToInt# u33 `xorI#` int8ToInt# v33), intToInt8# (int8ToInt# u34 `xorI#` int8ToInt# v34), intToInt8# (int8ToInt# u35 `xorI#` int8ToInt# v35), intToInt8# (int8ToInt# u36 `xorI#` int8ToInt# v36), intToInt8# (int8ToInt# u37 `xorI#` int8ToInt# v37), intToInt8# (int8ToInt# u38 `xorI#` int8ToInt# v38), intToInt8# (int8ToInt# u39 `xorI#` int8ToInt# v39), intToInt8# (int8ToInt# u40 `xorI#` int8ToInt# v40), intToInt8# (int8ToInt# u41 `xorI#` int8ToInt# v41), intToInt8# (int8ToInt# u42 `xorI#` int8ToInt# v42), intToInt8# (int8ToInt# u43 `xorI#` int8ToInt# v43), intToInt8# (int8ToInt# u44 `xorI#` int8ToInt# v44), intToInt8# (int8ToInt# u45 `xorI#` int8ToInt# v45), intToInt8# (int8ToInt# u46 `xorI#` int8ToInt# v46), intToInt8# (int8ToInt# u47 `xorI#` int8ToInt# v47), intToInt8# (int8ToInt# u48 `xorI#` int8ToInt# v48), intToInt8# (int8ToInt# u49 `xorI#` int8ToInt# v49), intToInt8# (int8ToInt# u50 `xorI#` int8ToInt# v50), intToInt8# (int8ToInt# u51 `xorI#` int8ToInt# v51), intToInt8# (int8ToInt# u52 `xorI#` int8ToInt# v52), intToInt8# (int8ToInt# u53 `xorI#` int8ToInt# v53), intToInt8# (int8ToInt# u54 `xorI#` int8ToInt# v54), intToInt8# (int8ToInt# u55 `xorI#` int8ToInt# v55), intToInt8# (int8ToInt# u56 `xorI#` int8ToInt# v56), intToInt8# (int8ToInt# u57 `xorI#` int8ToInt# v57), intToInt8# (int8ToInt# u58 `xorI#` int8ToInt# v58), intToInt8# (int8ToInt# u59 `xorI#` int8ToInt# v59), intToInt8# (int8ToInt# u60 `xorI#` int8ToInt# v60), intToInt8# (int8ToInt# u61 `xorI#` int8ToInt# v61), intToInt8# (int8ToInt# u62 `xorI#` int8ToInt# v62), intToInt8# (int8ToInt# u63 `xorI#` int8ToInt# v63) #)
{-# INLINE [0] xorInt8X64# #-}

xorInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#
xorInt16X32# u v = case unpackInt16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackInt16X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packInt16X32# (# intToInt16# (int16ToInt# u0 `xorI#` int16ToInt# v0), intToInt16# (int16ToInt# u1 `xorI#` int16ToInt# v1), intToInt16# (int16ToInt# u2 `xorI#` int16ToInt# v2), intToInt16# (int16ToInt# u3 `xorI#` int16ToInt# v3), intToInt16# (int16ToInt# u4 `xorI#` int16ToInt# v4), intToInt16# (int16ToInt# u5 `xorI#` int16ToInt# v5), intToInt16# (int16ToInt# u6 `xorI#` int16ToInt# v6), intToInt16# (int16ToInt# u7 `xorI#` int16ToInt# v7), intToInt16# (int16ToInt# u8 `xorI#` int16ToInt# v8), intToInt16# (int16ToInt# u9 `xorI#` int16ToInt# v9), intToInt16# (int16ToInt# u10 `xorI#` int16ToInt# v10), intToInt16# (int16ToInt# u11 `xorI#` int16ToInt# v11), intToInt16# (int16ToInt# u12 `xorI#` int16ToInt# v12), intToInt16# (int16ToInt# u13 `xorI#` int16ToInt# v13), intToInt16# (int16ToInt# u14 `xorI#` int16ToInt# v14), intToInt16# (int16ToInt# u15 `xorI#` int16ToInt# v15), intToInt16# (int16ToInt# u16 `xorI#` int16ToInt# v16), intToInt16# (int16ToInt# u17 `xorI#` int16ToInt# v17), intToInt16# (int16ToInt# u18 `xorI#` int16ToInt# v18), intToInt16# (int16ToInt# u19 `xorI#` int16ToInt# v19), intToInt16# (int16ToInt# u20 `xorI#` int16ToInt# v20), intToInt16# (int16ToInt# u21 `xorI#` int16ToInt# v21), intToInt16# (int16ToInt# u22 `xorI#` int16ToInt# v22), intToInt16# (int16ToInt# u23 `xorI#` int16ToInt# v23), intToInt16# (int16ToInt# u24 `xorI#` int16ToInt# v24), intToInt16# (int16ToInt# u25 `xorI#` int16ToInt# v25), intToInt16# (int16ToInt# u26 `xorI#` int16ToInt# v26), intToInt16# (int16ToInt# u27 `xorI#` int16ToInt# v27), intToInt16# (int16ToInt# u28 `xorI#` int16ToInt# v28), intToInt16# (int16ToInt# u29 `xorI#` int16ToInt# v29), intToInt16# (int16ToInt# u30 `xorI#` int16ToInt# v30), intToInt16# (int16ToInt# u31 `xorI#` int16ToInt# v31) #)
{-# INLINE [0] xorInt16X32# #-}

xorInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#
xorInt32X16# u v = case unpackInt32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt32X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt32X16# (# intToInt32# (int32ToInt# u0 `xorI#` int32ToInt# v0), intToInt32# (int32ToInt# u1 `xorI#` int32ToInt# v1), intToInt32# (int32ToInt# u2 `xorI#` int32ToInt# v2), intToInt32# (int32ToInt# u3 `xorI#` int32ToInt# v3), intToInt32# (int32ToInt# u4 `xorI#` int32ToInt# v4), intToInt32# (int32ToInt# u5 `xorI#` int32ToInt# v5), intToInt32# (int32ToInt# u6 `xorI#` int32ToInt# v6), intToInt32# (int32ToInt# u7 `xorI#` int32ToInt# v7), intToInt32# (int32ToInt# u8 `xorI#` int32ToInt# v8), intToInt32# (int32ToInt# u9 `xorI#` int32ToInt# v9), intToInt32# (int32ToInt# u10 `xorI#` int32ToInt# v10), intToInt32# (int32ToInt# u11 `xorI#` int32ToInt# v11), intToInt32# (int32ToInt# u12 `xorI#` int32ToInt# v12), intToInt32# (int32ToInt# u13 `xorI#` int32ToInt# v13), intToInt32# (int32ToInt# u14 `xorI#` int32ToInt# v14), intToInt32# (int32ToInt# u15 `xorI#` int32ToInt# v15) #)
{-# INLINE [0] xorInt32X16# #-}

xorInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#
xorInt64X8# u v = case unpackInt64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt64X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt64X8# (# word64ToInt64# (int64ToWord64# u0 `xor64#` int64ToWord64# v0), word64ToInt64# (int64ToWord64# u1 `xor64#` int64ToWord64# v1), word64ToInt64# (int64ToWord64# u2 `xor64#` int64ToWord64# v2), word64ToInt64# (int64ToWord64# u3 `xor64#` int64ToWord64# v3), word64ToInt64# (int64ToWord64# u4 `xor64#` int64ToWord64# v4), word64ToInt64# (int64ToWord64# u5 `xor64#` int64ToWord64# v5), word64ToInt64# (int64ToWord64# u6 `xor64#` int64ToWord64# v6), word64ToInt64# (int64ToWord64# u7 `xor64#` int64ToWord64# v7) #)
{-# INLINE [0] xorInt64X8# #-}

xorWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#
xorWord8X64# u v = case unpackWord8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> case unpackWord8X64# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60, v61, v62, v63 #) -> packWord8X64# (# wordToWord8# (word8ToWord# u0 `xor#` word8ToWord# v0), wordToWord8# (word8ToWord# u1 `xor#` word8ToWord# v1), wordToWord8# (word8ToWord# u2 `xor#` word8ToWord# v2), wordToWord8# (word8ToWord# u3 `xor#` word8ToWord# v3), wordToWord8# (word8ToWord# u4 `xor#` word8ToWord# v4), wordToWord8# (word8ToWord# u5 `xor#` word8ToWord# v5), wordToWord8# (word8ToWord# u6 `xor#` word8ToWord# v6), wordToWord8# (word8ToWord# u7 `xor#` word8ToWord# v7), wordToWord8# (word8ToWord# u8 `xor#` word8ToWord# v8), wordToWord8# (word8ToWord# u9 `xor#` word8ToWord# v9), wordToWord8# (word8ToWord# u10 `xor#` word8ToWord# v10), wordToWord8# (word8ToWord# u11 `xor#` word8ToWord# v11), wordToWord8# (word8ToWord# u12 `xor#` word8ToWord# v12), wordToWord8# (word8ToWord# u13 `xor#` word8ToWord# v13), wordToWord8# (word8ToWord# u14 `xor#` word8ToWord# v14), wordToWord8# (word8ToWord# u15 `xor#` word8ToWord# v15), wordToWord8# (word8ToWord# u16 `xor#` word8ToWord# v16), wordToWord8# (word8ToWord# u17 `xor#` word8ToWord# v17), wordToWord8# (word8ToWord# u18 `xor#` word8ToWord# v18), wordToWord8# (word8ToWord# u19 `xor#` word8ToWord# v19), wordToWord8# (word8ToWord# u20 `xor#` word8ToWord# v20), wordToWord8# (word8ToWord# u21 `xor#` word8ToWord# v21), wordToWord8# (word8ToWord# u22 `xor#` word8ToWord# v22), wordToWord8# (word8ToWord# u23 `xor#` word8ToWord# v23), wordToWord8# (word8ToWord# u24 `xor#` word8ToWord# v24), wordToWord8# (word8ToWord# u25 `xor#` word8ToWord# v25), wordToWord8# (word8ToWord# u26 `xor#` word8ToWord# v26), wordToWord8# (word8ToWord# u27 `xor#` word8ToWord# v27), wordToWord8# (word8ToWord# u28 `xor#` word8ToWord# v28), wordToWord8# (word8ToWord# u29 `xor#` word8ToWord# v29), wordToWord8# (word8ToWord# u30 `xor#` word8ToWord# v30), wordToWord8# (word8ToWord# u31 `xor#` word8ToWord# v31), wordToWord8# (word8ToWord# u32 `xor#` word8ToWord# v32), wordToWord8# (word8ToWord# u33 `xor#` word8ToWord# v33), wordToWord8# (word8ToWord# u34 `xor#` word8ToWord# v34), wordToWord8# (word8ToWord# u35 `xor#` word8ToWord# v35), wordToWord8# (word8ToWord# u36 `xor#` word8ToWord# v36), wordToWord8# (word8ToWord# u37 `xor#` word8ToWord# v37), wordToWord8# (word8ToWord# u38 `xor#` word8ToWord# v38), wordToWord8# (word8ToWord# u39 `xor#` word8ToWord# v39), wordToWord8# (word8ToWord# u40 `xor#` word8ToWord# v40), wordToWord8# (word8ToWord# u41 `xor#` word8ToWord# v41), wordToWord8# (word8ToWord# u42 `xor#` word8ToWord# v42), wordToWord8# (word8ToWord# u43 `xor#` word8ToWord# v43), wordToWord8# (word8ToWord# u44 `xor#` word8ToWord# v44), wordToWord8# (word8ToWord# u45 `xor#` word8ToWord# v45), wordToWord8# (word8ToWord# u46 `xor#` word8ToWord# v46), wordToWord8# (word8ToWord# u47 `xor#` word8ToWord# v47), wordToWord8# (word8ToWord# u48 `xor#` word8ToWord# v48), wordToWord8# (word8ToWord# u49 `xor#` word8ToWord# v49), wordToWord8# (word8ToWord# u50 `xor#` word8ToWord# v50), wordToWord8# (word8ToWord# u51 `xor#` word8ToWord# v51), wordToWord8# (word8ToWord# u52 `xor#` word8ToWord# v52), wordToWord8# (word8ToWord# u53 `xor#` word8ToWord# v53), wordToWord8# (word8ToWord# u54 `xor#` word8ToWord# v54), wordToWord8# (word8ToWord# u55 `xor#` word8ToWord# v55), wordToWord8# (word8ToWord# u56 `xor#` word8ToWord# v56), wordToWord8# (word8ToWord# u57 `xor#` word8ToWord# v57), wordToWord8# (word8ToWord# u58 `xor#` word8ToWord# v58), wordToWord8# (word8ToWord# u59 `xor#` word8ToWord# v59), wordToWord8# (word8ToWord# u60 `xor#` word8ToWord# v60), wordToWord8# (word8ToWord# u61 `xor#` word8ToWord# v61), wordToWord8# (word8ToWord# u62 `xor#` word8ToWord# v62), wordToWord8# (word8ToWord# u63 `xor#` word8ToWord# v63) #)
{-# INLINE [0] xorWord8X64# #-}

xorWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#
xorWord16X32# u v = case unpackWord16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackWord16X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packWord16X32# (# wordToWord16# (word16ToWord# u0 `xor#` word16ToWord# v0), wordToWord16# (word16ToWord# u1 `xor#` word16ToWord# v1), wordToWord16# (word16ToWord# u2 `xor#` word16ToWord# v2), wordToWord16# (word16ToWord# u3 `xor#` word16ToWord# v3), wordToWord16# (word16ToWord# u4 `xor#` word16ToWord# v4), wordToWord16# (word16ToWord# u5 `xor#` word16ToWord# v5), wordToWord16# (word16ToWord# u6 `xor#` word16ToWord# v6), wordToWord16# (word16ToWord# u7 `xor#` word16ToWord# v7), wordToWord16# (word16ToWord# u8 `xor#` word16ToWord# v8), wordToWord16# (word16ToWord# u9 `xor#` word16ToWord# v9), wordToWord16# (word16ToWord# u10 `xor#` word16ToWord# v10), wordToWord16# (word16ToWord# u11 `xor#` word16ToWord# v11), wordToWord16# (word16ToWord# u12 `xor#` word16ToWord# v12), wordToWord16# (word16ToWord# u13 `xor#` word16ToWord# v13), wordToWord16# (word16ToWord# u14 `xor#` word16ToWord# v14), wordToWord16# (word16ToWord# u15 `xor#` word16ToWord# v15), wordToWord16# (word16ToWord# u16 `xor#` word16ToWord# v16), wordToWord16# (word16ToWord# u17 `xor#` word16ToWord# v17), wordToWord16# (word16ToWord# u18 `xor#` word16ToWord# v18), wordToWord16# (word16ToWord# u19 `xor#` word16ToWord# v19), wordToWord16# (word16ToWord# u20 `xor#` word16ToWord# v20), wordToWord16# (word16ToWord# u21 `xor#` word16ToWord# v21), wordToWord16# (word16ToWord# u22 `xor#` word16ToWord# v22), wordToWord16# (word16ToWord# u23 `xor#` word16ToWord# v23), wordToWord16# (word16ToWord# u24 `xor#` word16ToWord# v24), wordToWord16# (word16ToWord# u25 `xor#` word16ToWord# v25), wordToWord16# (word16ToWord# u26 `xor#` word16ToWord# v26), wordToWord16# (word16ToWord# u27 `xor#` word16ToWord# v27), wordToWord16# (word16ToWord# u28 `xor#` word16ToWord# v28), wordToWord16# (word16ToWord# u29 `xor#` word16ToWord# v29), wordToWord16# (word16ToWord# u30 `xor#` word16ToWord# v30), wordToWord16# (word16ToWord# u31 `xor#` word16ToWord# v31) #)
{-# INLINE [0] xorWord16X32# #-}

xorWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#
xorWord32X16# u v = case unpackWord32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord32X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord32X16# (# wordToWord32# (word32ToWord# u0 `xor#` word32ToWord# v0), wordToWord32# (word32ToWord# u1 `xor#` word32ToWord# v1), wordToWord32# (word32ToWord# u2 `xor#` word32ToWord# v2), wordToWord32# (word32ToWord# u3 `xor#` word32ToWord# v3), wordToWord32# (word32ToWord# u4 `xor#` word32ToWord# v4), wordToWord32# (word32ToWord# u5 `xor#` word32ToWord# v5), wordToWord32# (word32ToWord# u6 `xor#` word32ToWord# v6), wordToWord32# (word32ToWord# u7 `xor#` word32ToWord# v7), wordToWord32# (word32ToWord# u8 `xor#` word32ToWord# v8), wordToWord32# (word32ToWord# u9 `xor#` word32ToWord# v9), wordToWord32# (word32ToWord# u10 `xor#` word32ToWord# v10), wordToWord32# (word32ToWord# u11 `xor#` word32ToWord# v11), wordToWord32# (word32ToWord# u12 `xor#` word32ToWord# v12), wordToWord32# (word32ToWord# u13 `xor#` word32ToWord# v13), wordToWord32# (word32ToWord# u14 `xor#` word32ToWord# v14), wordToWord32# (word32ToWord# u15 `xor#` word32ToWord# v15) #)
{-# INLINE [0] xorWord32X16# #-}

xorWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#
xorWord64X8# u v = case unpackWord64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord64X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord64X8# (# xor64# u0 v0, xor64# u1 v1, xor64# u2 v2, xor64# u3 v3, xor64# u4 v4, xor64# u5 v5, xor64# u6 v6, xor64# u7 v7 #)
{-# INLINE [0] xorWord64X8# #-}

#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)

minInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#
minInt8X64# u v = case unpackInt8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> case unpackInt8X64# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60, v61, v62, v63 #) -> packInt8X64# (# case ltInt8# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt8# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt8# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt8# u3 v3 of { 0# -> v3; _ -> u3 }, case ltInt8# u4 v4 of { 0# -> v4; _ -> u4 }, case ltInt8# u5 v5 of { 0# -> v5; _ -> u5 }, case ltInt8# u6 v6 of { 0# -> v6; _ -> u6 }, case ltInt8# u7 v7 of { 0# -> v7; _ -> u7 }, case ltInt8# u8 v8 of { 0# -> v8; _ -> u8 }, case ltInt8# u9 v9 of { 0# -> v9; _ -> u9 }, case ltInt8# u10 v10 of { 0# -> v10; _ -> u10 }, case ltInt8# u11 v11 of { 0# -> v11; _ -> u11 }, case ltInt8# u12 v12 of { 0# -> v12; _ -> u12 }, case ltInt8# u13 v13 of { 0# -> v13; _ -> u13 }, case ltInt8# u14 v14 of { 0# -> v14; _ -> u14 }, case ltInt8# u15 v15 of { 0# -> v15; _ -> u15 }, case ltInt8# u16 v16 of { 0# -> v16; _ -> u16 }, case ltInt8# u17 v17 of { 0# -> v17; _ -> u17 }, case ltInt8# u18 v18 of { 0# -> v18; _ -> u18 }, case ltInt8# u19 v19 of { 0# -> v19; _ -> u19 }, case ltInt8# u20 v20 of { 0# -> v20; _ -> u20 }, case ltInt8# u21 v21 of { 0# -> v21; _ -> u21 }, case ltInt8# u22 v22 of { 0# -> v22; _ -> u22 }, case ltInt8# u23 v23 of { 0# -> v23; _ -> u23 }, case ltInt8# u24 v24 of { 0# -> v24; _ -> u24 }, case ltInt8# u25 v25 of { 0# -> v25; _ -> u25 }, case ltInt8# u26 v26 of { 0# -> v26; _ -> u26 }, case ltInt8# u27 v27 of { 0# -> v27; _ -> u27 }, case ltInt8# u28 v28 of { 0# -> v28; _ -> u28 }, case ltInt8# u29 v29 of { 0# -> v29; _ -> u29 }, case ltInt8# u30 v30 of { 0# -> v30; _ -> u30 }, case ltInt8# u31 v31 of { 0# -> v31; _ -> u31 }, case ltInt8# u32 v32 of { 0# -> v32; _ -> u32 }, case ltInt8# u33 v33 of { 0# -> v33; _ -> u33 }, case ltInt8# u34 v34 of { 0# -> v34; _ -> u34 }, case ltInt8# u35 v35 of { 0# -> v35; _ -> u35 }, case ltInt8# u36 v36 of { 0# -> v36; _ -> u36 }, case ltInt8# u37 v37 of { 0# -> v37; _ -> u37 }, case ltInt8# u38 v38 of { 0# -> v38; _ -> u38 }, case ltInt8# u39 v39 of { 0# -> v39; _ -> u39 }, case ltInt8# u40 v40 of { 0# -> v40; _ -> u40 }, case ltInt8# u41 v41 of { 0# -> v41; _ -> u41 }, case ltInt8# u42 v42 of { 0# -> v42; _ -> u42 }, case ltInt8# u43 v43 of { 0# -> v43; _ -> u43 }, case ltInt8# u44 v44 of { 0# -> v44; _ -> u44 }, case ltInt8# u45 v45 of { 0# -> v45; _ -> u45 }, case ltInt8# u46 v46 of { 0# -> v46; _ -> u46 }, case ltInt8# u47 v47 of { 0# -> v47; _ -> u47 }, case ltInt8# u48 v48 of { 0# -> v48; _ -> u48 }, case ltInt8# u49 v49 of { 0# -> v49; _ -> u49 }, case ltInt8# u50 v50 of { 0# -> v50; _ -> u50 }, case ltInt8# u51 v51 of { 0# -> v51; _ -> u51 }, case ltInt8# u52 v52 of { 0# -> v52; _ -> u52 }, case ltInt8# u53 v53 of { 0# -> v53; _ -> u53 }, case ltInt8# u54 v54 of { 0# -> v54; _ -> u54 }, case ltInt8# u55 v55 of { 0# -> v55; _ -> u55 }, case ltInt8# u56 v56 of { 0# -> v56; _ -> u56 }, case ltInt8# u57 v57 of { 0# -> v57; _ -> u57 }, case ltInt8# u58 v58 of { 0# -> v58; _ -> u58 }, case ltInt8# u59 v59 of { 0# -> v59; _ -> u59 }, case ltInt8# u60 v60 of { 0# -> v60; _ -> u60 }, case ltInt8# u61 v61 of { 0# -> v61; _ -> u61 }, case ltInt8# u62 v62 of { 0# -> v62; _ -> u62 }, case ltInt8# u63 v63 of { 0# -> v63; _ -> u63 } #)
{-# INLINE [0] minInt8X64# #-}

minInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#
minInt16X32# u v = case unpackInt16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackInt16X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packInt16X32# (# case ltInt16# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt16# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt16# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt16# u3 v3 of { 0# -> v3; _ -> u3 }, case ltInt16# u4 v4 of { 0# -> v4; _ -> u4 }, case ltInt16# u5 v5 of { 0# -> v5; _ -> u5 }, case ltInt16# u6 v6 of { 0# -> v6; _ -> u6 }, case ltInt16# u7 v7 of { 0# -> v7; _ -> u7 }, case ltInt16# u8 v8 of { 0# -> v8; _ -> u8 }, case ltInt16# u9 v9 of { 0# -> v9; _ -> u9 }, case ltInt16# u10 v10 of { 0# -> v10; _ -> u10 }, case ltInt16# u11 v11 of { 0# -> v11; _ -> u11 }, case ltInt16# u12 v12 of { 0# -> v12; _ -> u12 }, case ltInt16# u13 v13 of { 0# -> v13; _ -> u13 }, case ltInt16# u14 v14 of { 0# -> v14; _ -> u14 }, case ltInt16# u15 v15 of { 0# -> v15; _ -> u15 }, case ltInt16# u16 v16 of { 0# -> v16; _ -> u16 }, case ltInt16# u17 v17 of { 0# -> v17; _ -> u17 }, case ltInt16# u18 v18 of { 0# -> v18; _ -> u18 }, case ltInt16# u19 v19 of { 0# -> v19; _ -> u19 }, case ltInt16# u20 v20 of { 0# -> v20; _ -> u20 }, case ltInt16# u21 v21 of { 0# -> v21; _ -> u21 }, case ltInt16# u22 v22 of { 0# -> v22; _ -> u22 }, case ltInt16# u23 v23 of { 0# -> v23; _ -> u23 }, case ltInt16# u24 v24 of { 0# -> v24; _ -> u24 }, case ltInt16# u25 v25 of { 0# -> v25; _ -> u25 }, case ltInt16# u26 v26 of { 0# -> v26; _ -> u26 }, case ltInt16# u27 v27 of { 0# -> v27; _ -> u27 }, case ltInt16# u28 v28 of { 0# -> v28; _ -> u28 }, case ltInt16# u29 v29 of { 0# -> v29; _ -> u29 }, case ltInt16# u30 v30 of { 0# -> v30; _ -> u30 }, case ltInt16# u31 v31 of { 0# -> v31; _ -> u31 } #)
{-# INLINE [0] minInt16X32# #-}

minInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#
minInt32X16# u v = case unpackInt32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt32X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt32X16# (# case ltInt32# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt32# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt32# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt32# u3 v3 of { 0# -> v3; _ -> u3 }, case ltInt32# u4 v4 of { 0# -> v4; _ -> u4 }, case ltInt32# u5 v5 of { 0# -> v5; _ -> u5 }, case ltInt32# u6 v6 of { 0# -> v6; _ -> u6 }, case ltInt32# u7 v7 of { 0# -> v7; _ -> u7 }, case ltInt32# u8 v8 of { 0# -> v8; _ -> u8 }, case ltInt32# u9 v9 of { 0# -> v9; _ -> u9 }, case ltInt32# u10 v10 of { 0# -> v10; _ -> u10 }, case ltInt32# u11 v11 of { 0# -> v11; _ -> u11 }, case ltInt32# u12 v12 of { 0# -> v12; _ -> u12 }, case ltInt32# u13 v13 of { 0# -> v13; _ -> u13 }, case ltInt32# u14 v14 of { 0# -> v14; _ -> u14 }, case ltInt32# u15 v15 of { 0# -> v15; _ -> u15 } #)
{-# INLINE [0] minInt32X16# #-}

minInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#
minInt64X8# u v = case unpackInt64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt64X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt64X8# (# case ltInt64# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt64# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt64# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt64# u3 v3 of { 0# -> v3; _ -> u3 }, case ltInt64# u4 v4 of { 0# -> v4; _ -> u4 }, case ltInt64# u5 v5 of { 0# -> v5; _ -> u5 }, case ltInt64# u6 v6 of { 0# -> v6; _ -> u6 }, case ltInt64# u7 v7 of { 0# -> v7; _ -> u7 } #)
{-# INLINE [0] minInt64X8# #-}

minWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#
minWord8X64# u v = case unpackWord8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> case unpackWord8X64# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60, v61, v62, v63 #) -> packWord8X64# (# case ltWord8# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord8# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord8# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord8# u3 v3 of { 0# -> v3; _ -> u3 }, case ltWord8# u4 v4 of { 0# -> v4; _ -> u4 }, case ltWord8# u5 v5 of { 0# -> v5; _ -> u5 }, case ltWord8# u6 v6 of { 0# -> v6; _ -> u6 }, case ltWord8# u7 v7 of { 0# -> v7; _ -> u7 }, case ltWord8# u8 v8 of { 0# -> v8; _ -> u8 }, case ltWord8# u9 v9 of { 0# -> v9; _ -> u9 }, case ltWord8# u10 v10 of { 0# -> v10; _ -> u10 }, case ltWord8# u11 v11 of { 0# -> v11; _ -> u11 }, case ltWord8# u12 v12 of { 0# -> v12; _ -> u12 }, case ltWord8# u13 v13 of { 0# -> v13; _ -> u13 }, case ltWord8# u14 v14 of { 0# -> v14; _ -> u14 }, case ltWord8# u15 v15 of { 0# -> v15; _ -> u15 }, case ltWord8# u16 v16 of { 0# -> v16; _ -> u16 }, case ltWord8# u17 v17 of { 0# -> v17; _ -> u17 }, case ltWord8# u18 v18 of { 0# -> v18; _ -> u18 }, case ltWord8# u19 v19 of { 0# -> v19; _ -> u19 }, case ltWord8# u20 v20 of { 0# -> v20; _ -> u20 }, case ltWord8# u21 v21 of { 0# -> v21; _ -> u21 }, case ltWord8# u22 v22 of { 0# -> v22; _ -> u22 }, case ltWord8# u23 v23 of { 0# -> v23; _ -> u23 }, case ltWord8# u24 v24 of { 0# -> v24; _ -> u24 }, case ltWord8# u25 v25 of { 0# -> v25; _ -> u25 }, case ltWord8# u26 v26 of { 0# -> v26; _ -> u26 }, case ltWord8# u27 v27 of { 0# -> v27; _ -> u27 }, case ltWord8# u28 v28 of { 0# -> v28; _ -> u28 }, case ltWord8# u29 v29 of { 0# -> v29; _ -> u29 }, case ltWord8# u30 v30 of { 0# -> v30; _ -> u30 }, case ltWord8# u31 v31 of { 0# -> v31; _ -> u31 }, case ltWord8# u32 v32 of { 0# -> v32; _ -> u32 }, case ltWord8# u33 v33 of { 0# -> v33; _ -> u33 }, case ltWord8# u34 v34 of { 0# -> v34; _ -> u34 }, case ltWord8# u35 v35 of { 0# -> v35; _ -> u35 }, case ltWord8# u36 v36 of { 0# -> v36; _ -> u36 }, case ltWord8# u37 v37 of { 0# -> v37; _ -> u37 }, case ltWord8# u38 v38 of { 0# -> v38; _ -> u38 }, case ltWord8# u39 v39 of { 0# -> v39; _ -> u39 }, case ltWord8# u40 v40 of { 0# -> v40; _ -> u40 }, case ltWord8# u41 v41 of { 0# -> v41; _ -> u41 }, case ltWord8# u42 v42 of { 0# -> v42; _ -> u42 }, case ltWord8# u43 v43 of { 0# -> v43; _ -> u43 }, case ltWord8# u44 v44 of { 0# -> v44; _ -> u44 }, case ltWord8# u45 v45 of { 0# -> v45; _ -> u45 }, case ltWord8# u46 v46 of { 0# -> v46; _ -> u46 }, case ltWord8# u47 v47 of { 0# -> v47; _ -> u47 }, case ltWord8# u48 v48 of { 0# -> v48; _ -> u48 }, case ltWord8# u49 v49 of { 0# -> v49; _ -> u49 }, case ltWord8# u50 v50 of { 0# -> v50; _ -> u50 }, case ltWord8# u51 v51 of { 0# -> v51; _ -> u51 }, case ltWord8# u52 v52 of { 0# -> v52; _ -> u52 }, case ltWord8# u53 v53 of { 0# -> v53; _ -> u53 }, case ltWord8# u54 v54 of { 0# -> v54; _ -> u54 }, case ltWord8# u55 v55 of { 0# -> v55; _ -> u55 }, case ltWord8# u56 v56 of { 0# -> v56; _ -> u56 }, case ltWord8# u57 v57 of { 0# -> v57; _ -> u57 }, case ltWord8# u58 v58 of { 0# -> v58; _ -> u58 }, case ltWord8# u59 v59 of { 0# -> v59; _ -> u59 }, case ltWord8# u60 v60 of { 0# -> v60; _ -> u60 }, case ltWord8# u61 v61 of { 0# -> v61; _ -> u61 }, case ltWord8# u62 v62 of { 0# -> v62; _ -> u62 }, case ltWord8# u63 v63 of { 0# -> v63; _ -> u63 } #)
{-# INLINE [0] minWord8X64# #-}

minWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#
minWord16X32# u v = case unpackWord16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackWord16X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packWord16X32# (# case ltWord16# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord16# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord16# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord16# u3 v3 of { 0# -> v3; _ -> u3 }, case ltWord16# u4 v4 of { 0# -> v4; _ -> u4 }, case ltWord16# u5 v5 of { 0# -> v5; _ -> u5 }, case ltWord16# u6 v6 of { 0# -> v6; _ -> u6 }, case ltWord16# u7 v7 of { 0# -> v7; _ -> u7 }, case ltWord16# u8 v8 of { 0# -> v8; _ -> u8 }, case ltWord16# u9 v9 of { 0# -> v9; _ -> u9 }, case ltWord16# u10 v10 of { 0# -> v10; _ -> u10 }, case ltWord16# u11 v11 of { 0# -> v11; _ -> u11 }, case ltWord16# u12 v12 of { 0# -> v12; _ -> u12 }, case ltWord16# u13 v13 of { 0# -> v13; _ -> u13 }, case ltWord16# u14 v14 of { 0# -> v14; _ -> u14 }, case ltWord16# u15 v15 of { 0# -> v15; _ -> u15 }, case ltWord16# u16 v16 of { 0# -> v16; _ -> u16 }, case ltWord16# u17 v17 of { 0# -> v17; _ -> u17 }, case ltWord16# u18 v18 of { 0# -> v18; _ -> u18 }, case ltWord16# u19 v19 of { 0# -> v19; _ -> u19 }, case ltWord16# u20 v20 of { 0# -> v20; _ -> u20 }, case ltWord16# u21 v21 of { 0# -> v21; _ -> u21 }, case ltWord16# u22 v22 of { 0# -> v22; _ -> u22 }, case ltWord16# u23 v23 of { 0# -> v23; _ -> u23 }, case ltWord16# u24 v24 of { 0# -> v24; _ -> u24 }, case ltWord16# u25 v25 of { 0# -> v25; _ -> u25 }, case ltWord16# u26 v26 of { 0# -> v26; _ -> u26 }, case ltWord16# u27 v27 of { 0# -> v27; _ -> u27 }, case ltWord16# u28 v28 of { 0# -> v28; _ -> u28 }, case ltWord16# u29 v29 of { 0# -> v29; _ -> u29 }, case ltWord16# u30 v30 of { 0# -> v30; _ -> u30 }, case ltWord16# u31 v31 of { 0# -> v31; _ -> u31 } #)
{-# INLINE [0] minWord16X32# #-}

minWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#
minWord32X16# u v = case unpackWord32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord32X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord32X16# (# case ltWord32# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord32# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord32# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord32# u3 v3 of { 0# -> v3; _ -> u3 }, case ltWord32# u4 v4 of { 0# -> v4; _ -> u4 }, case ltWord32# u5 v5 of { 0# -> v5; _ -> u5 }, case ltWord32# u6 v6 of { 0# -> v6; _ -> u6 }, case ltWord32# u7 v7 of { 0# -> v7; _ -> u7 }, case ltWord32# u8 v8 of { 0# -> v8; _ -> u8 }, case ltWord32# u9 v9 of { 0# -> v9; _ -> u9 }, case ltWord32# u10 v10 of { 0# -> v10; _ -> u10 }, case ltWord32# u11 v11 of { 0# -> v11; _ -> u11 }, case ltWord32# u12 v12 of { 0# -> v12; _ -> u12 }, case ltWord32# u13 v13 of { 0# -> v13; _ -> u13 }, case ltWord32# u14 v14 of { 0# -> v14; _ -> u14 }, case ltWord32# u15 v15 of { 0# -> v15; _ -> u15 } #)
{-# INLINE [0] minWord32X16# #-}

minWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#
minWord64X8# u v = case unpackWord64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord64X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord64X8# (# case ltWord64# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord64# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord64# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord64# u3 v3 of { 0# -> v3; _ -> u3 }, case ltWord64# u4 v4 of { 0# -> v4; _ -> u4 }, case ltWord64# u5 v5 of { 0# -> v5; _ -> u5 }, case ltWord64# u6 v6 of { 0# -> v6; _ -> u6 }, case ltWord64# u7 v7 of { 0# -> v7; _ -> u7 } #)
{-# INLINE [0] minWord64X8# #-}

maxInt8X64# :: Int8X64# -> Int8X64# -> Int8X64#
maxInt8X64# u v = case unpackInt8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> case unpackInt8X64# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60, v61, v62, v63 #) -> packInt8X64# (# case ltInt8# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt8# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt8# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt8# u3 v3 of { 0# -> u3; _ -> v3 }, case ltInt8# u4 v4 of { 0# -> u4; _ -> v4 }, case ltInt8# u5 v5 of { 0# -> u5; _ -> v5 }, case ltInt8# u6 v6 of { 0# -> u6; _ -> v6 }, case ltInt8# u7 v7 of { 0# -> u7; _ -> v7 }, case ltInt8# u8 v8 of { 0# -> u8; _ -> v8 }, case ltInt8# u9 v9 of { 0# -> u9; _ -> v9 }, case ltInt8# u10 v10 of { 0# -> u10; _ -> v10 }, case ltInt8# u11 v11 of { 0# -> u11; _ -> v11 }, case ltInt8# u12 v12 of { 0# -> u12; _ -> v12 }, case ltInt8# u13 v13 of { 0# -> u13; _ -> v13 }, case ltInt8# u14 v14 of { 0# -> u14; _ -> v14 }, case ltInt8# u15 v15 of { 0# -> u15; _ -> v15 }, case ltInt8# u16 v16 of { 0# -> u16; _ -> v16 }, case ltInt8# u17 v17 of { 0# -> u17; _ -> v17 }, case ltInt8# u18 v18 of { 0# -> u18; _ -> v18 }, case ltInt8# u19 v19 of { 0# -> u19; _ -> v19 }, case ltInt8# u20 v20 of { 0# -> u20; _ -> v20 }, case ltInt8# u21 v21 of { 0# -> u21; _ -> v21 }, case ltInt8# u22 v22 of { 0# -> u22; _ -> v22 }, case ltInt8# u23 v23 of { 0# -> u23; _ -> v23 }, case ltInt8# u24 v24 of { 0# -> u24; _ -> v24 }, case ltInt8# u25 v25 of { 0# -> u25; _ -> v25 }, case ltInt8# u26 v26 of { 0# -> u26; _ -> v26 }, case ltInt8# u27 v27 of { 0# -> u27; _ -> v27 }, case ltInt8# u28 v28 of { 0# -> u28; _ -> v28 }, case ltInt8# u29 v29 of { 0# -> u29; _ -> v29 }, case ltInt8# u30 v30 of { 0# -> u30; _ -> v30 }, case ltInt8# u31 v31 of { 0# -> u31; _ -> v31 }, case ltInt8# u32 v32 of { 0# -> u32; _ -> v32 }, case ltInt8# u33 v33 of { 0# -> u33; _ -> v33 }, case ltInt8# u34 v34 of { 0# -> u34; _ -> v34 }, case ltInt8# u35 v35 of { 0# -> u35; _ -> v35 }, case ltInt8# u36 v36 of { 0# -> u36; _ -> v36 }, case ltInt8# u37 v37 of { 0# -> u37; _ -> v37 }, case ltInt8# u38 v38 of { 0# -> u38; _ -> v38 }, case ltInt8# u39 v39 of { 0# -> u39; _ -> v39 }, case ltInt8# u40 v40 of { 0# -> u40; _ -> v40 }, case ltInt8# u41 v41 of { 0# -> u41; _ -> v41 }, case ltInt8# u42 v42 of { 0# -> u42; _ -> v42 }, case ltInt8# u43 v43 of { 0# -> u43; _ -> v43 }, case ltInt8# u44 v44 of { 0# -> u44; _ -> v44 }, case ltInt8# u45 v45 of { 0# -> u45; _ -> v45 }, case ltInt8# u46 v46 of { 0# -> u46; _ -> v46 }, case ltInt8# u47 v47 of { 0# -> u47; _ -> v47 }, case ltInt8# u48 v48 of { 0# -> u48; _ -> v48 }, case ltInt8# u49 v49 of { 0# -> u49; _ -> v49 }, case ltInt8# u50 v50 of { 0# -> u50; _ -> v50 }, case ltInt8# u51 v51 of { 0# -> u51; _ -> v51 }, case ltInt8# u52 v52 of { 0# -> u52; _ -> v52 }, case ltInt8# u53 v53 of { 0# -> u53; _ -> v53 }, case ltInt8# u54 v54 of { 0# -> u54; _ -> v54 }, case ltInt8# u55 v55 of { 0# -> u55; _ -> v55 }, case ltInt8# u56 v56 of { 0# -> u56; _ -> v56 }, case ltInt8# u57 v57 of { 0# -> u57; _ -> v57 }, case ltInt8# u58 v58 of { 0# -> u58; _ -> v58 }, case ltInt8# u59 v59 of { 0# -> u59; _ -> v59 }, case ltInt8# u60 v60 of { 0# -> u60; _ -> v60 }, case ltInt8# u61 v61 of { 0# -> u61; _ -> v61 }, case ltInt8# u62 v62 of { 0# -> u62; _ -> v62 }, case ltInt8# u63 v63 of { 0# -> u63; _ -> v63 } #)
{-# INLINE [0] maxInt8X64# #-}

maxInt16X32# :: Int16X32# -> Int16X32# -> Int16X32#
maxInt16X32# u v = case unpackInt16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackInt16X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packInt16X32# (# case ltInt16# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt16# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt16# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt16# u3 v3 of { 0# -> u3; _ -> v3 }, case ltInt16# u4 v4 of { 0# -> u4; _ -> v4 }, case ltInt16# u5 v5 of { 0# -> u5; _ -> v5 }, case ltInt16# u6 v6 of { 0# -> u6; _ -> v6 }, case ltInt16# u7 v7 of { 0# -> u7; _ -> v7 }, case ltInt16# u8 v8 of { 0# -> u8; _ -> v8 }, case ltInt16# u9 v9 of { 0# -> u9; _ -> v9 }, case ltInt16# u10 v10 of { 0# -> u10; _ -> v10 }, case ltInt16# u11 v11 of { 0# -> u11; _ -> v11 }, case ltInt16# u12 v12 of { 0# -> u12; _ -> v12 }, case ltInt16# u13 v13 of { 0# -> u13; _ -> v13 }, case ltInt16# u14 v14 of { 0# -> u14; _ -> v14 }, case ltInt16# u15 v15 of { 0# -> u15; _ -> v15 }, case ltInt16# u16 v16 of { 0# -> u16; _ -> v16 }, case ltInt16# u17 v17 of { 0# -> u17; _ -> v17 }, case ltInt16# u18 v18 of { 0# -> u18; _ -> v18 }, case ltInt16# u19 v19 of { 0# -> u19; _ -> v19 }, case ltInt16# u20 v20 of { 0# -> u20; _ -> v20 }, case ltInt16# u21 v21 of { 0# -> u21; _ -> v21 }, case ltInt16# u22 v22 of { 0# -> u22; _ -> v22 }, case ltInt16# u23 v23 of { 0# -> u23; _ -> v23 }, case ltInt16# u24 v24 of { 0# -> u24; _ -> v24 }, case ltInt16# u25 v25 of { 0# -> u25; _ -> v25 }, case ltInt16# u26 v26 of { 0# -> u26; _ -> v26 }, case ltInt16# u27 v27 of { 0# -> u27; _ -> v27 }, case ltInt16# u28 v28 of { 0# -> u28; _ -> v28 }, case ltInt16# u29 v29 of { 0# -> u29; _ -> v29 }, case ltInt16# u30 v30 of { 0# -> u30; _ -> v30 }, case ltInt16# u31 v31 of { 0# -> u31; _ -> v31 } #)
{-# INLINE [0] maxInt16X32# #-}

maxInt32X16# :: Int32X16# -> Int32X16# -> Int32X16#
maxInt32X16# u v = case unpackInt32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt32X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt32X16# (# case ltInt32# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt32# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt32# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt32# u3 v3 of { 0# -> u3; _ -> v3 }, case ltInt32# u4 v4 of { 0# -> u4; _ -> v4 }, case ltInt32# u5 v5 of { 0# -> u5; _ -> v5 }, case ltInt32# u6 v6 of { 0# -> u6; _ -> v6 }, case ltInt32# u7 v7 of { 0# -> u7; _ -> v7 }, case ltInt32# u8 v8 of { 0# -> u8; _ -> v8 }, case ltInt32# u9 v9 of { 0# -> u9; _ -> v9 }, case ltInt32# u10 v10 of { 0# -> u10; _ -> v10 }, case ltInt32# u11 v11 of { 0# -> u11; _ -> v11 }, case ltInt32# u12 v12 of { 0# -> u12; _ -> v12 }, case ltInt32# u13 v13 of { 0# -> u13; _ -> v13 }, case ltInt32# u14 v14 of { 0# -> u14; _ -> v14 }, case ltInt32# u15 v15 of { 0# -> u15; _ -> v15 } #)
{-# INLINE [0] maxInt32X16# #-}

maxInt64X8# :: Int64X8# -> Int64X8# -> Int64X8#
maxInt64X8# u v = case unpackInt64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt64X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt64X8# (# case ltInt64# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt64# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt64# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt64# u3 v3 of { 0# -> u3; _ -> v3 }, case ltInt64# u4 v4 of { 0# -> u4; _ -> v4 }, case ltInt64# u5 v5 of { 0# -> u5; _ -> v5 }, case ltInt64# u6 v6 of { 0# -> u6; _ -> v6 }, case ltInt64# u7 v7 of { 0# -> u7; _ -> v7 } #)
{-# INLINE [0] maxInt64X8# #-}

maxWord8X64# :: Word8X64# -> Word8X64# -> Word8X64#
maxWord8X64# u v = case unpackWord8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> case unpackWord8X64# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31, v32, v33, v34, v35, v36, v37, v38, v39, v40, v41, v42, v43, v44, v45, v46, v47, v48, v49, v50, v51, v52, v53, v54, v55, v56, v57, v58, v59, v60, v61, v62, v63 #) -> packWord8X64# (# case ltWord8# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord8# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord8# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord8# u3 v3 of { 0# -> u3; _ -> v3 }, case ltWord8# u4 v4 of { 0# -> u4; _ -> v4 }, case ltWord8# u5 v5 of { 0# -> u5; _ -> v5 }, case ltWord8# u6 v6 of { 0# -> u6; _ -> v6 }, case ltWord8# u7 v7 of { 0# -> u7; _ -> v7 }, case ltWord8# u8 v8 of { 0# -> u8; _ -> v8 }, case ltWord8# u9 v9 of { 0# -> u9; _ -> v9 }, case ltWord8# u10 v10 of { 0# -> u10; _ -> v10 }, case ltWord8# u11 v11 of { 0# -> u11; _ -> v11 }, case ltWord8# u12 v12 of { 0# -> u12; _ -> v12 }, case ltWord8# u13 v13 of { 0# -> u13; _ -> v13 }, case ltWord8# u14 v14 of { 0# -> u14; _ -> v14 }, case ltWord8# u15 v15 of { 0# -> u15; _ -> v15 }, case ltWord8# u16 v16 of { 0# -> u16; _ -> v16 }, case ltWord8# u17 v17 of { 0# -> u17; _ -> v17 }, case ltWord8# u18 v18 of { 0# -> u18; _ -> v18 }, case ltWord8# u19 v19 of { 0# -> u19; _ -> v19 }, case ltWord8# u20 v20 of { 0# -> u20; _ -> v20 }, case ltWord8# u21 v21 of { 0# -> u21; _ -> v21 }, case ltWord8# u22 v22 of { 0# -> u22; _ -> v22 }, case ltWord8# u23 v23 of { 0# -> u23; _ -> v23 }, case ltWord8# u24 v24 of { 0# -> u24; _ -> v24 }, case ltWord8# u25 v25 of { 0# -> u25; _ -> v25 }, case ltWord8# u26 v26 of { 0# -> u26; _ -> v26 }, case ltWord8# u27 v27 of { 0# -> u27; _ -> v27 }, case ltWord8# u28 v28 of { 0# -> u28; _ -> v28 }, case ltWord8# u29 v29 of { 0# -> u29; _ -> v29 }, case ltWord8# u30 v30 of { 0# -> u30; _ -> v30 }, case ltWord8# u31 v31 of { 0# -> u31; _ -> v31 }, case ltWord8# u32 v32 of { 0# -> u32; _ -> v32 }, case ltWord8# u33 v33 of { 0# -> u33; _ -> v33 }, case ltWord8# u34 v34 of { 0# -> u34; _ -> v34 }, case ltWord8# u35 v35 of { 0# -> u35; _ -> v35 }, case ltWord8# u36 v36 of { 0# -> u36; _ -> v36 }, case ltWord8# u37 v37 of { 0# -> u37; _ -> v37 }, case ltWord8# u38 v38 of { 0# -> u38; _ -> v38 }, case ltWord8# u39 v39 of { 0# -> u39; _ -> v39 }, case ltWord8# u40 v40 of { 0# -> u40; _ -> v40 }, case ltWord8# u41 v41 of { 0# -> u41; _ -> v41 }, case ltWord8# u42 v42 of { 0# -> u42; _ -> v42 }, case ltWord8# u43 v43 of { 0# -> u43; _ -> v43 }, case ltWord8# u44 v44 of { 0# -> u44; _ -> v44 }, case ltWord8# u45 v45 of { 0# -> u45; _ -> v45 }, case ltWord8# u46 v46 of { 0# -> u46; _ -> v46 }, case ltWord8# u47 v47 of { 0# -> u47; _ -> v47 }, case ltWord8# u48 v48 of { 0# -> u48; _ -> v48 }, case ltWord8# u49 v49 of { 0# -> u49; _ -> v49 }, case ltWord8# u50 v50 of { 0# -> u50; _ -> v50 }, case ltWord8# u51 v51 of { 0# -> u51; _ -> v51 }, case ltWord8# u52 v52 of { 0# -> u52; _ -> v52 }, case ltWord8# u53 v53 of { 0# -> u53; _ -> v53 }, case ltWord8# u54 v54 of { 0# -> u54; _ -> v54 }, case ltWord8# u55 v55 of { 0# -> u55; _ -> v55 }, case ltWord8# u56 v56 of { 0# -> u56; _ -> v56 }, case ltWord8# u57 v57 of { 0# -> u57; _ -> v57 }, case ltWord8# u58 v58 of { 0# -> u58; _ -> v58 }, case ltWord8# u59 v59 of { 0# -> u59; _ -> v59 }, case ltWord8# u60 v60 of { 0# -> u60; _ -> v60 }, case ltWord8# u61 v61 of { 0# -> u61; _ -> v61 }, case ltWord8# u62 v62 of { 0# -> u62; _ -> v62 }, case ltWord8# u63 v63 of { 0# -> u63; _ -> v63 } #)
{-# INLINE [0] maxWord8X64# #-}

maxWord16X32# :: Word16X32# -> Word16X32# -> Word16X32#
maxWord16X32# u v = case unpackWord16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackWord16X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packWord16X32# (# case ltWord16# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord16# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord16# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord16# u3 v3 of { 0# -> u3; _ -> v3 }, case ltWord16# u4 v4 of { 0# -> u4; _ -> v4 }, case ltWord16# u5 v5 of { 0# -> u5; _ -> v5 }, case ltWord16# u6 v6 of { 0# -> u6; _ -> v6 }, case ltWord16# u7 v7 of { 0# -> u7; _ -> v7 }, case ltWord16# u8 v8 of { 0# -> u8; _ -> v8 }, case ltWord16# u9 v9 of { 0# -> u9; _ -> v9 }, case ltWord16# u10 v10 of { 0# -> u10; _ -> v10 }, case ltWord16# u11 v11 of { 0# -> u11; _ -> v11 }, case ltWord16# u12 v12 of { 0# -> u12; _ -> v12 }, case ltWord16# u13 v13 of { 0# -> u13; _ -> v13 }, case ltWord16# u14 v14 of { 0# -> u14; _ -> v14 }, case ltWord16# u15 v15 of { 0# -> u15; _ -> v15 }, case ltWord16# u16 v16 of { 0# -> u16; _ -> v16 }, case ltWord16# u17 v17 of { 0# -> u17; _ -> v17 }, case ltWord16# u18 v18 of { 0# -> u18; _ -> v18 }, case ltWord16# u19 v19 of { 0# -> u19; _ -> v19 }, case ltWord16# u20 v20 of { 0# -> u20; _ -> v20 }, case ltWord16# u21 v21 of { 0# -> u21; _ -> v21 }, case ltWord16# u22 v22 of { 0# -> u22; _ -> v22 }, case ltWord16# u23 v23 of { 0# -> u23; _ -> v23 }, case ltWord16# u24 v24 of { 0# -> u24; _ -> v24 }, case ltWord16# u25 v25 of { 0# -> u25; _ -> v25 }, case ltWord16# u26 v26 of { 0# -> u26; _ -> v26 }, case ltWord16# u27 v27 of { 0# -> u27; _ -> v27 }, case ltWord16# u28 v28 of { 0# -> u28; _ -> v28 }, case ltWord16# u29 v29 of { 0# -> u29; _ -> v29 }, case ltWord16# u30 v30 of { 0# -> u30; _ -> v30 }, case ltWord16# u31 v31 of { 0# -> u31; _ -> v31 } #)
{-# INLINE [0] maxWord16X32# #-}

maxWord32X16# :: Word32X16# -> Word32X16# -> Word32X16#
maxWord32X16# u v = case unpackWord32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord32X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord32X16# (# case ltWord32# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord32# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord32# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord32# u3 v3 of { 0# -> u3; _ -> v3 }, case ltWord32# u4 v4 of { 0# -> u4; _ -> v4 }, case ltWord32# u5 v5 of { 0# -> u5; _ -> v5 }, case ltWord32# u6 v6 of { 0# -> u6; _ -> v6 }, case ltWord32# u7 v7 of { 0# -> u7; _ -> v7 }, case ltWord32# u8 v8 of { 0# -> u8; _ -> v8 }, case ltWord32# u9 v9 of { 0# -> u9; _ -> v9 }, case ltWord32# u10 v10 of { 0# -> u10; _ -> v10 }, case ltWord32# u11 v11 of { 0# -> u11; _ -> v11 }, case ltWord32# u12 v12 of { 0# -> u12; _ -> v12 }, case ltWord32# u13 v13 of { 0# -> u13; _ -> v13 }, case ltWord32# u14 v14 of { 0# -> u14; _ -> v14 }, case ltWord32# u15 v15 of { 0# -> u15; _ -> v15 } #)
{-# INLINE [0] maxWord32X16# #-}

maxWord64X8# :: Word64X8# -> Word64X8# -> Word64X8#
maxWord64X8# u v = case unpackWord64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord64X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord64X8# (# case ltWord64# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord64# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord64# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord64# u3 v3 of { 0# -> u3; _ -> v3 }, case ltWord64# u4 v4 of { 0# -> u4; _ -> v4 }, case ltWord64# u5 v5 of { 0# -> u5; _ -> v5 }, case ltWord64# u6 v6 of { 0# -> u6; _ -> v6 }, case ltWord64# u7 v7 of { 0# -> u7; _ -> v7 } #)
{-# INLINE [0] maxWord64X8# #-}

#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)

absInt8X64# :: Int8X64# -> Int8X64#
absInt8X64# u = case unpackInt8X64# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31, u32, u33, u34, u35, u36, u37, u38, u39, u40, u41, u42, u43, u44, u45, u46, u47, u48, u49, u50, u51, u52, u53, u54, u55, u56, u57, u58, u59, u60, u61, u62, u63 #) -> packInt8X64# (# case ltInt8# u0 (intToInt8# 0#) of { 0# -> u0; _ -> negateInt8# u0 }, case ltInt8# u1 (intToInt8# 0#) of { 0# -> u1; _ -> negateInt8# u1 }, case ltInt8# u2 (intToInt8# 0#) of { 0# -> u2; _ -> negateInt8# u2 }, case ltInt8# u3 (intToInt8# 0#) of { 0# -> u3; _ -> negateInt8# u3 }, case ltInt8# u4 (intToInt8# 0#) of { 0# -> u4; _ -> negateInt8# u4 }, case ltInt8# u5 (intToInt8# 0#) of { 0# -> u5; _ -> negateInt8# u5 }, case ltInt8# u6 (intToInt8# 0#) of { 0# -> u6; _ -> negateInt8# u6 }, case ltInt8# u7 (intToInt8# 0#) of { 0# -> u7; _ -> negateInt8# u7 }, case ltInt8# u8 (intToInt8# 0#) of { 0# -> u8; _ -> negateInt8# u8 }, case ltInt8# u9 (intToInt8# 0#) of { 0# -> u9; _ -> negateInt8# u9 }, case ltInt8# u10 (intToInt8# 0#) of { 0# -> u10; _ -> negateInt8# u10 }, case ltInt8# u11 (intToInt8# 0#) of { 0# -> u11; _ -> negateInt8# u11 }, case ltInt8# u12 (intToInt8# 0#) of { 0# -> u12; _ -> negateInt8# u12 }, case ltInt8# u13 (intToInt8# 0#) of { 0# -> u13; _ -> negateInt8# u13 }, case ltInt8# u14 (intToInt8# 0#) of { 0# -> u14; _ -> negateInt8# u14 }, case ltInt8# u15 (intToInt8# 0#) of { 0# -> u15; _ -> negateInt8# u15 }, case ltInt8# u16 (intToInt8# 0#) of { 0# -> u16; _ -> negateInt8# u16 }, case ltInt8# u17 (intToInt8# 0#) of { 0# -> u17; _ -> negateInt8# u17 }, case ltInt8# u18 (intToInt8# 0#) of { 0# -> u18; _ -> negateInt8# u18 }, case ltInt8# u19 (intToInt8# 0#) of { 0# -> u19; _ -> negateInt8# u19 }, case ltInt8# u20 (intToInt8# 0#) of { 0# -> u20; _ -> negateInt8# u20 }, case ltInt8# u21 (intToInt8# 0#) of { 0# -> u21; _ -> negateInt8# u21 }, case ltInt8# u22 (intToInt8# 0#) of { 0# -> u22; _ -> negateInt8# u22 }, case ltInt8# u23 (intToInt8# 0#) of { 0# -> u23; _ -> negateInt8# u23 }, case ltInt8# u24 (intToInt8# 0#) of { 0# -> u24; _ -> negateInt8# u24 }, case ltInt8# u25 (intToInt8# 0#) of { 0# -> u25; _ -> negateInt8# u25 }, case ltInt8# u26 (intToInt8# 0#) of { 0# -> u26; _ -> negateInt8# u26 }, case ltInt8# u27 (intToInt8# 0#) of { 0# -> u27; _ -> negateInt8# u27 }, case ltInt8# u28 (intToInt8# 0#) of { 0# -> u28; _ -> negateInt8# u28 }, case ltInt8# u29 (intToInt8# 0#) of { 0# -> u29; _ -> negateInt8# u29 }, case ltInt8# u30 (intToInt8# 0#) of { 0# -> u30; _ -> negateInt8# u30 }, case ltInt8# u31 (intToInt8# 0#) of { 0# -> u31; _ -> negateInt8# u31 }, case ltInt8# u32 (intToInt8# 0#) of { 0# -> u32; _ -> negateInt8# u32 }, case ltInt8# u33 (intToInt8# 0#) of { 0# -> u33; _ -> negateInt8# u33 }, case ltInt8# u34 (intToInt8# 0#) of { 0# -> u34; _ -> negateInt8# u34 }, case ltInt8# u35 (intToInt8# 0#) of { 0# -> u35; _ -> negateInt8# u35 }, case ltInt8# u36 (intToInt8# 0#) of { 0# -> u36; _ -> negateInt8# u36 }, case ltInt8# u37 (intToInt8# 0#) of { 0# -> u37; _ -> negateInt8# u37 }, case ltInt8# u38 (intToInt8# 0#) of { 0# -> u38; _ -> negateInt8# u38 }, case ltInt8# u39 (intToInt8# 0#) of { 0# -> u39; _ -> negateInt8# u39 }, case ltInt8# u40 (intToInt8# 0#) of { 0# -> u40; _ -> negateInt8# u40 }, case ltInt8# u41 (intToInt8# 0#) of { 0# -> u41; _ -> negateInt8# u41 }, case ltInt8# u42 (intToInt8# 0#) of { 0# -> u42; _ -> negateInt8# u42 }, case ltInt8# u43 (intToInt8# 0#) of { 0# -> u43; _ -> negateInt8# u43 }, case ltInt8# u44 (intToInt8# 0#) of { 0# -> u44; _ -> negateInt8# u44 }, case ltInt8# u45 (intToInt8# 0#) of { 0# -> u45; _ -> negateInt8# u45 }, case ltInt8# u46 (intToInt8# 0#) of { 0# -> u46; _ -> negateInt8# u46 }, case ltInt8# u47 (intToInt8# 0#) of { 0# -> u47; _ -> negateInt8# u47 }, case ltInt8# u48 (intToInt8# 0#) of { 0# -> u48; _ -> negateInt8# u48 }, case ltInt8# u49 (intToInt8# 0#) of { 0# -> u49; _ -> negateInt8# u49 }, case ltInt8# u50 (intToInt8# 0#) of { 0# -> u50; _ -> negateInt8# u50 }, case ltInt8# u51 (intToInt8# 0#) of { 0# -> u51; _ -> negateInt8# u51 }, case ltInt8# u52 (intToInt8# 0#) of { 0# -> u52; _ -> negateInt8# u52 }, case ltInt8# u53 (intToInt8# 0#) of { 0# -> u53; _ -> negateInt8# u53 }, case ltInt8# u54 (intToInt8# 0#) of { 0# -> u54; _ -> negateInt8# u54 }, case ltInt8# u55 (intToInt8# 0#) of { 0# -> u55; _ -> negateInt8# u55 }, case ltInt8# u56 (intToInt8# 0#) of { 0# -> u56; _ -> negateInt8# u56 }, case ltInt8# u57 (intToInt8# 0#) of { 0# -> u57; _ -> negateInt8# u57 }, case ltInt8# u58 (intToInt8# 0#) of { 0# -> u58; _ -> negateInt8# u58 }, case ltInt8# u59 (intToInt8# 0#) of { 0# -> u59; _ -> negateInt8# u59 }, case ltInt8# u60 (intToInt8# 0#) of { 0# -> u60; _ -> negateInt8# u60 }, case ltInt8# u61 (intToInt8# 0#) of { 0# -> u61; _ -> negateInt8# u61 }, case ltInt8# u62 (intToInt8# 0#) of { 0# -> u62; _ -> negateInt8# u62 }, case ltInt8# u63 (intToInt8# 0#) of { 0# -> u63; _ -> negateInt8# u63 } #)
{-# INLINE [0] absInt8X64# #-}

absInt16X32# :: Int16X32# -> Int16X32#
absInt16X32# u = case unpackInt16X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> packInt16X32# (# case ltInt16# u0 (intToInt16# 0#) of { 0# -> u0; _ -> negateInt16# u0 }, case ltInt16# u1 (intToInt16# 0#) of { 0# -> u1; _ -> negateInt16# u1 }, case ltInt16# u2 (intToInt16# 0#) of { 0# -> u2; _ -> negateInt16# u2 }, case ltInt16# u3 (intToInt16# 0#) of { 0# -> u3; _ -> negateInt16# u3 }, case ltInt16# u4 (intToInt16# 0#) of { 0# -> u4; _ -> negateInt16# u4 }, case ltInt16# u5 (intToInt16# 0#) of { 0# -> u5; _ -> negateInt16# u5 }, case ltInt16# u6 (intToInt16# 0#) of { 0# -> u6; _ -> negateInt16# u6 }, case ltInt16# u7 (intToInt16# 0#) of { 0# -> u7; _ -> negateInt16# u7 }, case ltInt16# u8 (intToInt16# 0#) of { 0# -> u8; _ -> negateInt16# u8 }, case ltInt16# u9 (intToInt16# 0#) of { 0# -> u9; _ -> negateInt16# u9 }, case ltInt16# u10 (intToInt16# 0#) of { 0# -> u10; _ -> negateInt16# u10 }, case ltInt16# u11 (intToInt16# 0#) of { 0# -> u11; _ -> negateInt16# u11 }, case ltInt16# u12 (intToInt16# 0#) of { 0# -> u12; _ -> negateInt16# u12 }, case ltInt16# u13 (intToInt16# 0#) of { 0# -> u13; _ -> negateInt16# u13 }, case ltInt16# u14 (intToInt16# 0#) of { 0# -> u14; _ -> negateInt16# u14 }, case ltInt16# u15 (intToInt16# 0#) of { 0# -> u15; _ -> negateInt16# u15 }, case ltInt16# u16 (intToInt16# 0#) of { 0# -> u16; _ -> negateInt16# u16 }, case ltInt16# u17 (intToInt16# 0#) of { 0# -> u17; _ -> negateInt16# u17 }, case ltInt16# u18 (intToInt16# 0#) of { 0# -> u18; _ -> negateInt16# u18 }, case ltInt16# u19 (intToInt16# 0#) of { 0# -> u19; _ -> negateInt16# u19 }, case ltInt16# u20 (intToInt16# 0#) of { 0# -> u20; _ -> negateInt16# u20 }, case ltInt16# u21 (intToInt16# 0#) of { 0# -> u21; _ -> negateInt16# u21 }, case ltInt16# u22 (intToInt16# 0#) of { 0# -> u22; _ -> negateInt16# u22 }, case ltInt16# u23 (intToInt16# 0#) of { 0# -> u23; _ -> negateInt16# u23 }, case ltInt16# u24 (intToInt16# 0#) of { 0# -> u24; _ -> negateInt16# u24 }, case ltInt16# u25 (intToInt16# 0#) of { 0# -> u25; _ -> negateInt16# u25 }, case ltInt16# u26 (intToInt16# 0#) of { 0# -> u26; _ -> negateInt16# u26 }, case ltInt16# u27 (intToInt16# 0#) of { 0# -> u27; _ -> negateInt16# u27 }, case ltInt16# u28 (intToInt16# 0#) of { 0# -> u28; _ -> negateInt16# u28 }, case ltInt16# u29 (intToInt16# 0#) of { 0# -> u29; _ -> negateInt16# u29 }, case ltInt16# u30 (intToInt16# 0#) of { 0# -> u30; _ -> negateInt16# u30 }, case ltInt16# u31 (intToInt16# 0#) of { 0# -> u31; _ -> negateInt16# u31 } #)
{-# INLINE [0] absInt16X32# #-}

absInt32X16# :: Int32X16# -> Int32X16#
absInt32X16# u = case unpackInt32X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packInt32X16# (# case ltInt32# u0 (intToInt32# 0#) of { 0# -> u0; _ -> negateInt32# u0 }, case ltInt32# u1 (intToInt32# 0#) of { 0# -> u1; _ -> negateInt32# u1 }, case ltInt32# u2 (intToInt32# 0#) of { 0# -> u2; _ -> negateInt32# u2 }, case ltInt32# u3 (intToInt32# 0#) of { 0# -> u3; _ -> negateInt32# u3 }, case ltInt32# u4 (intToInt32# 0#) of { 0# -> u4; _ -> negateInt32# u4 }, case ltInt32# u5 (intToInt32# 0#) of { 0# -> u5; _ -> negateInt32# u5 }, case ltInt32# u6 (intToInt32# 0#) of { 0# -> u6; _ -> negateInt32# u6 }, case ltInt32# u7 (intToInt32# 0#) of { 0# -> u7; _ -> negateInt32# u7 }, case ltInt32# u8 (intToInt32# 0#) of { 0# -> u8; _ -> negateInt32# u8 }, case ltInt32# u9 (intToInt32# 0#) of { 0# -> u9; _ -> negateInt32# u9 }, case ltInt32# u10 (intToInt32# 0#) of { 0# -> u10; _ -> negateInt32# u10 }, case ltInt32# u11 (intToInt32# 0#) of { 0# -> u11; _ -> negateInt32# u11 }, case ltInt32# u12 (intToInt32# 0#) of { 0# -> u12; _ -> negateInt32# u12 }, case ltInt32# u13 (intToInt32# 0#) of { 0# -> u13; _ -> negateInt32# u13 }, case ltInt32# u14 (intToInt32# 0#) of { 0# -> u14; _ -> negateInt32# u14 }, case ltInt32# u15 (intToInt32# 0#) of { 0# -> u15; _ -> negateInt32# u15 } #)
{-# INLINE [0] absInt32X16# #-}

absInt64X8# :: Int64X8# -> Int64X8#
absInt64X8# u = case unpackInt64X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packInt64X8# (# case ltInt64# u0 (intToInt64# 0#) of { 0# -> u0; _ -> negateInt64# u0 }, case ltInt64# u1 (intToInt64# 0#) of { 0# -> u1; _ -> negateInt64# u1 }, case ltInt64# u2 (intToInt64# 0#) of { 0# -> u2; _ -> negateInt64# u2 }, case ltInt64# u3 (intToInt64# 0#) of { 0# -> u3; _ -> negateInt64# u3 }, case ltInt64# u4 (intToInt64# 0#) of { 0# -> u4; _ -> negateInt64# u4 }, case ltInt64# u5 (intToInt64# 0#) of { 0# -> u5; _ -> negateInt64# u5 }, case ltInt64# u6 (intToInt64# 0#) of { 0# -> u6; _ -> negateInt64# u6 }, case ltInt64# u7 (intToInt64# 0#) of { 0# -> u7; _ -> negateInt64# u7 } #)
{-# INLINE [0] absInt64X8# #-}

absFloatX16# :: FloatX16# -> FloatX16#
absFloatX16# u = case unpackFloatX16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packFloatX16# (# fabsFloat# u0, fabsFloat# u1, fabsFloat# u2, fabsFloat# u3, fabsFloat# u4, fabsFloat# u5, fabsFloat# u6, fabsFloat# u7, fabsFloat# u8, fabsFloat# u9, fabsFloat# u10, fabsFloat# u11, fabsFloat# u12, fabsFloat# u13, fabsFloat# u14, fabsFloat# u15 #)
{-# INLINE [0] absFloatX16# #-}

absDoubleX8# :: DoubleX8# -> DoubleX8#
absDoubleX8# u = case unpackDoubleX8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packDoubleX8# (# fabsDouble# u0, fabsDouble# u1, fabsDouble# u2, fabsDouble# u3, fabsDouble# u4, fabsDouble# u5, fabsDouble# u6, fabsDouble# u7 #)
{-# INLINE [0] absDoubleX8# #-}

sqrtFloatX16# :: FloatX16# -> FloatX16#
sqrtFloatX16# u = case unpackFloatX16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packFloatX16# (# sqrtFloat# u0, sqrtFloat# u1, sqrtFloat# u2, sqrtFloat# u3, sqrtFloat# u4, sqrtFloat# u5, sqrtFloat# u6, sqrtFloat# u7, sqrtFloat# u8, sqrtFloat# u9, sqrtFloat# u10, sqrtFloat# u11, sqrtFloat# u12, sqrtFloat# u13, sqrtFloat# u14, sqrtFloat# u15 #)
{-# INLINE [0] sqrtFloatX16# #-}

sqrtDoubleX8# :: DoubleX8# -> DoubleX8#
sqrtDoubleX8# u = case unpackDoubleX8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packDoubleX8# (# sqrtDouble# u0, sqrtDouble# u1, sqrtDouble# u2, sqrtDouble# u3, sqrtDouble# u4, sqrtDouble# u5, sqrtDouble# u6, sqrtDouble# u7 #)
{-# INLINE [0] sqrtDoubleX8# #-}

#endif
