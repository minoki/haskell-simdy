-- This file was created by script/GenPrim.hs. Do not edit by hand!
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Simdy.Internal.VL128.Prim
  ( Int8X16#
  , Int16X8#
  , Int32X4#
  , Int64X2#
  , Word8X16#
  , Word16X8#
  , Word32X4#
  , Word64X2#
  , FloatX4#
  , DoubleX2#
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
  , packInt8X16#
  , packInt16X8#
  , packInt32X4#
  , packInt64X2#
  , packWord8X16#
  , packWord16X8#
  , packWord32X4#
  , packWord64X2#
  , packFloatX4#
  , packDoubleX2#
  , unpackInt8X16#
  , unpackInt16X8#
  , unpackInt32X4#
  , unpackInt64X2#
  , unpackWord8X16#
  , unpackWord16X8#
  , unpackWord32X4#
  , unpackWord64X2#
  , unpackFloatX4#
  , unpackDoubleX2#
  , insertInt8X16#
  , insertInt16X8#
  , insertInt32X4#
  , insertInt64X2#
  , insertWord8X16#
  , insertWord16X8#
  , insertWord32X4#
  , insertWord64X2#
  , insertFloatX4#
  , insertDoubleX2#
  , plusInt8X16#
  , plusInt16X8#
  , plusInt32X4#
  , plusInt64X2#
  , plusWord8X16#
  , plusWord16X8#
  , plusWord32X4#
  , plusWord64X2#
  , plusFloatX4#
  , plusDoubleX2#
  , minusInt8X16#
  , minusInt16X8#
  , minusInt32X4#
  , minusInt64X2#
  , minusWord8X16#
  , minusWord16X8#
  , minusWord32X4#
  , minusWord64X2#
  , minusFloatX4#
  , minusDoubleX2#
  , timesInt8X16#
  , timesInt16X8#
  , timesInt32X4#
  , timesInt64X2#
  , timesWord8X16#
  , timesWord16X8#
  , timesWord32X4#
  , timesWord64X2#
  , timesFloatX4#
  , timesDoubleX2#
  , divideFloatX4#
  , divideDoubleX2#
  , quotInt8X16#
  , quotInt16X8#
  , quotInt32X4#
  , quotInt64X2#
  , quotWord8X16#
  , quotWord16X8#
  , quotWord32X4#
  , quotWord64X2#
  , remInt8X16#
  , remInt16X8#
  , remInt32X4#
  , remInt64X2#
  , remWord8X16#
  , remWord16X8#
  , remWord32X4#
  , remWord64X2#
  , negateInt8X16#
  , negateInt16X8#
  , negateInt32X4#
  , negateInt64X2#
  , negateFloatX4#
  , negateDoubleX2#
  , indexInt8X16Array#
  , indexInt16X8Array#
  , indexInt32X4Array#
  , indexInt64X2Array#
  , indexWord8X16Array#
  , indexWord16X8Array#
  , indexWord32X4Array#
  , indexWord64X2Array#
  , indexFloatX4Array#
  , indexDoubleX2Array#
  , readInt8X16Array#
  , readInt16X8Array#
  , readInt32X4Array#
  , readInt64X2Array#
  , readWord8X16Array#
  , readWord16X8Array#
  , readWord32X4Array#
  , readWord64X2Array#
  , readFloatX4Array#
  , readDoubleX2Array#
  , writeInt8X16Array#
  , writeInt16X8Array#
  , writeInt32X4Array#
  , writeInt64X2Array#
  , writeWord8X16Array#
  , writeWord16X8Array#
  , writeWord32X4Array#
  , writeWord64X2Array#
  , writeFloatX4Array#
  , writeDoubleX2Array#
  , indexInt8X16OffAddr#
  , indexInt16X8OffAddr#
  , indexInt32X4OffAddr#
  , indexInt64X2OffAddr#
  , indexWord8X16OffAddr#
  , indexWord16X8OffAddr#
  , indexWord32X4OffAddr#
  , indexWord64X2OffAddr#
  , indexFloatX4OffAddr#
  , indexDoubleX2OffAddr#
  , readInt8X16OffAddr#
  , readInt16X8OffAddr#
  , readInt32X4OffAddr#
  , readInt64X2OffAddr#
  , readWord8X16OffAddr#
  , readWord16X8OffAddr#
  , readWord32X4OffAddr#
  , readWord64X2OffAddr#
  , readFloatX4OffAddr#
  , readDoubleX2OffAddr#
  , writeInt8X16OffAddr#
  , writeInt16X8OffAddr#
  , writeInt32X4OffAddr#
  , writeInt64X2OffAddr#
  , writeWord8X16OffAddr#
  , writeWord16X8OffAddr#
  , writeWord32X4OffAddr#
  , writeWord64X2OffAddr#
  , writeFloatX4OffAddr#
  , writeDoubleX2OffAddr#
  , indexInt8ArrayAsInt8X16#
  , indexInt16ArrayAsInt16X8#
  , indexInt32ArrayAsInt32X4#
  , indexInt64ArrayAsInt64X2#
  , indexWord8ArrayAsWord8X16#
  , indexWord16ArrayAsWord16X8#
  , indexWord32ArrayAsWord32X4#
  , indexWord64ArrayAsWord64X2#
  , indexFloatArrayAsFloatX4#
  , indexDoubleArrayAsDoubleX2#
  , readInt8ArrayAsInt8X16#
  , readInt16ArrayAsInt16X8#
  , readInt32ArrayAsInt32X4#
  , readInt64ArrayAsInt64X2#
  , readWord8ArrayAsWord8X16#
  , readWord16ArrayAsWord16X8#
  , readWord32ArrayAsWord32X4#
  , readWord64ArrayAsWord64X2#
  , readFloatArrayAsFloatX4#
  , readDoubleArrayAsDoubleX2#
  , writeInt8ArrayAsInt8X16#
  , writeInt16ArrayAsInt16X8#
  , writeInt32ArrayAsInt32X4#
  , writeInt64ArrayAsInt64X2#
  , writeWord8ArrayAsWord8X16#
  , writeWord16ArrayAsWord16X8#
  , writeWord32ArrayAsWord32X4#
  , writeWord64ArrayAsWord64X2#
  , writeFloatArrayAsFloatX4#
  , writeDoubleArrayAsDoubleX2#
  , indexInt8OffAddrAsInt8X16#
  , indexInt16OffAddrAsInt16X8#
  , indexInt32OffAddrAsInt32X4#
  , indexInt64OffAddrAsInt64X2#
  , indexWord8OffAddrAsWord8X16#
  , indexWord16OffAddrAsWord16X8#
  , indexWord32OffAddrAsWord32X4#
  , indexWord64OffAddrAsWord64X2#
  , indexFloatOffAddrAsFloatX4#
  , indexDoubleOffAddrAsDoubleX2#
  , readInt8OffAddrAsInt8X16#
  , readInt16OffAddrAsInt16X8#
  , readInt32OffAddrAsInt32X4#
  , readInt64OffAddrAsInt64X2#
  , readWord8OffAddrAsWord8X16#
  , readWord16OffAddrAsWord16X8#
  , readWord32OffAddrAsWord32X4#
  , readWord64OffAddrAsWord64X2#
  , readFloatOffAddrAsFloatX4#
  , readDoubleOffAddrAsDoubleX2#
  , writeInt8OffAddrAsInt8X16#
  , writeInt16OffAddrAsInt16X8#
  , writeInt32OffAddrAsInt32X4#
  , writeInt64OffAddrAsInt64X2#
  , writeWord8OffAddrAsWord8X16#
  , writeWord16OffAddrAsWord16X8#
  , writeWord32OffAddrAsWord32X4#
  , writeWord64OffAddrAsWord64X2#
  , writeFloatOffAddrAsFloatX4#
  , writeDoubleOffAddrAsDoubleX2#
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
  , fmaddFloatX4#
  , fmaddDoubleX2#
  , fmsubFloatX4#
  , fmsubDoubleX2#
  , fnmaddFloatX4#
  , fnmaddDoubleX2#
  , fnmsubFloatX4#
  , fnmsubDoubleX2#
  , shuffleInt8X16#
  , shuffleInt16X8#
  , shuffleInt32X4#
  , shuffleInt64X2#
  , shuffleWord8X16#
  , shuffleWord16X8#
  , shuffleWord32X4#
  , shuffleWord64X2#
  , shuffleFloatX4#
  , shuffleDoubleX2#
  , minFloatX4#
  , minDoubleX2#
  , maxFloatX4#
  , maxDoubleX2#
#endif
  , minInt8X16#
  , minInt16X8#
  , minInt32X4#
  , minInt64X2#
  , minWord8X16#
  , minWord16X8#
  , minWord32X4#
  , minWord64X2#
  , maxInt8X16#
  , maxInt16X8#
  , maxInt32X4#
  , maxInt64X2#
  , maxWord8X16#
  , maxWord16X8#
  , maxWord32X4#
  , maxWord64X2#
  , andInt8X16#
  , andInt16X8#
  , andInt32X4#
  , andInt64X2#
  , andWord8X16#
  , andWord16X8#
  , andWord32X4#
  , andWord64X2#
  , orInt8X16#
  , orInt16X8#
  , orInt32X4#
  , orInt64X2#
  , orWord8X16#
  , orWord16X8#
  , orWord32X4#
  , orWord64X2#
  , xorInt8X16#
  , xorInt16X8#
  , xorInt32X4#
  , xorInt64X2#
  , xorWord8X16#
  , xorWord16X8#
  , xorWord32X4#
  , xorWord64X2#
#if MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)
  , andFloatX4#
  , andDoubleX2#
  , orFloatX4#
  , orDoubleX2#
  , xorFloatX4#
  , xorDoubleX2#
#endif
  , absInt8X16#
  , absInt16X8#
  , absInt32X4#
  , absInt64X2#
  , absFloatX4#
  , absDoubleX2#
  , sqrtFloatX4#
  , sqrtDoubleX2#
  , complementInt8X16#
  , complementInt16X8#
  , complementInt32X4#
  , complementInt64X2#
  , complementWord8X16#
  , complementWord16X8#
  , complementWord32X4#
  , complementWord64X2#
  ) where

#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)

-- from ghc-experimental
import           GHC.PrimOps

#else

import           GHC.Exts

#endif

#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)

complementInt8X16# :: Int8X16# -> Int8X16#
complementInt8X16# u = case unpackInt8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packInt8X16# (# intToInt8# (notI# (int8ToInt# u0)), intToInt8# (notI# (int8ToInt# u1)), intToInt8# (notI# (int8ToInt# u2)), intToInt8# (notI# (int8ToInt# u3)), intToInt8# (notI# (int8ToInt# u4)), intToInt8# (notI# (int8ToInt# u5)), intToInt8# (notI# (int8ToInt# u6)), intToInt8# (notI# (int8ToInt# u7)), intToInt8# (notI# (int8ToInt# u8)), intToInt8# (notI# (int8ToInt# u9)), intToInt8# (notI# (int8ToInt# u10)), intToInt8# (notI# (int8ToInt# u11)), intToInt8# (notI# (int8ToInt# u12)), intToInt8# (notI# (int8ToInt# u13)), intToInt8# (notI# (int8ToInt# u14)), intToInt8# (notI# (int8ToInt# u15)) #)
{-# INLINE [0] complementInt8X16# #-}

complementInt16X8# :: Int16X8# -> Int16X8#
complementInt16X8# u = case unpackInt16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packInt16X8# (# intToInt16# (notI# (int16ToInt# u0)), intToInt16# (notI# (int16ToInt# u1)), intToInt16# (notI# (int16ToInt# u2)), intToInt16# (notI# (int16ToInt# u3)), intToInt16# (notI# (int16ToInt# u4)), intToInt16# (notI# (int16ToInt# u5)), intToInt16# (notI# (int16ToInt# u6)), intToInt16# (notI# (int16ToInt# u7)) #)
{-# INLINE [0] complementInt16X8# #-}

complementInt32X4# :: Int32X4# -> Int32X4#
complementInt32X4# u = case unpackInt32X4# u of (# u0, u1, u2, u3 #) -> packInt32X4# (# intToInt32# (notI# (int32ToInt# u0)), intToInt32# (notI# (int32ToInt# u1)), intToInt32# (notI# (int32ToInt# u2)), intToInt32# (notI# (int32ToInt# u3)) #)
{-# INLINE [0] complementInt32X4# #-}

complementInt64X2# :: Int64X2# -> Int64X2#
complementInt64X2# u = case unpackInt64X2# u of (# u0, u1 #) -> packInt64X2# (# word64ToInt64# (not64# (int64ToWord64# u0)), word64ToInt64# (not64# (int64ToWord64# u1)) #)
{-# INLINE [0] complementInt64X2# #-}

complementWord8X16# :: Word8X16# -> Word8X16#
complementWord8X16# u = case unpackWord8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packWord8X16# (# wordToWord8# (not# (word8ToWord# u0)), wordToWord8# (not# (word8ToWord# u1)), wordToWord8# (not# (word8ToWord# u2)), wordToWord8# (not# (word8ToWord# u3)), wordToWord8# (not# (word8ToWord# u4)), wordToWord8# (not# (word8ToWord# u5)), wordToWord8# (not# (word8ToWord# u6)), wordToWord8# (not# (word8ToWord# u7)), wordToWord8# (not# (word8ToWord# u8)), wordToWord8# (not# (word8ToWord# u9)), wordToWord8# (not# (word8ToWord# u10)), wordToWord8# (not# (word8ToWord# u11)), wordToWord8# (not# (word8ToWord# u12)), wordToWord8# (not# (word8ToWord# u13)), wordToWord8# (not# (word8ToWord# u14)), wordToWord8# (not# (word8ToWord# u15)) #)
{-# INLINE [0] complementWord8X16# #-}

complementWord16X8# :: Word16X8# -> Word16X8#
complementWord16X8# u = case unpackWord16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packWord16X8# (# wordToWord16# (not# (word16ToWord# u0)), wordToWord16# (not# (word16ToWord# u1)), wordToWord16# (not# (word16ToWord# u2)), wordToWord16# (not# (word16ToWord# u3)), wordToWord16# (not# (word16ToWord# u4)), wordToWord16# (not# (word16ToWord# u5)), wordToWord16# (not# (word16ToWord# u6)), wordToWord16# (not# (word16ToWord# u7)) #)
{-# INLINE [0] complementWord16X8# #-}

complementWord32X4# :: Word32X4# -> Word32X4#
complementWord32X4# u = case unpackWord32X4# u of (# u0, u1, u2, u3 #) -> packWord32X4# (# wordToWord32# (not# (word32ToWord# u0)), wordToWord32# (not# (word32ToWord# u1)), wordToWord32# (not# (word32ToWord# u2)), wordToWord32# (not# (word32ToWord# u3)) #)
{-# INLINE [0] complementWord32X4# #-}

complementWord64X2# :: Word64X2# -> Word64X2#
complementWord64X2# u = case unpackWord64X2# u of (# u0, u1 #) -> packWord64X2# (# not64# u0, not64# u1 #)
{-# INLINE [0] complementWord64X2# #-}

#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)

andInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#
andInt8X16# u v = case unpackInt8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt8X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt8X16# (# intToInt8# (int8ToInt# u0 `andI#` int8ToInt# v0), intToInt8# (int8ToInt# u1 `andI#` int8ToInt# v1), intToInt8# (int8ToInt# u2 `andI#` int8ToInt# v2), intToInt8# (int8ToInt# u3 `andI#` int8ToInt# v3), intToInt8# (int8ToInt# u4 `andI#` int8ToInt# v4), intToInt8# (int8ToInt# u5 `andI#` int8ToInt# v5), intToInt8# (int8ToInt# u6 `andI#` int8ToInt# v6), intToInt8# (int8ToInt# u7 `andI#` int8ToInt# v7), intToInt8# (int8ToInt# u8 `andI#` int8ToInt# v8), intToInt8# (int8ToInt# u9 `andI#` int8ToInt# v9), intToInt8# (int8ToInt# u10 `andI#` int8ToInt# v10), intToInt8# (int8ToInt# u11 `andI#` int8ToInt# v11), intToInt8# (int8ToInt# u12 `andI#` int8ToInt# v12), intToInt8# (int8ToInt# u13 `andI#` int8ToInt# v13), intToInt8# (int8ToInt# u14 `andI#` int8ToInt# v14), intToInt8# (int8ToInt# u15 `andI#` int8ToInt# v15) #)
{-# INLINE [0] andInt8X16# #-}

andInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#
andInt16X8# u v = case unpackInt16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt16X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt16X8# (# intToInt16# (int16ToInt# u0 `andI#` int16ToInt# v0), intToInt16# (int16ToInt# u1 `andI#` int16ToInt# v1), intToInt16# (int16ToInt# u2 `andI#` int16ToInt# v2), intToInt16# (int16ToInt# u3 `andI#` int16ToInt# v3), intToInt16# (int16ToInt# u4 `andI#` int16ToInt# v4), intToInt16# (int16ToInt# u5 `andI#` int16ToInt# v5), intToInt16# (int16ToInt# u6 `andI#` int16ToInt# v6), intToInt16# (int16ToInt# u7 `andI#` int16ToInt# v7) #)
{-# INLINE [0] andInt16X8# #-}

andInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
andInt32X4# u v = case unpackInt32X4# u of (# u0, u1, u2, u3 #) -> case unpackInt32X4# v of (# v0, v1, v2, v3 #) -> packInt32X4# (# intToInt32# (int32ToInt# u0 `andI#` int32ToInt# v0), intToInt32# (int32ToInt# u1 `andI#` int32ToInt# v1), intToInt32# (int32ToInt# u2 `andI#` int32ToInt# v2), intToInt32# (int32ToInt# u3 `andI#` int32ToInt# v3) #)
{-# INLINE [0] andInt32X4# #-}

andInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#
andInt64X2# u v = case unpackInt64X2# u of (# u0, u1 #) -> case unpackInt64X2# v of (# v0, v1 #) -> packInt64X2# (# word64ToInt64# (int64ToWord64# u0 `and64#` int64ToWord64# v0), word64ToInt64# (int64ToWord64# u1 `and64#` int64ToWord64# v1) #)
{-# INLINE [0] andInt64X2# #-}

andWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#
andWord8X16# u v = case unpackWord8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord8X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord8X16# (# wordToWord8# (word8ToWord# u0 `and#` word8ToWord# v0), wordToWord8# (word8ToWord# u1 `and#` word8ToWord# v1), wordToWord8# (word8ToWord# u2 `and#` word8ToWord# v2), wordToWord8# (word8ToWord# u3 `and#` word8ToWord# v3), wordToWord8# (word8ToWord# u4 `and#` word8ToWord# v4), wordToWord8# (word8ToWord# u5 `and#` word8ToWord# v5), wordToWord8# (word8ToWord# u6 `and#` word8ToWord# v6), wordToWord8# (word8ToWord# u7 `and#` word8ToWord# v7), wordToWord8# (word8ToWord# u8 `and#` word8ToWord# v8), wordToWord8# (word8ToWord# u9 `and#` word8ToWord# v9), wordToWord8# (word8ToWord# u10 `and#` word8ToWord# v10), wordToWord8# (word8ToWord# u11 `and#` word8ToWord# v11), wordToWord8# (word8ToWord# u12 `and#` word8ToWord# v12), wordToWord8# (word8ToWord# u13 `and#` word8ToWord# v13), wordToWord8# (word8ToWord# u14 `and#` word8ToWord# v14), wordToWord8# (word8ToWord# u15 `and#` word8ToWord# v15) #)
{-# INLINE [0] andWord8X16# #-}

andWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#
andWord16X8# u v = case unpackWord16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord16X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord16X8# (# wordToWord16# (word16ToWord# u0 `and#` word16ToWord# v0), wordToWord16# (word16ToWord# u1 `and#` word16ToWord# v1), wordToWord16# (word16ToWord# u2 `and#` word16ToWord# v2), wordToWord16# (word16ToWord# u3 `and#` word16ToWord# v3), wordToWord16# (word16ToWord# u4 `and#` word16ToWord# v4), wordToWord16# (word16ToWord# u5 `and#` word16ToWord# v5), wordToWord16# (word16ToWord# u6 `and#` word16ToWord# v6), wordToWord16# (word16ToWord# u7 `and#` word16ToWord# v7) #)
{-# INLINE [0] andWord16X8# #-}

andWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#
andWord32X4# u v = case unpackWord32X4# u of (# u0, u1, u2, u3 #) -> case unpackWord32X4# v of (# v0, v1, v2, v3 #) -> packWord32X4# (# wordToWord32# (word32ToWord# u0 `and#` word32ToWord# v0), wordToWord32# (word32ToWord# u1 `and#` word32ToWord# v1), wordToWord32# (word32ToWord# u2 `and#` word32ToWord# v2), wordToWord32# (word32ToWord# u3 `and#` word32ToWord# v3) #)
{-# INLINE [0] andWord32X4# #-}

andWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
andWord64X2# u v = case unpackWord64X2# u of (# u0, u1 #) -> case unpackWord64X2# v of (# v0, v1 #) -> packWord64X2# (# and64# u0 v0, and64# u1 v1 #)
{-# INLINE [0] andWord64X2# #-}

orInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#
orInt8X16# u v = case unpackInt8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt8X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt8X16# (# intToInt8# (int8ToInt# u0 `orI#` int8ToInt# v0), intToInt8# (int8ToInt# u1 `orI#` int8ToInt# v1), intToInt8# (int8ToInt# u2 `orI#` int8ToInt# v2), intToInt8# (int8ToInt# u3 `orI#` int8ToInt# v3), intToInt8# (int8ToInt# u4 `orI#` int8ToInt# v4), intToInt8# (int8ToInt# u5 `orI#` int8ToInt# v5), intToInt8# (int8ToInt# u6 `orI#` int8ToInt# v6), intToInt8# (int8ToInt# u7 `orI#` int8ToInt# v7), intToInt8# (int8ToInt# u8 `orI#` int8ToInt# v8), intToInt8# (int8ToInt# u9 `orI#` int8ToInt# v9), intToInt8# (int8ToInt# u10 `orI#` int8ToInt# v10), intToInt8# (int8ToInt# u11 `orI#` int8ToInt# v11), intToInt8# (int8ToInt# u12 `orI#` int8ToInt# v12), intToInt8# (int8ToInt# u13 `orI#` int8ToInt# v13), intToInt8# (int8ToInt# u14 `orI#` int8ToInt# v14), intToInt8# (int8ToInt# u15 `orI#` int8ToInt# v15) #)
{-# INLINE [0] orInt8X16# #-}

orInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#
orInt16X8# u v = case unpackInt16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt16X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt16X8# (# intToInt16# (int16ToInt# u0 `orI#` int16ToInt# v0), intToInt16# (int16ToInt# u1 `orI#` int16ToInt# v1), intToInt16# (int16ToInt# u2 `orI#` int16ToInt# v2), intToInt16# (int16ToInt# u3 `orI#` int16ToInt# v3), intToInt16# (int16ToInt# u4 `orI#` int16ToInt# v4), intToInt16# (int16ToInt# u5 `orI#` int16ToInt# v5), intToInt16# (int16ToInt# u6 `orI#` int16ToInt# v6), intToInt16# (int16ToInt# u7 `orI#` int16ToInt# v7) #)
{-# INLINE [0] orInt16X8# #-}

orInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
orInt32X4# u v = case unpackInt32X4# u of (# u0, u1, u2, u3 #) -> case unpackInt32X4# v of (# v0, v1, v2, v3 #) -> packInt32X4# (# intToInt32# (int32ToInt# u0 `orI#` int32ToInt# v0), intToInt32# (int32ToInt# u1 `orI#` int32ToInt# v1), intToInt32# (int32ToInt# u2 `orI#` int32ToInt# v2), intToInt32# (int32ToInt# u3 `orI#` int32ToInt# v3) #)
{-# INLINE [0] orInt32X4# #-}

orInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#
orInt64X2# u v = case unpackInt64X2# u of (# u0, u1 #) -> case unpackInt64X2# v of (# v0, v1 #) -> packInt64X2# (# word64ToInt64# (int64ToWord64# u0 `or64#` int64ToWord64# v0), word64ToInt64# (int64ToWord64# u1 `or64#` int64ToWord64# v1) #)
{-# INLINE [0] orInt64X2# #-}

orWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#
orWord8X16# u v = case unpackWord8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord8X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord8X16# (# wordToWord8# (word8ToWord# u0 `or#` word8ToWord# v0), wordToWord8# (word8ToWord# u1 `or#` word8ToWord# v1), wordToWord8# (word8ToWord# u2 `or#` word8ToWord# v2), wordToWord8# (word8ToWord# u3 `or#` word8ToWord# v3), wordToWord8# (word8ToWord# u4 `or#` word8ToWord# v4), wordToWord8# (word8ToWord# u5 `or#` word8ToWord# v5), wordToWord8# (word8ToWord# u6 `or#` word8ToWord# v6), wordToWord8# (word8ToWord# u7 `or#` word8ToWord# v7), wordToWord8# (word8ToWord# u8 `or#` word8ToWord# v8), wordToWord8# (word8ToWord# u9 `or#` word8ToWord# v9), wordToWord8# (word8ToWord# u10 `or#` word8ToWord# v10), wordToWord8# (word8ToWord# u11 `or#` word8ToWord# v11), wordToWord8# (word8ToWord# u12 `or#` word8ToWord# v12), wordToWord8# (word8ToWord# u13 `or#` word8ToWord# v13), wordToWord8# (word8ToWord# u14 `or#` word8ToWord# v14), wordToWord8# (word8ToWord# u15 `or#` word8ToWord# v15) #)
{-# INLINE [0] orWord8X16# #-}

orWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#
orWord16X8# u v = case unpackWord16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord16X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord16X8# (# wordToWord16# (word16ToWord# u0 `or#` word16ToWord# v0), wordToWord16# (word16ToWord# u1 `or#` word16ToWord# v1), wordToWord16# (word16ToWord# u2 `or#` word16ToWord# v2), wordToWord16# (word16ToWord# u3 `or#` word16ToWord# v3), wordToWord16# (word16ToWord# u4 `or#` word16ToWord# v4), wordToWord16# (word16ToWord# u5 `or#` word16ToWord# v5), wordToWord16# (word16ToWord# u6 `or#` word16ToWord# v6), wordToWord16# (word16ToWord# u7 `or#` word16ToWord# v7) #)
{-# INLINE [0] orWord16X8# #-}

orWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#
orWord32X4# u v = case unpackWord32X4# u of (# u0, u1, u2, u3 #) -> case unpackWord32X4# v of (# v0, v1, v2, v3 #) -> packWord32X4# (# wordToWord32# (word32ToWord# u0 `or#` word32ToWord# v0), wordToWord32# (word32ToWord# u1 `or#` word32ToWord# v1), wordToWord32# (word32ToWord# u2 `or#` word32ToWord# v2), wordToWord32# (word32ToWord# u3 `or#` word32ToWord# v3) #)
{-# INLINE [0] orWord32X4# #-}

orWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
orWord64X2# u v = case unpackWord64X2# u of (# u0, u1 #) -> case unpackWord64X2# v of (# v0, v1 #) -> packWord64X2# (# or64# u0 v0, or64# u1 v1 #)
{-# INLINE [0] orWord64X2# #-}

xorInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#
xorInt8X16# u v = case unpackInt8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt8X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt8X16# (# intToInt8# (int8ToInt# u0 `xorI#` int8ToInt# v0), intToInt8# (int8ToInt# u1 `xorI#` int8ToInt# v1), intToInt8# (int8ToInt# u2 `xorI#` int8ToInt# v2), intToInt8# (int8ToInt# u3 `xorI#` int8ToInt# v3), intToInt8# (int8ToInt# u4 `xorI#` int8ToInt# v4), intToInt8# (int8ToInt# u5 `xorI#` int8ToInt# v5), intToInt8# (int8ToInt# u6 `xorI#` int8ToInt# v6), intToInt8# (int8ToInt# u7 `xorI#` int8ToInt# v7), intToInt8# (int8ToInt# u8 `xorI#` int8ToInt# v8), intToInt8# (int8ToInt# u9 `xorI#` int8ToInt# v9), intToInt8# (int8ToInt# u10 `xorI#` int8ToInt# v10), intToInt8# (int8ToInt# u11 `xorI#` int8ToInt# v11), intToInt8# (int8ToInt# u12 `xorI#` int8ToInt# v12), intToInt8# (int8ToInt# u13 `xorI#` int8ToInt# v13), intToInt8# (int8ToInt# u14 `xorI#` int8ToInt# v14), intToInt8# (int8ToInt# u15 `xorI#` int8ToInt# v15) #)
{-# INLINE [0] xorInt8X16# #-}

xorInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#
xorInt16X8# u v = case unpackInt16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt16X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt16X8# (# intToInt16# (int16ToInt# u0 `xorI#` int16ToInt# v0), intToInt16# (int16ToInt# u1 `xorI#` int16ToInt# v1), intToInt16# (int16ToInt# u2 `xorI#` int16ToInt# v2), intToInt16# (int16ToInt# u3 `xorI#` int16ToInt# v3), intToInt16# (int16ToInt# u4 `xorI#` int16ToInt# v4), intToInt16# (int16ToInt# u5 `xorI#` int16ToInt# v5), intToInt16# (int16ToInt# u6 `xorI#` int16ToInt# v6), intToInt16# (int16ToInt# u7 `xorI#` int16ToInt# v7) #)
{-# INLINE [0] xorInt16X8# #-}

xorInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
xorInt32X4# u v = case unpackInt32X4# u of (# u0, u1, u2, u3 #) -> case unpackInt32X4# v of (# v0, v1, v2, v3 #) -> packInt32X4# (# intToInt32# (int32ToInt# u0 `xorI#` int32ToInt# v0), intToInt32# (int32ToInt# u1 `xorI#` int32ToInt# v1), intToInt32# (int32ToInt# u2 `xorI#` int32ToInt# v2), intToInt32# (int32ToInt# u3 `xorI#` int32ToInt# v3) #)
{-# INLINE [0] xorInt32X4# #-}

xorInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#
xorInt64X2# u v = case unpackInt64X2# u of (# u0, u1 #) -> case unpackInt64X2# v of (# v0, v1 #) -> packInt64X2# (# word64ToInt64# (int64ToWord64# u0 `xor64#` int64ToWord64# v0), word64ToInt64# (int64ToWord64# u1 `xor64#` int64ToWord64# v1) #)
{-# INLINE [0] xorInt64X2# #-}

xorWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#
xorWord8X16# u v = case unpackWord8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord8X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord8X16# (# wordToWord8# (word8ToWord# u0 `xor#` word8ToWord# v0), wordToWord8# (word8ToWord# u1 `xor#` word8ToWord# v1), wordToWord8# (word8ToWord# u2 `xor#` word8ToWord# v2), wordToWord8# (word8ToWord# u3 `xor#` word8ToWord# v3), wordToWord8# (word8ToWord# u4 `xor#` word8ToWord# v4), wordToWord8# (word8ToWord# u5 `xor#` word8ToWord# v5), wordToWord8# (word8ToWord# u6 `xor#` word8ToWord# v6), wordToWord8# (word8ToWord# u7 `xor#` word8ToWord# v7), wordToWord8# (word8ToWord# u8 `xor#` word8ToWord# v8), wordToWord8# (word8ToWord# u9 `xor#` word8ToWord# v9), wordToWord8# (word8ToWord# u10 `xor#` word8ToWord# v10), wordToWord8# (word8ToWord# u11 `xor#` word8ToWord# v11), wordToWord8# (word8ToWord# u12 `xor#` word8ToWord# v12), wordToWord8# (word8ToWord# u13 `xor#` word8ToWord# v13), wordToWord8# (word8ToWord# u14 `xor#` word8ToWord# v14), wordToWord8# (word8ToWord# u15 `xor#` word8ToWord# v15) #)
{-# INLINE [0] xorWord8X16# #-}

xorWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#
xorWord16X8# u v = case unpackWord16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord16X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord16X8# (# wordToWord16# (word16ToWord# u0 `xor#` word16ToWord# v0), wordToWord16# (word16ToWord# u1 `xor#` word16ToWord# v1), wordToWord16# (word16ToWord# u2 `xor#` word16ToWord# v2), wordToWord16# (word16ToWord# u3 `xor#` word16ToWord# v3), wordToWord16# (word16ToWord# u4 `xor#` word16ToWord# v4), wordToWord16# (word16ToWord# u5 `xor#` word16ToWord# v5), wordToWord16# (word16ToWord# u6 `xor#` word16ToWord# v6), wordToWord16# (word16ToWord# u7 `xor#` word16ToWord# v7) #)
{-# INLINE [0] xorWord16X8# #-}

xorWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#
xorWord32X4# u v = case unpackWord32X4# u of (# u0, u1, u2, u3 #) -> case unpackWord32X4# v of (# v0, v1, v2, v3 #) -> packWord32X4# (# wordToWord32# (word32ToWord# u0 `xor#` word32ToWord# v0), wordToWord32# (word32ToWord# u1 `xor#` word32ToWord# v1), wordToWord32# (word32ToWord# u2 `xor#` word32ToWord# v2), wordToWord32# (word32ToWord# u3 `xor#` word32ToWord# v3) #)
{-# INLINE [0] xorWord32X4# #-}

xorWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
xorWord64X2# u v = case unpackWord64X2# u of (# u0, u1 #) -> case unpackWord64X2# v of (# v0, v1 #) -> packWord64X2# (# xor64# u0 v0, xor64# u1 v1 #)
{-# INLINE [0] xorWord64X2# #-}

#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)

minInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#
minInt8X16# u v = case unpackInt8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt8X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt8X16# (# case ltInt8# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt8# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt8# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt8# u3 v3 of { 0# -> v3; _ -> u3 }, case ltInt8# u4 v4 of { 0# -> v4; _ -> u4 }, case ltInt8# u5 v5 of { 0# -> v5; _ -> u5 }, case ltInt8# u6 v6 of { 0# -> v6; _ -> u6 }, case ltInt8# u7 v7 of { 0# -> v7; _ -> u7 }, case ltInt8# u8 v8 of { 0# -> v8; _ -> u8 }, case ltInt8# u9 v9 of { 0# -> v9; _ -> u9 }, case ltInt8# u10 v10 of { 0# -> v10; _ -> u10 }, case ltInt8# u11 v11 of { 0# -> v11; _ -> u11 }, case ltInt8# u12 v12 of { 0# -> v12; _ -> u12 }, case ltInt8# u13 v13 of { 0# -> v13; _ -> u13 }, case ltInt8# u14 v14 of { 0# -> v14; _ -> u14 }, case ltInt8# u15 v15 of { 0# -> v15; _ -> u15 } #)
{-# INLINE [0] minInt8X16# #-}

minInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#
minInt16X8# u v = case unpackInt16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt16X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt16X8# (# case ltInt16# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt16# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt16# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt16# u3 v3 of { 0# -> v3; _ -> u3 }, case ltInt16# u4 v4 of { 0# -> v4; _ -> u4 }, case ltInt16# u5 v5 of { 0# -> v5; _ -> u5 }, case ltInt16# u6 v6 of { 0# -> v6; _ -> u6 }, case ltInt16# u7 v7 of { 0# -> v7; _ -> u7 } #)
{-# INLINE [0] minInt16X8# #-}

minInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
minInt32X4# u v = case unpackInt32X4# u of (# u0, u1, u2, u3 #) -> case unpackInt32X4# v of (# v0, v1, v2, v3 #) -> packInt32X4# (# case ltInt32# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt32# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt32# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt32# u3 v3 of { 0# -> v3; _ -> u3 } #)
{-# INLINE [0] minInt32X4# #-}

minInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#
minInt64X2# u v = case unpackInt64X2# u of (# u0, u1 #) -> case unpackInt64X2# v of (# v0, v1 #) -> packInt64X2# (# case ltInt64# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt64# u1 v1 of { 0# -> v1; _ -> u1 } #)
{-# INLINE [0] minInt64X2# #-}

minWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#
minWord8X16# u v = case unpackWord8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord8X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord8X16# (# case ltWord8# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord8# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord8# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord8# u3 v3 of { 0# -> v3; _ -> u3 }, case ltWord8# u4 v4 of { 0# -> v4; _ -> u4 }, case ltWord8# u5 v5 of { 0# -> v5; _ -> u5 }, case ltWord8# u6 v6 of { 0# -> v6; _ -> u6 }, case ltWord8# u7 v7 of { 0# -> v7; _ -> u7 }, case ltWord8# u8 v8 of { 0# -> v8; _ -> u8 }, case ltWord8# u9 v9 of { 0# -> v9; _ -> u9 }, case ltWord8# u10 v10 of { 0# -> v10; _ -> u10 }, case ltWord8# u11 v11 of { 0# -> v11; _ -> u11 }, case ltWord8# u12 v12 of { 0# -> v12; _ -> u12 }, case ltWord8# u13 v13 of { 0# -> v13; _ -> u13 }, case ltWord8# u14 v14 of { 0# -> v14; _ -> u14 }, case ltWord8# u15 v15 of { 0# -> v15; _ -> u15 } #)
{-# INLINE [0] minWord8X16# #-}

minWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#
minWord16X8# u v = case unpackWord16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord16X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord16X8# (# case ltWord16# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord16# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord16# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord16# u3 v3 of { 0# -> v3; _ -> u3 }, case ltWord16# u4 v4 of { 0# -> v4; _ -> u4 }, case ltWord16# u5 v5 of { 0# -> v5; _ -> u5 }, case ltWord16# u6 v6 of { 0# -> v6; _ -> u6 }, case ltWord16# u7 v7 of { 0# -> v7; _ -> u7 } #)
{-# INLINE [0] minWord16X8# #-}

minWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#
minWord32X4# u v = case unpackWord32X4# u of (# u0, u1, u2, u3 #) -> case unpackWord32X4# v of (# v0, v1, v2, v3 #) -> packWord32X4# (# case ltWord32# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord32# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord32# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord32# u3 v3 of { 0# -> v3; _ -> u3 } #)
{-# INLINE [0] minWord32X4# #-}

minWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
minWord64X2# u v = case unpackWord64X2# u of (# u0, u1 #) -> case unpackWord64X2# v of (# v0, v1 #) -> packWord64X2# (# case ltWord64# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord64# u1 v1 of { 0# -> v1; _ -> u1 } #)
{-# INLINE [0] minWord64X2# #-}

maxInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#
maxInt8X16# u v = case unpackInt8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt8X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt8X16# (# case ltInt8# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt8# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt8# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt8# u3 v3 of { 0# -> u3; _ -> v3 }, case ltInt8# u4 v4 of { 0# -> u4; _ -> v4 }, case ltInt8# u5 v5 of { 0# -> u5; _ -> v5 }, case ltInt8# u6 v6 of { 0# -> u6; _ -> v6 }, case ltInt8# u7 v7 of { 0# -> u7; _ -> v7 }, case ltInt8# u8 v8 of { 0# -> u8; _ -> v8 }, case ltInt8# u9 v9 of { 0# -> u9; _ -> v9 }, case ltInt8# u10 v10 of { 0# -> u10; _ -> v10 }, case ltInt8# u11 v11 of { 0# -> u11; _ -> v11 }, case ltInt8# u12 v12 of { 0# -> u12; _ -> v12 }, case ltInt8# u13 v13 of { 0# -> u13; _ -> v13 }, case ltInt8# u14 v14 of { 0# -> u14; _ -> v14 }, case ltInt8# u15 v15 of { 0# -> u15; _ -> v15 } #)
{-# INLINE [0] maxInt8X16# #-}

maxInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#
maxInt16X8# u v = case unpackInt16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt16X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt16X8# (# case ltInt16# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt16# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt16# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt16# u3 v3 of { 0# -> u3; _ -> v3 }, case ltInt16# u4 v4 of { 0# -> u4; _ -> v4 }, case ltInt16# u5 v5 of { 0# -> u5; _ -> v5 }, case ltInt16# u6 v6 of { 0# -> u6; _ -> v6 }, case ltInt16# u7 v7 of { 0# -> u7; _ -> v7 } #)
{-# INLINE [0] maxInt16X8# #-}

maxInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
maxInt32X4# u v = case unpackInt32X4# u of (# u0, u1, u2, u3 #) -> case unpackInt32X4# v of (# v0, v1, v2, v3 #) -> packInt32X4# (# case ltInt32# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt32# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt32# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt32# u3 v3 of { 0# -> u3; _ -> v3 } #)
{-# INLINE [0] maxInt32X4# #-}

maxInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#
maxInt64X2# u v = case unpackInt64X2# u of (# u0, u1 #) -> case unpackInt64X2# v of (# v0, v1 #) -> packInt64X2# (# case ltInt64# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt64# u1 v1 of { 0# -> u1; _ -> v1 } #)
{-# INLINE [0] maxInt64X2# #-}

maxWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#
maxWord8X16# u v = case unpackWord8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord8X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord8X16# (# case ltWord8# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord8# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord8# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord8# u3 v3 of { 0# -> u3; _ -> v3 }, case ltWord8# u4 v4 of { 0# -> u4; _ -> v4 }, case ltWord8# u5 v5 of { 0# -> u5; _ -> v5 }, case ltWord8# u6 v6 of { 0# -> u6; _ -> v6 }, case ltWord8# u7 v7 of { 0# -> u7; _ -> v7 }, case ltWord8# u8 v8 of { 0# -> u8; _ -> v8 }, case ltWord8# u9 v9 of { 0# -> u9; _ -> v9 }, case ltWord8# u10 v10 of { 0# -> u10; _ -> v10 }, case ltWord8# u11 v11 of { 0# -> u11; _ -> v11 }, case ltWord8# u12 v12 of { 0# -> u12; _ -> v12 }, case ltWord8# u13 v13 of { 0# -> u13; _ -> v13 }, case ltWord8# u14 v14 of { 0# -> u14; _ -> v14 }, case ltWord8# u15 v15 of { 0# -> u15; _ -> v15 } #)
{-# INLINE [0] maxWord8X16# #-}

maxWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#
maxWord16X8# u v = case unpackWord16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord16X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord16X8# (# case ltWord16# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord16# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord16# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord16# u3 v3 of { 0# -> u3; _ -> v3 }, case ltWord16# u4 v4 of { 0# -> u4; _ -> v4 }, case ltWord16# u5 v5 of { 0# -> u5; _ -> v5 }, case ltWord16# u6 v6 of { 0# -> u6; _ -> v6 }, case ltWord16# u7 v7 of { 0# -> u7; _ -> v7 } #)
{-# INLINE [0] maxWord16X8# #-}

maxWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#
maxWord32X4# u v = case unpackWord32X4# u of (# u0, u1, u2, u3 #) -> case unpackWord32X4# v of (# v0, v1, v2, v3 #) -> packWord32X4# (# case ltWord32# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord32# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord32# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord32# u3 v3 of { 0# -> u3; _ -> v3 } #)
{-# INLINE [0] maxWord32X4# #-}

maxWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#
maxWord64X2# u v = case unpackWord64X2# u of (# u0, u1 #) -> case unpackWord64X2# v of (# v0, v1 #) -> packWord64X2# (# case ltWord64# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord64# u1 v1 of { 0# -> u1; _ -> v1 } #)
{-# INLINE [0] maxWord64X2# #-}

#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)

absInt8X16# :: Int8X16# -> Int8X16#
absInt8X16# u = case unpackInt8X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packInt8X16# (# case ltInt8# u0 (intToInt8# 0#) of { 0# -> u0; _ -> negateInt8# u0 }, case ltInt8# u1 (intToInt8# 0#) of { 0# -> u1; _ -> negateInt8# u1 }, case ltInt8# u2 (intToInt8# 0#) of { 0# -> u2; _ -> negateInt8# u2 }, case ltInt8# u3 (intToInt8# 0#) of { 0# -> u3; _ -> negateInt8# u3 }, case ltInt8# u4 (intToInt8# 0#) of { 0# -> u4; _ -> negateInt8# u4 }, case ltInt8# u5 (intToInt8# 0#) of { 0# -> u5; _ -> negateInt8# u5 }, case ltInt8# u6 (intToInt8# 0#) of { 0# -> u6; _ -> negateInt8# u6 }, case ltInt8# u7 (intToInt8# 0#) of { 0# -> u7; _ -> negateInt8# u7 }, case ltInt8# u8 (intToInt8# 0#) of { 0# -> u8; _ -> negateInt8# u8 }, case ltInt8# u9 (intToInt8# 0#) of { 0# -> u9; _ -> negateInt8# u9 }, case ltInt8# u10 (intToInt8# 0#) of { 0# -> u10; _ -> negateInt8# u10 }, case ltInt8# u11 (intToInt8# 0#) of { 0# -> u11; _ -> negateInt8# u11 }, case ltInt8# u12 (intToInt8# 0#) of { 0# -> u12; _ -> negateInt8# u12 }, case ltInt8# u13 (intToInt8# 0#) of { 0# -> u13; _ -> negateInt8# u13 }, case ltInt8# u14 (intToInt8# 0#) of { 0# -> u14; _ -> negateInt8# u14 }, case ltInt8# u15 (intToInt8# 0#) of { 0# -> u15; _ -> negateInt8# u15 } #)
{-# INLINE [0] absInt8X16# #-}

absInt16X8# :: Int16X8# -> Int16X8#
absInt16X8# u = case unpackInt16X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packInt16X8# (# case ltInt16# u0 (intToInt16# 0#) of { 0# -> u0; _ -> negateInt16# u0 }, case ltInt16# u1 (intToInt16# 0#) of { 0# -> u1; _ -> negateInt16# u1 }, case ltInt16# u2 (intToInt16# 0#) of { 0# -> u2; _ -> negateInt16# u2 }, case ltInt16# u3 (intToInt16# 0#) of { 0# -> u3; _ -> negateInt16# u3 }, case ltInt16# u4 (intToInt16# 0#) of { 0# -> u4; _ -> negateInt16# u4 }, case ltInt16# u5 (intToInt16# 0#) of { 0# -> u5; _ -> negateInt16# u5 }, case ltInt16# u6 (intToInt16# 0#) of { 0# -> u6; _ -> negateInt16# u6 }, case ltInt16# u7 (intToInt16# 0#) of { 0# -> u7; _ -> negateInt16# u7 } #)
{-# INLINE [0] absInt16X8# #-}

absInt32X4# :: Int32X4# -> Int32X4#
absInt32X4# u = case unpackInt32X4# u of (# u0, u1, u2, u3 #) -> packInt32X4# (# case ltInt32# u0 (intToInt32# 0#) of { 0# -> u0; _ -> negateInt32# u0 }, case ltInt32# u1 (intToInt32# 0#) of { 0# -> u1; _ -> negateInt32# u1 }, case ltInt32# u2 (intToInt32# 0#) of { 0# -> u2; _ -> negateInt32# u2 }, case ltInt32# u3 (intToInt32# 0#) of { 0# -> u3; _ -> negateInt32# u3 } #)
{-# INLINE [0] absInt32X4# #-}

absInt64X2# :: Int64X2# -> Int64X2#
absInt64X2# u = case unpackInt64X2# u of (# u0, u1 #) -> packInt64X2# (# case ltInt64# u0 (intToInt64# 0#) of { 0# -> u0; _ -> negateInt64# u0 }, case ltInt64# u1 (intToInt64# 0#) of { 0# -> u1; _ -> negateInt64# u1 } #)
{-# INLINE [0] absInt64X2# #-}

#endif

#else

foreign import ccall unsafe "hs_simdy_complementInt8X16"
  complementInt8X16# :: Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_complementInt16X8"
  complementInt16X8# :: Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_complementInt32X4"
  complementInt32X4# :: Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_complementInt64X2"
  complementInt64X2# :: Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_complementWord8X16"
  complementWord8X16# :: Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_complementWord16X8"
  complementWord16X8# :: Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_complementWord32X4"
  complementWord32X4# :: Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_complementWord64X2"
  complementWord64X2# :: Word64X2# -> Word64X2#

#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)

foreign import ccall unsafe "hs_simdy_andInt8X16"
  andInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_andInt16X8"
  andInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_andInt32X4"
  andInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_andInt64X2"
  andInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_andWord8X16"
  andWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_andWord16X8"
  andWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_andWord32X4"
  andWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_andWord64X2"
  andWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#

foreign import ccall unsafe "hs_simdy_orInt8X16"
  orInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_orInt16X8"
  orInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_orInt32X4"
  orInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_orInt64X2"
  orInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_orWord8X16"
  orWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_orWord16X8"
  orWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_orWord32X4"
  orWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_orWord64X2"
  orWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#

foreign import ccall unsafe "hs_simdy_xorInt8X16"
  xorInt8X16# :: Int8X16# -> Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_xorInt16X8"
  xorInt16X8# :: Int16X8# -> Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_xorInt32X4"
  xorInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_xorInt64X2"
  xorInt64X2# :: Int64X2# -> Int64X2# -> Int64X2#

foreign import ccall unsafe "hs_simdy_xorWord8X16"
  xorWord8X16# :: Word8X16# -> Word8X16# -> Word8X16#

foreign import ccall unsafe "hs_simdy_xorWord16X8"
  xorWord16X8# :: Word16X8# -> Word16X8# -> Word16X8#

foreign import ccall unsafe "hs_simdy_xorWord32X4"
  xorWord32X4# :: Word32X4# -> Word32X4# -> Word32X4#

foreign import ccall unsafe "hs_simdy_xorWord64X2"
  xorWord64X2# :: Word64X2# -> Word64X2# -> Word64X2#

foreign import ccall unsafe "hs_simdy_absInt8X16"
  absInt8X16# :: Int8X16# -> Int8X16#

foreign import ccall unsafe "hs_simdy_absInt16X8"
  absInt16X8# :: Int16X8# -> Int16X8#

foreign import ccall unsafe "hs_simdy_absInt32X4"
  absInt32X4# :: Int32X4# -> Int32X4#

foreign import ccall unsafe "hs_simdy_absInt64X2"
  absInt64X2# :: Int64X2# -> Int64X2#

#endif
#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)

absFloatX4# :: FloatX4# -> FloatX4#
absFloatX4# u = case unpackFloatX4# u of (# u0, u1, u2, u3 #) -> packFloatX4# (# fabsFloat# u0, fabsFloat# u1, fabsFloat# u2, fabsFloat# u3 #)
{-# INLINE [0] absFloatX4# #-}

absDoubleX2# :: DoubleX2# -> DoubleX2#
absDoubleX2# u = case unpackDoubleX2# u of (# u0, u1 #) -> packDoubleX2# (# fabsDouble# u0, fabsDouble# u1 #)
{-# INLINE [0] absDoubleX2# #-}

sqrtFloatX4# :: FloatX4# -> FloatX4#
sqrtFloatX4# u = case unpackFloatX4# u of (# u0, u1, u2, u3 #) -> packFloatX4# (# sqrtFloat# u0, sqrtFloat# u1, sqrtFloat# u2, sqrtFloat# u3 #)
{-# INLINE [0] sqrtFloatX4# #-}

sqrtDoubleX2# :: DoubleX2# -> DoubleX2#
sqrtDoubleX2# u = case unpackDoubleX2# u of (# u0, u1 #) -> packDoubleX2# (# sqrtDouble# u0, sqrtDouble# u1 #)
{-# INLINE [0] sqrtDoubleX2# #-}

#endif
