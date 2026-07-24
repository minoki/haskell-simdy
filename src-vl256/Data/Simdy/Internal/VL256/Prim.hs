-- This file was created by script/GenPrim.hs. Do not edit by hand!
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Simdy.Internal.VL256.Prim
  ( Int8X32#
  , Int16X16#
  , Int32X8#
  , Int64X4#
  , Word8X32#
  , Word16X16#
  , Word32X8#
  , Word64X4#
  , FloatX8#
  , DoubleX4#
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
  , packInt8X32#
  , packInt16X16#
  , packInt32X8#
  , packInt64X4#
  , packWord8X32#
  , packWord16X16#
  , packWord32X8#
  , packWord64X4#
  , packFloatX8#
  , packDoubleX4#
  , unpackInt8X32#
  , unpackInt16X16#
  , unpackInt32X8#
  , unpackInt64X4#
  , unpackWord8X32#
  , unpackWord16X16#
  , unpackWord32X8#
  , unpackWord64X4#
  , unpackFloatX8#
  , unpackDoubleX4#
  , insertInt8X32#
  , insertInt16X16#
  , insertInt32X8#
  , insertInt64X4#
  , insertWord8X32#
  , insertWord16X16#
  , insertWord32X8#
  , insertWord64X4#
  , insertFloatX8#
  , insertDoubleX4#
  , plusInt8X32#
  , plusInt16X16#
  , plusInt32X8#
  , plusInt64X4#
  , plusWord8X32#
  , plusWord16X16#
  , plusWord32X8#
  , plusWord64X4#
  , plusFloatX8#
  , plusDoubleX4#
  , minusInt8X32#
  , minusInt16X16#
  , minusInt32X8#
  , minusInt64X4#
  , minusWord8X32#
  , minusWord16X16#
  , minusWord32X8#
  , minusWord64X4#
  , minusFloatX8#
  , minusDoubleX4#
  , timesInt8X32#
  , timesInt16X16#
  , timesInt32X8#
  , timesInt64X4#
  , timesWord8X32#
  , timesWord16X16#
  , timesWord32X8#
  , timesWord64X4#
  , timesFloatX8#
  , timesDoubleX4#
  , divideFloatX8#
  , divideDoubleX4#
  , quotInt8X32#
  , quotInt16X16#
  , quotInt32X8#
  , quotInt64X4#
  , quotWord8X32#
  , quotWord16X16#
  , quotWord32X8#
  , quotWord64X4#
  , remInt8X32#
  , remInt16X16#
  , remInt32X8#
  , remInt64X4#
  , remWord8X32#
  , remWord16X16#
  , remWord32X8#
  , remWord64X4#
  , negateInt8X32#
  , negateInt16X16#
  , negateInt32X8#
  , negateInt64X4#
  , negateFloatX8#
  , negateDoubleX4#
  , indexInt8X32Array#
  , indexInt16X16Array#
  , indexInt32X8Array#
  , indexInt64X4Array#
  , indexWord8X32Array#
  , indexWord16X16Array#
  , indexWord32X8Array#
  , indexWord64X4Array#
  , indexFloatX8Array#
  , indexDoubleX4Array#
  , readInt8X32Array#
  , readInt16X16Array#
  , readInt32X8Array#
  , readInt64X4Array#
  , readWord8X32Array#
  , readWord16X16Array#
  , readWord32X8Array#
  , readWord64X4Array#
  , readFloatX8Array#
  , readDoubleX4Array#
  , writeInt8X32Array#
  , writeInt16X16Array#
  , writeInt32X8Array#
  , writeInt64X4Array#
  , writeWord8X32Array#
  , writeWord16X16Array#
  , writeWord32X8Array#
  , writeWord64X4Array#
  , writeFloatX8Array#
  , writeDoubleX4Array#
  , indexInt8X32OffAddr#
  , indexInt16X16OffAddr#
  , indexInt32X8OffAddr#
  , indexInt64X4OffAddr#
  , indexWord8X32OffAddr#
  , indexWord16X16OffAddr#
  , indexWord32X8OffAddr#
  , indexWord64X4OffAddr#
  , indexFloatX8OffAddr#
  , indexDoubleX4OffAddr#
  , readInt8X32OffAddr#
  , readInt16X16OffAddr#
  , readInt32X8OffAddr#
  , readInt64X4OffAddr#
  , readWord8X32OffAddr#
  , readWord16X16OffAddr#
  , readWord32X8OffAddr#
  , readWord64X4OffAddr#
  , readFloatX8OffAddr#
  , readDoubleX4OffAddr#
  , writeInt8X32OffAddr#
  , writeInt16X16OffAddr#
  , writeInt32X8OffAddr#
  , writeInt64X4OffAddr#
  , writeWord8X32OffAddr#
  , writeWord16X16OffAddr#
  , writeWord32X8OffAddr#
  , writeWord64X4OffAddr#
  , writeFloatX8OffAddr#
  , writeDoubleX4OffAddr#
  , indexInt8ArrayAsInt8X32#
  , indexInt16ArrayAsInt16X16#
  , indexInt32ArrayAsInt32X8#
  , indexInt64ArrayAsInt64X4#
  , indexWord8ArrayAsWord8X32#
  , indexWord16ArrayAsWord16X16#
  , indexWord32ArrayAsWord32X8#
  , indexWord64ArrayAsWord64X4#
  , indexFloatArrayAsFloatX8#
  , indexDoubleArrayAsDoubleX4#
  , readInt8ArrayAsInt8X32#
  , readInt16ArrayAsInt16X16#
  , readInt32ArrayAsInt32X8#
  , readInt64ArrayAsInt64X4#
  , readWord8ArrayAsWord8X32#
  , readWord16ArrayAsWord16X16#
  , readWord32ArrayAsWord32X8#
  , readWord64ArrayAsWord64X4#
  , readFloatArrayAsFloatX8#
  , readDoubleArrayAsDoubleX4#
  , writeInt8ArrayAsInt8X32#
  , writeInt16ArrayAsInt16X16#
  , writeInt32ArrayAsInt32X8#
  , writeInt64ArrayAsInt64X4#
  , writeWord8ArrayAsWord8X32#
  , writeWord16ArrayAsWord16X16#
  , writeWord32ArrayAsWord32X8#
  , writeWord64ArrayAsWord64X4#
  , writeFloatArrayAsFloatX8#
  , writeDoubleArrayAsDoubleX4#
  , indexInt8OffAddrAsInt8X32#
  , indexInt16OffAddrAsInt16X16#
  , indexInt32OffAddrAsInt32X8#
  , indexInt64OffAddrAsInt64X4#
  , indexWord8OffAddrAsWord8X32#
  , indexWord16OffAddrAsWord16X16#
  , indexWord32OffAddrAsWord32X8#
  , indexWord64OffAddrAsWord64X4#
  , indexFloatOffAddrAsFloatX8#
  , indexDoubleOffAddrAsDoubleX4#
  , readInt8OffAddrAsInt8X32#
  , readInt16OffAddrAsInt16X16#
  , readInt32OffAddrAsInt32X8#
  , readInt64OffAddrAsInt64X4#
  , readWord8OffAddrAsWord8X32#
  , readWord16OffAddrAsWord16X16#
  , readWord32OffAddrAsWord32X8#
  , readWord64OffAddrAsWord64X4#
  , readFloatOffAddrAsFloatX8#
  , readDoubleOffAddrAsDoubleX4#
  , writeInt8OffAddrAsInt8X32#
  , writeInt16OffAddrAsInt16X16#
  , writeInt32OffAddrAsInt32X8#
  , writeInt64OffAddrAsInt64X4#
  , writeWord8OffAddrAsWord8X32#
  , writeWord16OffAddrAsWord16X16#
  , writeWord32OffAddrAsWord32X8#
  , writeWord64OffAddrAsWord64X4#
  , writeFloatOffAddrAsFloatX8#
  , writeDoubleOffAddrAsDoubleX4#
#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)
  , fmaddFloatX8#
  , fmaddDoubleX4#
  , fmsubFloatX8#
  , fmsubDoubleX4#
  , fnmaddFloatX8#
  , fnmaddDoubleX4#
  , fnmsubFloatX8#
  , fnmsubDoubleX4#
  , shuffleInt8X32#
  , shuffleInt16X16#
  , shuffleInt32X8#
  , shuffleInt64X4#
  , shuffleWord8X32#
  , shuffleWord16X16#
  , shuffleWord32X8#
  , shuffleWord64X4#
  , shuffleFloatX8#
  , shuffleDoubleX4#
  , minFloatX8#
  , minDoubleX4#
  , maxFloatX8#
  , maxDoubleX4#
#endif
  , minInt8X32#
  , minInt16X16#
  , minInt32X8#
  , minInt64X4#
  , minWord8X32#
  , minWord16X16#
  , minWord32X8#
  , minWord64X4#
  , maxInt8X32#
  , maxInt16X16#
  , maxInt32X8#
  , maxInt64X4#
  , maxWord8X32#
  , maxWord16X16#
  , maxWord32X8#
  , maxWord64X4#
  , andInt8X32#
  , andInt16X16#
  , andInt32X8#
  , andInt64X4#
  , andWord8X32#
  , andWord16X16#
  , andWord32X8#
  , andWord64X4#
  , orInt8X32#
  , orInt16X16#
  , orInt32X8#
  , orInt64X4#
  , orWord8X32#
  , orWord16X16#
  , orWord32X8#
  , orWord64X4#
  , xorInt8X32#
  , xorInt16X16#
  , xorInt32X8#
  , xorInt64X4#
  , xorWord8X32#
  , xorWord16X16#
  , xorWord32X8#
  , xorWord64X4#
#if MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)
  , andFloatX8#
  , andDoubleX4#
  , orFloatX8#
  , orDoubleX4#
  , xorFloatX8#
  , xorDoubleX4#
#endif
  , absInt8X32#
  , absInt16X16#
  , absInt32X8#
  , absInt64X4#
  , absFloatX8#
  , absDoubleX4#
  , sqrtFloatX8#
  , sqrtDoubleX4#
  , complementInt8X32#
  , complementInt16X16#
  , complementInt32X8#
  , complementInt64X4#
  , complementWord8X32#
  , complementWord16X16#
  , complementWord32X8#
  , complementWord64X4#
  , module Data.Simdy.Internal.VL128.Prim
  ) where
import           Data.Simdy.Internal.VL128.Prim

#if MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)

-- from ghc-experimental
import           GHC.PrimOps

#else

import           GHC.Exts

#endif

complementInt8X32# :: Int8X32# -> Int8X32#
complementInt8X32# u = case unpackInt8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> packInt8X32# (# intToInt8# (notI# (int8ToInt# u0)), intToInt8# (notI# (int8ToInt# u1)), intToInt8# (notI# (int8ToInt# u2)), intToInt8# (notI# (int8ToInt# u3)), intToInt8# (notI# (int8ToInt# u4)), intToInt8# (notI# (int8ToInt# u5)), intToInt8# (notI# (int8ToInt# u6)), intToInt8# (notI# (int8ToInt# u7)), intToInt8# (notI# (int8ToInt# u8)), intToInt8# (notI# (int8ToInt# u9)), intToInt8# (notI# (int8ToInt# u10)), intToInt8# (notI# (int8ToInt# u11)), intToInt8# (notI# (int8ToInt# u12)), intToInt8# (notI# (int8ToInt# u13)), intToInt8# (notI# (int8ToInt# u14)), intToInt8# (notI# (int8ToInt# u15)), intToInt8# (notI# (int8ToInt# u16)), intToInt8# (notI# (int8ToInt# u17)), intToInt8# (notI# (int8ToInt# u18)), intToInt8# (notI# (int8ToInt# u19)), intToInt8# (notI# (int8ToInt# u20)), intToInt8# (notI# (int8ToInt# u21)), intToInt8# (notI# (int8ToInt# u22)), intToInt8# (notI# (int8ToInt# u23)), intToInt8# (notI# (int8ToInt# u24)), intToInt8# (notI# (int8ToInt# u25)), intToInt8# (notI# (int8ToInt# u26)), intToInt8# (notI# (int8ToInt# u27)), intToInt8# (notI# (int8ToInt# u28)), intToInt8# (notI# (int8ToInt# u29)), intToInt8# (notI# (int8ToInt# u30)), intToInt8# (notI# (int8ToInt# u31)) #)
{-# INLINE [0] complementInt8X32# #-}

complementInt16X16# :: Int16X16# -> Int16X16#
complementInt16X16# u = case unpackInt16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packInt16X16# (# intToInt16# (notI# (int16ToInt# u0)), intToInt16# (notI# (int16ToInt# u1)), intToInt16# (notI# (int16ToInt# u2)), intToInt16# (notI# (int16ToInt# u3)), intToInt16# (notI# (int16ToInt# u4)), intToInt16# (notI# (int16ToInt# u5)), intToInt16# (notI# (int16ToInt# u6)), intToInt16# (notI# (int16ToInt# u7)), intToInt16# (notI# (int16ToInt# u8)), intToInt16# (notI# (int16ToInt# u9)), intToInt16# (notI# (int16ToInt# u10)), intToInt16# (notI# (int16ToInt# u11)), intToInt16# (notI# (int16ToInt# u12)), intToInt16# (notI# (int16ToInt# u13)), intToInt16# (notI# (int16ToInt# u14)), intToInt16# (notI# (int16ToInt# u15)) #)
{-# INLINE [0] complementInt16X16# #-}

complementInt32X8# :: Int32X8# -> Int32X8#
complementInt32X8# u = case unpackInt32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packInt32X8# (# intToInt32# (notI# (int32ToInt# u0)), intToInt32# (notI# (int32ToInt# u1)), intToInt32# (notI# (int32ToInt# u2)), intToInt32# (notI# (int32ToInt# u3)), intToInt32# (notI# (int32ToInt# u4)), intToInt32# (notI# (int32ToInt# u5)), intToInt32# (notI# (int32ToInt# u6)), intToInt32# (notI# (int32ToInt# u7)) #)
{-# INLINE [0] complementInt32X8# #-}

complementInt64X4# :: Int64X4# -> Int64X4#
complementInt64X4# u = case unpackInt64X4# u of (# u0, u1, u2, u3 #) -> packInt64X4# (# word64ToInt64# (not64# (int64ToWord64# u0)), word64ToInt64# (not64# (int64ToWord64# u1)), word64ToInt64# (not64# (int64ToWord64# u2)), word64ToInt64# (not64# (int64ToWord64# u3)) #)
{-# INLINE [0] complementInt64X4# #-}

complementWord8X32# :: Word8X32# -> Word8X32#
complementWord8X32# u = case unpackWord8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> packWord8X32# (# wordToWord8# (not# (word8ToWord# u0)), wordToWord8# (not# (word8ToWord# u1)), wordToWord8# (not# (word8ToWord# u2)), wordToWord8# (not# (word8ToWord# u3)), wordToWord8# (not# (word8ToWord# u4)), wordToWord8# (not# (word8ToWord# u5)), wordToWord8# (not# (word8ToWord# u6)), wordToWord8# (not# (word8ToWord# u7)), wordToWord8# (not# (word8ToWord# u8)), wordToWord8# (not# (word8ToWord# u9)), wordToWord8# (not# (word8ToWord# u10)), wordToWord8# (not# (word8ToWord# u11)), wordToWord8# (not# (word8ToWord# u12)), wordToWord8# (not# (word8ToWord# u13)), wordToWord8# (not# (word8ToWord# u14)), wordToWord8# (not# (word8ToWord# u15)), wordToWord8# (not# (word8ToWord# u16)), wordToWord8# (not# (word8ToWord# u17)), wordToWord8# (not# (word8ToWord# u18)), wordToWord8# (not# (word8ToWord# u19)), wordToWord8# (not# (word8ToWord# u20)), wordToWord8# (not# (word8ToWord# u21)), wordToWord8# (not# (word8ToWord# u22)), wordToWord8# (not# (word8ToWord# u23)), wordToWord8# (not# (word8ToWord# u24)), wordToWord8# (not# (word8ToWord# u25)), wordToWord8# (not# (word8ToWord# u26)), wordToWord8# (not# (word8ToWord# u27)), wordToWord8# (not# (word8ToWord# u28)), wordToWord8# (not# (word8ToWord# u29)), wordToWord8# (not# (word8ToWord# u30)), wordToWord8# (not# (word8ToWord# u31)) #)
{-# INLINE [0] complementWord8X32# #-}

complementWord16X16# :: Word16X16# -> Word16X16#
complementWord16X16# u = case unpackWord16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packWord16X16# (# wordToWord16# (not# (word16ToWord# u0)), wordToWord16# (not# (word16ToWord# u1)), wordToWord16# (not# (word16ToWord# u2)), wordToWord16# (not# (word16ToWord# u3)), wordToWord16# (not# (word16ToWord# u4)), wordToWord16# (not# (word16ToWord# u5)), wordToWord16# (not# (word16ToWord# u6)), wordToWord16# (not# (word16ToWord# u7)), wordToWord16# (not# (word16ToWord# u8)), wordToWord16# (not# (word16ToWord# u9)), wordToWord16# (not# (word16ToWord# u10)), wordToWord16# (not# (word16ToWord# u11)), wordToWord16# (not# (word16ToWord# u12)), wordToWord16# (not# (word16ToWord# u13)), wordToWord16# (not# (word16ToWord# u14)), wordToWord16# (not# (word16ToWord# u15)) #)
{-# INLINE [0] complementWord16X16# #-}

complementWord32X8# :: Word32X8# -> Word32X8#
complementWord32X8# u = case unpackWord32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packWord32X8# (# wordToWord32# (not# (word32ToWord# u0)), wordToWord32# (not# (word32ToWord# u1)), wordToWord32# (not# (word32ToWord# u2)), wordToWord32# (not# (word32ToWord# u3)), wordToWord32# (not# (word32ToWord# u4)), wordToWord32# (not# (word32ToWord# u5)), wordToWord32# (not# (word32ToWord# u6)), wordToWord32# (not# (word32ToWord# u7)) #)
{-# INLINE [0] complementWord32X8# #-}

complementWord64X4# :: Word64X4# -> Word64X4#
complementWord64X4# u = case unpackWord64X4# u of (# u0, u1, u2, u3 #) -> packWord64X4# (# not64# u0, not64# u1, not64# u2, not64# u3 #)
{-# INLINE [0] complementWord64X4# #-}

#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)

andInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#
andInt8X32# u v = case unpackInt8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackInt8X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packInt8X32# (# intToInt8# (int8ToInt# u0 `andI#` int8ToInt# v0), intToInt8# (int8ToInt# u1 `andI#` int8ToInt# v1), intToInt8# (int8ToInt# u2 `andI#` int8ToInt# v2), intToInt8# (int8ToInt# u3 `andI#` int8ToInt# v3), intToInt8# (int8ToInt# u4 `andI#` int8ToInt# v4), intToInt8# (int8ToInt# u5 `andI#` int8ToInt# v5), intToInt8# (int8ToInt# u6 `andI#` int8ToInt# v6), intToInt8# (int8ToInt# u7 `andI#` int8ToInt# v7), intToInt8# (int8ToInt# u8 `andI#` int8ToInt# v8), intToInt8# (int8ToInt# u9 `andI#` int8ToInt# v9), intToInt8# (int8ToInt# u10 `andI#` int8ToInt# v10), intToInt8# (int8ToInt# u11 `andI#` int8ToInt# v11), intToInt8# (int8ToInt# u12 `andI#` int8ToInt# v12), intToInt8# (int8ToInt# u13 `andI#` int8ToInt# v13), intToInt8# (int8ToInt# u14 `andI#` int8ToInt# v14), intToInt8# (int8ToInt# u15 `andI#` int8ToInt# v15), intToInt8# (int8ToInt# u16 `andI#` int8ToInt# v16), intToInt8# (int8ToInt# u17 `andI#` int8ToInt# v17), intToInt8# (int8ToInt# u18 `andI#` int8ToInt# v18), intToInt8# (int8ToInt# u19 `andI#` int8ToInt# v19), intToInt8# (int8ToInt# u20 `andI#` int8ToInt# v20), intToInt8# (int8ToInt# u21 `andI#` int8ToInt# v21), intToInt8# (int8ToInt# u22 `andI#` int8ToInt# v22), intToInt8# (int8ToInt# u23 `andI#` int8ToInt# v23), intToInt8# (int8ToInt# u24 `andI#` int8ToInt# v24), intToInt8# (int8ToInt# u25 `andI#` int8ToInt# v25), intToInt8# (int8ToInt# u26 `andI#` int8ToInt# v26), intToInt8# (int8ToInt# u27 `andI#` int8ToInt# v27), intToInt8# (int8ToInt# u28 `andI#` int8ToInt# v28), intToInt8# (int8ToInt# u29 `andI#` int8ToInt# v29), intToInt8# (int8ToInt# u30 `andI#` int8ToInt# v30), intToInt8# (int8ToInt# u31 `andI#` int8ToInt# v31) #)
{-# INLINE [0] andInt8X32# #-}

andInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#
andInt16X16# u v = case unpackInt16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt16X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt16X16# (# intToInt16# (int16ToInt# u0 `andI#` int16ToInt# v0), intToInt16# (int16ToInt# u1 `andI#` int16ToInt# v1), intToInt16# (int16ToInt# u2 `andI#` int16ToInt# v2), intToInt16# (int16ToInt# u3 `andI#` int16ToInt# v3), intToInt16# (int16ToInt# u4 `andI#` int16ToInt# v4), intToInt16# (int16ToInt# u5 `andI#` int16ToInt# v5), intToInt16# (int16ToInt# u6 `andI#` int16ToInt# v6), intToInt16# (int16ToInt# u7 `andI#` int16ToInt# v7), intToInt16# (int16ToInt# u8 `andI#` int16ToInt# v8), intToInt16# (int16ToInt# u9 `andI#` int16ToInt# v9), intToInt16# (int16ToInt# u10 `andI#` int16ToInt# v10), intToInt16# (int16ToInt# u11 `andI#` int16ToInt# v11), intToInt16# (int16ToInt# u12 `andI#` int16ToInt# v12), intToInt16# (int16ToInt# u13 `andI#` int16ToInt# v13), intToInt16# (int16ToInt# u14 `andI#` int16ToInt# v14), intToInt16# (int16ToInt# u15 `andI#` int16ToInt# v15) #)
{-# INLINE [0] andInt16X16# #-}

andInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#
andInt32X8# u v = case unpackInt32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt32X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt32X8# (# intToInt32# (int32ToInt# u0 `andI#` int32ToInt# v0), intToInt32# (int32ToInt# u1 `andI#` int32ToInt# v1), intToInt32# (int32ToInt# u2 `andI#` int32ToInt# v2), intToInt32# (int32ToInt# u3 `andI#` int32ToInt# v3), intToInt32# (int32ToInt# u4 `andI#` int32ToInt# v4), intToInt32# (int32ToInt# u5 `andI#` int32ToInt# v5), intToInt32# (int32ToInt# u6 `andI#` int32ToInt# v6), intToInt32# (int32ToInt# u7 `andI#` int32ToInt# v7) #)
{-# INLINE [0] andInt32X8# #-}

andInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#
andInt64X4# u v = case unpackInt64X4# u of (# u0, u1, u2, u3 #) -> case unpackInt64X4# v of (# v0, v1, v2, v3 #) -> packInt64X4# (# word64ToInt64# (int64ToWord64# u0 `and64#` int64ToWord64# v0), word64ToInt64# (int64ToWord64# u1 `and64#` int64ToWord64# v1), word64ToInt64# (int64ToWord64# u2 `and64#` int64ToWord64# v2), word64ToInt64# (int64ToWord64# u3 `and64#` int64ToWord64# v3) #)
{-# INLINE [0] andInt64X4# #-}

andWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#
andWord8X32# u v = case unpackWord8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackWord8X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packWord8X32# (# wordToWord8# (word8ToWord# u0 `and#` word8ToWord# v0), wordToWord8# (word8ToWord# u1 `and#` word8ToWord# v1), wordToWord8# (word8ToWord# u2 `and#` word8ToWord# v2), wordToWord8# (word8ToWord# u3 `and#` word8ToWord# v3), wordToWord8# (word8ToWord# u4 `and#` word8ToWord# v4), wordToWord8# (word8ToWord# u5 `and#` word8ToWord# v5), wordToWord8# (word8ToWord# u6 `and#` word8ToWord# v6), wordToWord8# (word8ToWord# u7 `and#` word8ToWord# v7), wordToWord8# (word8ToWord# u8 `and#` word8ToWord# v8), wordToWord8# (word8ToWord# u9 `and#` word8ToWord# v9), wordToWord8# (word8ToWord# u10 `and#` word8ToWord# v10), wordToWord8# (word8ToWord# u11 `and#` word8ToWord# v11), wordToWord8# (word8ToWord# u12 `and#` word8ToWord# v12), wordToWord8# (word8ToWord# u13 `and#` word8ToWord# v13), wordToWord8# (word8ToWord# u14 `and#` word8ToWord# v14), wordToWord8# (word8ToWord# u15 `and#` word8ToWord# v15), wordToWord8# (word8ToWord# u16 `and#` word8ToWord# v16), wordToWord8# (word8ToWord# u17 `and#` word8ToWord# v17), wordToWord8# (word8ToWord# u18 `and#` word8ToWord# v18), wordToWord8# (word8ToWord# u19 `and#` word8ToWord# v19), wordToWord8# (word8ToWord# u20 `and#` word8ToWord# v20), wordToWord8# (word8ToWord# u21 `and#` word8ToWord# v21), wordToWord8# (word8ToWord# u22 `and#` word8ToWord# v22), wordToWord8# (word8ToWord# u23 `and#` word8ToWord# v23), wordToWord8# (word8ToWord# u24 `and#` word8ToWord# v24), wordToWord8# (word8ToWord# u25 `and#` word8ToWord# v25), wordToWord8# (word8ToWord# u26 `and#` word8ToWord# v26), wordToWord8# (word8ToWord# u27 `and#` word8ToWord# v27), wordToWord8# (word8ToWord# u28 `and#` word8ToWord# v28), wordToWord8# (word8ToWord# u29 `and#` word8ToWord# v29), wordToWord8# (word8ToWord# u30 `and#` word8ToWord# v30), wordToWord8# (word8ToWord# u31 `and#` word8ToWord# v31) #)
{-# INLINE [0] andWord8X32# #-}

andWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#
andWord16X16# u v = case unpackWord16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord16X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord16X16# (# wordToWord16# (word16ToWord# u0 `and#` word16ToWord# v0), wordToWord16# (word16ToWord# u1 `and#` word16ToWord# v1), wordToWord16# (word16ToWord# u2 `and#` word16ToWord# v2), wordToWord16# (word16ToWord# u3 `and#` word16ToWord# v3), wordToWord16# (word16ToWord# u4 `and#` word16ToWord# v4), wordToWord16# (word16ToWord# u5 `and#` word16ToWord# v5), wordToWord16# (word16ToWord# u6 `and#` word16ToWord# v6), wordToWord16# (word16ToWord# u7 `and#` word16ToWord# v7), wordToWord16# (word16ToWord# u8 `and#` word16ToWord# v8), wordToWord16# (word16ToWord# u9 `and#` word16ToWord# v9), wordToWord16# (word16ToWord# u10 `and#` word16ToWord# v10), wordToWord16# (word16ToWord# u11 `and#` word16ToWord# v11), wordToWord16# (word16ToWord# u12 `and#` word16ToWord# v12), wordToWord16# (word16ToWord# u13 `and#` word16ToWord# v13), wordToWord16# (word16ToWord# u14 `and#` word16ToWord# v14), wordToWord16# (word16ToWord# u15 `and#` word16ToWord# v15) #)
{-# INLINE [0] andWord16X16# #-}

andWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#
andWord32X8# u v = case unpackWord32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord32X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord32X8# (# wordToWord32# (word32ToWord# u0 `and#` word32ToWord# v0), wordToWord32# (word32ToWord# u1 `and#` word32ToWord# v1), wordToWord32# (word32ToWord# u2 `and#` word32ToWord# v2), wordToWord32# (word32ToWord# u3 `and#` word32ToWord# v3), wordToWord32# (word32ToWord# u4 `and#` word32ToWord# v4), wordToWord32# (word32ToWord# u5 `and#` word32ToWord# v5), wordToWord32# (word32ToWord# u6 `and#` word32ToWord# v6), wordToWord32# (word32ToWord# u7 `and#` word32ToWord# v7) #)
{-# INLINE [0] andWord32X8# #-}

andWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
andWord64X4# u v = case unpackWord64X4# u of (# u0, u1, u2, u3 #) -> case unpackWord64X4# v of (# v0, v1, v2, v3 #) -> packWord64X4# (# and64# u0 v0, and64# u1 v1, and64# u2 v2, and64# u3 v3 #)
{-# INLINE [0] andWord64X4# #-}

orInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#
orInt8X32# u v = case unpackInt8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackInt8X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packInt8X32# (# intToInt8# (int8ToInt# u0 `orI#` int8ToInt# v0), intToInt8# (int8ToInt# u1 `orI#` int8ToInt# v1), intToInt8# (int8ToInt# u2 `orI#` int8ToInt# v2), intToInt8# (int8ToInt# u3 `orI#` int8ToInt# v3), intToInt8# (int8ToInt# u4 `orI#` int8ToInt# v4), intToInt8# (int8ToInt# u5 `orI#` int8ToInt# v5), intToInt8# (int8ToInt# u6 `orI#` int8ToInt# v6), intToInt8# (int8ToInt# u7 `orI#` int8ToInt# v7), intToInt8# (int8ToInt# u8 `orI#` int8ToInt# v8), intToInt8# (int8ToInt# u9 `orI#` int8ToInt# v9), intToInt8# (int8ToInt# u10 `orI#` int8ToInt# v10), intToInt8# (int8ToInt# u11 `orI#` int8ToInt# v11), intToInt8# (int8ToInt# u12 `orI#` int8ToInt# v12), intToInt8# (int8ToInt# u13 `orI#` int8ToInt# v13), intToInt8# (int8ToInt# u14 `orI#` int8ToInt# v14), intToInt8# (int8ToInt# u15 `orI#` int8ToInt# v15), intToInt8# (int8ToInt# u16 `orI#` int8ToInt# v16), intToInt8# (int8ToInt# u17 `orI#` int8ToInt# v17), intToInt8# (int8ToInt# u18 `orI#` int8ToInt# v18), intToInt8# (int8ToInt# u19 `orI#` int8ToInt# v19), intToInt8# (int8ToInt# u20 `orI#` int8ToInt# v20), intToInt8# (int8ToInt# u21 `orI#` int8ToInt# v21), intToInt8# (int8ToInt# u22 `orI#` int8ToInt# v22), intToInt8# (int8ToInt# u23 `orI#` int8ToInt# v23), intToInt8# (int8ToInt# u24 `orI#` int8ToInt# v24), intToInt8# (int8ToInt# u25 `orI#` int8ToInt# v25), intToInt8# (int8ToInt# u26 `orI#` int8ToInt# v26), intToInt8# (int8ToInt# u27 `orI#` int8ToInt# v27), intToInt8# (int8ToInt# u28 `orI#` int8ToInt# v28), intToInt8# (int8ToInt# u29 `orI#` int8ToInt# v29), intToInt8# (int8ToInt# u30 `orI#` int8ToInt# v30), intToInt8# (int8ToInt# u31 `orI#` int8ToInt# v31) #)
{-# INLINE [0] orInt8X32# #-}

orInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#
orInt16X16# u v = case unpackInt16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt16X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt16X16# (# intToInt16# (int16ToInt# u0 `orI#` int16ToInt# v0), intToInt16# (int16ToInt# u1 `orI#` int16ToInt# v1), intToInt16# (int16ToInt# u2 `orI#` int16ToInt# v2), intToInt16# (int16ToInt# u3 `orI#` int16ToInt# v3), intToInt16# (int16ToInt# u4 `orI#` int16ToInt# v4), intToInt16# (int16ToInt# u5 `orI#` int16ToInt# v5), intToInt16# (int16ToInt# u6 `orI#` int16ToInt# v6), intToInt16# (int16ToInt# u7 `orI#` int16ToInt# v7), intToInt16# (int16ToInt# u8 `orI#` int16ToInt# v8), intToInt16# (int16ToInt# u9 `orI#` int16ToInt# v9), intToInt16# (int16ToInt# u10 `orI#` int16ToInt# v10), intToInt16# (int16ToInt# u11 `orI#` int16ToInt# v11), intToInt16# (int16ToInt# u12 `orI#` int16ToInt# v12), intToInt16# (int16ToInt# u13 `orI#` int16ToInt# v13), intToInt16# (int16ToInt# u14 `orI#` int16ToInt# v14), intToInt16# (int16ToInt# u15 `orI#` int16ToInt# v15) #)
{-# INLINE [0] orInt16X16# #-}

orInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#
orInt32X8# u v = case unpackInt32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt32X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt32X8# (# intToInt32# (int32ToInt# u0 `orI#` int32ToInt# v0), intToInt32# (int32ToInt# u1 `orI#` int32ToInt# v1), intToInt32# (int32ToInt# u2 `orI#` int32ToInt# v2), intToInt32# (int32ToInt# u3 `orI#` int32ToInt# v3), intToInt32# (int32ToInt# u4 `orI#` int32ToInt# v4), intToInt32# (int32ToInt# u5 `orI#` int32ToInt# v5), intToInt32# (int32ToInt# u6 `orI#` int32ToInt# v6), intToInt32# (int32ToInt# u7 `orI#` int32ToInt# v7) #)
{-# INLINE [0] orInt32X8# #-}

orInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#
orInt64X4# u v = case unpackInt64X4# u of (# u0, u1, u2, u3 #) -> case unpackInt64X4# v of (# v0, v1, v2, v3 #) -> packInt64X4# (# word64ToInt64# (int64ToWord64# u0 `or64#` int64ToWord64# v0), word64ToInt64# (int64ToWord64# u1 `or64#` int64ToWord64# v1), word64ToInt64# (int64ToWord64# u2 `or64#` int64ToWord64# v2), word64ToInt64# (int64ToWord64# u3 `or64#` int64ToWord64# v3) #)
{-# INLINE [0] orInt64X4# #-}

orWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#
orWord8X32# u v = case unpackWord8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackWord8X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packWord8X32# (# wordToWord8# (word8ToWord# u0 `or#` word8ToWord# v0), wordToWord8# (word8ToWord# u1 `or#` word8ToWord# v1), wordToWord8# (word8ToWord# u2 `or#` word8ToWord# v2), wordToWord8# (word8ToWord# u3 `or#` word8ToWord# v3), wordToWord8# (word8ToWord# u4 `or#` word8ToWord# v4), wordToWord8# (word8ToWord# u5 `or#` word8ToWord# v5), wordToWord8# (word8ToWord# u6 `or#` word8ToWord# v6), wordToWord8# (word8ToWord# u7 `or#` word8ToWord# v7), wordToWord8# (word8ToWord# u8 `or#` word8ToWord# v8), wordToWord8# (word8ToWord# u9 `or#` word8ToWord# v9), wordToWord8# (word8ToWord# u10 `or#` word8ToWord# v10), wordToWord8# (word8ToWord# u11 `or#` word8ToWord# v11), wordToWord8# (word8ToWord# u12 `or#` word8ToWord# v12), wordToWord8# (word8ToWord# u13 `or#` word8ToWord# v13), wordToWord8# (word8ToWord# u14 `or#` word8ToWord# v14), wordToWord8# (word8ToWord# u15 `or#` word8ToWord# v15), wordToWord8# (word8ToWord# u16 `or#` word8ToWord# v16), wordToWord8# (word8ToWord# u17 `or#` word8ToWord# v17), wordToWord8# (word8ToWord# u18 `or#` word8ToWord# v18), wordToWord8# (word8ToWord# u19 `or#` word8ToWord# v19), wordToWord8# (word8ToWord# u20 `or#` word8ToWord# v20), wordToWord8# (word8ToWord# u21 `or#` word8ToWord# v21), wordToWord8# (word8ToWord# u22 `or#` word8ToWord# v22), wordToWord8# (word8ToWord# u23 `or#` word8ToWord# v23), wordToWord8# (word8ToWord# u24 `or#` word8ToWord# v24), wordToWord8# (word8ToWord# u25 `or#` word8ToWord# v25), wordToWord8# (word8ToWord# u26 `or#` word8ToWord# v26), wordToWord8# (word8ToWord# u27 `or#` word8ToWord# v27), wordToWord8# (word8ToWord# u28 `or#` word8ToWord# v28), wordToWord8# (word8ToWord# u29 `or#` word8ToWord# v29), wordToWord8# (word8ToWord# u30 `or#` word8ToWord# v30), wordToWord8# (word8ToWord# u31 `or#` word8ToWord# v31) #)
{-# INLINE [0] orWord8X32# #-}

orWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#
orWord16X16# u v = case unpackWord16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord16X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord16X16# (# wordToWord16# (word16ToWord# u0 `or#` word16ToWord# v0), wordToWord16# (word16ToWord# u1 `or#` word16ToWord# v1), wordToWord16# (word16ToWord# u2 `or#` word16ToWord# v2), wordToWord16# (word16ToWord# u3 `or#` word16ToWord# v3), wordToWord16# (word16ToWord# u4 `or#` word16ToWord# v4), wordToWord16# (word16ToWord# u5 `or#` word16ToWord# v5), wordToWord16# (word16ToWord# u6 `or#` word16ToWord# v6), wordToWord16# (word16ToWord# u7 `or#` word16ToWord# v7), wordToWord16# (word16ToWord# u8 `or#` word16ToWord# v8), wordToWord16# (word16ToWord# u9 `or#` word16ToWord# v9), wordToWord16# (word16ToWord# u10 `or#` word16ToWord# v10), wordToWord16# (word16ToWord# u11 `or#` word16ToWord# v11), wordToWord16# (word16ToWord# u12 `or#` word16ToWord# v12), wordToWord16# (word16ToWord# u13 `or#` word16ToWord# v13), wordToWord16# (word16ToWord# u14 `or#` word16ToWord# v14), wordToWord16# (word16ToWord# u15 `or#` word16ToWord# v15) #)
{-# INLINE [0] orWord16X16# #-}

orWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#
orWord32X8# u v = case unpackWord32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord32X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord32X8# (# wordToWord32# (word32ToWord# u0 `or#` word32ToWord# v0), wordToWord32# (word32ToWord# u1 `or#` word32ToWord# v1), wordToWord32# (word32ToWord# u2 `or#` word32ToWord# v2), wordToWord32# (word32ToWord# u3 `or#` word32ToWord# v3), wordToWord32# (word32ToWord# u4 `or#` word32ToWord# v4), wordToWord32# (word32ToWord# u5 `or#` word32ToWord# v5), wordToWord32# (word32ToWord# u6 `or#` word32ToWord# v6), wordToWord32# (word32ToWord# u7 `or#` word32ToWord# v7) #)
{-# INLINE [0] orWord32X8# #-}

orWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
orWord64X4# u v = case unpackWord64X4# u of (# u0, u1, u2, u3 #) -> case unpackWord64X4# v of (# v0, v1, v2, v3 #) -> packWord64X4# (# or64# u0 v0, or64# u1 v1, or64# u2 v2, or64# u3 v3 #)
{-# INLINE [0] orWord64X4# #-}

xorInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#
xorInt8X32# u v = case unpackInt8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackInt8X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packInt8X32# (# intToInt8# (int8ToInt# u0 `xorI#` int8ToInt# v0), intToInt8# (int8ToInt# u1 `xorI#` int8ToInt# v1), intToInt8# (int8ToInt# u2 `xorI#` int8ToInt# v2), intToInt8# (int8ToInt# u3 `xorI#` int8ToInt# v3), intToInt8# (int8ToInt# u4 `xorI#` int8ToInt# v4), intToInt8# (int8ToInt# u5 `xorI#` int8ToInt# v5), intToInt8# (int8ToInt# u6 `xorI#` int8ToInt# v6), intToInt8# (int8ToInt# u7 `xorI#` int8ToInt# v7), intToInt8# (int8ToInt# u8 `xorI#` int8ToInt# v8), intToInt8# (int8ToInt# u9 `xorI#` int8ToInt# v9), intToInt8# (int8ToInt# u10 `xorI#` int8ToInt# v10), intToInt8# (int8ToInt# u11 `xorI#` int8ToInt# v11), intToInt8# (int8ToInt# u12 `xorI#` int8ToInt# v12), intToInt8# (int8ToInt# u13 `xorI#` int8ToInt# v13), intToInt8# (int8ToInt# u14 `xorI#` int8ToInt# v14), intToInt8# (int8ToInt# u15 `xorI#` int8ToInt# v15), intToInt8# (int8ToInt# u16 `xorI#` int8ToInt# v16), intToInt8# (int8ToInt# u17 `xorI#` int8ToInt# v17), intToInt8# (int8ToInt# u18 `xorI#` int8ToInt# v18), intToInt8# (int8ToInt# u19 `xorI#` int8ToInt# v19), intToInt8# (int8ToInt# u20 `xorI#` int8ToInt# v20), intToInt8# (int8ToInt# u21 `xorI#` int8ToInt# v21), intToInt8# (int8ToInt# u22 `xorI#` int8ToInt# v22), intToInt8# (int8ToInt# u23 `xorI#` int8ToInt# v23), intToInt8# (int8ToInt# u24 `xorI#` int8ToInt# v24), intToInt8# (int8ToInt# u25 `xorI#` int8ToInt# v25), intToInt8# (int8ToInt# u26 `xorI#` int8ToInt# v26), intToInt8# (int8ToInt# u27 `xorI#` int8ToInt# v27), intToInt8# (int8ToInt# u28 `xorI#` int8ToInt# v28), intToInt8# (int8ToInt# u29 `xorI#` int8ToInt# v29), intToInt8# (int8ToInt# u30 `xorI#` int8ToInt# v30), intToInt8# (int8ToInt# u31 `xorI#` int8ToInt# v31) #)
{-# INLINE [0] xorInt8X32# #-}

xorInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#
xorInt16X16# u v = case unpackInt16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt16X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt16X16# (# intToInt16# (int16ToInt# u0 `xorI#` int16ToInt# v0), intToInt16# (int16ToInt# u1 `xorI#` int16ToInt# v1), intToInt16# (int16ToInt# u2 `xorI#` int16ToInt# v2), intToInt16# (int16ToInt# u3 `xorI#` int16ToInt# v3), intToInt16# (int16ToInt# u4 `xorI#` int16ToInt# v4), intToInt16# (int16ToInt# u5 `xorI#` int16ToInt# v5), intToInt16# (int16ToInt# u6 `xorI#` int16ToInt# v6), intToInt16# (int16ToInt# u7 `xorI#` int16ToInt# v7), intToInt16# (int16ToInt# u8 `xorI#` int16ToInt# v8), intToInt16# (int16ToInt# u9 `xorI#` int16ToInt# v9), intToInt16# (int16ToInt# u10 `xorI#` int16ToInt# v10), intToInt16# (int16ToInt# u11 `xorI#` int16ToInt# v11), intToInt16# (int16ToInt# u12 `xorI#` int16ToInt# v12), intToInt16# (int16ToInt# u13 `xorI#` int16ToInt# v13), intToInt16# (int16ToInt# u14 `xorI#` int16ToInt# v14), intToInt16# (int16ToInt# u15 `xorI#` int16ToInt# v15) #)
{-# INLINE [0] xorInt16X16# #-}

xorInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#
xorInt32X8# u v = case unpackInt32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt32X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt32X8# (# intToInt32# (int32ToInt# u0 `xorI#` int32ToInt# v0), intToInt32# (int32ToInt# u1 `xorI#` int32ToInt# v1), intToInt32# (int32ToInt# u2 `xorI#` int32ToInt# v2), intToInt32# (int32ToInt# u3 `xorI#` int32ToInt# v3), intToInt32# (int32ToInt# u4 `xorI#` int32ToInt# v4), intToInt32# (int32ToInt# u5 `xorI#` int32ToInt# v5), intToInt32# (int32ToInt# u6 `xorI#` int32ToInt# v6), intToInt32# (int32ToInt# u7 `xorI#` int32ToInt# v7) #)
{-# INLINE [0] xorInt32X8# #-}

xorInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#
xorInt64X4# u v = case unpackInt64X4# u of (# u0, u1, u2, u3 #) -> case unpackInt64X4# v of (# v0, v1, v2, v3 #) -> packInt64X4# (# word64ToInt64# (int64ToWord64# u0 `xor64#` int64ToWord64# v0), word64ToInt64# (int64ToWord64# u1 `xor64#` int64ToWord64# v1), word64ToInt64# (int64ToWord64# u2 `xor64#` int64ToWord64# v2), word64ToInt64# (int64ToWord64# u3 `xor64#` int64ToWord64# v3) #)
{-# INLINE [0] xorInt64X4# #-}

xorWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#
xorWord8X32# u v = case unpackWord8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackWord8X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packWord8X32# (# wordToWord8# (word8ToWord# u0 `xor#` word8ToWord# v0), wordToWord8# (word8ToWord# u1 `xor#` word8ToWord# v1), wordToWord8# (word8ToWord# u2 `xor#` word8ToWord# v2), wordToWord8# (word8ToWord# u3 `xor#` word8ToWord# v3), wordToWord8# (word8ToWord# u4 `xor#` word8ToWord# v4), wordToWord8# (word8ToWord# u5 `xor#` word8ToWord# v5), wordToWord8# (word8ToWord# u6 `xor#` word8ToWord# v6), wordToWord8# (word8ToWord# u7 `xor#` word8ToWord# v7), wordToWord8# (word8ToWord# u8 `xor#` word8ToWord# v8), wordToWord8# (word8ToWord# u9 `xor#` word8ToWord# v9), wordToWord8# (word8ToWord# u10 `xor#` word8ToWord# v10), wordToWord8# (word8ToWord# u11 `xor#` word8ToWord# v11), wordToWord8# (word8ToWord# u12 `xor#` word8ToWord# v12), wordToWord8# (word8ToWord# u13 `xor#` word8ToWord# v13), wordToWord8# (word8ToWord# u14 `xor#` word8ToWord# v14), wordToWord8# (word8ToWord# u15 `xor#` word8ToWord# v15), wordToWord8# (word8ToWord# u16 `xor#` word8ToWord# v16), wordToWord8# (word8ToWord# u17 `xor#` word8ToWord# v17), wordToWord8# (word8ToWord# u18 `xor#` word8ToWord# v18), wordToWord8# (word8ToWord# u19 `xor#` word8ToWord# v19), wordToWord8# (word8ToWord# u20 `xor#` word8ToWord# v20), wordToWord8# (word8ToWord# u21 `xor#` word8ToWord# v21), wordToWord8# (word8ToWord# u22 `xor#` word8ToWord# v22), wordToWord8# (word8ToWord# u23 `xor#` word8ToWord# v23), wordToWord8# (word8ToWord# u24 `xor#` word8ToWord# v24), wordToWord8# (word8ToWord# u25 `xor#` word8ToWord# v25), wordToWord8# (word8ToWord# u26 `xor#` word8ToWord# v26), wordToWord8# (word8ToWord# u27 `xor#` word8ToWord# v27), wordToWord8# (word8ToWord# u28 `xor#` word8ToWord# v28), wordToWord8# (word8ToWord# u29 `xor#` word8ToWord# v29), wordToWord8# (word8ToWord# u30 `xor#` word8ToWord# v30), wordToWord8# (word8ToWord# u31 `xor#` word8ToWord# v31) #)
{-# INLINE [0] xorWord8X32# #-}

xorWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#
xorWord16X16# u v = case unpackWord16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord16X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord16X16# (# wordToWord16# (word16ToWord# u0 `xor#` word16ToWord# v0), wordToWord16# (word16ToWord# u1 `xor#` word16ToWord# v1), wordToWord16# (word16ToWord# u2 `xor#` word16ToWord# v2), wordToWord16# (word16ToWord# u3 `xor#` word16ToWord# v3), wordToWord16# (word16ToWord# u4 `xor#` word16ToWord# v4), wordToWord16# (word16ToWord# u5 `xor#` word16ToWord# v5), wordToWord16# (word16ToWord# u6 `xor#` word16ToWord# v6), wordToWord16# (word16ToWord# u7 `xor#` word16ToWord# v7), wordToWord16# (word16ToWord# u8 `xor#` word16ToWord# v8), wordToWord16# (word16ToWord# u9 `xor#` word16ToWord# v9), wordToWord16# (word16ToWord# u10 `xor#` word16ToWord# v10), wordToWord16# (word16ToWord# u11 `xor#` word16ToWord# v11), wordToWord16# (word16ToWord# u12 `xor#` word16ToWord# v12), wordToWord16# (word16ToWord# u13 `xor#` word16ToWord# v13), wordToWord16# (word16ToWord# u14 `xor#` word16ToWord# v14), wordToWord16# (word16ToWord# u15 `xor#` word16ToWord# v15) #)
{-# INLINE [0] xorWord16X16# #-}

xorWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#
xorWord32X8# u v = case unpackWord32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord32X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord32X8# (# wordToWord32# (word32ToWord# u0 `xor#` word32ToWord# v0), wordToWord32# (word32ToWord# u1 `xor#` word32ToWord# v1), wordToWord32# (word32ToWord# u2 `xor#` word32ToWord# v2), wordToWord32# (word32ToWord# u3 `xor#` word32ToWord# v3), wordToWord32# (word32ToWord# u4 `xor#` word32ToWord# v4), wordToWord32# (word32ToWord# u5 `xor#` word32ToWord# v5), wordToWord32# (word32ToWord# u6 `xor#` word32ToWord# v6), wordToWord32# (word32ToWord# u7 `xor#` word32ToWord# v7) #)
{-# INLINE [0] xorWord32X8# #-}

xorWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
xorWord64X4# u v = case unpackWord64X4# u of (# u0, u1, u2, u3 #) -> case unpackWord64X4# v of (# v0, v1, v2, v3 #) -> packWord64X4# (# xor64# u0 v0, xor64# u1 v1, xor64# u2 v2, xor64# u3 v3 #)
{-# INLINE [0] xorWord64X4# #-}

#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9, 12, 0, 0)

minInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#
minInt8X32# u v = case unpackInt8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackInt8X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packInt8X32# (# case ltInt8# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt8# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt8# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt8# u3 v3 of { 0# -> v3; _ -> u3 }, case ltInt8# u4 v4 of { 0# -> v4; _ -> u4 }, case ltInt8# u5 v5 of { 0# -> v5; _ -> u5 }, case ltInt8# u6 v6 of { 0# -> v6; _ -> u6 }, case ltInt8# u7 v7 of { 0# -> v7; _ -> u7 }, case ltInt8# u8 v8 of { 0# -> v8; _ -> u8 }, case ltInt8# u9 v9 of { 0# -> v9; _ -> u9 }, case ltInt8# u10 v10 of { 0# -> v10; _ -> u10 }, case ltInt8# u11 v11 of { 0# -> v11; _ -> u11 }, case ltInt8# u12 v12 of { 0# -> v12; _ -> u12 }, case ltInt8# u13 v13 of { 0# -> v13; _ -> u13 }, case ltInt8# u14 v14 of { 0# -> v14; _ -> u14 }, case ltInt8# u15 v15 of { 0# -> v15; _ -> u15 }, case ltInt8# u16 v16 of { 0# -> v16; _ -> u16 }, case ltInt8# u17 v17 of { 0# -> v17; _ -> u17 }, case ltInt8# u18 v18 of { 0# -> v18; _ -> u18 }, case ltInt8# u19 v19 of { 0# -> v19; _ -> u19 }, case ltInt8# u20 v20 of { 0# -> v20; _ -> u20 }, case ltInt8# u21 v21 of { 0# -> v21; _ -> u21 }, case ltInt8# u22 v22 of { 0# -> v22; _ -> u22 }, case ltInt8# u23 v23 of { 0# -> v23; _ -> u23 }, case ltInt8# u24 v24 of { 0# -> v24; _ -> u24 }, case ltInt8# u25 v25 of { 0# -> v25; _ -> u25 }, case ltInt8# u26 v26 of { 0# -> v26; _ -> u26 }, case ltInt8# u27 v27 of { 0# -> v27; _ -> u27 }, case ltInt8# u28 v28 of { 0# -> v28; _ -> u28 }, case ltInt8# u29 v29 of { 0# -> v29; _ -> u29 }, case ltInt8# u30 v30 of { 0# -> v30; _ -> u30 }, case ltInt8# u31 v31 of { 0# -> v31; _ -> u31 } #)
{-# INLINE [0] minInt8X32# #-}

minInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#
minInt16X16# u v = case unpackInt16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt16X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt16X16# (# case ltInt16# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt16# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt16# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt16# u3 v3 of { 0# -> v3; _ -> u3 }, case ltInt16# u4 v4 of { 0# -> v4; _ -> u4 }, case ltInt16# u5 v5 of { 0# -> v5; _ -> u5 }, case ltInt16# u6 v6 of { 0# -> v6; _ -> u6 }, case ltInt16# u7 v7 of { 0# -> v7; _ -> u7 }, case ltInt16# u8 v8 of { 0# -> v8; _ -> u8 }, case ltInt16# u9 v9 of { 0# -> v9; _ -> u9 }, case ltInt16# u10 v10 of { 0# -> v10; _ -> u10 }, case ltInt16# u11 v11 of { 0# -> v11; _ -> u11 }, case ltInt16# u12 v12 of { 0# -> v12; _ -> u12 }, case ltInt16# u13 v13 of { 0# -> v13; _ -> u13 }, case ltInt16# u14 v14 of { 0# -> v14; _ -> u14 }, case ltInt16# u15 v15 of { 0# -> v15; _ -> u15 } #)
{-# INLINE [0] minInt16X16# #-}

minInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#
minInt32X8# u v = case unpackInt32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt32X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt32X8# (# case ltInt32# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt32# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt32# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt32# u3 v3 of { 0# -> v3; _ -> u3 }, case ltInt32# u4 v4 of { 0# -> v4; _ -> u4 }, case ltInt32# u5 v5 of { 0# -> v5; _ -> u5 }, case ltInt32# u6 v6 of { 0# -> v6; _ -> u6 }, case ltInt32# u7 v7 of { 0# -> v7; _ -> u7 } #)
{-# INLINE [0] minInt32X8# #-}

minInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#
minInt64X4# u v = case unpackInt64X4# u of (# u0, u1, u2, u3 #) -> case unpackInt64X4# v of (# v0, v1, v2, v3 #) -> packInt64X4# (# case ltInt64# u0 v0 of { 0# -> v0; _ -> u0 }, case ltInt64# u1 v1 of { 0# -> v1; _ -> u1 }, case ltInt64# u2 v2 of { 0# -> v2; _ -> u2 }, case ltInt64# u3 v3 of { 0# -> v3; _ -> u3 } #)
{-# INLINE [0] minInt64X4# #-}

minWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#
minWord8X32# u v = case unpackWord8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackWord8X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packWord8X32# (# case ltWord8# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord8# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord8# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord8# u3 v3 of { 0# -> v3; _ -> u3 }, case ltWord8# u4 v4 of { 0# -> v4; _ -> u4 }, case ltWord8# u5 v5 of { 0# -> v5; _ -> u5 }, case ltWord8# u6 v6 of { 0# -> v6; _ -> u6 }, case ltWord8# u7 v7 of { 0# -> v7; _ -> u7 }, case ltWord8# u8 v8 of { 0# -> v8; _ -> u8 }, case ltWord8# u9 v9 of { 0# -> v9; _ -> u9 }, case ltWord8# u10 v10 of { 0# -> v10; _ -> u10 }, case ltWord8# u11 v11 of { 0# -> v11; _ -> u11 }, case ltWord8# u12 v12 of { 0# -> v12; _ -> u12 }, case ltWord8# u13 v13 of { 0# -> v13; _ -> u13 }, case ltWord8# u14 v14 of { 0# -> v14; _ -> u14 }, case ltWord8# u15 v15 of { 0# -> v15; _ -> u15 }, case ltWord8# u16 v16 of { 0# -> v16; _ -> u16 }, case ltWord8# u17 v17 of { 0# -> v17; _ -> u17 }, case ltWord8# u18 v18 of { 0# -> v18; _ -> u18 }, case ltWord8# u19 v19 of { 0# -> v19; _ -> u19 }, case ltWord8# u20 v20 of { 0# -> v20; _ -> u20 }, case ltWord8# u21 v21 of { 0# -> v21; _ -> u21 }, case ltWord8# u22 v22 of { 0# -> v22; _ -> u22 }, case ltWord8# u23 v23 of { 0# -> v23; _ -> u23 }, case ltWord8# u24 v24 of { 0# -> v24; _ -> u24 }, case ltWord8# u25 v25 of { 0# -> v25; _ -> u25 }, case ltWord8# u26 v26 of { 0# -> v26; _ -> u26 }, case ltWord8# u27 v27 of { 0# -> v27; _ -> u27 }, case ltWord8# u28 v28 of { 0# -> v28; _ -> u28 }, case ltWord8# u29 v29 of { 0# -> v29; _ -> u29 }, case ltWord8# u30 v30 of { 0# -> v30; _ -> u30 }, case ltWord8# u31 v31 of { 0# -> v31; _ -> u31 } #)
{-# INLINE [0] minWord8X32# #-}

minWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#
minWord16X16# u v = case unpackWord16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord16X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord16X16# (# case ltWord16# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord16# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord16# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord16# u3 v3 of { 0# -> v3; _ -> u3 }, case ltWord16# u4 v4 of { 0# -> v4; _ -> u4 }, case ltWord16# u5 v5 of { 0# -> v5; _ -> u5 }, case ltWord16# u6 v6 of { 0# -> v6; _ -> u6 }, case ltWord16# u7 v7 of { 0# -> v7; _ -> u7 }, case ltWord16# u8 v8 of { 0# -> v8; _ -> u8 }, case ltWord16# u9 v9 of { 0# -> v9; _ -> u9 }, case ltWord16# u10 v10 of { 0# -> v10; _ -> u10 }, case ltWord16# u11 v11 of { 0# -> v11; _ -> u11 }, case ltWord16# u12 v12 of { 0# -> v12; _ -> u12 }, case ltWord16# u13 v13 of { 0# -> v13; _ -> u13 }, case ltWord16# u14 v14 of { 0# -> v14; _ -> u14 }, case ltWord16# u15 v15 of { 0# -> v15; _ -> u15 } #)
{-# INLINE [0] minWord16X16# #-}

minWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#
minWord32X8# u v = case unpackWord32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord32X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord32X8# (# case ltWord32# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord32# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord32# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord32# u3 v3 of { 0# -> v3; _ -> u3 }, case ltWord32# u4 v4 of { 0# -> v4; _ -> u4 }, case ltWord32# u5 v5 of { 0# -> v5; _ -> u5 }, case ltWord32# u6 v6 of { 0# -> v6; _ -> u6 }, case ltWord32# u7 v7 of { 0# -> v7; _ -> u7 } #)
{-# INLINE [0] minWord32X8# #-}

minWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
minWord64X4# u v = case unpackWord64X4# u of (# u0, u1, u2, u3 #) -> case unpackWord64X4# v of (# v0, v1, v2, v3 #) -> packWord64X4# (# case ltWord64# u0 v0 of { 0# -> v0; _ -> u0 }, case ltWord64# u1 v1 of { 0# -> v1; _ -> u1 }, case ltWord64# u2 v2 of { 0# -> v2; _ -> u2 }, case ltWord64# u3 v3 of { 0# -> v3; _ -> u3 } #)
{-# INLINE [0] minWord64X4# #-}

maxInt8X32# :: Int8X32# -> Int8X32# -> Int8X32#
maxInt8X32# u v = case unpackInt8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackInt8X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packInt8X32# (# case ltInt8# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt8# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt8# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt8# u3 v3 of { 0# -> u3; _ -> v3 }, case ltInt8# u4 v4 of { 0# -> u4; _ -> v4 }, case ltInt8# u5 v5 of { 0# -> u5; _ -> v5 }, case ltInt8# u6 v6 of { 0# -> u6; _ -> v6 }, case ltInt8# u7 v7 of { 0# -> u7; _ -> v7 }, case ltInt8# u8 v8 of { 0# -> u8; _ -> v8 }, case ltInt8# u9 v9 of { 0# -> u9; _ -> v9 }, case ltInt8# u10 v10 of { 0# -> u10; _ -> v10 }, case ltInt8# u11 v11 of { 0# -> u11; _ -> v11 }, case ltInt8# u12 v12 of { 0# -> u12; _ -> v12 }, case ltInt8# u13 v13 of { 0# -> u13; _ -> v13 }, case ltInt8# u14 v14 of { 0# -> u14; _ -> v14 }, case ltInt8# u15 v15 of { 0# -> u15; _ -> v15 }, case ltInt8# u16 v16 of { 0# -> u16; _ -> v16 }, case ltInt8# u17 v17 of { 0# -> u17; _ -> v17 }, case ltInt8# u18 v18 of { 0# -> u18; _ -> v18 }, case ltInt8# u19 v19 of { 0# -> u19; _ -> v19 }, case ltInt8# u20 v20 of { 0# -> u20; _ -> v20 }, case ltInt8# u21 v21 of { 0# -> u21; _ -> v21 }, case ltInt8# u22 v22 of { 0# -> u22; _ -> v22 }, case ltInt8# u23 v23 of { 0# -> u23; _ -> v23 }, case ltInt8# u24 v24 of { 0# -> u24; _ -> v24 }, case ltInt8# u25 v25 of { 0# -> u25; _ -> v25 }, case ltInt8# u26 v26 of { 0# -> u26; _ -> v26 }, case ltInt8# u27 v27 of { 0# -> u27; _ -> v27 }, case ltInt8# u28 v28 of { 0# -> u28; _ -> v28 }, case ltInt8# u29 v29 of { 0# -> u29; _ -> v29 }, case ltInt8# u30 v30 of { 0# -> u30; _ -> v30 }, case ltInt8# u31 v31 of { 0# -> u31; _ -> v31 } #)
{-# INLINE [0] maxInt8X32# #-}

maxInt16X16# :: Int16X16# -> Int16X16# -> Int16X16#
maxInt16X16# u v = case unpackInt16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackInt16X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packInt16X16# (# case ltInt16# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt16# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt16# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt16# u3 v3 of { 0# -> u3; _ -> v3 }, case ltInt16# u4 v4 of { 0# -> u4; _ -> v4 }, case ltInt16# u5 v5 of { 0# -> u5; _ -> v5 }, case ltInt16# u6 v6 of { 0# -> u6; _ -> v6 }, case ltInt16# u7 v7 of { 0# -> u7; _ -> v7 }, case ltInt16# u8 v8 of { 0# -> u8; _ -> v8 }, case ltInt16# u9 v9 of { 0# -> u9; _ -> v9 }, case ltInt16# u10 v10 of { 0# -> u10; _ -> v10 }, case ltInt16# u11 v11 of { 0# -> u11; _ -> v11 }, case ltInt16# u12 v12 of { 0# -> u12; _ -> v12 }, case ltInt16# u13 v13 of { 0# -> u13; _ -> v13 }, case ltInt16# u14 v14 of { 0# -> u14; _ -> v14 }, case ltInt16# u15 v15 of { 0# -> u15; _ -> v15 } #)
{-# INLINE [0] maxInt16X16# #-}

maxInt32X8# :: Int32X8# -> Int32X8# -> Int32X8#
maxInt32X8# u v = case unpackInt32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackInt32X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packInt32X8# (# case ltInt32# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt32# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt32# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt32# u3 v3 of { 0# -> u3; _ -> v3 }, case ltInt32# u4 v4 of { 0# -> u4; _ -> v4 }, case ltInt32# u5 v5 of { 0# -> u5; _ -> v5 }, case ltInt32# u6 v6 of { 0# -> u6; _ -> v6 }, case ltInt32# u7 v7 of { 0# -> u7; _ -> v7 } #)
{-# INLINE [0] maxInt32X8# #-}

maxInt64X4# :: Int64X4# -> Int64X4# -> Int64X4#
maxInt64X4# u v = case unpackInt64X4# u of (# u0, u1, u2, u3 #) -> case unpackInt64X4# v of (# v0, v1, v2, v3 #) -> packInt64X4# (# case ltInt64# u0 v0 of { 0# -> u0; _ -> v0 }, case ltInt64# u1 v1 of { 0# -> u1; _ -> v1 }, case ltInt64# u2 v2 of { 0# -> u2; _ -> v2 }, case ltInt64# u3 v3 of { 0# -> u3; _ -> v3 } #)
{-# INLINE [0] maxInt64X4# #-}

maxWord8X32# :: Word8X32# -> Word8X32# -> Word8X32#
maxWord8X32# u v = case unpackWord8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> case unpackWord8X32# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v30, v31 #) -> packWord8X32# (# case ltWord8# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord8# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord8# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord8# u3 v3 of { 0# -> u3; _ -> v3 }, case ltWord8# u4 v4 of { 0# -> u4; _ -> v4 }, case ltWord8# u5 v5 of { 0# -> u5; _ -> v5 }, case ltWord8# u6 v6 of { 0# -> u6; _ -> v6 }, case ltWord8# u7 v7 of { 0# -> u7; _ -> v7 }, case ltWord8# u8 v8 of { 0# -> u8; _ -> v8 }, case ltWord8# u9 v9 of { 0# -> u9; _ -> v9 }, case ltWord8# u10 v10 of { 0# -> u10; _ -> v10 }, case ltWord8# u11 v11 of { 0# -> u11; _ -> v11 }, case ltWord8# u12 v12 of { 0# -> u12; _ -> v12 }, case ltWord8# u13 v13 of { 0# -> u13; _ -> v13 }, case ltWord8# u14 v14 of { 0# -> u14; _ -> v14 }, case ltWord8# u15 v15 of { 0# -> u15; _ -> v15 }, case ltWord8# u16 v16 of { 0# -> u16; _ -> v16 }, case ltWord8# u17 v17 of { 0# -> u17; _ -> v17 }, case ltWord8# u18 v18 of { 0# -> u18; _ -> v18 }, case ltWord8# u19 v19 of { 0# -> u19; _ -> v19 }, case ltWord8# u20 v20 of { 0# -> u20; _ -> v20 }, case ltWord8# u21 v21 of { 0# -> u21; _ -> v21 }, case ltWord8# u22 v22 of { 0# -> u22; _ -> v22 }, case ltWord8# u23 v23 of { 0# -> u23; _ -> v23 }, case ltWord8# u24 v24 of { 0# -> u24; _ -> v24 }, case ltWord8# u25 v25 of { 0# -> u25; _ -> v25 }, case ltWord8# u26 v26 of { 0# -> u26; _ -> v26 }, case ltWord8# u27 v27 of { 0# -> u27; _ -> v27 }, case ltWord8# u28 v28 of { 0# -> u28; _ -> v28 }, case ltWord8# u29 v29 of { 0# -> u29; _ -> v29 }, case ltWord8# u30 v30 of { 0# -> u30; _ -> v30 }, case ltWord8# u31 v31 of { 0# -> u31; _ -> v31 } #)
{-# INLINE [0] maxWord8X32# #-}

maxWord16X16# :: Word16X16# -> Word16X16# -> Word16X16#
maxWord16X16# u v = case unpackWord16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> case unpackWord16X16# v of (# v0, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15 #) -> packWord16X16# (# case ltWord16# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord16# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord16# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord16# u3 v3 of { 0# -> u3; _ -> v3 }, case ltWord16# u4 v4 of { 0# -> u4; _ -> v4 }, case ltWord16# u5 v5 of { 0# -> u5; _ -> v5 }, case ltWord16# u6 v6 of { 0# -> u6; _ -> v6 }, case ltWord16# u7 v7 of { 0# -> u7; _ -> v7 }, case ltWord16# u8 v8 of { 0# -> u8; _ -> v8 }, case ltWord16# u9 v9 of { 0# -> u9; _ -> v9 }, case ltWord16# u10 v10 of { 0# -> u10; _ -> v10 }, case ltWord16# u11 v11 of { 0# -> u11; _ -> v11 }, case ltWord16# u12 v12 of { 0# -> u12; _ -> v12 }, case ltWord16# u13 v13 of { 0# -> u13; _ -> v13 }, case ltWord16# u14 v14 of { 0# -> u14; _ -> v14 }, case ltWord16# u15 v15 of { 0# -> u15; _ -> v15 } #)
{-# INLINE [0] maxWord16X16# #-}

maxWord32X8# :: Word32X8# -> Word32X8# -> Word32X8#
maxWord32X8# u v = case unpackWord32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> case unpackWord32X8# v of (# v0, v1, v2, v3, v4, v5, v6, v7 #) -> packWord32X8# (# case ltWord32# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord32# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord32# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord32# u3 v3 of { 0# -> u3; _ -> v3 }, case ltWord32# u4 v4 of { 0# -> u4; _ -> v4 }, case ltWord32# u5 v5 of { 0# -> u5; _ -> v5 }, case ltWord32# u6 v6 of { 0# -> u6; _ -> v6 }, case ltWord32# u7 v7 of { 0# -> u7; _ -> v7 } #)
{-# INLINE [0] maxWord32X8# #-}

maxWord64X4# :: Word64X4# -> Word64X4# -> Word64X4#
maxWord64X4# u v = case unpackWord64X4# u of (# u0, u1, u2, u3 #) -> case unpackWord64X4# v of (# v0, v1, v2, v3 #) -> packWord64X4# (# case ltWord64# u0 v0 of { 0# -> u0; _ -> v0 }, case ltWord64# u1 v1 of { 0# -> u1; _ -> v1 }, case ltWord64# u2 v2 of { 0# -> u2; _ -> v2 }, case ltWord64# u3 v3 of { 0# -> u3; _ -> v3 } #)
{-# INLINE [0] maxWord64X4# #-}

#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)

absInt8X32# :: Int8X32# -> Int8X32#
absInt8X32# u = case unpackInt8X32# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15, u16, u17, u18, u19, u20, u21, u22, u23, u24, u25, u26, u27, u28, u29, u30, u31 #) -> packInt8X32# (# case ltInt8# u0 0#Int8 of { 0# -> u0; _ -> negateInt8# u0 }, case ltInt8# u1 0#Int8 of { 0# -> u1; _ -> negateInt8# u1 }, case ltInt8# u2 0#Int8 of { 0# -> u2; _ -> negateInt8# u2 }, case ltInt8# u3 0#Int8 of { 0# -> u3; _ -> negateInt8# u3 }, case ltInt8# u4 0#Int8 of { 0# -> u4; _ -> negateInt8# u4 }, case ltInt8# u5 0#Int8 of { 0# -> u5; _ -> negateInt8# u5 }, case ltInt8# u6 0#Int8 of { 0# -> u6; _ -> negateInt8# u6 }, case ltInt8# u7 0#Int8 of { 0# -> u7; _ -> negateInt8# u7 }, case ltInt8# u8 0#Int8 of { 0# -> u8; _ -> negateInt8# u8 }, case ltInt8# u9 0#Int8 of { 0# -> u9; _ -> negateInt8# u9 }, case ltInt8# u10 0#Int8 of { 0# -> u10; _ -> negateInt8# u10 }, case ltInt8# u11 0#Int8 of { 0# -> u11; _ -> negateInt8# u11 }, case ltInt8# u12 0#Int8 of { 0# -> u12; _ -> negateInt8# u12 }, case ltInt8# u13 0#Int8 of { 0# -> u13; _ -> negateInt8# u13 }, case ltInt8# u14 0#Int8 of { 0# -> u14; _ -> negateInt8# u14 }, case ltInt8# u15 0#Int8 of { 0# -> u15; _ -> negateInt8# u15 }, case ltInt8# u16 0#Int8 of { 0# -> u16; _ -> negateInt8# u16 }, case ltInt8# u17 0#Int8 of { 0# -> u17; _ -> negateInt8# u17 }, case ltInt8# u18 0#Int8 of { 0# -> u18; _ -> negateInt8# u18 }, case ltInt8# u19 0#Int8 of { 0# -> u19; _ -> negateInt8# u19 }, case ltInt8# u20 0#Int8 of { 0# -> u20; _ -> negateInt8# u20 }, case ltInt8# u21 0#Int8 of { 0# -> u21; _ -> negateInt8# u21 }, case ltInt8# u22 0#Int8 of { 0# -> u22; _ -> negateInt8# u22 }, case ltInt8# u23 0#Int8 of { 0# -> u23; _ -> negateInt8# u23 }, case ltInt8# u24 0#Int8 of { 0# -> u24; _ -> negateInt8# u24 }, case ltInt8# u25 0#Int8 of { 0# -> u25; _ -> negateInt8# u25 }, case ltInt8# u26 0#Int8 of { 0# -> u26; _ -> negateInt8# u26 }, case ltInt8# u27 0#Int8 of { 0# -> u27; _ -> negateInt8# u27 }, case ltInt8# u28 0#Int8 of { 0# -> u28; _ -> negateInt8# u28 }, case ltInt8# u29 0#Int8 of { 0# -> u29; _ -> negateInt8# u29 }, case ltInt8# u30 0#Int8 of { 0# -> u30; _ -> negateInt8# u30 }, case ltInt8# u31 0#Int8 of { 0# -> u31; _ -> negateInt8# u31 } #)
{-# INLINE [0] absInt8X32# #-}

absInt16X16# :: Int16X16# -> Int16X16#
absInt16X16# u = case unpackInt16X16# u of (# u0, u1, u2, u3, u4, u5, u6, u7, u8, u9, u10, u11, u12, u13, u14, u15 #) -> packInt16X16# (# case ltInt16# u0 0#Int16 of { 0# -> u0; _ -> negateInt16# u0 }, case ltInt16# u1 0#Int16 of { 0# -> u1; _ -> negateInt16# u1 }, case ltInt16# u2 0#Int16 of { 0# -> u2; _ -> negateInt16# u2 }, case ltInt16# u3 0#Int16 of { 0# -> u3; _ -> negateInt16# u3 }, case ltInt16# u4 0#Int16 of { 0# -> u4; _ -> negateInt16# u4 }, case ltInt16# u5 0#Int16 of { 0# -> u5; _ -> negateInt16# u5 }, case ltInt16# u6 0#Int16 of { 0# -> u6; _ -> negateInt16# u6 }, case ltInt16# u7 0#Int16 of { 0# -> u7; _ -> negateInt16# u7 }, case ltInt16# u8 0#Int16 of { 0# -> u8; _ -> negateInt16# u8 }, case ltInt16# u9 0#Int16 of { 0# -> u9; _ -> negateInt16# u9 }, case ltInt16# u10 0#Int16 of { 0# -> u10; _ -> negateInt16# u10 }, case ltInt16# u11 0#Int16 of { 0# -> u11; _ -> negateInt16# u11 }, case ltInt16# u12 0#Int16 of { 0# -> u12; _ -> negateInt16# u12 }, case ltInt16# u13 0#Int16 of { 0# -> u13; _ -> negateInt16# u13 }, case ltInt16# u14 0#Int16 of { 0# -> u14; _ -> negateInt16# u14 }, case ltInt16# u15 0#Int16 of { 0# -> u15; _ -> negateInt16# u15 } #)
{-# INLINE [0] absInt16X16# #-}

absInt32X8# :: Int32X8# -> Int32X8#
absInt32X8# u = case unpackInt32X8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packInt32X8# (# case ltInt32# u0 0#Int32 of { 0# -> u0; _ -> negateInt32# u0 }, case ltInt32# u1 0#Int32 of { 0# -> u1; _ -> negateInt32# u1 }, case ltInt32# u2 0#Int32 of { 0# -> u2; _ -> negateInt32# u2 }, case ltInt32# u3 0#Int32 of { 0# -> u3; _ -> negateInt32# u3 }, case ltInt32# u4 0#Int32 of { 0# -> u4; _ -> negateInt32# u4 }, case ltInt32# u5 0#Int32 of { 0# -> u5; _ -> negateInt32# u5 }, case ltInt32# u6 0#Int32 of { 0# -> u6; _ -> negateInt32# u6 }, case ltInt32# u7 0#Int32 of { 0# -> u7; _ -> negateInt32# u7 } #)
{-# INLINE [0] absInt32X8# #-}

absInt64X4# :: Int64X4# -> Int64X4#
absInt64X4# u = case unpackInt64X4# u of (# u0, u1, u2, u3 #) -> packInt64X4# (# case ltInt64# u0 0#Int64 of { 0# -> u0; _ -> negateInt64# u0 }, case ltInt64# u1 0#Int64 of { 0# -> u1; _ -> negateInt64# u1 }, case ltInt64# u2 0#Int64 of { 0# -> u2; _ -> negateInt64# u2 }, case ltInt64# u3 0#Int64 of { 0# -> u3; _ -> negateInt64# u3 } #)
{-# INLINE [0] absInt64X4# #-}

#endif

#if !MIN_VERSION_GLASGOW_HASKELL(9, 15, 0, 0)

absFloatX8# :: FloatX8# -> FloatX8#
absFloatX8# u = case unpackFloatX8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packFloatX8# (# fabsFloat# u0, fabsFloat# u1, fabsFloat# u2, fabsFloat# u3, fabsFloat# u4, fabsFloat# u5, fabsFloat# u6, fabsFloat# u7 #)
{-# INLINE [0] absFloatX8# #-}

absDoubleX4# :: DoubleX4# -> DoubleX4#
absDoubleX4# u = case unpackDoubleX4# u of (# u0, u1, u2, u3 #) -> packDoubleX4# (# fabsDouble# u0, fabsDouble# u1, fabsDouble# u2, fabsDouble# u3 #)
{-# INLINE [0] absDoubleX4# #-}

sqrtFloatX8# :: FloatX8# -> FloatX8#
sqrtFloatX8# u = case unpackFloatX8# u of (# u0, u1, u2, u3, u4, u5, u6, u7 #) -> packFloatX8# (# sqrtFloat# u0, sqrtFloat# u1, sqrtFloat# u2, sqrtFloat# u3, sqrtFloat# u4, sqrtFloat# u5, sqrtFloat# u6, sqrtFloat# u7 #)
{-# INLINE [0] sqrtFloatX8# #-}

sqrtDoubleX4# :: DoubleX4# -> DoubleX4#
sqrtDoubleX4# u = case unpackDoubleX4# u of (# u0, u1, u2, u3 #) -> packDoubleX4# (# sqrtDouble# u0, sqrtDouble# u1, sqrtDouble# u2, sqrtDouble# u3 #)
{-# INLINE [0] sqrtDoubleX4# #-}

#endif
