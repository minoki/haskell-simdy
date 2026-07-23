-- This file was created by script/Gen.hs. Do not edit by hand!
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Simdy.Internal.NoSIMD.HalfVector where
import           Data.Bits
import           Data.Complex
import           Data.Monoid
import           Data.Semigroup
import           Data.Simdy.Internal.Class
import           GHC.Exts
import           GHC.Int
import           GHC.Word
import           Data.Functor.Identity
import           Data.Simdy.Internal.NoSIMD.X2
import           Data.Simdy.Internal.NoSIMD.X4
import           Data.Simdy.Internal.NoSIMD.X8
import           Data.Simdy.Internal.NoSIMD.X16
import           Data.Simdy.Internal.NoSIMD.X32
import           Data.Simdy.Internal.NoSIMD.X64
type instance HalfVector X2 = Identity
instance SplitShortVector X2 Bool where
  splitShortVector (MkBoolX2 !x) = (Identity (testBit x 0), Identity (testBit x 1))
  joinShortVector (Identity !x) (Identity !y) = MkBoolX2 ((if x then 1 else 0) .|. (if y then 2 else 0))
instance SplitShortVector X2 Float where
  splitShortVector (MkFloatX2WithElems x0 x1) = (Identity x0, Identity x1)
  joinShortVector (Identity x0) (Identity x1) = MkFloatX2WithElems x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Double where
  splitShortVector (MkDoubleX2WithElems x0 x1) = (Identity x0, Identity x1)
  joinShortVector (Identity x0) (Identity x1) = MkDoubleX2WithElems x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Int8 where
  splitShortVector (MkInt8X2WithElems x0 x1) = (Identity x0, Identity x1)
  joinShortVector (Identity x0) (Identity x1) = MkInt8X2WithElems x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Int16 where
  splitShortVector (MkInt16X2WithElems x0 x1) = (Identity x0, Identity x1)
  joinShortVector (Identity x0) (Identity x1) = MkInt16X2WithElems x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Int32 where
  splitShortVector (MkInt32X2WithElems x0 x1) = (Identity x0, Identity x1)
  joinShortVector (Identity x0) (Identity x1) = MkInt32X2WithElems x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Int64 where
  splitShortVector (MkInt64X2WithElems x0 x1) = (Identity x0, Identity x1)
  joinShortVector (Identity x0) (Identity x1) = MkInt64X2WithElems x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Word8 where
  splitShortVector (MkWord8X2WithElems x0 x1) = (Identity x0, Identity x1)
  joinShortVector (Identity x0) (Identity x1) = MkWord8X2WithElems x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Word16 where
  splitShortVector (MkWord16X2WithElems x0 x1) = (Identity x0, Identity x1)
  joinShortVector (Identity x0) (Identity x1) = MkWord16X2WithElems x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Word32 where
  splitShortVector (MkWord32X2WithElems x0 x1) = (Identity x0, Identity x1)
  joinShortVector (Identity x0) (Identity x1) = MkWord32X2WithElems x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Word64 where
  splitShortVector (MkWord64X2WithElems x0 x1) = (Identity x0, Identity x1)
  joinShortVector (Identity x0) (Identity x1) = MkWord64X2WithElems x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 a => SplitShortVector X2 (Sum a) where
  splitShortVector = coerce (splitShortVector @X2 @a)
  joinShortVector = coerce (joinShortVector @X2 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 a => SplitShortVector X2 (Product a) where
  splitShortVector = coerce (splitShortVector @X2 @a)
  joinShortVector = coerce (joinShortVector @X2 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 a => SplitShortVector X2 (Min a) where
  splitShortVector = coerce (splitShortVector @X2 @a)
  joinShortVector = coerce (joinShortVector @X2 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 a => SplitShortVector X2 (Max a) where
  splitShortVector = coerce (splitShortVector @X2 @a)
  joinShortVector = coerce (joinShortVector @X2 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 a => SplitShortVector X2 (Complex a) where
  splitShortVector (MkComplexX2 s t) = case splitShortVector s of (Identity x0, Identity x1) -> case splitShortVector t of (Identity y0, Identity y1) -> (Identity (x0 :+ y0), Identity (x1 :+ y1))
  joinShortVector (Identity (x0 :+ y0)) (Identity (x1 :+ y1)) = MkComplexX2 (joinShortVector (Identity x0) (Identity x1)) (joinShortVector (Identity y0) (Identity y1))
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 () where
  splitShortVector MkUnitX2 = (Identity (), Identity ())
  joinShortVector _ _ = MkUnitX2
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X2 a0, SplitShortVector X2 a1) => SplitShortVector X2 (a0, a1) where
  splitShortVector (MkTuple2X2 v0 v1) = case splitShortVector v0 of (Identity a0, Identity b0) -> case splitShortVector v1 of (Identity a1, Identity b1) -> (Identity (a0, a1), Identity (b0, b1))
  joinShortVector (Identity (a0, a1)) (Identity (b0, b1)) = MkTuple2X2 (joinShortVector (Identity a0) (Identity b0)) (joinShortVector (Identity a1) (Identity b1))
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X2 a0, SplitShortVector X2 a1, SplitShortVector X2 a2) => SplitShortVector X2 (a0, a1, a2) where
  splitShortVector (MkTuple3X2 v0 v1 v2) = case splitShortVector v0 of (Identity a0, Identity b0) -> case splitShortVector v1 of (Identity a1, Identity b1) -> case splitShortVector v2 of (Identity a2, Identity b2) -> (Identity (a0, a1, a2), Identity (b0, b1, b2))
  joinShortVector (Identity (a0, a1, a2)) (Identity (b0, b1, b2)) = MkTuple3X2 (joinShortVector (Identity a0) (Identity b0)) (joinShortVector (Identity a1) (Identity b1)) (joinShortVector (Identity a2) (Identity b2))
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X2 a0, SplitShortVector X2 a1, SplitShortVector X2 a2, SplitShortVector X2 a3) => SplitShortVector X2 (a0, a1, a2, a3) where
  splitShortVector (MkTuple4X2 v0 v1 v2 v3) = case splitShortVector v0 of (Identity a0, Identity b0) -> case splitShortVector v1 of (Identity a1, Identity b1) -> case splitShortVector v2 of (Identity a2, Identity b2) -> case splitShortVector v3 of (Identity a3, Identity b3) -> (Identity (a0, a1, a2, a3), Identity (b0, b1, b2, b3))
  joinShortVector (Identity (a0, a1, a2, a3)) (Identity (b0, b1, b2, b3)) = MkTuple4X2 (joinShortVector (Identity a0) (Identity b0)) (joinShortVector (Identity a1) (Identity b1)) (joinShortVector (Identity a2) (Identity b2)) (joinShortVector (Identity a3) (Identity b3))
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X2 a0, SplitShortVector X2 a1, SplitShortVector X2 a2, SplitShortVector X2 a3, SplitShortVector X2 a4) => SplitShortVector X2 (a0, a1, a2, a3, a4) where
  splitShortVector (MkTuple5X2 v0 v1 v2 v3 v4) = case splitShortVector v0 of (Identity a0, Identity b0) -> case splitShortVector v1 of (Identity a1, Identity b1) -> case splitShortVector v2 of (Identity a2, Identity b2) -> case splitShortVector v3 of (Identity a3, Identity b3) -> case splitShortVector v4 of (Identity a4, Identity b4) -> (Identity (a0, a1, a2, a3, a4), Identity (b0, b1, b2, b3, b4))
  joinShortVector (Identity (a0, a1, a2, a3, a4)) (Identity (b0, b1, b2, b3, b4)) = MkTuple5X2 (joinShortVector (Identity a0) (Identity b0)) (joinShortVector (Identity a1) (Identity b1)) (joinShortVector (Identity a2) (Identity b2)) (joinShortVector (Identity a3) (Identity b3)) (joinShortVector (Identity a4) (Identity b4))
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X2 a0, SplitShortVector X2 a1, SplitShortVector X2 a2, SplitShortVector X2 a3, SplitShortVector X2 a4, SplitShortVector X2 a5) => SplitShortVector X2 (a0, a1, a2, a3, a4, a5) where
  splitShortVector (MkTuple6X2 v0 v1 v2 v3 v4 v5) = case splitShortVector v0 of (Identity a0, Identity b0) -> case splitShortVector v1 of (Identity a1, Identity b1) -> case splitShortVector v2 of (Identity a2, Identity b2) -> case splitShortVector v3 of (Identity a3, Identity b3) -> case splitShortVector v4 of (Identity a4, Identity b4) -> case splitShortVector v5 of (Identity a5, Identity b5) -> (Identity (a0, a1, a2, a3, a4, a5), Identity (b0, b1, b2, b3, b4, b5))
  joinShortVector (Identity (a0, a1, a2, a3, a4, a5)) (Identity (b0, b1, b2, b3, b4, b5)) = MkTuple6X2 (joinShortVector (Identity a0) (Identity b0)) (joinShortVector (Identity a1) (Identity b1)) (joinShortVector (Identity a2) (Identity b2)) (joinShortVector (Identity a3) (Identity b3)) (joinShortVector (Identity a4) (Identity b4)) (joinShortVector (Identity a5) (Identity b5))
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
type instance HalfVector X4 = X2
instance SplitShortVector X4 Bool where
  splitShortVector (MkBoolX4 !x) = (MkBoolX2 $ fromIntegral $ x .&. 0x3, MkBoolX2 $ fromIntegral $ x `unsafeShiftR` 2)
  joinShortVector (MkBoolX2 !x) (MkBoolX2 !y) = MkBoolX4 (fromIntegral x .|. (fromIntegral y `unsafeShiftL` 2))
instance SplitShortVector X4 Float where
  splitShortVector (MkFloatX4WithElems x0 x1 x2 x3) = (MkFloatX2WithElems x0 x1, MkFloatX2WithElems x2 x3)
  joinShortVector (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems x2 x3) = MkFloatX4WithElems x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Double where
  splitShortVector (MkDoubleX4WithElems x0 x1 x2 x3) = (MkDoubleX2WithElems x0 x1, MkDoubleX2WithElems x2 x3)
  joinShortVector (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems x2 x3) = MkDoubleX4WithElems x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Int8 where
  splitShortVector (MkInt8X4WithElems x0 x1 x2 x3) = (MkInt8X2WithElems x0 x1, MkInt8X2WithElems x2 x3)
  joinShortVector (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems x2 x3) = MkInt8X4WithElems x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Int16 where
  splitShortVector (MkInt16X4WithElems x0 x1 x2 x3) = (MkInt16X2WithElems x0 x1, MkInt16X2WithElems x2 x3)
  joinShortVector (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems x2 x3) = MkInt16X4WithElems x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Int32 where
  splitShortVector (MkInt32X4WithElems x0 x1 x2 x3) = (MkInt32X2WithElems x0 x1, MkInt32X2WithElems x2 x3)
  joinShortVector (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems x2 x3) = MkInt32X4WithElems x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Int64 where
  splitShortVector (MkInt64X4WithElems x0 x1 x2 x3) = (MkInt64X2WithElems x0 x1, MkInt64X2WithElems x2 x3)
  joinShortVector (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems x2 x3) = MkInt64X4WithElems x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Word8 where
  splitShortVector (MkWord8X4WithElems x0 x1 x2 x3) = (MkWord8X2WithElems x0 x1, MkWord8X2WithElems x2 x3)
  joinShortVector (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems x2 x3) = MkWord8X4WithElems x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Word16 where
  splitShortVector (MkWord16X4WithElems x0 x1 x2 x3) = (MkWord16X2WithElems x0 x1, MkWord16X2WithElems x2 x3)
  joinShortVector (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems x2 x3) = MkWord16X4WithElems x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Word32 where
  splitShortVector (MkWord32X4WithElems x0 x1 x2 x3) = (MkWord32X2WithElems x0 x1, MkWord32X2WithElems x2 x3)
  joinShortVector (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems x2 x3) = MkWord32X4WithElems x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Word64 where
  splitShortVector (MkWord64X4WithElems x0 x1 x2 x3) = (MkWord64X2WithElems x0 x1, MkWord64X2WithElems x2 x3)
  joinShortVector (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems x2 x3) = MkWord64X4WithElems x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 a => SplitShortVector X4 (Sum a) where
  splitShortVector = coerce (splitShortVector @X4 @a)
  joinShortVector = coerce (joinShortVector @X4 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 a => SplitShortVector X4 (Product a) where
  splitShortVector = coerce (splitShortVector @X4 @a)
  joinShortVector = coerce (joinShortVector @X4 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 a => SplitShortVector X4 (Min a) where
  splitShortVector = coerce (splitShortVector @X4 @a)
  joinShortVector = coerce (joinShortVector @X4 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 a => SplitShortVector X4 (Max a) where
  splitShortVector = coerce (splitShortVector @X4 @a)
  joinShortVector = coerce (joinShortVector @X4 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 a => SplitShortVector X4 (Complex a) where
  splitShortVector (MkComplexX4 s t) = case splitShortVector s of (x0, x1) -> case splitShortVector t of (y0, y1) -> (MkComplexX2 x0 y0, MkComplexX2 x1 y1)
  joinShortVector (MkComplexX2 x0 y0) (MkComplexX2 x1 y1) = MkComplexX4 (joinShortVector x0 x1) (joinShortVector y0 y1)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 () where
  splitShortVector MkUnitX4 = (MkUnitX2, MkUnitX2)
  joinShortVector MkUnitX2 MkUnitX2 = MkUnitX4
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X4 a0, SplitShortVector X4 a1) => SplitShortVector X4 (a0, a1) where
  splitShortVector (MkTuple2X4 v0 v1) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> (MkTuple2X2 a0 a1, MkTuple2X2 b0 b1)
  joinShortVector (MkTuple2X2 a0 a1) (MkTuple2X2 b0 b1) = MkTuple2X4 (joinShortVector a0 b0) (joinShortVector a1 b1)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X4 a0, SplitShortVector X4 a1, SplitShortVector X4 a2) => SplitShortVector X4 (a0, a1, a2) where
  splitShortVector (MkTuple3X4 v0 v1 v2) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> (MkTuple3X2 a0 a1 a2, MkTuple3X2 b0 b1 b2)
  joinShortVector (MkTuple3X2 a0 a1 a2) (MkTuple3X2 b0 b1 b2) = MkTuple3X4 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X4 a0, SplitShortVector X4 a1, SplitShortVector X4 a2, SplitShortVector X4 a3) => SplitShortVector X4 (a0, a1, a2, a3) where
  splitShortVector (MkTuple4X4 v0 v1 v2 v3) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> (MkTuple4X2 a0 a1 a2 a3, MkTuple4X2 b0 b1 b2 b3)
  joinShortVector (MkTuple4X2 a0 a1 a2 a3) (MkTuple4X2 b0 b1 b2 b3) = MkTuple4X4 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X4 a0, SplitShortVector X4 a1, SplitShortVector X4 a2, SplitShortVector X4 a3, SplitShortVector X4 a4) => SplitShortVector X4 (a0, a1, a2, a3, a4) where
  splitShortVector (MkTuple5X4 v0 v1 v2 v3 v4) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> case splitShortVector v4 of (a4, b4) -> (MkTuple5X2 a0 a1 a2 a3 a4, MkTuple5X2 b0 b1 b2 b3 b4)
  joinShortVector (MkTuple5X2 a0 a1 a2 a3 a4) (MkTuple5X2 b0 b1 b2 b3 b4) = MkTuple5X4 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3) (joinShortVector a4 b4)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X4 a0, SplitShortVector X4 a1, SplitShortVector X4 a2, SplitShortVector X4 a3, SplitShortVector X4 a4, SplitShortVector X4 a5) => SplitShortVector X4 (a0, a1, a2, a3, a4, a5) where
  splitShortVector (MkTuple6X4 v0 v1 v2 v3 v4 v5) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> case splitShortVector v4 of (a4, b4) -> case splitShortVector v5 of (a5, b5) -> (MkTuple6X2 a0 a1 a2 a3 a4 a5, MkTuple6X2 b0 b1 b2 b3 b4 b5)
  joinShortVector (MkTuple6X2 a0 a1 a2 a3 a4 a5) (MkTuple6X2 b0 b1 b2 b3 b4 b5) = MkTuple6X4 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3) (joinShortVector a4 b4) (joinShortVector a5 b5)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
type instance HalfVector X8 = X4
instance SplitShortVector X8 Bool where
  splitShortVector (MkBoolX8 !x) = (MkBoolX4 $ fromIntegral $ x .&. 0xf, MkBoolX4 $ fromIntegral $ x `unsafeShiftR` 4)
  joinShortVector (MkBoolX4 !x) (MkBoolX4 !y) = MkBoolX8 (fromIntegral x .|. (fromIntegral y `unsafeShiftL` 4))
instance SplitShortVector X8 Float where
  splitShortVector (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (MkFloatX4WithElems x0 x1 x2 x3, MkFloatX4WithElems x4 x5 x6 x7)
  joinShortVector (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems x4 x5 x6 x7) = MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Double where
  splitShortVector (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (MkDoubleX4WithElems x0 x1 x2 x3, MkDoubleX4WithElems x4 x5 x6 x7)
  joinShortVector (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems x4 x5 x6 x7) = MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Int8 where
  splitShortVector (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (MkInt8X4WithElems x0 x1 x2 x3, MkInt8X4WithElems x4 x5 x6 x7)
  joinShortVector (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems x4 x5 x6 x7) = MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Int16 where
  splitShortVector (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (MkInt16X4WithElems x0 x1 x2 x3, MkInt16X4WithElems x4 x5 x6 x7)
  joinShortVector (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems x4 x5 x6 x7) = MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Int32 where
  splitShortVector (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (MkInt32X4WithElems x0 x1 x2 x3, MkInt32X4WithElems x4 x5 x6 x7)
  joinShortVector (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems x4 x5 x6 x7) = MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Int64 where
  splitShortVector (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (MkInt64X4WithElems x0 x1 x2 x3, MkInt64X4WithElems x4 x5 x6 x7)
  joinShortVector (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems x4 x5 x6 x7) = MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Word8 where
  splitShortVector (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (MkWord8X4WithElems x0 x1 x2 x3, MkWord8X4WithElems x4 x5 x6 x7)
  joinShortVector (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems x4 x5 x6 x7) = MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Word16 where
  splitShortVector (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (MkWord16X4WithElems x0 x1 x2 x3, MkWord16X4WithElems x4 x5 x6 x7)
  joinShortVector (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems x4 x5 x6 x7) = MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Word32 where
  splitShortVector (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (MkWord32X4WithElems x0 x1 x2 x3, MkWord32X4WithElems x4 x5 x6 x7)
  joinShortVector (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems x4 x5 x6 x7) = MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Word64 where
  splitShortVector (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (MkWord64X4WithElems x0 x1 x2 x3, MkWord64X4WithElems x4 x5 x6 x7)
  joinShortVector (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems x4 x5 x6 x7) = MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 a => SplitShortVector X8 (Sum a) where
  splitShortVector = coerce (splitShortVector @X8 @a)
  joinShortVector = coerce (joinShortVector @X8 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 a => SplitShortVector X8 (Product a) where
  splitShortVector = coerce (splitShortVector @X8 @a)
  joinShortVector = coerce (joinShortVector @X8 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 a => SplitShortVector X8 (Min a) where
  splitShortVector = coerce (splitShortVector @X8 @a)
  joinShortVector = coerce (joinShortVector @X8 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 a => SplitShortVector X8 (Max a) where
  splitShortVector = coerce (splitShortVector @X8 @a)
  joinShortVector = coerce (joinShortVector @X8 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 a => SplitShortVector X8 (Complex a) where
  splitShortVector (MkComplexX8 s t) = case splitShortVector s of (x0, x1) -> case splitShortVector t of (y0, y1) -> (MkComplexX4 x0 y0, MkComplexX4 x1 y1)
  joinShortVector (MkComplexX4 x0 y0) (MkComplexX4 x1 y1) = MkComplexX8 (joinShortVector x0 x1) (joinShortVector y0 y1)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 () where
  splitShortVector MkUnitX8 = (MkUnitX4, MkUnitX4)
  joinShortVector MkUnitX4 MkUnitX4 = MkUnitX8
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X8 a0, SplitShortVector X8 a1) => SplitShortVector X8 (a0, a1) where
  splitShortVector (MkTuple2X8 v0 v1) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> (MkTuple2X4 a0 a1, MkTuple2X4 b0 b1)
  joinShortVector (MkTuple2X4 a0 a1) (MkTuple2X4 b0 b1) = MkTuple2X8 (joinShortVector a0 b0) (joinShortVector a1 b1)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X8 a0, SplitShortVector X8 a1, SplitShortVector X8 a2) => SplitShortVector X8 (a0, a1, a2) where
  splitShortVector (MkTuple3X8 v0 v1 v2) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> (MkTuple3X4 a0 a1 a2, MkTuple3X4 b0 b1 b2)
  joinShortVector (MkTuple3X4 a0 a1 a2) (MkTuple3X4 b0 b1 b2) = MkTuple3X8 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X8 a0, SplitShortVector X8 a1, SplitShortVector X8 a2, SplitShortVector X8 a3) => SplitShortVector X8 (a0, a1, a2, a3) where
  splitShortVector (MkTuple4X8 v0 v1 v2 v3) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> (MkTuple4X4 a0 a1 a2 a3, MkTuple4X4 b0 b1 b2 b3)
  joinShortVector (MkTuple4X4 a0 a1 a2 a3) (MkTuple4X4 b0 b1 b2 b3) = MkTuple4X8 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X8 a0, SplitShortVector X8 a1, SplitShortVector X8 a2, SplitShortVector X8 a3, SplitShortVector X8 a4) => SplitShortVector X8 (a0, a1, a2, a3, a4) where
  splitShortVector (MkTuple5X8 v0 v1 v2 v3 v4) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> case splitShortVector v4 of (a4, b4) -> (MkTuple5X4 a0 a1 a2 a3 a4, MkTuple5X4 b0 b1 b2 b3 b4)
  joinShortVector (MkTuple5X4 a0 a1 a2 a3 a4) (MkTuple5X4 b0 b1 b2 b3 b4) = MkTuple5X8 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3) (joinShortVector a4 b4)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X8 a0, SplitShortVector X8 a1, SplitShortVector X8 a2, SplitShortVector X8 a3, SplitShortVector X8 a4, SplitShortVector X8 a5) => SplitShortVector X8 (a0, a1, a2, a3, a4, a5) where
  splitShortVector (MkTuple6X8 v0 v1 v2 v3 v4 v5) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> case splitShortVector v4 of (a4, b4) -> case splitShortVector v5 of (a5, b5) -> (MkTuple6X4 a0 a1 a2 a3 a4 a5, MkTuple6X4 b0 b1 b2 b3 b4 b5)
  joinShortVector (MkTuple6X4 a0 a1 a2 a3 a4 a5) (MkTuple6X4 b0 b1 b2 b3 b4 b5) = MkTuple6X8 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3) (joinShortVector a4 b4) (joinShortVector a5 b5)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
type instance HalfVector X16 = X8
instance SplitShortVector X16 a where
  splitShortVector (MkX16WithX8 u0 u1) = (u0, u1)
  joinShortVector = MkX16WithX8
type instance HalfVector X32 = X16
instance SplitShortVector X32 a where
  splitShortVector (MkX32WithX8 u0 u1 u2 u3) = (MkX16WithX8 u0 u1, MkX16WithX8 u2 u3)
  joinShortVector (MkX16WithX8 u0 u1) (MkX16WithX8 u2 u3) = MkX32WithX8 u0 u1 u2 u3
type instance HalfVector X64 = X32
instance SplitShortVector X64 a where
  splitShortVector (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = (MkX32WithX8 u0 u1 u2 u3, MkX32WithX8 u4 u5 u6 u7)
  joinShortVector (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 u4 u5 u6 u7) = MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7
