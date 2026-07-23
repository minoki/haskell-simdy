-- This file was created by script/Gen.hs. Do not edit by hand!
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Simdy.Internal.VL512.HalfVector where
import           Data.Bits
import           Data.Complex
import           Data.Monoid
import           Data.Semigroup
import           Data.Simdy.Internal.Class
import           GHC.Exts
import           GHC.Int
import           GHC.Word
import           Data.Simdy.Internal.VL256 (X4 (..))
import           Data.Simdy.Internal.VL512.X8
import           Data.Simdy.Internal.VL512.X16
import           Data.Simdy.Internal.VL512.X32
import           Data.Simdy.Internal.VL512.X64
type instance HalfVector X8 = X4
instance SplitShortVector X8 Bool where
  splitShortVector (MkBoolX8 !x) = (MkBoolX4 $ fromIntegral $ x .&. 0xf, MkBoolX4 $ fromIntegral $ x `unsafeShiftR` 4)
  joinShortVector (MkBoolX4 !x) (MkBoolX4 !y) = MkBoolX8 (fromIntegral x .|. (fromIntegral y `unsafeShiftL` 4))
instance SplitShortVector X8 Float where
  splitShortVector v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> (mkX4 x0 x1 x2 x3, mkX4 x4 x5 x6 x7)
  joinShortVector u v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (x4, x5, x6, x7) -> mkX8 x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Double where
  splitShortVector v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> (mkX4 x0 x1 x2 x3, mkX4 x4 x5 x6 x7)
  joinShortVector u v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (x4, x5, x6, x7) -> mkX8 x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Int8 where
  splitShortVector v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> (mkX4 x0 x1 x2 x3, mkX4 x4 x5 x6 x7)
  joinShortVector u v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (x4, x5, x6, x7) -> mkX8 x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Int16 where
  splitShortVector v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> (mkX4 x0 x1 x2 x3, mkX4 x4 x5 x6 x7)
  joinShortVector u v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (x4, x5, x6, x7) -> mkX8 x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Int32 where
  splitShortVector v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> (mkX4 x0 x1 x2 x3, mkX4 x4 x5 x6 x7)
  joinShortVector u v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (x4, x5, x6, x7) -> mkX8 x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Int64 where
  splitShortVector v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> (mkX4 x0 x1 x2 x3, mkX4 x4 x5 x6 x7)
  joinShortVector u v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (x4, x5, x6, x7) -> mkX8 x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Word8 where
  splitShortVector v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> (mkX4 x0 x1 x2 x3, mkX4 x4 x5 x6 x7)
  joinShortVector u v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (x4, x5, x6, x7) -> mkX8 x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Word16 where
  splitShortVector v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> (mkX4 x0 x1 x2 x3, mkX4 x4 x5 x6 x7)
  joinShortVector u v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (x4, x5, x6, x7) -> mkX8 x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Word32 where
  splitShortVector v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> (mkX4 x0 x1 x2 x3, mkX4 x4 x5 x6 x7)
  joinShortVector u v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (x4, x5, x6, x7) -> mkX8 x0 x1 x2 x3 x4 x5 x6 x7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Word64 where
  splitShortVector v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> (mkX4 x0 x1 x2 x3, mkX4 x4 x5 x6 x7)
  joinShortVector u v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (x4, x5, x6, x7) -> mkX8 x0 x1 x2 x3 x4 x5 x6 x7
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
instance SplitShortVector X16 Bool where
  splitShortVector (MkBoolX16 !x) = (MkBoolX8 $ fromIntegral $ x .&. 0xff, MkBoolX8 $ fromIntegral $ x `unsafeShiftR` 8)
  joinShortVector (MkBoolX8 !x) (MkBoolX8 !y) = MkBoolX16 (fromIntegral x .|. (fromIntegral y `unsafeShiftL` 8))
instance SplitShortVector X16 Float where
  splitShortVector v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> (mkX8 x0 x1 x2 x3 x4 x5 x6 x7, mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector u v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Double where
  splitShortVector (MkDoubleX16WithVec512 v0 v1) = (MkDoubleX8 v0, MkDoubleX8 v1)
  joinShortVector (MkDoubleX8 v0) (MkDoubleX8 v1) = MkDoubleX16WithVec512 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Int8 where
  splitShortVector v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> (mkX8 x0 x1 x2 x3 x4 x5 x6 x7, mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector u v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Int16 where
  splitShortVector v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> (mkX8 x0 x1 x2 x3 x4 x5 x6 x7, mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector u v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Int32 where
  splitShortVector v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> (mkX8 x0 x1 x2 x3 x4 x5 x6 x7, mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector u v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Int64 where
  splitShortVector (MkInt64X16WithVec512 v0 v1) = (MkInt64X8 v0, MkInt64X8 v1)
  joinShortVector (MkInt64X8 v0) (MkInt64X8 v1) = MkInt64X16WithVec512 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word8 where
  splitShortVector v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> (mkX8 x0 x1 x2 x3 x4 x5 x6 x7, mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector u v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word16 where
  splitShortVector v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> (mkX8 x0 x1 x2 x3 x4 x5 x6 x7, mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector u v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word32 where
  splitShortVector v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> (mkX8 x0 x1 x2 x3 x4 x5 x6 x7, mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector u v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word64 where
  splitShortVector (MkWord64X16WithVec512 v0 v1) = (MkWord64X8 v0, MkWord64X8 v1)
  joinShortVector (MkWord64X8 v0) (MkWord64X8 v1) = MkWord64X16WithVec512 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 a => SplitShortVector X16 (Sum a) where
  splitShortVector = coerce (splitShortVector @X16 @a)
  joinShortVector = coerce (joinShortVector @X16 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 a => SplitShortVector X16 (Product a) where
  splitShortVector = coerce (splitShortVector @X16 @a)
  joinShortVector = coerce (joinShortVector @X16 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 a => SplitShortVector X16 (Min a) where
  splitShortVector = coerce (splitShortVector @X16 @a)
  joinShortVector = coerce (joinShortVector @X16 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 a => SplitShortVector X16 (Max a) where
  splitShortVector = coerce (splitShortVector @X16 @a)
  joinShortVector = coerce (joinShortVector @X16 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 a => SplitShortVector X16 (Complex a) where
  splitShortVector (MkComplexX16 s t) = case splitShortVector s of (x0, x1) -> case splitShortVector t of (y0, y1) -> (MkComplexX8 x0 y0, MkComplexX8 x1 y1)
  joinShortVector (MkComplexX8 x0 y0) (MkComplexX8 x1 y1) = MkComplexX16 (joinShortVector x0 x1) (joinShortVector y0 y1)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 () where
  splitShortVector MkUnitX16 = (MkUnitX8, MkUnitX8)
  joinShortVector MkUnitX8 MkUnitX8 = MkUnitX16
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X16 a0, SplitShortVector X16 a1) => SplitShortVector X16 (a0, a1) where
  splitShortVector (MkTuple2X16 v0 v1) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> (MkTuple2X8 a0 a1, MkTuple2X8 b0 b1)
  joinShortVector (MkTuple2X8 a0 a1) (MkTuple2X8 b0 b1) = MkTuple2X16 (joinShortVector a0 b0) (joinShortVector a1 b1)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X16 a0, SplitShortVector X16 a1, SplitShortVector X16 a2) => SplitShortVector X16 (a0, a1, a2) where
  splitShortVector (MkTuple3X16 v0 v1 v2) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> (MkTuple3X8 a0 a1 a2, MkTuple3X8 b0 b1 b2)
  joinShortVector (MkTuple3X8 a0 a1 a2) (MkTuple3X8 b0 b1 b2) = MkTuple3X16 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X16 a0, SplitShortVector X16 a1, SplitShortVector X16 a2, SplitShortVector X16 a3) => SplitShortVector X16 (a0, a1, a2, a3) where
  splitShortVector (MkTuple4X16 v0 v1 v2 v3) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> (MkTuple4X8 a0 a1 a2 a3, MkTuple4X8 b0 b1 b2 b3)
  joinShortVector (MkTuple4X8 a0 a1 a2 a3) (MkTuple4X8 b0 b1 b2 b3) = MkTuple4X16 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X16 a0, SplitShortVector X16 a1, SplitShortVector X16 a2, SplitShortVector X16 a3, SplitShortVector X16 a4) => SplitShortVector X16 (a0, a1, a2, a3, a4) where
  splitShortVector (MkTuple5X16 v0 v1 v2 v3 v4) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> case splitShortVector v4 of (a4, b4) -> (MkTuple5X8 a0 a1 a2 a3 a4, MkTuple5X8 b0 b1 b2 b3 b4)
  joinShortVector (MkTuple5X8 a0 a1 a2 a3 a4) (MkTuple5X8 b0 b1 b2 b3 b4) = MkTuple5X16 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3) (joinShortVector a4 b4)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X16 a0, SplitShortVector X16 a1, SplitShortVector X16 a2, SplitShortVector X16 a3, SplitShortVector X16 a4, SplitShortVector X16 a5) => SplitShortVector X16 (a0, a1, a2, a3, a4, a5) where
  splitShortVector (MkTuple6X16 v0 v1 v2 v3 v4 v5) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> case splitShortVector v4 of (a4, b4) -> case splitShortVector v5 of (a5, b5) -> (MkTuple6X8 a0 a1 a2 a3 a4 a5, MkTuple6X8 b0 b1 b2 b3 b4 b5)
  joinShortVector (MkTuple6X8 a0 a1 a2 a3 a4 a5) (MkTuple6X8 b0 b1 b2 b3 b4 b5) = MkTuple6X16 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3) (joinShortVector a4 b4) (joinShortVector a5 b5)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
type instance HalfVector X32 = X16
instance SplitShortVector X32 Bool where
  splitShortVector (MkBoolX32 !x) = (MkBoolX16 $ fromIntegral $ x .&. 0xffff, MkBoolX16 $ fromIntegral $ x `unsafeShiftR` 16)
  joinShortVector (MkBoolX16 !x) (MkBoolX16 !y) = MkBoolX32 (fromIntegral x .|. (fromIntegral y `unsafeShiftL` 16))
instance SplitShortVector X32 Float where
  splitShortVector (MkFloatX32WithVec512 v0 v1) = (MkFloatX16 v0, MkFloatX16 v1)
  joinShortVector (MkFloatX16 v0) (MkFloatX16 v1) = MkFloatX32WithVec512 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Double where
  splitShortVector (MkDoubleX32WithVec512 v0 v1 v2 v3) = (MkDoubleX16WithVec512 v0 v1, MkDoubleX16WithVec512 v2 v3)
  joinShortVector (MkDoubleX16WithVec512 v0 v1) (MkDoubleX16WithVec512 v2 v3) = MkDoubleX32WithVec512 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Int8 where
  splitShortVector v = case unpackX32 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> (mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, mkX16 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector u v = case unpackX16 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> case unpackX16 v of (x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> mkX32 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Int16 where
  splitShortVector v = case unpackX32 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> (mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, mkX16 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector u v = case unpackX16 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> case unpackX16 v of (x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> mkX32 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Int32 where
  splitShortVector (MkInt32X32WithVec512 v0 v1) = (MkInt32X16 v0, MkInt32X16 v1)
  joinShortVector (MkInt32X16 v0) (MkInt32X16 v1) = MkInt32X32WithVec512 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Int64 where
  splitShortVector (MkInt64X32WithVec512 v0 v1 v2 v3) = (MkInt64X16WithVec512 v0 v1, MkInt64X16WithVec512 v2 v3)
  joinShortVector (MkInt64X16WithVec512 v0 v1) (MkInt64X16WithVec512 v2 v3) = MkInt64X32WithVec512 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word8 where
  splitShortVector v = case unpackX32 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> (mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, mkX16 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector u v = case unpackX16 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> case unpackX16 v of (x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> mkX32 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word16 where
  splitShortVector v = case unpackX32 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> (mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, mkX16 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector u v = case unpackX16 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> case unpackX16 v of (x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> mkX32 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word32 where
  splitShortVector (MkWord32X32WithVec512 v0 v1) = (MkWord32X16 v0, MkWord32X16 v1)
  joinShortVector (MkWord32X16 v0) (MkWord32X16 v1) = MkWord32X32WithVec512 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word64 where
  splitShortVector (MkWord64X32WithVec512 v0 v1 v2 v3) = (MkWord64X16WithVec512 v0 v1, MkWord64X16WithVec512 v2 v3)
  joinShortVector (MkWord64X16WithVec512 v0 v1) (MkWord64X16WithVec512 v2 v3) = MkWord64X32WithVec512 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 a => SplitShortVector X32 (Sum a) where
  splitShortVector = coerce (splitShortVector @X32 @a)
  joinShortVector = coerce (joinShortVector @X32 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 a => SplitShortVector X32 (Product a) where
  splitShortVector = coerce (splitShortVector @X32 @a)
  joinShortVector = coerce (joinShortVector @X32 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 a => SplitShortVector X32 (Min a) where
  splitShortVector = coerce (splitShortVector @X32 @a)
  joinShortVector = coerce (joinShortVector @X32 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 a => SplitShortVector X32 (Max a) where
  splitShortVector = coerce (splitShortVector @X32 @a)
  joinShortVector = coerce (joinShortVector @X32 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 a => SplitShortVector X32 (Complex a) where
  splitShortVector (MkComplexX32 s t) = case splitShortVector s of (x0, x1) -> case splitShortVector t of (y0, y1) -> (MkComplexX16 x0 y0, MkComplexX16 x1 y1)
  joinShortVector (MkComplexX16 x0 y0) (MkComplexX16 x1 y1) = MkComplexX32 (joinShortVector x0 x1) (joinShortVector y0 y1)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 () where
  splitShortVector MkUnitX32 = (MkUnitX16, MkUnitX16)
  joinShortVector MkUnitX16 MkUnitX16 = MkUnitX32
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X32 a0, SplitShortVector X32 a1) => SplitShortVector X32 (a0, a1) where
  splitShortVector (MkTuple2X32 v0 v1) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> (MkTuple2X16 a0 a1, MkTuple2X16 b0 b1)
  joinShortVector (MkTuple2X16 a0 a1) (MkTuple2X16 b0 b1) = MkTuple2X32 (joinShortVector a0 b0) (joinShortVector a1 b1)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X32 a0, SplitShortVector X32 a1, SplitShortVector X32 a2) => SplitShortVector X32 (a0, a1, a2) where
  splitShortVector (MkTuple3X32 v0 v1 v2) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> (MkTuple3X16 a0 a1 a2, MkTuple3X16 b0 b1 b2)
  joinShortVector (MkTuple3X16 a0 a1 a2) (MkTuple3X16 b0 b1 b2) = MkTuple3X32 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X32 a0, SplitShortVector X32 a1, SplitShortVector X32 a2, SplitShortVector X32 a3) => SplitShortVector X32 (a0, a1, a2, a3) where
  splitShortVector (MkTuple4X32 v0 v1 v2 v3) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> (MkTuple4X16 a0 a1 a2 a3, MkTuple4X16 b0 b1 b2 b3)
  joinShortVector (MkTuple4X16 a0 a1 a2 a3) (MkTuple4X16 b0 b1 b2 b3) = MkTuple4X32 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X32 a0, SplitShortVector X32 a1, SplitShortVector X32 a2, SplitShortVector X32 a3, SplitShortVector X32 a4) => SplitShortVector X32 (a0, a1, a2, a3, a4) where
  splitShortVector (MkTuple5X32 v0 v1 v2 v3 v4) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> case splitShortVector v4 of (a4, b4) -> (MkTuple5X16 a0 a1 a2 a3 a4, MkTuple5X16 b0 b1 b2 b3 b4)
  joinShortVector (MkTuple5X16 a0 a1 a2 a3 a4) (MkTuple5X16 b0 b1 b2 b3 b4) = MkTuple5X32 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3) (joinShortVector a4 b4)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X32 a0, SplitShortVector X32 a1, SplitShortVector X32 a2, SplitShortVector X32 a3, SplitShortVector X32 a4, SplitShortVector X32 a5) => SplitShortVector X32 (a0, a1, a2, a3, a4, a5) where
  splitShortVector (MkTuple6X32 v0 v1 v2 v3 v4 v5) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> case splitShortVector v4 of (a4, b4) -> case splitShortVector v5 of (a5, b5) -> (MkTuple6X16 a0 a1 a2 a3 a4 a5, MkTuple6X16 b0 b1 b2 b3 b4 b5)
  joinShortVector (MkTuple6X16 a0 a1 a2 a3 a4 a5) (MkTuple6X16 b0 b1 b2 b3 b4 b5) = MkTuple6X32 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3) (joinShortVector a4 b4) (joinShortVector a5 b5)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
type instance HalfVector X64 = X32
instance SplitShortVector X64 Bool where
  splitShortVector (MkBoolX64 !x) = (MkBoolX32 $ fromIntegral $ x .&. 0xffffffff, MkBoolX32 $ fromIntegral $ x `unsafeShiftR` 32)
  joinShortVector (MkBoolX32 !x) (MkBoolX32 !y) = MkBoolX64 (fromIntegral x .|. (fromIntegral y `unsafeShiftL` 32))
instance SplitShortVector X64 Float where
  splitShortVector (MkFloatX64WithVec512 v0 v1 v2 v3) = (MkFloatX32WithVec512 v0 v1, MkFloatX32WithVec512 v2 v3)
  joinShortVector (MkFloatX32WithVec512 v0 v1) (MkFloatX32WithVec512 v2 v3) = MkFloatX64WithVec512 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Double where
  splitShortVector (MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = (MkDoubleX32WithVec512 v0 v1 v2 v3, MkDoubleX32WithVec512 v4 v5 v6 v7)
  joinShortVector (MkDoubleX32WithVec512 v0 v1 v2 v3) (MkDoubleX32WithVec512 v4 v5 v6 v7) = MkDoubleX64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Int8 where
  splitShortVector v = case unpackX64 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) -> (mkX32 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31, mkX32 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63)
  joinShortVector u v = case unpackX32 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> case unpackX32 v of (x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) -> mkX64 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Int16 where
  splitShortVector (MkInt16X64WithVec512 v0 v1) = (MkInt16X32 v0, MkInt16X32 v1)
  joinShortVector (MkInt16X32 v0) (MkInt16X32 v1) = MkInt16X64WithVec512 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Int32 where
  splitShortVector (MkInt32X64WithVec512 v0 v1 v2 v3) = (MkInt32X32WithVec512 v0 v1, MkInt32X32WithVec512 v2 v3)
  joinShortVector (MkInt32X32WithVec512 v0 v1) (MkInt32X32WithVec512 v2 v3) = MkInt32X64WithVec512 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Int64 where
  splitShortVector (MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = (MkInt64X32WithVec512 v0 v1 v2 v3, MkInt64X32WithVec512 v4 v5 v6 v7)
  joinShortVector (MkInt64X32WithVec512 v0 v1 v2 v3) (MkInt64X32WithVec512 v4 v5 v6 v7) = MkInt64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word8 where
  splitShortVector v = case unpackX64 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) -> (mkX32 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31, mkX32 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63)
  joinShortVector u v = case unpackX32 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> case unpackX32 v of (x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) -> mkX64 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word16 where
  splitShortVector (MkWord16X64WithVec512 v0 v1) = (MkWord16X32 v0, MkWord16X32 v1)
  joinShortVector (MkWord16X32 v0) (MkWord16X32 v1) = MkWord16X64WithVec512 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word32 where
  splitShortVector (MkWord32X64WithVec512 v0 v1 v2 v3) = (MkWord32X32WithVec512 v0 v1, MkWord32X32WithVec512 v2 v3)
  joinShortVector (MkWord32X32WithVec512 v0 v1) (MkWord32X32WithVec512 v2 v3) = MkWord32X64WithVec512 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word64 where
  splitShortVector (MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7) = (MkWord64X32WithVec512 v0 v1 v2 v3, MkWord64X32WithVec512 v4 v5 v6 v7)
  joinShortVector (MkWord64X32WithVec512 v0 v1 v2 v3) (MkWord64X32WithVec512 v4 v5 v6 v7) = MkWord64X64WithVec512 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 a => SplitShortVector X64 (Sum a) where
  splitShortVector = coerce (splitShortVector @X64 @a)
  joinShortVector = coerce (joinShortVector @X64 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 a => SplitShortVector X64 (Product a) where
  splitShortVector = coerce (splitShortVector @X64 @a)
  joinShortVector = coerce (joinShortVector @X64 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 a => SplitShortVector X64 (Min a) where
  splitShortVector = coerce (splitShortVector @X64 @a)
  joinShortVector = coerce (joinShortVector @X64 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 a => SplitShortVector X64 (Max a) where
  splitShortVector = coerce (splitShortVector @X64 @a)
  joinShortVector = coerce (joinShortVector @X64 @a)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 a => SplitShortVector X64 (Complex a) where
  splitShortVector (MkComplexX64 s t) = case splitShortVector s of (x0, x1) -> case splitShortVector t of (y0, y1) -> (MkComplexX32 x0 y0, MkComplexX32 x1 y1)
  joinShortVector (MkComplexX32 x0 y0) (MkComplexX32 x1 y1) = MkComplexX64 (joinShortVector x0 x1) (joinShortVector y0 y1)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 () where
  splitShortVector MkUnitX64 = (MkUnitX32, MkUnitX32)
  joinShortVector MkUnitX32 MkUnitX32 = MkUnitX64
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X64 a0, SplitShortVector X64 a1) => SplitShortVector X64 (a0, a1) where
  splitShortVector (MkTuple2X64 v0 v1) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> (MkTuple2X32 a0 a1, MkTuple2X32 b0 b1)
  joinShortVector (MkTuple2X32 a0 a1) (MkTuple2X32 b0 b1) = MkTuple2X64 (joinShortVector a0 b0) (joinShortVector a1 b1)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X64 a0, SplitShortVector X64 a1, SplitShortVector X64 a2) => SplitShortVector X64 (a0, a1, a2) where
  splitShortVector (MkTuple3X64 v0 v1 v2) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> (MkTuple3X32 a0 a1 a2, MkTuple3X32 b0 b1 b2)
  joinShortVector (MkTuple3X32 a0 a1 a2) (MkTuple3X32 b0 b1 b2) = MkTuple3X64 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X64 a0, SplitShortVector X64 a1, SplitShortVector X64 a2, SplitShortVector X64 a3) => SplitShortVector X64 (a0, a1, a2, a3) where
  splitShortVector (MkTuple4X64 v0 v1 v2 v3) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> (MkTuple4X32 a0 a1 a2 a3, MkTuple4X32 b0 b1 b2 b3)
  joinShortVector (MkTuple4X32 a0 a1 a2 a3) (MkTuple4X32 b0 b1 b2 b3) = MkTuple4X64 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X64 a0, SplitShortVector X64 a1, SplitShortVector X64 a2, SplitShortVector X64 a3, SplitShortVector X64 a4) => SplitShortVector X64 (a0, a1, a2, a3, a4) where
  splitShortVector (MkTuple5X64 v0 v1 v2 v3 v4) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> case splitShortVector v4 of (a4, b4) -> (MkTuple5X32 a0 a1 a2 a3 a4, MkTuple5X32 b0 b1 b2 b3 b4)
  joinShortVector (MkTuple5X32 a0 a1 a2 a3 a4) (MkTuple5X32 b0 b1 b2 b3 b4) = MkTuple5X64 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3) (joinShortVector a4 b4)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance (SplitShortVector X64 a0, SplitShortVector X64 a1, SplitShortVector X64 a2, SplitShortVector X64 a3, SplitShortVector X64 a4, SplitShortVector X64 a5) => SplitShortVector X64 (a0, a1, a2, a3, a4, a5) where
  splitShortVector (MkTuple6X64 v0 v1 v2 v3 v4 v5) = case splitShortVector v0 of (a0, b0) -> case splitShortVector v1 of (a1, b1) -> case splitShortVector v2 of (a2, b2) -> case splitShortVector v3 of (a3, b3) -> case splitShortVector v4 of (a4, b4) -> case splitShortVector v5 of (a5, b5) -> (MkTuple6X32 a0 a1 a2 a3 a4 a5, MkTuple6X32 b0 b1 b2 b3 b4 b5)
  joinShortVector (MkTuple6X32 a0 a1 a2 a3 a4 a5) (MkTuple6X32 b0 b1 b2 b3 b4 b5) = MkTuple6X64 (joinShortVector a0 b0) (joinShortVector a1 b1) (joinShortVector a2 b2) (joinShortVector a3 b3) (joinShortVector a4 b4) (joinShortVector a5 b5)
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
