-- This file was created by script/Gen.hs. Do not edit by hand!
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Simdy.Internal.VL128.HalfVector where
import           Data.Bits
import           Data.Complex
import           Data.Monoid
import           Data.Semigroup
import           Data.Simdy.Internal.Class
import           GHC.Exts
import           GHC.Int
import           GHC.Word
import           Data.Functor.Identity
import           Data.Simdy.Internal.VL128.X2
import           Data.Simdy.Internal.VL128.X4
import           Data.Simdy.Internal.VL128.X8
import           Data.Simdy.Internal.VL128.X16
import           Data.Simdy.Internal.VL128.X32
import           Data.Simdy.Internal.VL128.X64
type instance HalfVector X2 = Identity
instance SplitShortVector X2 Bool where
  splitShortVector (MkBoolX2 !x) = (Identity (testBit x 0), Identity (testBit x 1))
  joinShortVector (Identity !x) (Identity !y) = MkBoolX2 ((if x then 1 else 0) .|. (if y then 2 else 0))
instance SplitShortVector X2 Float where
  splitShortVector v = coerce (unpackX2 v)
  joinShortVector (Identity x0) (Identity x1) = mkX2 x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Double where
  splitShortVector v = coerce (unpackX2 v)
  joinShortVector (Identity x0) (Identity x1) = mkX2 x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)
instance SplitShortVector X2 Int8 where
  splitShortVector v = coerce (unpackX2 v)
  joinShortVector (Identity x0) (Identity x1) = mkX2 x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Int16 where
  splitShortVector v = coerce (unpackX2 v)
  joinShortVector (Identity x0) (Identity x1) = mkX2 x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Int32 where
  splitShortVector v = coerce (unpackX2 v)
  joinShortVector (Identity x0) (Identity x1) = mkX2 x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Int64 where
  splitShortVector v = coerce (unpackX2 v)
  joinShortVector (Identity x0) (Identity x1) = mkX2 x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Word8 where
  splitShortVector v = coerce (unpackX2 v)
  joinShortVector (Identity x0) (Identity x1) = mkX2 x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Word16 where
  splitShortVector v = coerce (unpackX2 v)
  joinShortVector (Identity x0) (Identity x1) = mkX2 x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Word32 where
  splitShortVector v = coerce (unpackX2 v)
  joinShortVector (Identity x0) (Identity x1) = mkX2 x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X2 Word64 where
  splitShortVector v = coerce (unpackX2 v)
  joinShortVector (Identity x0) (Identity x1) = mkX2 x0 x1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#else
-- The NCG of GHC 9.12 does not support integer vectors
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
#endif
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
  splitShortVector v = case unpackX4 v of (x0, x1, x2, x3) -> (mkX2 x0 x1, mkX2 x2 x3)
  joinShortVector u v = case unpackX2 u of (x0, x1) -> case unpackX2 v of (x2, x3) -> mkX4 x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Double where
  splitShortVector (MkDoubleX4WithVec128 v0 v1) = (MkDoubleX2 v0, MkDoubleX2 v1)
  joinShortVector (MkDoubleX2 v0) (MkDoubleX2 v1) = MkDoubleX4WithVec128 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)
instance SplitShortVector X4 Int8 where
  splitShortVector v = case unpackX4 v of (x0, x1, x2, x3) -> (mkX2 x0 x1, mkX2 x2 x3)
  joinShortVector u v = case unpackX2 u of (x0, x1) -> case unpackX2 v of (x2, x3) -> mkX4 x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Int16 where
  splitShortVector v = case unpackX4 v of (x0, x1, x2, x3) -> (mkX2 x0 x1, mkX2 x2 x3)
  joinShortVector u v = case unpackX2 u of (x0, x1) -> case unpackX2 v of (x2, x3) -> mkX4 x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Int32 where
  splitShortVector v = case unpackX4 v of (x0, x1, x2, x3) -> (mkX2 x0 x1, mkX2 x2 x3)
  joinShortVector u v = case unpackX2 u of (x0, x1) -> case unpackX2 v of (x2, x3) -> mkX4 x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Int64 where
  splitShortVector (MkInt64X4WithVec128 v0 v1) = (MkInt64X2 v0, MkInt64X2 v1)
  joinShortVector (MkInt64X2 v0) (MkInt64X2 v1) = MkInt64X4WithVec128 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Word8 where
  splitShortVector v = case unpackX4 v of (x0, x1, x2, x3) -> (mkX2 x0 x1, mkX2 x2 x3)
  joinShortVector u v = case unpackX2 u of (x0, x1) -> case unpackX2 v of (x2, x3) -> mkX4 x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Word16 where
  splitShortVector v = case unpackX4 v of (x0, x1, x2, x3) -> (mkX2 x0 x1, mkX2 x2 x3)
  joinShortVector u v = case unpackX2 u of (x0, x1) -> case unpackX2 v of (x2, x3) -> mkX4 x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Word32 where
  splitShortVector v = case unpackX4 v of (x0, x1, x2, x3) -> (mkX2 x0 x1, mkX2 x2 x3)
  joinShortVector u v = case unpackX2 u of (x0, x1) -> case unpackX2 v of (x2, x3) -> mkX4 x0 x1 x2 x3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X4 Word64 where
  splitShortVector (MkWord64X4WithVec128 v0 v1) = (MkWord64X2 v0, MkWord64X2 v1)
  joinShortVector (MkWord64X2 v0) (MkWord64X2 v1) = MkWord64X4WithVec128 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#else
-- The NCG of GHC 9.12 does not support integer vectors
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
#endif
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
  splitShortVector (MkFloatX8WithVec128 v0 v1) = (MkFloatX4 v0, MkFloatX4 v1)
  joinShortVector (MkFloatX4 v0) (MkFloatX4 v1) = MkFloatX8WithVec128 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Double where
  splitShortVector (MkDoubleX8WithVec128 v0 v1 v2 v3) = (MkDoubleX4WithVec128 v0 v1, MkDoubleX4WithVec128 v2 v3)
  joinShortVector (MkDoubleX4WithVec128 v0 v1) (MkDoubleX4WithVec128 v2 v3) = MkDoubleX8WithVec128 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)
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
  splitShortVector (MkInt32X8WithVec128 v0 v1) = (MkInt32X4 v0, MkInt32X4 v1)
  joinShortVector (MkInt32X4 v0) (MkInt32X4 v1) = MkInt32X8WithVec128 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Int64 where
  splitShortVector (MkInt64X8WithVec128 v0 v1 v2 v3) = (MkInt64X4WithVec128 v0 v1, MkInt64X4WithVec128 v2 v3)
  joinShortVector (MkInt64X4WithVec128 v0 v1) (MkInt64X4WithVec128 v2 v3) = MkInt64X8WithVec128 v0 v1 v2 v3
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
  splitShortVector (MkWord32X8WithVec128 v0 v1) = (MkWord32X4 v0, MkWord32X4 v1)
  joinShortVector (MkWord32X4 v0) (MkWord32X4 v1) = MkWord32X8WithVec128 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X8 Word64 where
  splitShortVector (MkWord64X8WithVec128 v0 v1 v2 v3) = (MkWord64X4WithVec128 v0 v1, MkWord64X4WithVec128 v2 v3)
  joinShortVector (MkWord64X4WithVec128 v0 v1) (MkWord64X4WithVec128 v2 v3) = MkWord64X8WithVec128 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#else
-- The NCG of GHC 9.12 does not support integer vectors
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
#endif
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
  splitShortVector (MkFloatX16WithVec128 v0 v1 v2 v3) = (MkFloatX8WithVec128 v0 v1, MkFloatX8WithVec128 v2 v3)
  joinShortVector (MkFloatX8WithVec128 v0 v1) (MkFloatX8WithVec128 v2 v3) = MkFloatX16WithVec128 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Double where
  splitShortVector (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = (MkDoubleX8WithVec128 v0 v1 v2 v3, MkDoubleX8WithVec128 v4 v5 v6 v7)
  joinShortVector (MkDoubleX8WithVec128 v0 v1 v2 v3) (MkDoubleX8WithVec128 v4 v5 v6 v7) = MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)
instance SplitShortVector X16 Int8 where
  splitShortVector v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> (mkX8 x0 x1 x2 x3 x4 x5 x6 x7, mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector u v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Int16 where
  splitShortVector (MkInt16X16WithVec128 v0 v1) = (MkInt16X8 v0, MkInt16X8 v1)
  joinShortVector (MkInt16X8 v0) (MkInt16X8 v1) = MkInt16X16WithVec128 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Int32 where
  splitShortVector (MkInt32X16WithVec128 v0 v1 v2 v3) = (MkInt32X8WithVec128 v0 v1, MkInt32X8WithVec128 v2 v3)
  joinShortVector (MkInt32X8WithVec128 v0 v1) (MkInt32X8WithVec128 v2 v3) = MkInt32X16WithVec128 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Int64 where
  splitShortVector (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = (MkInt64X8WithVec128 v0 v1 v2 v3, MkInt64X8WithVec128 v4 v5 v6 v7)
  joinShortVector (MkInt64X8WithVec128 v0 v1 v2 v3) (MkInt64X8WithVec128 v4 v5 v6 v7) = MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word8 where
  splitShortVector v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> (mkX8 x0 x1 x2 x3 x4 x5 x6 x7, mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector u v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word16 where
  splitShortVector (MkWord16X16WithVec128 v0 v1) = (MkWord16X8 v0, MkWord16X8 v1)
  joinShortVector (MkWord16X8 v0) (MkWord16X8 v1) = MkWord16X16WithVec128 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word32 where
  splitShortVector (MkWord32X16WithVec128 v0 v1 v2 v3) = (MkWord32X8WithVec128 v0 v1, MkWord32X8WithVec128 v2 v3)
  joinShortVector (MkWord32X8WithVec128 v0 v1) (MkWord32X8WithVec128 v2 v3) = MkWord32X16WithVec128 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word64 where
  splitShortVector (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = (MkWord64X8WithVec128 v0 v1 v2 v3, MkWord64X8WithVec128 v4 v5 v6 v7)
  joinShortVector (MkWord64X8WithVec128 v0 v1 v2 v3) (MkWord64X8WithVec128 v4 v5 v6 v7) = MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#else
-- The NCG of GHC 9.12 does not support integer vectors
instance SplitShortVector X16 Int8 where
  splitShortVector (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7, MkInt8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Int16 where
  splitShortVector (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7, MkInt16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Int32 where
  splitShortVector (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7, MkInt32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Int64 where
  splitShortVector (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7, MkInt64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word8 where
  splitShortVector (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7, MkWord8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word16 where
  splitShortVector (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7, MkWord16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word32 where
  splitShortVector (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7, MkWord32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X16 Word64 where
  splitShortVector (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7, MkWord64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15)
  joinShortVector (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#endif
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
  splitShortVector (MkFloatX32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = (MkFloatX16WithVec128 v0 v1 v2 v3, MkFloatX16WithVec128 v4 v5 v6 v7)
  joinShortVector (MkFloatX16WithVec128 v0 v1 v2 v3) (MkFloatX16WithVec128 v4 v5 v6 v7) = MkFloatX32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Double where
  splitShortVector (MkDoubleX32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) = (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7, MkDoubleX16WithVec128 v8 v9 v10 v11 v12 v13 v14 v15)
  joinShortVector (MkDoubleX16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) (MkDoubleX16WithVec128 v8 v9 v10 v11 v12 v13 v14 v15) = MkDoubleX32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)
instance SplitShortVector X32 Int8 where
  splitShortVector (MkInt8X32WithVec128 v0 v1) = (MkInt8X16 v0, MkInt8X16 v1)
  joinShortVector (MkInt8X16 v0) (MkInt8X16 v1) = MkInt8X32WithVec128 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Int16 where
  splitShortVector (MkInt16X32WithVec128 v0 v1 v2 v3) = (MkInt16X16WithVec128 v0 v1, MkInt16X16WithVec128 v2 v3)
  joinShortVector (MkInt16X16WithVec128 v0 v1) (MkInt16X16WithVec128 v2 v3) = MkInt16X32WithVec128 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Int32 where
  splitShortVector (MkInt32X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = (MkInt32X16WithVec128 v0 v1 v2 v3, MkInt32X16WithVec128 v4 v5 v6 v7)
  joinShortVector (MkInt32X16WithVec128 v0 v1 v2 v3) (MkInt32X16WithVec128 v4 v5 v6 v7) = MkInt32X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Int64 where
  splitShortVector (MkInt64X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) = (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7, MkInt64X16WithVec128 v8 v9 v10 v11 v12 v13 v14 v15)
  joinShortVector (MkInt64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) (MkInt64X16WithVec128 v8 v9 v10 v11 v12 v13 v14 v15) = MkInt64X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word8 where
  splitShortVector (MkWord8X32WithVec128 v0 v1) = (MkWord8X16 v0, MkWord8X16 v1)
  joinShortVector (MkWord8X16 v0) (MkWord8X16 v1) = MkWord8X32WithVec128 v0 v1
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word16 where
  splitShortVector (MkWord16X32WithVec128 v0 v1 v2 v3) = (MkWord16X16WithVec128 v0 v1, MkWord16X16WithVec128 v2 v3)
  joinShortVector (MkWord16X16WithVec128 v0 v1) (MkWord16X16WithVec128 v2 v3) = MkWord16X32WithVec128 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word32 where
  splitShortVector (MkWord32X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = (MkWord32X16WithVec128 v0 v1 v2 v3, MkWord32X16WithVec128 v4 v5 v6 v7)
  joinShortVector (MkWord32X16WithVec128 v0 v1 v2 v3) (MkWord32X16WithVec128 v4 v5 v6 v7) = MkWord32X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word64 where
  splitShortVector (MkWord64X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) = (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7, MkWord64X16WithVec128 v8 v9 v10 v11 v12 v13 v14 v15)
  joinShortVector (MkWord64X16WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) (MkWord64X16WithVec128 v8 v9 v10 v11 v12 v13 v14 v15) = MkWord64X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#else
-- The NCG of GHC 9.12 does not support integer vectors
instance SplitShortVector X32 Int8 where
  splitShortVector (MkInt8X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, MkInt8X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector (MkInt8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt8X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = MkInt8X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Int16 where
  splitShortVector (MkInt16X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, MkInt16X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector (MkInt16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt16X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = MkInt16X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Int32 where
  splitShortVector (MkInt32X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, MkInt32X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector (MkInt32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt32X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = MkInt32X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Int64 where
  splitShortVector (MkInt64X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, MkInt64X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector (MkInt64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkInt64X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = MkInt64X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word8 where
  splitShortVector (MkWord8X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, MkWord8X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector (MkWord8X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord8X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = MkWord8X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word16 where
  splitShortVector (MkWord16X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, MkWord16X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector (MkWord16X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord16X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = MkWord16X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word32 where
  splitShortVector (MkWord32X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, MkWord32X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector (MkWord32X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord32X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = MkWord32X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X32 Word64 where
  splitShortVector (MkWord64X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15, MkWord64X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31)
  joinShortVector (MkWord64X16WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (MkWord64X16WithElems x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) = MkWord64X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#endif
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
  splitShortVector (MkFloatX64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) = (MkFloatX32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7, MkFloatX32WithVec128 v8 v9 v10 v11 v12 v13 v14 v15)
  joinShortVector (MkFloatX32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) (MkFloatX32WithVec128 v8 v9 v10 v11 v12 v13 v14 v15) = MkFloatX64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Double where
  splitShortVector (MkDoubleX64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31) = (MkDoubleX32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15, MkDoubleX32WithVec128 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31)
  joinShortVector (MkDoubleX32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) (MkDoubleX32WithVec128 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31) = MkDoubleX64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)
instance SplitShortVector X64 Int8 where
  splitShortVector (MkInt8X64WithVec128 v0 v1 v2 v3) = (MkInt8X32WithVec128 v0 v1, MkInt8X32WithVec128 v2 v3)
  joinShortVector (MkInt8X32WithVec128 v0 v1) (MkInt8X32WithVec128 v2 v3) = MkInt8X64WithVec128 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Int16 where
  splitShortVector (MkInt16X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = (MkInt16X32WithVec128 v0 v1 v2 v3, MkInt16X32WithVec128 v4 v5 v6 v7)
  joinShortVector (MkInt16X32WithVec128 v0 v1 v2 v3) (MkInt16X32WithVec128 v4 v5 v6 v7) = MkInt16X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Int32 where
  splitShortVector (MkInt32X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) = (MkInt32X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7, MkInt32X32WithVec128 v8 v9 v10 v11 v12 v13 v14 v15)
  joinShortVector (MkInt32X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) (MkInt32X32WithVec128 v8 v9 v10 v11 v12 v13 v14 v15) = MkInt32X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Int64 where
  splitShortVector (MkInt64X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31) = (MkInt64X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15, MkInt64X32WithVec128 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31)
  joinShortVector (MkInt64X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) (MkInt64X32WithVec128 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31) = MkInt64X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word8 where
  splitShortVector (MkWord8X64WithVec128 v0 v1 v2 v3) = (MkWord8X32WithVec128 v0 v1, MkWord8X32WithVec128 v2 v3)
  joinShortVector (MkWord8X32WithVec128 v0 v1) (MkWord8X32WithVec128 v2 v3) = MkWord8X64WithVec128 v0 v1 v2 v3
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word16 where
  splitShortVector (MkWord16X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) = (MkWord16X32WithVec128 v0 v1 v2 v3, MkWord16X32WithVec128 v4 v5 v6 v7)
  joinShortVector (MkWord16X32WithVec128 v0 v1 v2 v3) (MkWord16X32WithVec128 v4 v5 v6 v7) = MkWord16X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word32 where
  splitShortVector (MkWord32X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) = (MkWord32X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7, MkWord32X32WithVec128 v8 v9 v10 v11 v12 v13 v14 v15)
  joinShortVector (MkWord32X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7) (MkWord32X32WithVec128 v8 v9 v10 v11 v12 v13 v14 v15) = MkWord32X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word64 where
  splitShortVector (MkWord64X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31) = (MkWord64X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15, MkWord64X32WithVec128 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31)
  joinShortVector (MkWord64X32WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15) (MkWord64X32WithVec128 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31) = MkWord64X64WithVec128 v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#else
-- The NCG of GHC 9.12 does not support integer vectors
instance SplitShortVector X64 Int8 where
  splitShortVector (MkInt8X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = (MkInt8X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31, MkInt8X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63)
  joinShortVector (MkInt8X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) (MkInt8X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = MkInt8X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Int16 where
  splitShortVector (MkInt16X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = (MkInt16X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31, MkInt16X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63)
  joinShortVector (MkInt16X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) (MkInt16X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = MkInt16X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Int32 where
  splitShortVector (MkInt32X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = (MkInt32X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31, MkInt32X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63)
  joinShortVector (MkInt32X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) (MkInt32X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = MkInt32X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Int64 where
  splitShortVector (MkInt64X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = (MkInt64X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31, MkInt64X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63)
  joinShortVector (MkInt64X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) (MkInt64X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = MkInt64X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word8 where
  splitShortVector (MkWord8X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = (MkWord8X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31, MkWord8X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63)
  joinShortVector (MkWord8X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) (MkWord8X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = MkWord8X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word16 where
  splitShortVector (MkWord16X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = (MkWord16X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31, MkWord16X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63)
  joinShortVector (MkWord16X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) (MkWord16X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = MkWord16X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word32 where
  splitShortVector (MkWord32X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = (MkWord32X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31, MkWord32X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63)
  joinShortVector (MkWord32X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) (MkWord32X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = MkWord32X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
instance SplitShortVector X64 Word64 where
  splitShortVector (MkWord64X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = (MkWord64X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31, MkWord64X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63)
  joinShortVector (MkWord64X32WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31) (MkWord64X32WithElems x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63) = MkWord64X64WithElems x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 x36 x37 x38 x39 x40 x41 x42 x43 x44 x45 x46 x47 x48 x49 x50 x51 x52 x53 x54 x55 x56 x57 x58 x59 x60 x61 x62 x63
  {-# INLINE splitShortVector #-}
  {-# INLINE joinShortVector #-}
#endif
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
