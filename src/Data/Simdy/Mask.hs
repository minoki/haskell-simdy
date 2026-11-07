-- |
-- Comparison and masking operations for SIMD vectors.
--
-- SIMD comparisons return a 'Mask' (a vector of booleans) rather than a single 'Bool'.
-- Use 'selectSIMD' to choose lanes based on a mask, or 'Boolean' operators to combine masks.
--
-- The @(==^)@, @(/=^)@, @(\<^)@, @(\<=^)@, @(>^)@, @(>=^)@ operators are
-- convenience wrappers that avoid name clashes with "Prelude".
module Data.Simdy.Mask
  ( -- * Boolean algebra on masks
    Boolean (..)
    -- * Mask type
  , Mask
    -- * Conditional selection
  , selectSIMD
  , Selectable (..)
    -- * Equality comparison
  , Equatable (..)
  , (==^)
  , (/=^)
    -- * Ordering comparison
  , Ordered (..)
  , (<^)
  , (<=^)
  , (>^)
  , (>=^)
    -- * Conversion from/to integral types
  , PackMask (..)
  , maskToWord8
  , maskFromWord8
  , Data.Simdy.Mask.maskToWord16
  , Data.Simdy.Mask.maskFromWord16
  , Data.Simdy.Mask.maskToWord32
  , Data.Simdy.Mask.maskFromWord32
  , Data.Simdy.Mask.maskToWord64
  , Data.Simdy.Mask.maskFromWord64
  ) where
import           Data.Simdy.Internal.Bits
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default
import           Data.Word (Word16, Word32, Word64)

maskToWord16 :: X16 Bool -> Word16
maskToWord16 = Data.Simdy.Internal.Class.maskToWord16
{-# INLINE maskToWord16 #-}

maskFromWord16 :: Word16 -> X16 Bool
maskFromWord16 = Data.Simdy.Internal.Class.maskFromWord16
{-# INLINE maskFromWord16 #-}

maskToWord32 :: X32 Bool -> Word32
maskToWord32 = Data.Simdy.Internal.Class.maskToWord32
{-# INLINE maskToWord32 #-}

maskFromWord32 :: Word32 -> X32 Bool
maskFromWord32 = Data.Simdy.Internal.Class.maskFromWord32
{-# INLINE maskFromWord32 #-}

maskToWord64 :: X64 Bool -> Word64
maskToWord64 = Data.Simdy.Internal.Class.maskToWord64
{-# INLINE maskToWord64 #-}

maskFromWord64 :: Word64 -> X64 Bool
maskFromWord64 = Data.Simdy.Internal.Class.maskFromWord64
{-# INLINE maskFromWord64 #-}
