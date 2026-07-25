{-# LANGUAGE DerivingVia #-}
module Data.Simdy.Internal.Bits where
import qualified Data.Bits as B
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Int
import           Data.Word
import           Numeric.Natural

infixl 7 .&.
infixl 5 .|.
infixl 6 `xor`
infixl 8 `shiftL`, `shiftR`, `unsafeShiftL`, `unsafeShiftR`

-- | Bitwise logic operations.
--
-- Instances are provided for standard integral types (via "Data.Bits"),
-- 'Data.Functor.Identity.Identity', and SIMD vector types.
class Boolean a where
  -- | Bitwise AND.
  (.&.) :: a -> a -> a
  -- | Bitwise OR.
  (.|.) :: a -> a -> a
  -- | Bitwise XOR.
  xor :: a -> a -> a
  -- | Bitwise complement (flip all bits).
  complement :: a -> a

-- | Bitwise shift operations.
class Boolean a => BitShift a where
  -- | Shift left by the given number of bits.
  shiftL :: a -> Int -> a
  -- | Shift left without bounds checking.
  unsafeShiftL :: a -> Int -> a
  -- | Shift right by the given number of bits.
  shiftR :: a -> Int -> a
  -- | Shift right without bounds checking.
  unsafeShiftR :: a -> Int -> a
  -- The remaining "Data.Bits" methods are not implemented yet:
  -- shift :: a -> Int -> a
  -- zeroBits :: a
  -- bit :: Int -> a
  -- setBit :: a -> Int -> a
  -- clearBit :: a -> Int -> a
  -- complementBit :: a -> Int -> a
  -- testBit :: a -> Int -> Mask a
  -- bitSizeMaybe :: a -> Maybe Int
  -- isSigned :: a -> Bool
  -- rotateL :: a -> Int -> a
  -- rotateR :: a -> Int -> a
  -- popCount :: a -> Xn Int

-- class BitShift b => MiniFiniteBits b where
--   finiteBitSize :: b -> Int
--   countLeadingZeros :: b -> Xn Int
--   countTrailingZeros :: b -> Xn Int

newtype StandardBits a = MkStandardBits a

instance B.Bits a => Boolean (StandardBits a) where
  (.&.) = coerce ((B..&.) @a)
  (.|.) = coerce ((B..|.) @a)
  xor = coerce (B.xor @a)
  complement = coerce (B.complement @a)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}

instance B.Bits a => BitShift (StandardBits a) where
  shiftL = coerce (B.shiftL @a)
  unsafeShiftL = coerce (B.unsafeShiftL @a)
  shiftR = coerce (B.shiftR @a)
  unsafeShiftR = coerce (B.unsafeShiftR @a)
  {-# INLINE shiftL #-}
  {-# INLINE unsafeShiftL #-}
  {-# INLINE shiftR #-}
  {-# INLINE unsafeShiftR #-}

deriving via StandardBits Bool instance Boolean Bool
deriving via StandardBits Int instance Boolean Int
deriving via StandardBits Int8 instance Boolean Int8
deriving via StandardBits Int16 instance Boolean Int16
deriving via StandardBits Int32 instance Boolean Int32
deriving via StandardBits Int64 instance Boolean Int64
deriving via StandardBits Word instance Boolean Word
deriving via StandardBits Word8 instance Boolean Word8
deriving via StandardBits Word16 instance Boolean Word16
deriving via StandardBits Word32 instance Boolean Word32
deriving via StandardBits Word64 instance Boolean Word64
deriving via StandardBits Integer instance Boolean Integer
deriving via StandardBits Natural instance Boolean Natural

deriving newtype instance Boolean a => Boolean (Identity a)

deriving via StandardBits Int instance BitShift Int
deriving via StandardBits Int8 instance BitShift Int8
deriving via StandardBits Int16 instance BitShift Int16
deriving via StandardBits Int32 instance BitShift Int32
deriving via StandardBits Int64 instance BitShift Int64
deriving via StandardBits Word instance BitShift Word
deriving via StandardBits Word8 instance BitShift Word8
deriving via StandardBits Word16 instance BitShift Word16
deriving via StandardBits Word32 instance BitShift Word32
deriving via StandardBits Word64 instance BitShift Word64
deriving via StandardBits Integer instance BitShift Integer
deriving via StandardBits Natural instance BitShift Natural

deriving newtype instance BitShift a => BitShift (Identity a)
