{-# LANGUAGE DerivingVia #-}
module Data.Simdy.Class.Bits where
import qualified Data.Bits as B
import           Data.Int
import           Data.Word
import Data.Functor.Identity
import Numeric.Natural
import Data.Coerce

infixl 7 .&.
infixl 5 .|.
infixl 6 `xor`
infixl 8 `shiftL`, `shiftR`, `unsafeShiftL`, `unsafeShiftR`

class MiniBits a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  xor :: a -> a -> a
  complement :: a -> a
  shiftL :: a -> Int -> a
  unsafeShiftL :: a -> Int -> a
  shiftR :: a -> Int -> a
  unsafeShiftR :: a -> Int -> a
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

-- class MiniBits b => MiniFiniteBits b where
--   finiteBitSize :: b -> Int
--   countLeadingZeros :: b -> Xn Int
--   countTrailingZeros :: b -> Xn Int

newtype StandardBits a = MkStandardBits a

instance B.Bits a => MiniBits (StandardBits a) where
  (.&.) = coerce ((B..&.) @a)
  (.|.) = coerce ((B..|.) @a)
  xor = coerce (B.xor @a)
  complement = coerce (B.complement @a)
  shiftL = coerce (B.shiftL @a)
  unsafeShiftL = coerce (B.unsafeShiftL @a)
  shiftR = coerce (B.shiftR @a)
  unsafeShiftR = coerce (B.unsafeShiftR @a)

deriving via StandardBits Int instance MiniBits Int
deriving via StandardBits Int8 instance MiniBits Int8
deriving via StandardBits Int16 instance MiniBits Int16
deriving via StandardBits Int32 instance MiniBits Int32
deriving via StandardBits Int64 instance MiniBits Int64
deriving via StandardBits Word instance MiniBits Word
deriving via StandardBits Word8 instance MiniBits Word8
deriving via StandardBits Word16 instance MiniBits Word16
deriving via StandardBits Word32 instance MiniBits Word32
deriving via StandardBits Word64 instance MiniBits Word64
deriving via StandardBits Integer instance MiniBits Integer
deriving via StandardBits Natural instance MiniBits Natural

deriving newtype instance MiniBits a => MiniBits (Identity a)
