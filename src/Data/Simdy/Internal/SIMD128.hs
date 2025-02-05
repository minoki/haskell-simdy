{-|
This module contains types and classes that use 128-bit vectors (x86 SSE, Arm NEON).

In general, the types and classes exported from this module are not compatible with other modules with different vector lengths (i.e. "Data.Simdy.Internal.NoSIMD", "Data.Simdy.Internal.SIMD256", "Data.Simdy.Internal.SIMD512").
-}
{-# LANGUAGE QuantifiedConstraints #-}
module Data.Simdy.Internal.SIMD128
  ( module M
  , SIMD (horizontalFold)
  , SIMDElement
  , broadcast
  , liftSIMD
  , liftSIMD2
  , SIMDEq
  , SIMDOrd
  , SIMDNum
  , SIMDFractional
  , SIMDFloating
  , SIMDPrim
  , SIMDUnbox
  , SIMDStorable
  ) where
import           Data.Complex
import           Data.Functor.Identity
import           Data.Int
import           Data.Primitive
import           Data.Semigroup
import           Data.Simdy.Internal.Class hiding (broadcast, liftSIMD, liftSIMD2)
import qualified Data.Simdy.Internal.Class as I
import           Data.Simdy.Internal.SIMD128.HalfVector ()
import           Data.Simdy.Internal.SIMD128.X16 as M
import           Data.Simdy.Internal.SIMD128.X2 as M
import           Data.Simdy.Internal.SIMD128.X32 as M
import           Data.Simdy.Internal.SIMD128.X4 as M
import           Data.Simdy.Internal.SIMD128.X8 as M
import qualified Data.Vector.Unboxed as VU
import           Data.Word
import           Foreign.Storable

-- | An instance of 'SIMDElement' supports basic SIMD operations (pack\/unpack\/broadcast)
class ( PackX2 X2 a
      , PackX4 X4 a
      , PackX8 X8 a
      , PackX16 X16 a
      , PackX32 X32 a
      , Broadcast X2 a
      , Broadcast X4 a
      , Broadcast X8 a
      , Broadcast X16 a
      , Broadcast X32 a
      , SplitShortVector X2 a
      , SplitShortVector X4 a
      , SplitShortVector X8 a
      , SplitShortVector X16 a
      , SplitShortVector X32 a
      , SelectableF X2 a
      , SelectableF X4 a
      , SelectableF X8 a
      , SelectableF X16 a
      , SelectableF X32 a
      ) => SIMDElement a
instance SIMDElement Bool
instance SIMDElement Float
instance SIMDElement Double
instance SIMDElement Int8
instance SIMDElement Int16
instance SIMDElement Int32
instance SIMDElement Int64
instance SIMDElement Word8
instance SIMDElement Word16
instance SIMDElement Word32
instance SIMDElement Word64
instance SIMDElement a => SIMDElement (Sum a)
instance SIMDElement a => SIMDElement (Product a)
instance SIMDElement a => SIMDElement (Min a)
instance SIMDElement a => SIMDElement (Max a)
instance SIMDElement a => SIMDElement (Complex a)
instance SIMDElement ()
instance (SIMDElement a0, SIMDElement a1) => SIMDElement (a0, a1)
instance (SIMDElement a0, SIMDElement a1, SIMDElement a2) => SIMDElement (a0, a1, a2)
instance (SIMDElement a0, SIMDElement a1, SIMDElement a2, SIMDElement a3) => SIMDElement (a0, a1, a2, a3)
instance (SIMDElement a0, SIMDElement a1, SIMDElement a2, SIMDElement a3, SIMDElement a4) => SIMDElement (a0, a1, a2, a3, a4)
instance (SIMDElement a0, SIMDElement a1, SIMDElement a2, SIMDElement a3, SIMDElement a4, SIMDElement a5) => SIMDElement (a0, a1, a2, a3, a4, a5)

-- | Vectors whose element type is an instance of 'SIMDEq' can be compared using 'Equatable' class
--
-- @('SIMD' f, 'SIMDEq' a)@ implies @'Equatable' (f a)@.
class ( Eq a
      , SIMDElement a
      , EquatableF X2 a
      , EquatableF X4 a
      , EquatableF X8 a
      , EquatableF X16 a
      , EquatableF X32 a
      ) => SIMDEq a

-- | Vectors whose element type is an instance of 'SIMDOrd' can be compared using 'Ordered' class
--
-- @('SIMD' f, 'SIMDOrd' a)@ implies @'Ordered' (f a)@.
class ( Ord a
      , SIMDElement a
      , OrderedF X2 a
      , OrderedF X4 a
      , OrderedF X8 a
      , OrderedF X16 a
      , OrderedF X32 a
      ) => SIMDOrd a

-- | An instance of 'SIMDNum' has its 'Num' instance lifted to SIMD vector types
--
-- @('SIMD' f, 'SIMDNum' a)@ implies @'Num' (f a)@.
class ( Num a
      , SIMDElement a
      , NumF X2 a
      , NumF X4 a
      , NumF X8 a
      , NumF X16 a
      , NumF X32 a
      ) => SIMDNum a
instance SIMDNum Float
instance SIMDNum Double
instance SIMDNum Int8
instance SIMDNum Int16
instance SIMDNum Int32
instance SIMDNum Int64
instance SIMDNum Word8
instance SIMDNum Word16
instance SIMDNum Word32
instance SIMDNum Word64
-- instance (RealFloat a, SIMDNum a) => SIMDNum (Complex a)

-- | An instance of 'SIMDFractional' has its 'Fractional' instance lifted to SIMD vector types
--
-- @('SIMD' f, 'SIMDFractional' a)@ implies @'Fractional' (f a)@.
class ( Fractional a
      , SIMDNum a
      , FractionalF X2 a
      , FractionalF X4 a
      , FractionalF X8 a
      , FractionalF X16 a
      , FractionalF X32 a
      ) => SIMDFractional a
instance SIMDFractional Float
instance SIMDFractional Double
-- instance (RealFloat a, SIMDFractional a) => SIMDFractional (Complex a)

-- | An instance of 'SIMDFloating' has its 'Floating' instance lifted to SIMD vector types
--
-- @('SIMD' f, 'SIMDFloating' a)@ implies @'Floating' (f a)@.
class ( Floating a
      , SIMDFractional a
      , FloatingF X2 a
      , FloatingF X4 a
      , FloatingF X8 a
      , FloatingF X16 a
      , FloatingF X32 a
      ) => SIMDFloating a
instance SIMDFloating Float
instance SIMDFloating Double
-- instance (RealFloat a, SIMDFloating a) => SIMDFloating (Complex a)

class ( Prim a
      , SIMDElement a
      , PrimSIMD X2 a
      , PrimSIMD X4 a
      , PrimSIMD X8 a
      , PrimSIMD X16 a
      , PrimSIMD X32 a
      ) => SIMDPrim a
instance SIMDPrim Float
instance SIMDPrim Double
instance SIMDPrim Int8
instance SIMDPrim Int16
instance SIMDPrim Int32
instance SIMDPrim Int64
instance SIMDPrim Word8
instance SIMDPrim Word16
instance SIMDPrim Word32
instance SIMDPrim Word64

-- | An instance of 'SIMDUnbox' supports unboxed vectors
class ( VU.Unbox a
      , SIMDElement a
      , UnboxSIMD X2 a
      , UnboxSIMD X4 a
      , UnboxSIMD X8 a
      , UnboxSIMD X16 a
      , UnboxSIMD X32 a
      ) => SIMDUnbox a
instance SIMDUnbox Float
instance SIMDUnbox Double
instance SIMDUnbox Int8
instance SIMDUnbox Int16
instance SIMDUnbox Int32
instance SIMDUnbox Int64
instance SIMDUnbox Word8
instance SIMDUnbox Word16
instance SIMDUnbox Word32
instance SIMDUnbox Word64
instance SIMDUnbox a => SIMDUnbox (Sum a)
instance SIMDUnbox a => SIMDUnbox (Product a)
instance SIMDUnbox a => SIMDUnbox (Min a)
instance SIMDUnbox a => SIMDUnbox (Max a)
instance SIMDUnbox a => SIMDUnbox (Complex a)
instance SIMDUnbox ()
instance (SIMDUnbox a0, SIMDUnbox a1) => SIMDUnbox (a0, a1)
instance (SIMDUnbox a0, SIMDUnbox a1, SIMDUnbox a2) => SIMDUnbox (a0, a1, a2)
instance (SIMDUnbox a0, SIMDUnbox a1, SIMDUnbox a2, SIMDUnbox a3) => SIMDUnbox (a0, a1, a2, a3)
instance (SIMDUnbox a0, SIMDUnbox a1, SIMDUnbox a2, SIMDUnbox a3, SIMDUnbox a4) => SIMDUnbox (a0, a1, a2, a3, a4)
instance (SIMDUnbox a0, SIMDUnbox a1, SIMDUnbox a2, SIMDUnbox a3, SIMDUnbox a4, SIMDUnbox a5) => SIMDUnbox (a0, a1, a2, a3, a4, a5)

class ( Storable a
      , SIMDElement a
      , StorableSIMD X2 a
      , StorableSIMD X4 a
      , StorableSIMD X8 a
      , StorableSIMD X16 a
      , StorableSIMD X32 a
      ) => SIMDStorable a
instance SIMDStorable Float
instance SIMDStorable Double
instance SIMDStorable Int8
instance SIMDStorable Int16
instance SIMDStorable Int32
instance SIMDStorable Int64
instance SIMDStorable Word8
instance SIMDStorable Word16
instance SIMDStorable Word32
instance SIMDStorable Word64

-- | SIMD vector types
class ( KnownSIMDLength f
      , forall a. SIMDElement a => Broadcast f a
      , forall a b. (SIMDElement a, SIMDElement b) => LiftSIMD f a b
      , forall a b c. (SIMDElement a, SIMDElement b, SIMDElement c) => LiftSIMD2 f a b c
      , Boolean (f Bool)
      , forall a. SIMDElement a => SelectableF f a
      , forall a. SIMDElement a => Selectable (f a)
      , forall a. SIMDEq a => EquatableF f a
      , forall a. SIMDEq a => Equatable (f a)
      , forall a. SIMDOrd a => OrderedF f a
      , forall a. SIMDOrd a => Ordered (f a)
      , forall a. SIMDNum a => Num (f a)
      , forall a. SIMDFractional a => Fractional (f a)
      , forall a. SIMDFloating a => Floating (f a)
      ) => SIMD f where
  horizontalFold :: SIMDElement a => (forall g. SIMD g => g a -> g a -> g a) -> f a -> a
instance SIMD Identity where
  horizontalFold _ = runIdentity
  {-# INLINE horizontalFold #-}
instance SIMD X2 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> runIdentity (op low high)
  {-# INLINE horizontalFold #-}
instance SIMD X4 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> horizontalFold op (op low high)
  {-# INLINE horizontalFold #-}
instance SIMD X8 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> horizontalFold op (op low high)
  {-# INLINE horizontalFold #-}
instance SIMD X16 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> horizontalFold op (op low high)
  {-# INLINE horizontalFold #-}
instance SIMD X32 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> horizontalFold op (op low high)
  {-# INLINE horizontalFold #-}

-- | Broadcasts a value to the entire vector.
--
-- Conceptually, @'broadcast' x = packXN x x x ... x@.
broadcast :: (SIMD f, SIMDElement a) => a -> f a
broadcast = I.broadcast
{-# INLINE broadcast #-}

-- | Lifts a unary function to the vector.
--
-- In general, the resulting function does not use SIMD instructions.
liftSIMD :: (SIMD f, SIMDElement a, SIMDElement b) => (a -> b) -> f a -> f b
liftSIMD = I.liftSIMD
{-# INLINE liftSIMD #-}

-- | Lifts a binary function to the vector.
--
-- In general, the resulting function does not use SIMD instructions.
liftSIMD2 :: (SIMD f, SIMDElement a, SIMDElement b, SIMDElement c) => (a -> b -> c) -> f a -> f b -> f c
liftSIMD2 = I.liftSIMD2
{-# INLINE liftSIMD2 #-}
