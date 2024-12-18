{-# LANGUAGE QuantifiedConstraints #-}
module Data.Simdy.Internal.SIMD128
  ( module M
  , SIMD
  , SIMDElement
  , MultiNum
  , MultiFractional
  , MultiFloating
  , MultiPrim
  , MultiUnbox
  , MultiStorable
  ) where
import           Data.Complex
import           Data.Functor.Identity
import           Data.Int
import           Data.Primitive
import           Data.Semigroup
import           Data.Simdy.Class
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
      , UnpackX2 X2 a
      , PackX4 X4 a
      , UnpackX4 X4 a
      , PackX8 X8 a
      , PackX8 X8 a
      , UnpackX16 X16 a
      , UnpackX16 X16 a
      , PackX32 X32 a
      , UnpackX32 X32 a
      , Broadcast X2 a
      , Broadcast X4 a
      , Broadcast X8 a
      , Broadcast X16 a
      , Broadcast X32 a
      {-
      , MonoMap X2 a
      , MonoMap X4 a
      , MonoMap X8 a
      , MonoMap X16 a
      , MonoMap X32 a
      , MonoZipWith X2 a
      , MonoZipWith X4 a
      , MonoZipWith X8 a
      , MonoZipWith X16 a
      , MonoZipWith X32 a
      -}
      , SplitShortVector X2 a
      , SplitShortVector X4 a
      , SplitShortVector X8 a
      , SplitShortVector X16 a
      , SplitShortVector X32 a
      ) => SIMDElement a
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

-- | An instance of 'MultiNum' has its 'Num' instance lifted to SIMD vector types
--
-- @('SIMD' f, 'MultiNum' a)@ implies @'Num' (f a)@.
class ( Num a
      , NumF X2 a
      , NumF X4 a
      , NumF X8 a
      , NumF X16 a
      , NumF X32 a
      , SIMDElement a
      ) => MultiNum a
instance MultiNum Float
instance MultiNum Double
instance MultiNum Int8
instance MultiNum Int16
instance MultiNum Int32
instance MultiNum Int64
instance MultiNum Word8
instance MultiNum Word16
instance MultiNum Word32
instance MultiNum Word64
-- instance (RealFloat a, MultiNum a) => MultiNum (Complex a)

-- | An instance of 'MultiFractional' has its 'Fractional' instance lifted to SIMD vector types
--
-- @('SIMD' f, 'MultiFractional' a)@ implies @'Fractional' (f a)@.
class ( Fractional a
      , FractionalF X2 a
      , FractionalF X4 a
      , FractionalF X8 a
      , FractionalF X16 a
      , FractionalF X32 a
      , MultiNum a
      ) => MultiFractional a
instance MultiFractional Float
instance MultiFractional Double
-- instance (RealFloat a, MultiFractional a) => MultiFractional (Complex a)

-- | An instance of 'MultiFloating' has its 'Floating' instance lifted to SIMD vector types
--
-- @('SIMD' f, 'MultiFloating' a)@ implies @'Floating' (f a)@.
class ( Floating a
      , FloatingF X2 a
      , FloatingF X4 a
      , FloatingF X8 a
      , FloatingF X16 a
      , FloatingF X32 a
      , MultiFractional a
      ) => MultiFloating a
instance MultiFloating Float
instance MultiFloating Double
-- instance (RealFloat a, MultiFloating a) => MultiFloating (Complex a)

class ( Prim a
      , PrimSIMD X2 a
      , PrimSIMD X4 a
      , PrimSIMD X8 a
      , PrimSIMD X16 a
      , PrimSIMD X32 a
      , SIMDElement a
      ) => MultiPrim a
instance MultiPrim Float
instance MultiPrim Double
instance MultiPrim Int8
instance MultiPrim Int16
instance MultiPrim Int32
instance MultiPrim Int64
instance MultiPrim Word8
instance MultiPrim Word16
instance MultiPrim Word32
instance MultiPrim Word64

-- | An instance of 'MultiUnbox' supports unboxed vectors
class ( VU.Unbox a
      , UnboxSIMD X2 a
      , UnboxSIMD X4 a
      , UnboxSIMD X8 a
      , UnboxSIMD X16 a
      , UnboxSIMD X32 a
      , SIMDElement a
      ) => MultiUnbox a
instance MultiUnbox Float
instance MultiUnbox Double
instance MultiUnbox Int8
instance MultiUnbox Int16
instance MultiUnbox Int32
instance MultiUnbox Int64
instance MultiUnbox Word8
instance MultiUnbox Word16
instance MultiUnbox Word32
instance MultiUnbox Word64
instance MultiUnbox a => MultiUnbox (Sum a)
instance MultiUnbox a => MultiUnbox (Product a)
instance MultiUnbox a => MultiUnbox (Min a)
instance MultiUnbox a => MultiUnbox (Max a)
instance MultiUnbox a => MultiUnbox (Complex a)
instance MultiUnbox ()
instance (MultiUnbox a0, MultiUnbox a1) => MultiUnbox (a0, a1)
instance (MultiUnbox a0, MultiUnbox a1, MultiUnbox a2) => MultiUnbox (a0, a1, a2)
instance (MultiUnbox a0, MultiUnbox a1, MultiUnbox a2, MultiUnbox a3) => MultiUnbox (a0, a1, a2, a3)
instance (MultiUnbox a0, MultiUnbox a1, MultiUnbox a2, MultiUnbox a3, MultiUnbox a4) => MultiUnbox (a0, a1, a2, a3, a4)
instance (MultiUnbox a0, MultiUnbox a1, MultiUnbox a2, MultiUnbox a3, MultiUnbox a4, MultiUnbox a5) => MultiUnbox (a0, a1, a2, a3, a4, a5)

class ( Storable a
      , StorableSIMD X2 a
      , StorableSIMD X4 a
      , StorableSIMD X8 a
      , StorableSIMD X16 a
      , StorableSIMD X32 a
      , SIMDElement a
      ) => MultiStorable a
instance MultiStorable Float
instance MultiStorable Double
instance MultiStorable Int8
instance MultiStorable Int16
instance MultiStorable Int32
instance MultiStorable Int64
instance MultiStorable Word8
instance MultiStorable Word16
instance MultiStorable Word32
instance MultiStorable Word64

-- | SIMD vector types
class ( ShortVectorLength f
      , forall a. SIMDElement a => Broadcast f a
      , forall a. MultiNum a => Num (f a)
      , forall a. MultiFractional a => Fractional (f a)
      , forall a. MultiFloating a => Floating (f a)
      ) => SIMD f
instance SIMD Identity
instance SIMD X2
instance SIMD X4
instance SIMD X8
instance SIMD X16
instance SIMD X32
