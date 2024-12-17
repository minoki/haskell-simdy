{-# LANGUAGE QuantifiedConstraints #-}
module Data.Simdy.Internal.SIMD128
  (module M
  ,ShortVector
  ,ShortVectorElement
  ,MultiNum
  ,MultiFractional
  ,MultiFloating
  ,MultiPrim
  ,MultiUnbox
  ,MultiStorable
  ) where
import Data.Simdy.Class
import Data.Simdy.Internal.SIMD128.X2 as M
import Data.Simdy.Internal.SIMD128.X4 as M
import Data.Simdy.Internal.SIMD128.X8 as M
import Data.Simdy.Internal.SIMD128.X16 as M
import Data.Simdy.Internal.SIMD128.X32 as M
import Data.Int
import Data.Word
import Data.Semigroup
import Data.Complex
import Data.Functor.Identity
import qualified Data.Vector.Unboxed as VU
import Data.Primitive
import Foreign.Storable

class (PackX2 X2 a
      ,UnpackX2 X2 a
      ,PackX4 X4 a
      ,UnpackX4 X4 a
      ,PackX8 X8 a
      ,PackX8 X8 a
      ,UnpackX16 X16 a
      ,UnpackX16 X16 a
      ,PackX32 X32 a
      ,UnpackX32 X32 a
      ,Broadcast X2 a
      ,Broadcast X4 a
      ,Broadcast X8 a
      ,Broadcast X16 a
      ,Broadcast X32 a
      {-
      ,MonoMap X2 a
      ,MonoMap X4 a
      ,MonoMap X8 a
      ,MonoMap X16 a
      ,MonoMap X32 a
      ,MonoZipWith X2 a
      ,MonoZipWith X4 a
      ,MonoZipWith X8 a
      ,MonoZipWith X16 a
      ,MonoZipWith X32 a
      -}
      ,SplitShortVector X2 a
      ,SplitShortVector X4 a
      ,SplitShortVector X8 a
      ,SplitShortVector X16 a
      ,SplitShortVector X32 a
      ) => ShortVectorElement a
instance ShortVectorElement Float
instance ShortVectorElement Double
instance ShortVectorElement Int8
instance ShortVectorElement Int16
instance ShortVectorElement Int32
instance ShortVectorElement Int64
instance ShortVectorElement Word8
instance ShortVectorElement Word16
instance ShortVectorElement Word32
instance ShortVectorElement Word64
instance ShortVectorElement a => ShortVectorElement (Sum a)
instance ShortVectorElement a => ShortVectorElement (Product a)
instance ShortVectorElement a => ShortVectorElement (Min a)
instance ShortVectorElement a => ShortVectorElement (Max a)
instance ShortVectorElement a => ShortVectorElement (Complex a)
instance ShortVectorElement ()
instance (ShortVectorElement a0, ShortVectorElement a1) => ShortVectorElement (a0, a1)
instance (ShortVectorElement a0, ShortVectorElement a1, ShortVectorElement a2) => ShortVectorElement (a0, a1, a2)
instance (ShortVectorElement a0, ShortVectorElement a1, ShortVectorElement a2, ShortVectorElement a3) => ShortVectorElement (a0, a1, a2, a3)
instance (ShortVectorElement a0, ShortVectorElement a1, ShortVectorElement a2, ShortVectorElement a3, ShortVectorElement a4) => ShortVectorElement (a0, a1, a2, a3, a4)
instance (ShortVectorElement a0, ShortVectorElement a1, ShortVectorElement a2, ShortVectorElement a3, ShortVectorElement a4, ShortVectorElement a5) => ShortVectorElement (a0, a1, a2, a3, a4, a5)

class (Num a
      ,NumF X2 a
      ,NumF X4 a
      ,NumF X8 a
      ,NumF X16 a
      ,NumF X32 a
      ,ShortVectorElement a
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

class (Fractional a
      ,FractionalF X2 a
      ,FractionalF X4 a
      ,FractionalF X8 a
      ,FractionalF X16 a
      ,FractionalF X32 a
      ,MultiNum a
      ) => MultiFractional a
instance MultiFractional Float
instance MultiFractional Double
-- instance (RealFloat a, MultiFractional a) => MultiFractional (Complex a)

class (Floating a
      ,FloatingF X2 a
      ,FloatingF X4 a
      ,FloatingF X8 a
      ,FloatingF X16 a
      ,FloatingF X32 a
      ,MultiFractional a
      ) => MultiFloating a
instance MultiFloating Float
instance MultiFloating Double
-- instance (RealFloat a, MultiFloating a) => MultiFloating (Complex a)

class (Prim a
      ,PrimSV X2 a
      ,PrimSV X4 a
      ,PrimSV X8 a
      ,PrimSV X16 a
      ,PrimSV X32 a
      ,ShortVectorElement a
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

class (VU.Unbox a
      ,UnboxSV X2 a
      ,UnboxSV X4 a
      ,UnboxSV X8 a
      ,UnboxSV X16 a
      ,UnboxSV X32 a
      ,ShortVectorElement a
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

class (Storable a
      ,StorableSV X2 a
      ,StorableSV X4 a
      ,StorableSV X8 a
      ,StorableSV X16 a
      ,StorableSV X32 a
      ,ShortVectorElement a
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

class (ShortVectorLength f
      ,forall a. ShortVectorElement a => Broadcast f a
      ,forall a. MultiNum a => Num (f a)
      ,forall a. MultiFractional a => Fractional (f a)
      ,forall a. MultiFloating a => Floating (f a)
      ) => ShortVector f
instance ShortVector Identity
instance ShortVector X2
instance ShortVector X4
instance ShortVector X8
instance ShortVector X16
instance ShortVector X32
