{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Data.Simdy.Vector.Unboxed where
import           Data.Coerce (coerce)
import           Data.Complex (Complex)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Monoid (Product, Sum)
import           Data.Semigroup (Max, Min)
import           Data.Simdy.Internal.Class (EnumFromZero, LiftConstructor)
import           Data.Simdy.Internal.Default
import           Data.Simdy.Vector.Class (MultiUnbox)
import qualified Data.Simdy.Vector.Generic as G
import qualified Data.Vector.Unboxed as VU
import           Data.Word (Word16, Word32, Word64, Word8)

class (forall f. SIMD f => MultiUnbox f a, VU.Unbox a) => SIMDUnbox a
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
instance (SIMDUnbox a, SIMDUnbox b) => SIMDUnbox (a, b)
instance (SIMDUnbox a, SIMDUnbox b, SIMDUnbox c) => SIMDUnbox (a, b, c)
instance (SIMDUnbox a, SIMDUnbox b, SIMDUnbox c, SIMDUnbox d) => SIMDUnbox (a, b, c, d)
instance (SIMDUnbox a, SIMDUnbox b, SIMDUnbox c, SIMDUnbox d, SIMDUnbox e) => SIMDUnbox (a, b, c, d, e)
instance (SIMDUnbox a, SIMDUnbox b, SIMDUnbox c, SIMDUnbox d, SIMDUnbox e, SIMDUnbox f) => SIMDUnbox (a, b, c, d, e, f)

indexedX :: forall x i a. (SIMD x, MultiUnbox x a, MultiUnbox x i, EnumFromZero x i, LiftConstructor x) => VU.Vector a -> VU.Vector (i, a)
indexedX = G.indexed @x
{-# INLINE indexedX #-}

mapX :: forall x a b. (SIMD x, MultiUnbox x a, MultiUnbox x b) => (x a -> x b) -> (a -> b) -> VU.Vector a -> VU.Vector b
mapX = G.map
{-# INLINE mapX #-}

mapX2 :: (MultiUnbox X2 a, MultiUnbox X2 b) => (forall f. SIMD f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX2 f = mapX @X2 f (coerce (f @Identity))
{-# INLINE mapX2 #-}

mapX4 :: (MultiUnbox X4 a, MultiUnbox X4 b) => (forall f. SIMD f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX4 f = mapX @X4 f (coerce (f @Identity))
{-# INLINE mapX4 #-}

mapX8 :: (MultiUnbox X8 a, MultiUnbox X8 b) => (forall f. SIMD f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX8 f = mapX @X8 f (coerce (f @Identity))
{-# INLINE mapX8 #-}

mapX16 :: (MultiUnbox X16 a, MultiUnbox X16 b) => (forall f. SIMD f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX16 f = mapX @X16 f (coerce (f @Identity))
{-# INLINE mapX16 #-}

mapX32 :: (MultiUnbox X32 a, MultiUnbox X32 b) => (forall f. SIMD f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX32 f = mapX @X32 f (coerce (f @Identity))
{-# INLINE mapX32 #-}

imapX :: forall x i a b. (SIMD x, MultiUnbox x a, MultiUnbox x b, EnumFromZero x i, LiftConstructor x) => (x i -> x a -> x b) -> (i -> a -> b) -> VU.Vector a -> VU.Vector b
imapX = G.imap
{-# INLINE imapX #-}

imapX2 :: (MultiUnbox X2 a, MultiUnbox X2 b, EnumFromZero X2 i) => (forall f. SIMD f => f i -> f a -> f b) -> VU.Vector a -> VU.Vector b
imapX2 f = imapX @X2 f (coerce (f @Identity))
{-# INLINE imapX2 #-}

imapX4 :: (MultiUnbox X4 a, MultiUnbox X4 b, EnumFromZero X4 i) => (forall f. SIMD f => f i -> f a -> f b) -> VU.Vector a -> VU.Vector b
imapX4 f = imapX @X4 f (coerce (f @Identity))
{-# INLINE imapX4 #-}

imapX8 :: (MultiUnbox X8 a, MultiUnbox X8 b, EnumFromZero X8 i) => (forall f. SIMD f => f i -> f a -> f b) -> VU.Vector a -> VU.Vector b
imapX8 f = imapX @X8 f (coerce (f @Identity))
{-# INLINE imapX8 #-}

imapX16 :: (MultiUnbox X16 a, MultiUnbox X16 b, EnumFromZero X16 i) => (forall f. SIMD f => f i -> f a -> f b) -> VU.Vector a -> VU.Vector b
imapX16 f = imapX @X16 f (coerce (f @Identity))
{-# INLINE imapX16 #-}

imapX32 :: (MultiUnbox X32 a, MultiUnbox X32 b, EnumFromZero X32 i) => (forall f. SIMD f => f i -> f a -> f b) -> VU.Vector a -> VU.Vector b
imapX32 f = imapX @X32 f (coerce (f @Identity))
{-# INLINE imapX32 #-}

zipWithX :: forall x a b c. (SIMD x, MultiUnbox x a, MultiUnbox x b, MultiUnbox x c, LiftConstructor x) => (x a -> x b -> x c) -> (a -> b -> c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX = G.zipWith
{-# INLINE zipWithX #-}

zipWithX2 :: (MultiUnbox X2 a, MultiUnbox X2 b, MultiUnbox X2 c) => (forall f. SIMD f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX2 f = zipWithX @X2 f (coerce (f @Identity))
{-# INLINE zipWithX2 #-}

zipWithX4 :: (MultiUnbox X4 a, MultiUnbox X4 b, MultiUnbox X4 c) => (forall f. SIMD f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX4 f = zipWithX @X4 f (coerce (f @Identity))
{-# INLINE zipWithX4 #-}

zipWithX8 :: (MultiUnbox X8 a, MultiUnbox X8 b, MultiUnbox X8 c) => (forall f. SIMD f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX8 f = zipWithX @X8 f (coerce (f @Identity))
{-# INLINE zipWithX8 #-}

zipWithX16 :: (MultiUnbox X16 a, MultiUnbox X16 b, MultiUnbox X16 c) => (forall f. SIMD f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX16 f = zipWithX @X16 f (coerce (f @Identity))
{-# INLINE zipWithX16 #-}

zipWithX32 :: (MultiUnbox X32 a, MultiUnbox X32 b, MultiUnbox X32 c) => (forall f. SIMD f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX32 f = zipWithX @X32 f (coerce (f @Identity))
{-# INLINE zipWithX32 #-}

izipWithX :: forall x i a b c. (SIMD x, MultiUnbox x a, MultiUnbox x b, MultiUnbox x c, EnumFromZero x i, LiftConstructor x) => (x i -> x a -> x b -> x c) -> (i -> a -> b -> c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
izipWithX = G.izipWith
{-# INLINE izipWithX #-}

izipWithX2 :: (MultiUnbox X2 a, MultiUnbox X2 b, MultiUnbox X2 c, EnumFromZero X2 i) => (forall f. SIMD f => f i -> f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
izipWithX2 f = izipWithX @X2 f (coerce (f @Identity))
{-# INLINE izipWithX2 #-}

izipWithX4 :: (MultiUnbox X4 a, MultiUnbox X4 b, MultiUnbox X4 c, EnumFromZero X4 i) => (forall f. SIMD f => f i -> f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
izipWithX4 f = izipWithX @X4 f (coerce (f @Identity))
{-# INLINE izipWithX4 #-}

izipWithX8 :: (MultiUnbox X8 a, MultiUnbox X8 b, MultiUnbox X8 c, EnumFromZero X8 i) => (forall f. SIMD f => f i -> f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
izipWithX8 f = izipWithX @X8 f (coerce (f @Identity))
{-# INLINE izipWithX8 #-}

izipWithX16 :: (MultiUnbox X16 a, MultiUnbox X16 b, MultiUnbox X16 c, EnumFromZero X16 i) => (forall f. SIMD f => f i -> f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
izipWithX16 f = izipWithX @X16 f (coerce (f @Identity))
{-# INLINE izipWithX16 #-}

izipWithX32 :: (MultiUnbox X32 a, MultiUnbox X32 b, MultiUnbox X32 c, EnumFromZero X32 i) => (forall f. SIMD f => f i -> f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
izipWithX32 f = izipWithX @X32 f (coerce (f @Identity))
{-# INLINE izipWithX32 #-}
