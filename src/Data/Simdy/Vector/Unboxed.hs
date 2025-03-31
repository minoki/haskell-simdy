{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Simdy.Vector.Unboxed where
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Simdy.Internal.Class (LiftConstructor)
import           Data.Simdy.Internal.Default
import           Data.Simdy.Vector.Class (MultiUnbox)
import qualified Data.Simdy.Vector.Generic as G
import qualified Data.Vector.Unboxed as VU
-- import qualified Data.Vector.Unboxed.Mutable as VUM

type SIMDUnbox a = (VU.Unbox a, MultiUnbox X2 a, MultiUnbox X4 a, MultiUnbox X8 a, MultiUnbox X16 a, MultiUnbox X32 a)

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
