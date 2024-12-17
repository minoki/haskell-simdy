{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
module Data.Simdy.Unboxed where
import Data.Simdy.Class
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Functor.Identity
import Data.Coerce (coerce)
#if defined(USE_SIMD512)
import Data.Simdy.Internal.SIMD512
#elif defined(USE_SIMD256)
import Data.Simdy.Internal.SIMD256
#elif defined(USE_SIMD128)
import Data.Simdy.Internal.SIMD128
#else
import Data.Simdy.Internal.NoSIMD
#endif

mapX :: forall m a b. (SIMD m, VU.Unbox a, UnboxSIMD m a, VU.Unbox b, UnboxSIMD m b) => (m a -> m b) -> (a -> b) -> VU.Vector a -> VU.Vector b
mapX fv fs !v = VU.create $ do
  let !n = VU.length v
      !m = shortVectorLength @m
  !result <- VUM.unsafeNew n
  let goVector !i = if i + m <= n
                    then
                      do let !s = unsafeIndexUnboxedSIMD @m v i
                         unsafeWriteUnboxedSIMD result i (fv s)
                         goVector (i + m)
                    else
                      goScalar i
      goScalar !i | i < n = do let !x = VU.unsafeIndex v i
                               VUM.unsafeWrite result i (fs x)
                               goScalar (i + 1)
                  | otherwise = pure ()
  goVector 0
  pure result

mapX2 :: (MultiUnbox a, MultiUnbox b) => (forall f. SIMD f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX2 f = mapX @X2 f (coerce (f @Identity))
{-# INLINE mapX2 #-}

mapX4 :: (MultiUnbox a, MultiUnbox b) => (forall f. SIMD f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX4 f = mapX @X4 f (coerce (f @Identity))
{-# INLINE mapX4 #-}

mapX8 :: (MultiUnbox a, MultiUnbox b) => (forall f. SIMD f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX8 f = mapX @X8 f (coerce (f @Identity))
{-# INLINE mapX8 #-}

mapX16 :: (MultiUnbox a, MultiUnbox b) => (forall f. SIMD f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX16 f = mapX @X16 f (coerce (f @Identity))
{-# INLINE mapX16 #-}

mapX32 :: (MultiUnbox a, MultiUnbox b) => (forall f. SIMD f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX32 f = mapX @X32 f (coerce (f @Identity))
{-# INLINE mapX32 #-}

zipWithX :: forall m a b c. (SIMD m, VU.Unbox a, UnboxSIMD m a, VU.Unbox b, UnboxSIMD m b, VU.Unbox c, UnboxSIMD m c) => (m a -> m b -> m c) -> (a -> b -> c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX fv fs !v0 !v1 = VU.create $ do
  let !n = min (VU.length v0) (VU.length v1)
      !m = shortVectorLength @m
  !result <- VUM.unsafeNew n
  let goVec !i = if i + m <= n
                 then
                   do let !s0 = unsafeIndexUnboxedSIMD @m v0 i
                          !s1 = unsafeIndexUnboxedSIMD @m v1 i
                      unsafeWriteUnboxedSIMD result i (fv s0 s1)
                      goVec (i + m)
                 else
                   goScalar i
      goScalar !i | i < n = do let !x0 = VU.unsafeIndex v0 i
                                   !x1 = VU.unsafeIndex v1 i
                               VUM.unsafeWrite result i (fs x0 x1)
                               goScalar (i + 1)
                  | otherwise = pure ()
  goVec 0
  pure result

zipWithX2 :: (MultiUnbox a, MultiUnbox b, MultiUnbox c) => (forall f. SIMD f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX2 f = zipWithX @X2 f (coerce (f @Identity))
{-# INLINE zipWithX2 #-}

zipWithX4 :: (MultiUnbox a, MultiUnbox b, MultiUnbox c) => (forall f. SIMD f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX4 f = zipWithX @X4 f (coerce (f @Identity))
{-# INLINE zipWithX4 #-}

zipWithX8 :: (MultiUnbox a, MultiUnbox b, MultiUnbox c) => (forall f. SIMD f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX8 f = zipWithX @X8 f (coerce (f @Identity))
{-# INLINE zipWithX8 #-}

zipWithX16 :: (MultiUnbox a, MultiUnbox b, MultiUnbox c) => (forall f. SIMD f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX16 f = zipWithX @X16 f (coerce (f @Identity))
{-# INLINE zipWithX16 #-}

zipWithX32 :: (MultiUnbox a, MultiUnbox b, MultiUnbox c) => (forall f. SIMD f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX32 f = zipWithX @X32 f (coerce (f @Identity))
{-# INLINE zipWithX32 #-}
