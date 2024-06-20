{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.ShortVector.Unboxed where
import Data.ShortVector.Class
import Data.ShortVector.Internal.SIMD128
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Functor.Identity

mapX :: forall m a b. (ShortVector m, VU.Unbox a, UnboxSV m a, VU.Unbox b, UnboxSV m b) => (forall f. ShortVector f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX f !v = VU.create $ do
  let !n = VU.length v
      !m = shortVectorLength @m
  result <- VUM.unsafeNew n
  let goVec !i = if i + m <= n
                 then
                   do let !s = unsafeIndexUnboxedSV @m v i
                      unsafeWriteUnboxedSV result i (f s)
                      goVec (i + m)
                 else
                   goScalar i
      goScalar !i | i < n = do let !x = VU.unsafeIndex v i
                               VUM.unsafeWrite result i (runIdentity (f (Identity x)))
                               goScalar (i + 1)
                  | otherwise = pure ()
  goVec 0
  pure result

mapX2 :: (MultiUnbox a, MultiUnbox b) => (forall f. ShortVector f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX2 = mapX @X2

mapX4 :: (MultiUnbox a, MultiUnbox b) => (forall f. ShortVector f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX4 = mapX @X4

mapX8 :: (MultiUnbox a, MultiUnbox b) => (forall f. ShortVector f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX8 = mapX @X8

mapX16 :: (MultiUnbox a, MultiUnbox b) => (forall f. ShortVector f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX16 = mapX @X16

mapX32 :: (MultiUnbox a, MultiUnbox b) => (forall f. ShortVector f => f a -> f b) -> VU.Vector a -> VU.Vector b
mapX32 = mapX @X32

zipWithX :: forall m a b c. (ShortVector m, VU.Unbox a, UnboxSV m a, VU.Unbox b, UnboxSV m b, VU.Unbox c, UnboxSV m c) => (forall f. ShortVector f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX f !v0 !v1 = VU.create $ do
  let !n = min (VU.length v0) (VU.length v1)
      !m = shortVectorLength @m
  result <- VUM.unsafeNew n
  let goVec !i = if i + m <= n
                 then
                   do let !s0 = unsafeIndexUnboxedSV @m v0 i
                          !s1 = unsafeIndexUnboxedSV @m v1 i
                      unsafeWriteUnboxedSV result i (f s0 s1)
                      goVec (i + m)
                 else
                   goScalar i
      goScalar !i | i < n = do let !x0 = VU.unsafeIndex v0 i
                                   !x1 = VU.unsafeIndex v1 i
                               VUM.unsafeWrite result i (runIdentity (f (Identity x0) (Identity x1)))
                               goScalar (i + 1)
                  | otherwise = pure ()
  goVec 0
  pure result

zipWithX2 :: (MultiUnbox a, MultiUnbox b, MultiUnbox c) => (forall f. ShortVector f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX2 = zipWithX @X2

zipWithX4 :: (MultiUnbox a, MultiUnbox b, MultiUnbox c) => (forall f. ShortVector f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX4 = zipWithX @X4

zipWithX8 :: (MultiUnbox a, MultiUnbox b, MultiUnbox c) => (forall f. ShortVector f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX8 = zipWithX @X8

zipWithX16 :: (MultiUnbox a, MultiUnbox b, MultiUnbox c) => (forall f. ShortVector f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX16 = zipWithX @X16

zipWithX32 :: (MultiUnbox a, MultiUnbox b, MultiUnbox c) => (forall f. ShortVector f => f a -> f b -> f c) -> VU.Vector a -> VU.Vector b -> VU.Vector c
zipWithX32 = zipWithX @X32
