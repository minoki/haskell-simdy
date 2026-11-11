-- |
-- SIMD-aware streams for fusion.
--
-- A 'Stream' is a stream that can step either one SIMD vector at a time
-- ('sMultiStep') or one scalar at a time ('sStep'), enabling efficient
-- processing of vectors whose length is not a multiple of the SIMD width.
module Data.Simdy.Stream
  ( Internal.Stream
  , consume
  , replicate
  , map
  , Internal.mapD
  , mapM
  , Internal.mapMD
  , mapM_
  , mapMD_
  , zipWithM
  , Internal.zipWithMD
  , zipWithM_
  , zipWithMD_
  , zipWith3M
  , Internal.zipWith3MD
  , zipWith4M
  , zipWith4MD
  , zipWith5M
  , zipWith5MD
  , zipWith6M
  , zipWith6MD
  , zipWith
  , Internal.zipWithD
  , zipWith3
  , Internal.zipWith3D
  , zipWith4
  , zipWith4D
  , zipWith5
  , zipWith5D
  , zipWith6
  , zipWith6D
  , zip
  , zip3
  , zip4
  , zip5
  , zip6
  , fold
  , fold'
  , and
  , or
  , indexed
  , enumFromN
  , enumFromStepN
  , Internal.lift
  ) where
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (Identity, runIdentity))
import           Data.Simdy.Internal.Class (simdLength)
import           Data.Simdy.Internal.Default (SIMD (horizontalFold),
                                              SIMDElement, SIMDEnumFromZero)
import           Data.Simdy.Internal.Stream (Stream (..))
import qualified Data.Simdy.Internal.Stream as Internal
import           Prelude hiding (map, mapM, mapM_, replicate, zip, zip3,
                          zipWith, zipWith3)

-- | Consume all elements of a bundle, discarding the results.
{-# INLINE consume #-}
consume :: forall x m a. (Monad m, SIMD x) => Stream m x a -> m ()
consume = Internal.consume

-- | Create a bundle of @n@ copies of a value.
{-# INLINE replicate #-}
replicate :: forall x m a. (Monad m, SIMD x, SIMDElement a) => Int -> a -> Stream m x a
replicate = Internal.replicate

-- | Apply a function to every element.
{-# INLINE map #-}
map :: forall x m a b. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b) -> Stream m x a -> Stream m x b
map f = Internal.mapD f (coerce (f @Identity))

-- | Monadic map over all elements.
{-# INLINE mapM #-}
mapM :: forall x m a b. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> m (xx b)) -> Stream m x a -> Stream m x b
mapM f = Internal.mapMD f (coerce (fmap runIdentity . f))

-- | Monadic map over all elements, discarding results.
{-# INLINE mapM_ #-}
mapM_ :: forall x m a b. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> m (xx b)) -> Stream m x a -> m ()
mapM_ f = Internal.mapMD_ f (coerce (fmap runIdentity . f))

-- | Monadic map over all elements, discarding results.
{-# INLINE mapMD_ #-}
mapMD_ :: forall x m a b. (Monad m, SIMD x) => (x a -> m (x b)) -> (a -> m b) -> Stream m x a -> m ()
mapMD_ = Internal.mapMD_

-- | Monadic element-wise combination of two bundles.
{-# INLINE zipWithM #-}
zipWithM :: forall x m a b c. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> m (xx c)) -> Stream m x a -> Stream m x b -> Stream m x c
zipWithM f = Internal.zipWithMD f (coerce (\a -> fmap runIdentity . f a))

-- | Monadic element-wise combination, discarding results.
{-# INLINE zipWithM_ #-}
zipWithM_ :: forall x m a b c. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> m (xx c)) -> Stream m x a -> Stream m x b -> m ()
zipWithM_ f = Internal.zipWithMD_ f (coerce (\a -> fmap runIdentity . f a))

-- | Monadic element-wise combination, discarding results.
{-# INLINE zipWithMD_ #-}
zipWithMD_ :: forall x m a b c. (Monad m, SIMD x) => (x a -> x b -> m (x c)) -> (a -> b -> m c) -> Stream m x a -> Stream m x b -> m ()
zipWithMD_ = Internal.zipWithMD_

-- | Monadic element-wise combination of three bundles.
{-# INLINE zipWith3M #-}
zipWith3M :: forall x m a b c d. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> xx c -> m (xx d)) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d
zipWith3M f = Internal.zipWith3MD f (coerce (\a b -> fmap runIdentity . f a b))

-- | Monadic element-wise combination of four bundles.
{-# INLINE zipWith4M #-}
zipWith4M :: forall x m a b c d e. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d -> m (xx e)) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e
zipWith4M f = Internal.zipWith4MD f (coerce (\a b c -> fmap runIdentity . f a b c))

-- | Monadic element-wise combination of four bundles.
{-# INLINE zipWith4MD #-}
zipWith4MD :: forall x m a b c d e. (Monad m, SIMD x) => (x a -> x b -> x c -> x d -> m (x e)) -> (a -> b -> c -> d -> m e) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e
zipWith4MD = Internal.zipWith4MD

-- | Monadic element-wise combination of five bundles.
{-# INLINE zipWith5M #-}
zipWith5M :: forall x m a b c d e f. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d -> xx e -> m (xx f)) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f
zipWith5M f = Internal.zipWith5MD f (coerce (\a b c d -> fmap runIdentity . f a b c d))

-- | Monadic element-wise combination of five bundles.
{-# INLINE zipWith5MD #-}
zipWith5MD :: forall x m a b c d e f. (Monad m, SIMD x) => (x a -> x b -> x c -> x d -> x e -> m (x f)) -> (a -> b -> c -> d -> e -> m f) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f
zipWith5MD = Internal.zipWith5MD

-- | Monadic element-wise combination of six bundles.
{-# INLINE zipWith6M #-}
zipWith6M :: forall x m a b c d e f g. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d -> xx e -> xx f -> m (xx g)) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f -> Stream m x g
zipWith6M f = Internal.zipWith6MD f (coerce (\a b c d e -> fmap runIdentity . f a b c d e))

-- | Monadic element-wise combination of six bundles.
{-# INLINE zipWith6MD #-}
zipWith6MD :: forall x m a b c d e f g. (Monad m, SIMD x) => (x a -> x b -> x c -> x d -> x e -> x f -> m (x g)) -> (a -> b -> c -> d -> e -> f -> m g) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f -> Stream m x g
zipWith6MD = Internal.zipWith6MD

-- | Element-wise combination of two bundles (pure version).
{-# INLINE zipWith #-}
zipWith :: forall x m a b c. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> xx c) -> Stream m x a -> Stream m x b -> Stream m x c
zipWith f = Internal.zipWithD f (coerce (f @Identity))

-- | Element-wise combination of three bundles.
{-# INLINE zipWith3 #-}
zipWith3 :: forall x m a b c d. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d
zipWith3 f = Internal.zipWith3D f (coerce (f @Identity))

-- | Element-wise combination of four bundles.
{-# INLINE zipWith4 #-}
zipWith4 :: forall x m a b c d e. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d -> xx e) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e
zipWith4 f = Internal.zipWith4D f (coerce (f @Identity))

-- | Element-wise combination of four bundles.
{-# INLINE zipWith4D #-}
zipWith4D :: forall x m a b c d e. (Monad m, SIMD x) => (x a -> x b -> x c -> x d -> x e) -> (a -> b -> c -> d -> e) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e
zipWith4D = Internal.zipWith4D

-- | Element-wise combination of five bundles.
{-# INLINE zipWith5 #-}
zipWith5 :: forall x m a b c d e f. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d -> xx e -> xx f) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f
zipWith5 f = Internal.zipWith5D f (coerce (f @Identity))

-- | Element-wise combination of five bundles.
{-# INLINE zipWith5D #-}
zipWith5D :: forall x m a b c d e f. (Monad m, SIMD x) => (x a -> x b -> x c -> x d -> x e -> x f) -> (a -> b -> c -> d -> e -> f) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f
zipWith5D = Internal.zipWith5D

-- | Element-wise combination of six bundles.
{-# INLINE zipWith6 #-}
zipWith6 :: forall x m a b c d e f g. (Monad m, SIMD x) => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d -> xx e -> xx f -> xx g) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f -> Stream m x g
zipWith6 f = Internal.zipWith6D f (coerce (f @Identity))

-- | Element-wise combination of six bundles.
{-# INLINE zipWith6D #-}
zipWith6D :: forall x m a b c d e f g. (Monad m, SIMD x) => (x a -> x b -> x c -> x d -> x e -> x f -> x g) -> (a -> b -> c -> d -> e -> f -> g) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f -> Stream m x g
zipWith6D = Internal.zipWith6D

-- | Zip two bundles into a bundle of pairs.
{-# INLINE zip #-}
zip :: forall x m a b. (Monad m, SIMD x) => Stream m x a -> Stream m x b -> Stream m x (a, b)
zip = Internal.zip

-- | Zip three bundles into a bundle of 3-tuples.
{-# INLINE zip3 #-}
zip3 :: forall x m a b c. (Monad m, SIMD x) => Stream m x a -> Stream m x b -> Stream m x c -> Stream m x (a, b, c)
zip3 = Internal.zip3

-- | Zip four bundles into a bundle of 4-tuples.
{-# INLINE zip4 #-}
zip4 :: forall x m a b c d. (Monad m, SIMD x) => Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x (a, b, c, d)
zip4 = Internal.zip4

-- | Zip five bundles into a bundle of 5-tuples.
{-# INLINE zip5 #-}
zip5 :: forall x m a b c d e. (Monad m, SIMD x) => Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x (a, b, c, d, e)
zip5 = Internal.zip5

-- | Zip six bundles into a bundle of 6-tuples.
{-# INLINE zip6 #-}
zip6 :: forall x m a b c d e f. (Monad m, SIMD x) => Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f -> Stream m x (a, b, c, d, e, f)
zip6 = Internal.zip6

-- | Left fold over all elements using a rank-2 combining function.
-- The combining function is applied at SIMD width during the main loop
-- and at scalar width for the remainder, then a horizontal fold reduces
-- the SIMD accumulator to a scalar.
{-# INLINE [1] fold #-}
fold :: forall x m a. (Monad m, SIMD x, SIMDElement a) => (forall g. SIMD g => g a -> g a -> g a) -> x a -> Stream m x a -> m a
fold append initial (MkStream multiStep step s0 n) = loopMulti n s0 initial
  where
    m = simdLength @x
    loopMulti !i s acc
      | i >= m = do (s', v) <- multiStep s
                    let acc' = append acc v
                    loopMulti (i - m) s' acc'
      | otherwise = loopScalar i s (horizontalFold append acc)
    loopScalar !i s acc
      | i > 0 = do (s', x) <- step s
                   let acc' = runIdentity (append (Identity acc) (Identity x))
                   loopScalar (i - 1) s' acc'
      | otherwise = pure acc

-- | Strict left fold. Like 'fold' but forces the accumulator at each step.
{-# INLINE [1] fold' #-}
fold' :: forall x m a. (Monad m, SIMD x, SIMDElement a) => (forall g. SIMD g => g a -> g a -> g a) -> x a -> Stream m x a -> m a
fold' append initial (MkStream multiStep step s0 n) = loopMulti n s0 initial
  where
    m = simdLength @x
    loopMulti !i s !acc
      | i >= m = do (s', v) <- multiStep s
                    let !acc' = append acc v
                    loopMulti (i - m) s' acc'
      | otherwise = loopScalar i s (horizontalFold append acc)
    loopScalar !i s !acc
      | i > 0 = do (s', x) <- step s
                   let !acc' = runIdentity (append (Identity acc) (Identity x))
                   loopScalar (i - 1) s' acc'
      | otherwise = pure acc

-- | Pair each element with its index.
{-# INLINE indexed #-}
indexed :: forall x m i a. (Monad m, SIMD x, SIMDEnumFromZero i) => Stream m x a -> Stream m x (i, a)
indexed = Internal.indexed

-- indexedR :: (Monad m, Num i) => i -> Stream m x a -> Stream m x (i, a)

-- | Generate a bundle of @n@ consecutive values starting from @i0@: @[i0, i0+1, ..., i0+n-1]@.
{-# INLINE enumFromN #-}
enumFromN :: forall x m a. (Monad m, SIMD x, SIMDEnumFromZero a) => a -> Int -> Stream m x a
enumFromN = Internal.enumFromN

-- | Generate @n@ values with a given step: @[i0, i0+s, i0+2*s, ...]@.
{-# INLINE enumFromStepN #-}
enumFromStepN :: forall x m a. (Monad m, SIMD x, SIMDEnumFromZero a) => a -> a -> Int -> Stream m x a
enumFromStepN = Internal.enumFromStepN
