-- |
-- SIMD-aware stream bundles for fusion.
--
-- A 'Bundle' is a stream that can step either one SIMD vector at a time
-- ('sMultiStep') or one scalar at a time ('sStep'), enabling efficient
-- processing of vectors whose length is not a multiple of the SIMD width.
--
-- This module follows the stream fusion approach of the @vector@ library.
-- Rewrite rules eliminate intermediate 'Bundle' allocations:
--
-- @
-- stream (new (unstreamAsNew s)) = s
-- @
module Data.Simdy.Vector.Bundle where
import           Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import           Control.Monad.ST (ST, runST)
import           Data.Functor.Identity (Identity (Identity, runIdentity))
import           Data.Simdy.Internal.Class hiding ((<), (<=), (>), (>=), min)
import           Data.Simdy.Internal.Default (SIMD (horizontalFold),
                                              SIMDElement)
import           Data.Simdy.Vector.Class
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import           Prelude hiding (mapM, zip, zip3, zipWith, zipWith3)

-- INLINE_FUSED: INLINE [1]
-- INLINE_INNER: INLINE [0]

{-
type MultisP m f a = Stream m (Either a (f a))

data MultisC m f a where
  MultisC :: (s -> m (Step s (f a)))
          -> (s -> m (Step s a))
          -> s
          -> MultisC f a

type Multis m f a = Either (MultisC m f a) (MultisP m f a)

data Bundle m f a = Bundle
  { sElems :: Stream m a
  , sChunks :: Stream m (Chunk a)
  , sMultis :: Multis m f a
  , sSize :: Size
  }
-}

-- | A SIMD-aware stream with existential state.
--
-- The stream processes elements in two modes:
--
-- * 'sMultiStep': advance by one full SIMD vector width (fast path).
-- * 'sStep': advance by a single scalar element (used for the remainder).
data Bundle m f a = forall s. MkBundle
  { sMultiStep    :: s -> m (s, f a)  -- ^ SIMD-width step function.
  , sStep         :: s -> m (s, a)    -- ^ Scalar step function (for remainder elements).
  , sInitialState :: s                -- ^ Initial stream state.
  , sSize         :: !Int             -- ^ Total number of scalar elements.
  }

-- | Consume all elements of a bundle, discarding the results.
{-# INLINE [1] consume #-}
consume :: forall f m a. (Monad m, KnownSIMDLength f) => Bundle m f a -> m ()
consume (MkBundle multiStep step s0 n) = loop n s0
  where
    m = simdLength @f
    loop !i s | i >= m = do (s', _) <- multiStep s
                            loop (i - m) s'
              | i > 0 = do (s', _) <- step s
                           loop (i - 1) s'
              | otherwise = pure ()

-- | Create a bundle of @n@ copies of a value.
{-# INLINE [1] replicate #-}
replicate :: forall f m a. (Monad m, KnownSIMDLength f, Broadcast f a) => Int -> a -> Bundle m f a
replicate !n !x = MkBundle multiStep step (broadcast @f x) n
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep !v = pure (v, v)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step v = pure (v, x)

-- | Apply a function to every element. Takes both a SIMD-width function and a scalar function.
{-# INLINE map #-}
map :: Monad m => (f a -> f b) -> (a -> b) -> Bundle m f a -> Bundle m f b
map vf f = mapM (pure . vf) (pure . f)

-- | Monadic map over all elements.
{-# INLINE [1] mapM #-}
mapM :: Monad m => (f a -> m (f b)) -> (a -> m b) -> Bundle m f a -> Bundle m f b
mapM vf f (MkBundle vg g s0 n) = MkBundle multiStep step s0 n
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep s = do (s', v) <- vg s
                     v' <- vf v
                     pure (s', v')
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step s = do (s', x) <- g s
                x' <- f x
                pure (s', x')

-- | Monadic map over all elements, discarding results.
{-# INLINE mapM_ #-}
mapM_ :: (Monad m, KnownSIMDLength f) => (f a -> m (f b)) -> (a -> m b) -> Bundle m f a -> m ()
mapM_ vf f = consume . mapM vf f

-- | Monadic element-wise combination of two bundles.
{-# INLINE [1] zipWithM #-}
zipWithM :: Monad m => (f a -> f b -> m (f c)) -> (a -> b -> m c) -> Bundle m f a -> Bundle m f b -> Bundle m f c
zipWithM vf f (MkBundle vg g init0 n0) (MkBundle vh h init1 n1)
  = MkBundle multiStep step (init0, init1) (min n0 n1)
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep (s0, s1) = do (s0', a) <- vg s0
                            (s1', b) <- vh s1
                            c <- vf a b
                            pure ((s0', s1'), c)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step (s0, s1) = do (s0', a) <- g s0
                       (s1', b) <- h s1
                       c <- f a b
                       pure ((s0', s1'), c)

-- | Monadic element-wise combination, discarding results.
{-# INLINE zipWithM_ #-}
zipWithM_ :: (Monad m, KnownSIMDLength f) => (f a -> f b -> m (f c)) -> (a -> b -> m c) -> Bundle m f a -> Bundle m f b -> m ()
zipWithM_ vf f a b = consume (zipWithM vf f a b)

-- | Monadic element-wise combination of three bundles.
-- INLINE_FUSED
{-# INLINE [1] zipWith3M #-}
zipWith3M :: Monad m => (f a -> f b -> f c -> m (f d)) -> (a -> b -> c -> m d) -> Bundle m f a -> Bundle m f b -> Bundle m f c -> Bundle m f d
zipWith3M vf f (MkBundle vg0 g0 init0 n0) (MkBundle vg1 g1 init1 n1) (MkBundle vg2 g2 init2 n2)
  = MkBundle multiStep step (init0, init1, init2) (min n0 (min n1 n2))
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep (s0, s1, s2) = do (s0', a0) <- vg0 s0
                                (s1', a1) <- vg1 s1
                                (s2', a2) <- vg2 s2
                                b <- vf a0 a1 a2
                                pure ((s0', s1', s2'), b)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step (s0, s1, s2) = do (s0', a0) <- g0 s0
                           (s1', a1) <- g1 s1
                           (s2', a2) <- g2 s2
                           b <- f a0 a1 a2
                           pure ((s0', s1', s2'), b)

-- | Monadic element-wise combination of four bundles.
{-# INLINE zipWith4M #-}
zipWith4M :: (Monad m, LiftConstructor f) => (f a -> f b -> f c -> f d -> m (f e)) -> (a -> b -> c -> d -> m e) -> Bundle m f a -> Bundle m f b -> Bundle m f c -> Bundle m f d -> Bundle m f e
zipWith4M vf f sa sb sc sd = zipWithM vf' (\(a,b) (c,d) -> f a b c d) (zip sa sb) (zip sc sd)
  where
    vf' ab cd = case deconstructTuple2 ab of
                  (a,b) -> case deconstructTuple2 cd of
                             (c,d) -> vf a b c d

-- | Monadic element-wise combination of five bundles.
{-# INLINE zipWith5M #-}
zipWith5M :: (Monad m, LiftConstructor v) => (v a -> v b -> v c -> v d -> v e -> m (v f)) -> (a -> b -> c -> d -> e -> m f) -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d -> Bundle m v e -> Bundle m v f
zipWith5M vf f sa sb sc sd se = zipWithM vf' (\(a,b,c) (d,e) -> f a b c d e) (zip3 sa sb sc) (zip sd se)
  where
    vf' abc de = case deconstructTuple3 abc of
                   (a,b,c) -> case deconstructTuple2 de of
                                (d,e) -> vf a b c d e

-- | Monadic element-wise combination of six bundles.
{-# INLINE zipWith6M #-}
zipWith6M :: (Monad m, LiftConstructor v) => (v a -> v b -> v c -> v d -> v e -> v f -> m (v g)) -> (a -> b -> c -> d -> e -> f -> m g) -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d -> Bundle m v e -> Bundle m v f -> Bundle m v g
zipWith6M vf fn sa sb sc sd se sf = zipWithM vf' (\(a,b,c) (d,e,f) -> fn a b c d e f) (zip3 sa sb sc) (zip3 sd se sf)
  where
    vf' abc def = case deconstructTuple3 abc of
                    (a,b,c) -> case deconstructTuple3 def of
                                 (d,e,f) -> vf a b c d e f

-- | Element-wise combination of two bundles (pure version).
{-# INLINE zipWith #-}
zipWith :: Monad m => (f a -> f b -> f c) -> (a -> b -> c) -> Bundle m f a -> Bundle m f b -> Bundle m f c
zipWith vf f = zipWithM (\a b -> pure (vf a b)) (\a b -> pure (f a b))

-- | Element-wise combination of three bundles.
{-# INLINE zipWith3 #-}
zipWith3 :: Monad m => (f a -> f b -> f c -> f d) -> (a -> b -> c -> d) -> Bundle m f a -> Bundle m f b -> Bundle m f c -> Bundle m f d
zipWith3 vf f = zipWith3M (\a b c -> pure (vf a b c)) (\a b c -> pure (f a b c))

-- | Element-wise combination of four bundles.
{-# INLINE zipWith4 #-}
zipWith4 :: (Monad m, LiftConstructor f) => (f a -> f b -> f c -> f d -> f e) -> (a -> b -> c -> d -> e) -> Bundle m f a -> Bundle m f b -> Bundle m f c -> Bundle m f d -> Bundle m f e
zipWith4 vf f = zipWith4M (\a b c d -> pure (vf a b c d)) (\a b c d -> pure (f a b c d))

-- | Element-wise combination of five bundles.
{-# INLINE zipWith5 #-}
zipWith5 :: (Monad m, LiftConstructor v) => (v a -> v b -> v c -> v d -> v e -> v f) -> (a -> b -> c -> d -> e -> f) -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d -> Bundle m v e -> Bundle m v f
zipWith5 vf f = zipWith5M (\a b c d e -> pure (vf a b c d e)) (\a b c d e -> pure (f a b c d e))

-- | Element-wise combination of six bundles.
{-# INLINE zipWith6 #-}
zipWith6 :: (Monad m, LiftConstructor v) => (v a -> v b -> v c -> v d -> v e -> v f -> v g) -> (a -> b -> c -> d -> e -> f -> g) -> Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d -> Bundle m v e -> Bundle m v f -> Bundle m v g
zipWith6 vf fn = zipWith6M (\a b c d e f -> pure (vf a b c d e f)) (\a b c d e f -> pure (fn a b c d e f))

-- | Zip two bundles into a bundle of pairs.
{-# INLINE zip #-}
zip :: (Monad m, LiftConstructor f) => Bundle m f a -> Bundle m f b -> Bundle m f (a, b)
zip = zipWith mkTuple2 (,)

-- | Zip three bundles into a bundle of 3-tuples.
{-# INLINE zip3 #-}
zip3 :: (Monad m, LiftConstructor f) => Bundle m f a -> Bundle m f b -> Bundle m f c -> Bundle m f (a, b, c)
zip3 = zipWith3 mkTuple3 (,,)

-- | Zip four bundles into a bundle of 4-tuples.
{-# INLINE zip4 #-}
zip4 :: (Monad m, LiftConstructor f) => Bundle m f a -> Bundle m f b -> Bundle m f c -> Bundle m f d -> Bundle m f (a, b, c, d)
zip4 = zipWith4 mkTuple4 (,,,)

-- | Zip five bundles into a bundle of 5-tuples.
{-# INLINE zip5 #-}
zip5 :: (Monad m, LiftConstructor f) => Bundle m f a -> Bundle m f b -> Bundle m f c -> Bundle m f d -> Bundle m f e -> Bundle m f (a, b, c, d, e)
zip5 = zipWith5 mkTuple5 (,,,,)

-- | Zip six bundles into a bundle of 6-tuples.
{-# INLINE zip6 #-}
zip6 :: (Monad m, LiftConstructor v) => Bundle m v a -> Bundle m v b -> Bundle m v c -> Bundle m v d -> Bundle m v e -> Bundle m v f -> Bundle m v (a, b, c, d, e, f)
zip6 = zipWith6 mkTuple6 (,,,,,)

-- | Left fold over all elements using a rank-2 combining function.
-- The combining function is applied at SIMD width during the main loop
-- and at scalar width for the remainder, then a horizontal fold reduces
-- the SIMD accumulator to a scalar.
{-# INLINE [1] fold #-}
fold :: forall f m a. (Monad m, SIMD f, SIMDElement a) => (forall g. SIMD g => g a -> g a -> g a) -> f a -> Bundle m f a -> m a
fold append initial (MkBundle multiStep step s0 n) = loopMulti n s0 initial
  where
    m = simdLength @f
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
fold' :: forall f m a. (Monad m, SIMD f, SIMDElement a) => (forall g. SIMD g => g a -> g a -> g a) -> f a -> Bundle m f a -> m a
fold' append initial (MkBundle multiStep step s0 n) = loopMulti n s0 initial
  where
    m = simdLength @f
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
{-# INLINE [1] indexed #-}
indexed :: forall f m i a. (Monad m, EnumFromZero f i, LiftConstructor f) => Bundle m f a -> Bundle m f (i, a)
indexed (MkBundle vf f s0 n) = MkBundle multiStep step (s0, 0, enumFromZero @f @i) n
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep (s, !i, !k) = do (s', v) <- vf s
                               let !i' = i + fromIntegral (simdLength @f)
                                   !k' = k `plusF` fromIntegerF (toInteger (simdLength @f))
                               pure ((s', i', k'), mkTuple2 k v)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step (s, !i, k) = do (s', x) <- f s
                         let !i' = i + 1
                         pure ((s', i', k), (i, x))

-- indexedR :: (Monad m, Num i) => i -> Bundle m f a -> Bundle m f (i, a)

-- | Generate a bundle of @n@ consecutive values starting from @i0@: @[i0, i0+1, ..., i0+n-1]@.
{-# INLINE [1] enumFromN #-}
enumFromN :: forall f m a. (Monad m, EnumFromZero f a) => a -> Int -> Bundle m f a
enumFromN i0 !n = MkBundle multiStep step (i0, enumFromZero @f @a `plusF` broadcast i0) n
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep (!i, !k) = do let !i' = i + fromIntegral (simdLength @f)
                                !k' = k `plusF` fromIntegerF (toInteger (simdLength @f))
                            pure ((i', k'), k)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step (!i, k) = do let !i' = i + 1
                      pure ((i', k), i)

-- | Generate @n@ values with a given step: @[i0, i0+s, i0+2*s, ...]@.
{-# INLINE [1] enumFromStepN #-}
enumFromStepN :: forall f m a. (Monad m, EnumFromZero f a) => a -> a -> Int -> Bundle m f a
enumFromStepN i0 s !n = MkBundle multiStep step (i0, enumFromZero @f @a `plusF` broadcast i0) n
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep (!i, !k) = do let !i' = i + s * fromIntegral (simdLength @f)
                                !k' = k `plusF` broadcast (s * fromIntegral (simdLength @f))
                            pure ((i', k'), k)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step (!i, k) = do let !i' = i + s
                      pure ((i', k), i)

-- | Convert an immutable vector into a 'Bundle' for fusion.
{-# INLINE [1] stream #-}
stream :: forall f m v a. (Monad m, SIMDVector v f a) => v a -> Bundle m f a
stream !v = MkBundle multiStep step 0 (VG.length v)
  where
    m = simdLength @f
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep !i = do let !x = unsafeIndexMulti v i
                          !j = i + m
                      pure (j, x)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step !i = let !x = VG.unsafeIndex v i
                  !j = i + 1
              in pure (j, x)

-- | Convert a mutable vector into a 'Bundle' for fusion.
{-# INLINE [1] mstream #-}
mstream :: forall f m v a. (PrimMonad m, SIMDMVector v f a) => v (PrimState m) a -> Bundle m f a
mstream !v = MkBundle multiStep step 0 (VGM.length v)
  where
    m = simdLength @f
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep !i = stToPrim $ do
      !x <- unsafeReadMulti v i
      let !j = i + m
      pure (j, x)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step !i = stToPrim $ do
      !x <- VGM.unsafeRead v i
      let !j = i + 1
      pure (j, x)

-- | Lift a pure bundle into an arbitrary monad.
{-# INLINE [1] lift #-}
lift :: Monad m => Bundle Identity f a -> Bundle m f a
lift (MkBundle multiStep step s0 n) = MkBundle (pure . runIdentity . multiStep) (pure . runIdentity . step) s0 n

-- | Write a bundle into a new mutable vector.
{-# INLINE [1] munstream #-}
munstream :: forall f m v a. (PrimMonad m, SIMDMVector v f a) => Bundle m f a -> m (v (PrimState m) a)
munstream (MkBundle multiStep step s0 n) = do
  !v <- VGM.unsafeNew n
  let !m = simdLength @f
      loopMulti !i s
        | i <= n - m = do
          (s', x) <- multiStep s
          stToPrim $ unsafeWriteMulti v i x
          loopMulti (i + m) s'
        | otherwise = loopScalar i s
      loopScalar !i s
        | i < n = do
          (s', x) <- step s
          VGM.unsafeWrite v i x
          loopScalar (i + 1) s'
        | otherwise = pure ()
  loopMulti 0 s0
  pure v

-- | Write a pure bundle into a new mutable vector.
{-# INLINE unstreamAsMutable #-}
unstreamAsMutable :: (PrimMonad m, SIMDMVector v f a) => Bundle Identity f a -> m (v (PrimState m) a)
unstreamAsMutable = munstream . lift

-- | Delayed vector construction, used internally by stream fusion.
data New v a = MkNew (forall s. ST s (VG.Mutable v s a))

-- | Convert a pure bundle into a delayed vector construction.
{-# INLINE [1] unstreamAsNew #-}
unstreamAsNew :: SIMDVector v f a => Bundle Identity f a -> New v a
unstreamAsNew s = MkNew (munstream (lift s))

-- | Execute a delayed vector construction, freezing the result.
{-# INLINE [1] new #-}
new :: VG.Vector v a => New v a -> v a
new (MkNew m) = runST (m >>= VG.unsafeFreeze)

{-# RULES
"stream/unstream [Simdy Vector]" forall s.
  stream (new (unstreamAsNew s)) = s
"mstream/unstream [Simdy Vector]" forall s.
  mstream (new (unstreamAsNew s)) = s
  #-}
