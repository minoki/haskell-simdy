module Data.Simdy.Internal.Stream where
import           Data.Functor.Identity (Identity (runIdentity))
import           Data.Simdy.Internal.Class hiding ((<), (<=), (>), (>=), min)
import           Prelude hiding (zip, zip3)

-- INLINE_FUSED: INLINE [1]
-- INLINE_INNER: INLINE [0]

-- | A SIMD-aware stream with existential state.
--
-- The stream processes elements in two modes:
--
-- * 'sMultiStep': advance by one full SIMD vector width (fast path).
-- * 'sStep': advance by a single scalar element (used for the remainder).
data Stream m x a = forall s. MkStream
  { sMultiStep    :: s -> m (s, x a)  -- ^ SIMD-width step function.
  , sStep         :: s -> m (s, a)    -- ^ Scalar step function (for remainder elements).
  , sInitialState :: s                -- ^ Initial stream state.
  , sSize         :: !Int             -- ^ Total number of scalar elements.
  }

-- | Consume all elements of a bundle, discarding the results.
{-# INLINE [1] consume #-}
consume :: forall x m a. (Monad m, KnownSIMDLength x) => Stream m x a -> m ()
consume (MkStream multiStep step s0 n) = loop n s0
  where
    m = simdLength @x
    loop !i s | i >= m = do (s', _) <- multiStep s
                            loop (i - m) s'
              | i > 0 = do (s', _) <- step s
                           loop (i - 1) s'
              | otherwise = pure ()

-- | Create a bundle of @n@ copies of a value.
{-# INLINE [1] replicate #-}
replicate :: forall x m a. (Monad m, KnownSIMDLength x, Broadcast x a) => Int -> a -> Stream m x a
replicate !n !x = MkStream multiStep step (broadcast @x x) n
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep !v = pure (v, v)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step v = pure (v, x)

-- | Apply a function to every element. Takes both a SIMD-width function and a scalar function.
{-# INLINE mapD #-}
mapD :: forall x m a b. Monad m => (x a -> x b) -> (a -> b) -> Stream m x a -> Stream m x b
mapD vf f = mapMD (pure . vf) (pure . f)

-- | Monadic map over all elements.
{-# INLINE [1] mapMD #-}
mapMD :: forall x m a b. Monad m => (x a -> m (x b)) -> (a -> m b) -> Stream m x a -> Stream m x b
mapMD vf f (MkStream vg g s0 n) = MkStream multiStep step s0 n
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
{-# INLINE mapMD_ #-}
mapMD_ :: forall x m a b. (Monad m, KnownSIMDLength x) => (x a -> m (x b)) -> (a -> m b) -> Stream m x a -> m ()
mapMD_ vf f = consume . mapMD vf f

-- | Monadic element-wise combination of two bundles.
{-# INLINE [1] zipWithMD #-}
zipWithMD :: forall x m a b c. Monad m => (x a -> x b -> m (x c)) -> (a -> b -> m c) -> Stream m x a -> Stream m x b -> Stream m x c
zipWithMD vf f (MkStream vg g init0 n0) (MkStream vh h init1 n1)
  = MkStream multiStep step (init0, init1) (min n0 n1)
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
{-# INLINE zipWithMD_ #-}
zipWithMD_ :: forall x m a b c. (Monad m, KnownSIMDLength x) => (x a -> x b -> m (x c)) -> (a -> b -> m c) -> Stream m x a -> Stream m x b -> m ()
zipWithMD_ vf f a b = consume (zipWithMD vf f a b)

-- | Monadic element-wise combination of three bundles.
-- INLINE_FUSED
{-# INLINE [1] zipWith3MD #-}
zipWith3MD :: forall x m a b c d. Monad m => (x a -> x b -> x c -> m (x d)) -> (a -> b -> c -> m d) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d
zipWith3MD vf f (MkStream vg0 g0 init0 n0) (MkStream vg1 g1 init1 n1) (MkStream vg2 g2 init2 n2)
  = MkStream multiStep step (init0, init1, init2) (min n0 (min n1 n2))
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
{-# INLINE zipWith4MD #-}
zipWith4MD :: forall x m a b c d e. (Monad m, LiftConstructor x) => (x a -> x b -> x c -> x d -> m (x e)) -> (a -> b -> c -> d -> m e) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e
zipWith4MD vf f sa sb sc sd = zipWithMD vf' (\(a,b) (c,d) -> f a b c d) (zip sa sb) (zip sc sd)
  where
    vf' ab cd = case deconstructTuple2 ab of
                  (a,b) -> case deconstructTuple2 cd of
                             (c,d) -> vf a b c d

-- | Monadic element-wise combination of five bundles.
{-# INLINE zipWith5MD #-}
zipWith5MD :: forall x m a b c d e f. (Monad m, LiftConstructor x) => (x a -> x b -> x c -> x d -> x e -> m (x f)) -> (a -> b -> c -> d -> e -> m f) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f
zipWith5MD vf f sa sb sc sd se = zipWithMD vf' (\(a,b,c) (d,e) -> f a b c d e) (zip3 sa sb sc) (zip sd se)
  where
    vf' abc de = case deconstructTuple3 abc of
                   (a,b,c) -> case deconstructTuple2 de of
                                (d,e) -> vf a b c d e

-- | Monadic element-wise combination of six bundles.
{-# INLINE zipWith6MD #-}
zipWith6MD :: forall x m a b c d e f g. (Monad m, LiftConstructor x) => (x a -> x b -> x c -> x d -> x e -> x f -> m (x g)) -> (a -> b -> c -> d -> e -> f -> m g) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f -> Stream m x g
zipWith6MD vf fn sa sb sc sd se sf = zipWithMD vf' (\(a,b,c) (d,e,f) -> fn a b c d e f) (zip3 sa sb sc) (zip3 sd se sf)
  where
    vf' abc def = case deconstructTuple3 abc of
                    (a,b,c) -> case deconstructTuple3 def of
                                 (d,e,f) -> vf a b c d e f

-- | Element-wise combination of two bundles (pure version).
{-# INLINE zipWithD #-}
zipWithD :: forall x m a b c. Monad m => (x a -> x b -> x c) -> (a -> b -> c) -> Stream m x a -> Stream m x b -> Stream m x c
zipWithD vf f = zipWithMD (\a b -> pure (vf a b)) (\a b -> pure (f a b))

-- | Element-wise combination of three bundles.
{-# INLINE zipWith3D #-}
zipWith3D :: forall x m a b c d. Monad m => (x a -> x b -> x c -> x d) -> (a -> b -> c -> d) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d
zipWith3D vf f = zipWith3MD (\a b c -> pure (vf a b c)) (\a b c -> pure (f a b c))

-- | Element-wise combination of four bundles.
{-# INLINE zipWith4D #-}
zipWith4D :: forall x m a b c d e. (Monad m, LiftConstructor x) => (x a -> x b -> x c -> x d -> x e) -> (a -> b -> c -> d -> e) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e
zipWith4D vf f = zipWith4MD (\a b c d -> pure (vf a b c d)) (\a b c d -> pure (f a b c d))

-- | Element-wise combination of five bundles.
{-# INLINE zipWith5D #-}
zipWith5D :: forall x m a b c d e f. (Monad m, LiftConstructor x) => (x a -> x b -> x c -> x d -> x e -> x f) -> (a -> b -> c -> d -> e -> f) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f
zipWith5D vf f = zipWith5MD (\a b c d e -> pure (vf a b c d e)) (\a b c d e -> pure (f a b c d e))

-- | Element-wise combination of six bundles.
{-# INLINE zipWith6D #-}
zipWith6D :: forall x m a b c d e f g. (Monad m, LiftConstructor x) => (x a -> x b -> x c -> x d -> x e -> x f -> x g) -> (a -> b -> c -> d -> e -> f -> g) -> Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f -> Stream m x g
zipWith6D vf fn = zipWith6MD (\a b c d e f -> pure (vf a b c d e f)) (\a b c d e f -> pure (fn a b c d e f))

-- | Zip two bundles into a bundle of pairs.
{-# INLINE zip #-}
zip :: forall x m a b. (Monad m, LiftConstructor x) => Stream m x a -> Stream m x b -> Stream m x (a, b)
zip = zipWithD mkTuple2 (,)

-- | Zip three bundles into a bundle of 3-tuples.
{-# INLINE zip3 #-}
zip3 :: forall x m a b c. (Monad m, LiftConstructor x) => Stream m x a -> Stream m x b -> Stream m x c -> Stream m x (a, b, c)
zip3 = zipWith3D mkTuple3 (,,)

-- | Zip four bundles into a bundle of 4-tuples.
{-# INLINE zip4 #-}
zip4 :: forall x m a b c d. (Monad m, LiftConstructor x) => Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x (a, b, c, d)
zip4 = zipWith4D mkTuple4 (,,,)

-- | Zip five bundles into a bundle of 5-tuples.
{-# INLINE zip5 #-}
zip5 :: forall x m a b c d e. (Monad m, LiftConstructor x) => Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x (a, b, c, d, e)
zip5 = zipWith5D mkTuple5 (,,,,)

-- | Zip six bundles into a bundle of 6-tuples.
{-# INLINE zip6 #-}
zip6 :: forall x m a b c d e f. (Monad m, LiftConstructor x) => Stream m x a -> Stream m x b -> Stream m x c -> Stream m x d -> Stream m x e -> Stream m x f -> Stream m x (a, b, c, d, e, f)
zip6 = zipWith6D mkTuple6 (,,,,,)

-- | Pair each element with its index.
{-# INLINE [1] indexed #-}
indexed :: forall x m i a. (Monad m, EnumFromZero x i, LiftConstructor x) => Stream m x a -> Stream m x (i, a)
indexed (MkStream vf f s0 n) = MkStream multiStep step (s0, 0, enumFromZero @x @i) n
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep (s, !i, !k) = do (s', v) <- vf s
                               let !i' = i + fromIntegral (simdLength @x)
                                   !k' = k `plusF` fromIntegerF (toInteger (simdLength @x))
                               pure ((s', i', k'), mkTuple2 k v)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step (s, !i, k) = do (s', x) <- f s
                         let !i' = i + 1
                         pure ((s', i', k), (i, x))

-- indexedR :: (Monad m, Num i) => i -> Stream m x a -> Stream m x (i, a)

-- | Generate a bundle of @n@ consecutive values starting from @i0@: @[i0, i0+1, ..., i0+n-1]@.
{-# INLINE [1] enumFromN #-}
enumFromN :: forall x m a. (Monad m, EnumFromZero x a) => a -> Int -> Stream m x a
enumFromN i0 !n = MkStream multiStep step (i0, enumFromZero @x @a `plusF` broadcast i0) n
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep (!i, !k) = do let !i' = i + fromIntegral (simdLength @x)
                                !k' = k `plusF` fromIntegerF (toInteger (simdLength @x))
                            pure ((i', k'), k)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step (!i, k) = do let !i' = i + 1
                      pure ((i', k), i)

-- | Generate @n@ values with a given step: @[i0, i0+s, i0+2*s, ...]@.
{-# INLINE [1] enumFromStepN #-}
enumFromStepN :: forall x m a. (Monad m, EnumFromZero x a) => a -> a -> Int -> Stream m x a
enumFromStepN i0 s !n = MkStream multiStep step (i0, enumFromZero @x @a `plusF` broadcast i0) n
  where
    -- INLINE_INNER
    {-# INLINE [0] multiStep #-}
    multiStep (!i, !k) = do let !i' = i + s * fromIntegral (simdLength @x)
                                !k' = k `plusF` broadcast (s * fromIntegral (simdLength @x))
                            pure ((i', k'), k)
    -- INLINE_INNER
    {-# INLINE [0] step #-}
    step (!i, k) = do let !i' = i + s
                      pure ((i', k), i)

-- | Lift a pure bundle into an arbitrary monad.
{-# INLINE [1] lift #-}
lift :: forall x m a. Monad m => Stream Identity x a -> Stream m x a
lift (MkStream multiStep step s0 n) = MkStream (pure . runIdentity . multiStep) (pure . runIdentity . step) s0 n
