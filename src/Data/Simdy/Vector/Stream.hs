-- |
-- SIMD-aware stream bundles for fusion.
--
-- A 'Stream' is a stream that can step either one SIMD vector at a time
-- ('sMultiStep') or one scalar at a time ('sStep'), enabling efficient
-- processing of vectors whose length is not a multiple of the SIMD width.
--
-- This module follows the stream fusion approach of the @vector@ library.
-- Rewrite rules eliminate intermediate 'Stream' allocations:
--
-- @
-- stream (new (unstreamAsNew s)) = s
-- @
module Data.Simdy.Vector.Stream where
import           Control.Monad.Primitive (PrimMonad, PrimState, stToPrim)
import           Control.Monad.ST (ST, runST)
import           Data.Functor.Identity (Identity)
import           Data.Simdy.Internal.Class hiding ((<), (<=), (>), (>=), min)
import           Data.Simdy.Internal.Stream
import           Data.Simdy.Vector.Class
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import           Prelude hiding (mapM, zip, zip3, zipWith, zipWith3)

-- INLINE_FUSED: INLINE [1]
-- INLINE_INNER: INLINE [0]

-- | Convert an immutable vector into a 'Stream' for fusion.
{-# INLINE [1] stream #-}
stream :: forall f m v a. (Monad m, SIMDVector v f a) => v a -> Stream m f a
stream !v = MkStream multiStep step 0 (VG.length v)
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

-- | Convert a mutable vector into a 'Stream' for fusion.
{-# INLINE [1] mstream #-}
mstream :: forall f m v a. (PrimMonad m, SIMDMVector v f a) => v (PrimState m) a -> Stream m f a
mstream !v = MkStream multiStep step 0 (VGM.length v)
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

-- | Write a bundle into a new mutable vector.
{-# INLINE [1] munstream #-}
munstream :: forall f m v a. (PrimMonad m, SIMDMVector v f a) => Stream m f a -> m (v (PrimState m) a)
munstream (MkStream multiStep step s0 n) = do
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
unstreamAsMutable :: (PrimMonad m, SIMDMVector v f a) => Stream Identity f a -> m (v (PrimState m) a)
unstreamAsMutable = munstream . lift

-- | Delayed vector construction, used internally by stream fusion.
data New v a = MkNew (forall s. ST s (VG.Mutable v s a))

-- | Convert a pure bundle into a delayed vector construction.
{-# INLINE [1] unstreamAsNew #-}
unstreamAsNew :: SIMDVector v f a => Stream Identity f a -> New v a
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
