{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- SIMD-accelerated operations on generic vectors (any 'SIMDVector' instance).
--
-- These functions use stream fusion via "Data.Simdy.Vector.Bundle".
-- Each operation takes both a SIMD-width function (operating on @f a@) and
-- a scalar function (operating on @a@), so the SIMD path handles the bulk
-- of the work while the scalar path processes any remainder elements.
--
-- The SIMD vector type @f@ is typically inferred or specified via @TypeApplications@:
--
-- @
-- import "Data.Simdy.Vector.Generic" as G
-- result = G.map \@X4 simdFn scalarFn myVector
-- @
module Data.Simdy.Vector.Generic
  ( -- * Type classes
    SIMDMVector (..)
  , SIMDVector (..)
    -- * Construction
  , replicate
  , enumFromN
  , enumFromStepN
    -- * Indexing
  , indexed
    -- * Mapping
  , map
  , imap
  , mapM_
  , imapM_
  , forM_
  , iforM_
    -- * Zipping
  , zipWith
  , zipWith3
  , zipWith4
  , zipWith5
  , zipWith6
  , izipWith
  , izipWith3
  , izipWith4
  , izipWith5
  , izipWith6
  , zip
  , zip3
  , zip4
  , zip5
  , zip6
  , zipWithM_
  , izipWithM_
    -- * Folding
  , fold
  , fold'
    -- * Conversion
  , convert
  ) where
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default (SIMD, SIMDElement)
import           Data.Simdy.Vector.Bundle (stream)
import qualified Data.Simdy.Vector.Bundle as Bundle
import           Data.Simdy.Vector.Class
-- import qualified Data.Vector.Generic as VG
-- import qualified Data.Vector.Generic.Mutable as VGM
import           Data.Functor.Identity
import           Prelude hiding (map, mapM_, replicate, zip, zip3, zipWith,
                          zipWith3)

-- | Create a vector of @n@ copies of a value.
{-# INLINE replicate #-}
replicate :: forall f v a. (SIMDVector v f a, Broadcast f a) => Int -> a -> v a
replicate !n !x = unstream $ Bundle.replicate @f n x

{-# INLINE unstream #-}
unstream :: SIMDVector v f a => Bundle.Bundle Identity f a -> v a
unstream s = Bundle.new (Bundle.unstreamAsNew s)

-- {-# INLINE unstreamM #-}
-- unstreamM :: (Monad m, SIMDVector v f a) => Bundle m f a -> m (v a)
-- unstreamM s = ?

-- | Generate @n@ consecutive values: @[start, start+1, ..., start+n-1]@.
{-# INLINE enumFromN #-}
enumFromN :: forall f v a. (SIMDVector v f a, EnumFromZero f a) => a -> Int -> v a
enumFromN i0 n = unstream $ Bundle.enumFromN @f i0 n

-- | Generate @n@ values with a given step: @[start, start+step, start+2*step, ...]@.
{-# INLINE enumFromStepN #-}
enumFromStepN :: forall f v a. (SIMDVector v f a, EnumFromZero f a) => a -> a -> Int -> v a
enumFromStepN i0 s n = unstream $ Bundle.enumFromStepN @f i0 s n

-- | Pair each element with its index: @indexed [a, b, c] = [(0,a), (1,b), (2,c)]@.
{-# INLINE indexed #-}
indexed :: forall f v i a. (SIMDVector v f a, SIMDVector v f (i, a), EnumFromZero f i, LiftConstructor f) => v a -> v (i, a)
indexed = unstream . Bundle.indexed @f . stream

-- | Map a function over every element. Takes a SIMD-width function and a scalar fallback.
{-# INLINE map #-}
map :: (SIMDVector v f a, SIMDVector v f b) => (f a -> f b) -> (a -> b) -> v a -> v b
map vf f = unstream . Bundle.map vf f . stream

-- | Map with index. The SIMD-width function receives a vector of indices.
{-# INLINE imap #-}
imap :: (SIMDVector v f a, SIMDVector v f b, EnumFromZero f i, LiftConstructor f) => (f i -> f a -> f b) -> (i -> a -> b) -> v a -> v b
imap vf f = unstream . Bundle.map (\t -> case deconstructTuple2 t of (i, x) -> vf i x) (\(i, x) -> f i x) . Bundle.indexed . stream

-- {-# INLINE mapM #-}
-- mapM :: (Monad m, SIMDVector v a, SIMDVector v b) => (f a -> m (f b)) -> (a -> m b) -> v a -> m (v b)
-- mapM vf f = unstreamM . Bundle.mapM v f . stream

-- imapM

-- | Monadic map, discarding results.
{-# INLINE mapM_ #-}
mapM_ :: (Monad m, SIMDVector v f a) => (f a -> m (f b)) -> (a -> m b) -> v a -> m ()
mapM_ vf f = Bundle.mapM_ vf f . stream

-- | Indexed monadic map, discarding results.
{-# INLINE imapM_ #-}
imapM_ :: (Monad m, SIMDVector v f a, EnumFromZero f i, LiftConstructor f) => (f i -> f a -> m (f b)) -> (i -> a -> m b) -> v a -> m ()
imapM_ vf f = Bundle.mapM_ (\t -> case deconstructTuple2 t of (i, x) -> vf i x) (\(i, x) -> f i x) . Bundle.indexed . stream

-- forM

-- | Like 'mapM_' with the vector argument first.
{-# INLINE forM_ #-}
forM_ :: (Monad m, SIMDVector v f a) => v a -> (f a -> m (f b)) -> (a -> m b) -> m ()
forM_ xs vf f = mapM_ vf f xs

-- iforM

-- | Like 'imapM_' with the vector argument first.
{-# INLINE iforM_ #-}
iforM_ :: (Monad m, SIMDVector v f a, EnumFromZero f i, LiftConstructor f) => v a -> (f i -> f a -> m (f b)) -> (i -> a -> m b) -> m ()
iforM_ xs vf f = imapM_ vf f xs

-- | Element-wise combination of two vectors.
{-# INLINE zipWith #-}
zipWith :: forall f v a b c. (SIMDVector v f a, SIMDVector v f b, SIMDVector v f c, LiftConstructor f)
        => (f a -> f b -> f c) -> (a -> b -> c)
        -> v a -> v b -> v c
zipWith vf f = \as bs -> unstream (Bundle.zipWith vf f (stream as) (stream bs))

-- | Element-wise combination of three vectors.
{-# INLINE zipWith3 #-}
zipWith3 :: forall f v a b c d. (SIMDVector v f a, SIMDVector v f b, SIMDVector v f c, SIMDVector v f d, LiftConstructor f)
         => (f a -> f b -> f c -> f d) -> (a -> b -> c -> d)
         -> v a -> v b -> v c -> v d
zipWith3 vf f = \as bs cs -> unstream (Bundle.zipWith3 vf f (stream as) (stream bs) (stream cs))

-- | Element-wise combination of four vectors.
{-# INLINE zipWith4 #-}
zipWith4 :: forall f v a b c d e. (SIMDVector v f a, SIMDVector v f b, SIMDVector v f c, SIMDVector v f d, SIMDVector v f e, LiftConstructor f)
         => (f a -> f b -> f c -> f d -> f e) -> (a -> b -> c -> d -> e)
         -> v a -> v b -> v c -> v d -> v e
zipWith4 vf f = \as bs cs ds -> unstream (Bundle.zipWith4 vf f (stream as) (stream bs) (stream cs) (stream ds))

-- | Element-wise combination of five vectors.
{-# INLINE zipWith5 #-}
zipWith5 :: forall x v a b c d e f. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, LiftConstructor x)
         => (x a -> x b -> x c -> x d -> x e -> x f) -> (a -> b -> c -> d -> e -> f)
         -> v a -> v b -> v c -> v d -> v e -> v f
zipWith5 vf f = \as bs cs ds es -> unstream (Bundle.zipWith5 vf f (stream as) (stream bs) (stream cs) (stream ds) (stream es))

-- | Element-wise combination of six vectors.
{-# INLINE zipWith6 #-}
zipWith6 :: forall x v a b c d e f g. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, SIMDVector v x g, LiftConstructor x)
         => (x a -> x b -> x c -> x d -> x e -> x f -> x g) -> (a -> b -> c -> d -> e -> f -> g)
         -> v a -> v b -> v c -> v d -> v e -> v f -> v g
zipWith6 vf f = \as bs cs ds es fs -> unstream (Bundle.zipWith6 vf f (stream as) (stream bs) (stream cs) (stream ds) (stream es) (stream fs))

-- | Indexed element-wise combination of two vectors.
{-# INLINE izipWith #-}
izipWith :: forall f v i a b c. (SIMDVector v f a, SIMDVector v f b, SIMDVector v f c, EnumFromZero f i, LiftConstructor f)
         => (f i -> f a -> f b -> f c) -> (i -> a -> b -> c)
         -> v a -> v b -> v c
izipWith vf f = \as bs -> unstream (Bundle.zipWith (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Bundle.indexed $ stream as) (stream bs))

-- | Indexed element-wise combination of three vectors.
{-# INLINE izipWith3 #-}
izipWith3 :: forall f v i a b c d. (SIMDVector v f a, SIMDVector v f b, SIMDVector v f c, SIMDVector v f d, EnumFromZero f i, LiftConstructor f)
          => (f i -> f a -> f b -> f c -> f d) -> (i -> a -> b -> c -> d)
          -> v a -> v b -> v c -> v d
izipWith3 vf f = \as bs cs -> unstream (Bundle.zipWith3 (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Bundle.indexed $ stream as) (stream bs) (stream cs))

-- | Indexed element-wise combination of four vectors.
{-# INLINE izipWith4 #-}
izipWith4 :: forall f v i a b c d e. (SIMDVector v f a, SIMDVector v f b, SIMDVector v f c, SIMDVector v f d, SIMDVector v f e, EnumFromZero f i, LiftConstructor f)
          => (f i -> f a -> f b -> f c -> f d -> f e) -> (i -> a -> b -> c -> d -> e)
          -> v a -> v b -> v c -> v d -> v e
izipWith4 vf f = \as bs cs ds -> unstream (Bundle.zipWith4 (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Bundle.indexed $ stream as) (stream bs) (stream cs) (stream ds))

-- | Indexed element-wise combination of five vectors.
{-# INLINE izipWith5 #-}
izipWith5 :: forall x v i a b c d e f. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, EnumFromZero x i, LiftConstructor x)
          => (x i -> x a -> x b -> x c -> x d -> x e -> x f) -> (i -> a -> b -> c -> d -> e -> f)
          -> v a -> v b -> v c -> v d -> v e -> v f
izipWith5 vf f = \as bs cs ds es -> unstream (Bundle.zipWith5 (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Bundle.indexed $ stream as) (stream bs) (stream cs) (stream ds) (stream es))

-- | Indexed element-wise combination of six vectors.
{-# INLINE izipWith6 #-}
izipWith6 :: forall x v i a b c d e f g. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, SIMDVector v x g, EnumFromZero x i, LiftConstructor x)
          => (x i -> x a -> x b -> x c -> x d -> x e -> x f -> x g) -> (i -> a -> b -> c -> d -> e -> f -> g)
          -> v a -> v b -> v c -> v d -> v e -> v f -> v g
izipWith6 vf f = \as bs cs ds es fs -> unstream (Bundle.zipWith6 (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Bundle.indexed $ stream as) (stream bs) (stream cs) (stream ds) (stream es) (stream fs))

-- | Zip two vectors into a vector of pairs.
{-# INLINE zip #-}
zip :: forall f v a b. (SIMDVector v f a, SIMDVector v f b, SIMDVector v f (a, b), LiftConstructor f) => v a -> v b -> v (a, b)
zip = zipWith (mkTuple2 @f) (,)

-- | Zip three vectors into a vector of 3-tuples.
{-# INLINE zip3 #-}
zip3 :: forall f v a b c. (SIMDVector v f a, SIMDVector v f b, SIMDVector v f c, SIMDVector v f (a, b, c), LiftConstructor f) => v a -> v b -> v c -> v (a, b, c)
zip3 = zipWith3 (mkTuple3 @f) (,,)

-- | Zip four vectors into a vector of 4-tuples.
{-# INLINE zip4 #-}
zip4 :: forall f v a b c d. (SIMDVector v f a, SIMDVector v f b, SIMDVector v f c, SIMDVector v f d, SIMDVector v f (a, b, c, d), LiftConstructor f) => v a -> v b -> v c -> v d -> v (a, b, c, d)
zip4 = zipWith4 (mkTuple4 @f) (,,,)

-- | Zip five vectors into a vector of 5-tuples.
{-# INLINE zip5 #-}
zip5 :: forall f v a b c d e. (SIMDVector v f a, SIMDVector v f b, SIMDVector v f c, SIMDVector v f d, SIMDVector v f e, SIMDVector v f (a, b, c, d, e), LiftConstructor f) => v a -> v b -> v c -> v d -> v e -> v (a, b, c, d, e)
zip5 = zipWith5 (mkTuple5 @f) (,,,,)

-- | Zip six vectors into a vector of 6-tuples.
{-# INLINE zip6 #-}
zip6 :: forall x v a b c d e f. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, SIMDVector v x (a, b, c, d, e, f), LiftConstructor x) => v a -> v b -> v c -> v d -> v e -> v f -> v (a, b, c, d, e, f)
zip6 = zipWith6 (mkTuple6 @x) (,,,,,)

-- zipWithM
-- izipWithM

-- | Monadic element-wise combination, discarding results.
{-# INLINE zipWithM_ #-}
zipWithM_ :: (Monad m, SIMDVector v f a, SIMDVector v f b)
          => (f a -> f b -> m (f c)) -> (a -> b -> m c)
          -> v a -> v b -> m ()
zipWithM_ vf f = \as bs -> Bundle.zipWithM_ vf f (stream as) (stream bs)

-- | Indexed monadic element-wise combination, discarding results.
{-# INLINE izipWithM_ #-}
izipWithM_ :: (Monad m, SIMDVector v f a, SIMDVector v f b, EnumFromZero f i, LiftConstructor f)
           => (f i -> f a -> f b -> m (f c)) -> (i -> a -> b -> m c)
           -> v a -> v b -> m ()
izipWithM_ vf f = \as bs -> Bundle.zipWithM_ (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Bundle.indexed $ stream as) (stream bs)

-- unzip
-- unzip3
-- unzip4
-- unzip5
-- unzip6

-- | Left fold over a vector using a rank-2 combining function.
-- The function is applied at SIMD width for the bulk, then reduced horizontally.
{-# INLINE fold #-}
fold :: forall f v a. (SIMD f, SIMDElement a, SIMDVector v f a) => (forall g. SIMD g => g a -> g a -> g a) -> f a -> v a -> a
fold append initial = runIdentity . Bundle.fold append initial . stream

-- | Strict left fold. Like 'fold' but forces the accumulator at each step.
{-# INLINE fold' #-}
fold' :: forall f v a. (SIMD f, SIMDElement a, SIMDVector v f a) => (forall g. SIMD g => g a -> g a -> g a) -> f a -> v a -> a
fold' append initial = runIdentity . Bundle.fold' append initial . stream

-- | Convert between vector representations (e.g. primitive to unboxed) using stream fusion.
{-# INLINE convert #-}
convert :: forall f v w a. (SIMDVector v f a, SIMDVector w f a) => v a -> w a
convert = unstream . stream @f
