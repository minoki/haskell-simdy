{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
-- SIMD-accelerated operations on generic vectors (any 'SIMDVector' instance).
--
-- These functions use stream fusion via "Data.Simdy.Vector.Stream".
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
  , mapD
  , imap
  , imapD
  , mapM_
  , mapMD_
  , imapM_
  , imapMD_
  , forM_
  , forMD_
  , iforM_
  , iforMD_
    -- * Zipping
  , zipWith
  , zipWithD
  , zipWith3
  , zipWith3D
  , zipWith4
  , zipWith4D
  , zipWith5
  , zipWith5D
  , zipWith6
  , zipWith6D
  , izipWith
  , izipWithD
  , izipWith3
  , izipWith3D
  , izipWith4
  , izipWith4D
  , izipWith5
  , izipWith5D
  , izipWith6
  , izipWith6D
  , zip
  , zip3
  , zip4
  , zip5
  , zip6
  , zipWithM_
  , zipWithMD_
  , izipWithM_
  , izipWithMD_
    -- * Folding
  , fold
  , fold'
    -- * Conversion
  , convert
  ) where
import           Data.Coerce (coerce)
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default (SIMD, SIMDElement, SIMDEnumFromZero)
import           Data.Simdy.Vector.Stream (stream)
import qualified Data.Simdy.Vector.Stream as Stream
import qualified Data.Simdy.Internal.Stream as Stream
import qualified Data.Simdy.Stream as Stream (fold, fold')
import           Data.Simdy.Vector.Class
-- import qualified Data.Vector.Generic as VG
-- import qualified Data.Vector.Generic.Mutable as VGM
import           Data.Functor.Identity
import           Prelude hiding (map, mapM_, replicate, zip, zip3, zipWith,
                          zipWith3)

-- | Create a vector of @n@ copies of a value.
{-# INLINE replicate #-}
replicate :: forall x v a. (SIMDVector v x a, Broadcast x a) => Int -> a -> v a
replicate !n !x = unstream $ Stream.replicate @x n x

{-# INLINE unstream #-}
unstream :: SIMDVector v x a => Stream.Stream Identity x a -> v a
unstream s = Stream.new (Stream.unstreamAsNew s)

-- {-# INLINE unstreamM #-}
-- unstreamM :: (Monad m, SIMDVector v x a) => Stream m x a -> m (v a)
-- unstreamM s = ?

-- | Generate @n@ consecutive values: @[start, start+1, ..., start+n-1]@.
{-# INLINE enumFromN #-}
enumFromN :: forall x v a. (SIMDVector v x a, EnumFromZero x a) => a -> Int -> v a
enumFromN i0 n = unstream $ Stream.enumFromN @x i0 n

-- | Generate @n@ values with a given step: @[start, start+step, start+2*step, ...]@.
{-# INLINE enumFromStepN #-}
enumFromStepN :: forall x v a. (SIMDVector v x a, EnumFromZero x a) => a -> a -> Int -> v a
enumFromStepN i0 s n = unstream $ Stream.enumFromStepN @x i0 s n

-- | Pair each element with its index: @indexed [a, b, c] = [(0,a), (1,b), (2,c)]@.
{-# INLINE indexed #-}
indexed :: forall x v i a. (SIMDVector v x a, SIMDVector v x (i, a), EnumFromZero x i, LiftConstructor x) => v a -> v (i, a)
indexed = unstream . Stream.indexed @x . stream

-- | Map a function over every element.
{-# INLINE map #-}
map :: forall x v a b. (SIMD x, SIMDVector v x a, SIMDVector v x b) => (forall xx. SIMD xx => xx a -> xx b) -> v a -> v b
map f = unstream . Stream.mapD (f @x) (coerce (f @Identity)) . stream

-- | Map a function over every element. Takes a SIMD-width function and a scalar fallback.
{-# INLINE mapD #-}
mapD :: forall x v a b. (SIMDVector v x a, SIMDVector v x b) => (x a -> x b) -> (a -> b) -> v a -> v b
mapD vf f = unstream . Stream.mapD vf f . stream

-- | Map with index. The SIMD-width function receives a vector of indices.
{-# INLINE imap #-}
imap :: forall x v i a b. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDEnumFromZero i) => (forall xx. SIMD xx => xx i -> xx a -> xx b) -> v a -> v b
imap f = unstream . Stream.mapD (\t -> case deconstructTuple2 t of (i, x) -> f @x i x) (\(i, x) -> coerce (f @Identity) i x) . Stream.indexed . stream

-- | Map with index. The SIMD-width function receives a vector of indices.
{-# INLINE imapD #-}
imapD :: forall x v i a b. (SIMDVector v x a, SIMDVector v x b, EnumFromZero x i, LiftConstructor x) => (x i -> x a -> x b) -> (i -> a -> b) -> v a -> v b
imapD vf f = unstream . Stream.mapD (\t -> case deconstructTuple2 t of (i, x) -> vf i x) (\(i, x) -> f i x) . Stream.indexed . stream

-- {-# INLINE mapM #-}
-- mapM :: (Monad m, SIMDVector v a, SIMDVector v b) => (x a -> m (x b)) -> (a -> m b) -> v a -> m (v b)
-- mapM vf f = unstreamM . Stream.mapM v f . stream

-- imapM

-- | Monadic map, discarding results.
{-# INLINE mapM_ #-}
mapM_ :: forall x v m a b. (Monad m, SIMD x, SIMDVector v x a) => (forall xx. SIMD xx => xx a -> m (xx b)) -> v a -> m ()
mapM_ f = Stream.mapMD_ (f @x) (coerce (fmap runIdentity . f)) . stream

-- | Monadic map, discarding results.
{-# INLINE mapMD_ #-}
mapMD_ :: forall x v m a b. (Monad m, SIMDVector v x a) => (x a -> m (x b)) -> (a -> m b) -> v a -> m ()
mapMD_ vf f = Stream.mapMD_ vf f . stream

-- | Indexed monadic map, discarding results.
{-# INLINE imapM_ #-}
imapM_ :: forall x v m i a b. (Monad m, SIMD x, SIMDVector v x a, SIMDEnumFromZero i) => (forall xx. SIMD xx => xx i -> xx a -> m (xx b)) -> v a -> m ()
imapM_ f = Stream.mapMD_ (\t -> case deconstructTuple2 t of (i, x) -> f @x i x) (\(i, x) -> runIdentity <$> f (Identity i) (Identity x)) . Stream.indexed . stream

-- | Indexed monadic map, discarding results.
{-# INLINE imapMD_ #-}
imapMD_ :: forall x v m i a b. (Monad m, SIMDVector v x a, EnumFromZero x i, LiftConstructor x) => (x i -> x a -> m (x b)) -> (i -> a -> m b) -> v a -> m ()
imapMD_ vf f = Stream.mapMD_ (\t -> case deconstructTuple2 t of (i, x) -> vf i x) (\(i, x) -> f i x) . Stream.indexed . stream

-- forM

-- | Like 'mapM_' with the vector argument first.
{-# INLINE forM_ #-}
forM_ :: forall x v m a b. (Monad m, SIMD x, SIMDVector v x a) => v a -> (forall xx. SIMD xx => xx a -> m (xx b)) -> m ()
forM_ xs f = mapM_ @x @v @m @a @b f xs

-- | Like 'mapMD_' with the vector argument first.
{-# INLINE forMD_ #-}
forMD_ :: forall x v m a b. (Monad m, SIMDVector v x a) => v a -> (x a -> m (x b)) -> (a -> m b) -> m ()
forMD_ xs vf f = mapMD_ vf f xs

-- iforM

-- | Like 'imapM_' with the vector argument first.
{-# INLINE iforM_ #-}
iforM_ :: forall x v m i a b. (Monad m, SIMD x, SIMDVector v x a, SIMDEnumFromZero i) => v a -> (forall xx. SIMD xx => xx i -> xx a -> m (xx b)) -> m ()
iforM_ xs f = imapM_ @x @v @m @i @a @b f xs

-- | Like 'imapMD_' with the vector argument first.
{-# INLINE iforMD_ #-}
iforMD_ :: forall x v m i a b. (Monad m, SIMDVector v x a, EnumFromZero x i, LiftConstructor x) => v a -> (x i -> x a -> m (x b)) -> (i -> a -> m b) -> m ()
iforMD_ xs vf f = imapMD_ vf f xs

-- | Element-wise combination of two vectors.
{-# INLINE zipWith #-}
zipWith :: forall x v a b c. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDVector v x c)
        => (forall xx. SIMD xx => xx a -> xx b -> xx c)
        -> v a -> v b -> v c
zipWith f = \as bs -> unstream (Stream.zipWithD (f @x) (coerce (f @Identity)) (stream as) (stream bs))

-- | Element-wise combination of two vectors.
{-# INLINE zipWithD #-}
zipWithD :: forall x v a b c. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, LiftConstructor x)
         => (x a -> x b -> x c) -> (a -> b -> c)
         -> v a -> v b -> v c
zipWithD vf f = \as bs -> unstream (Stream.zipWithD vf f (stream as) (stream bs))

-- | Element-wise combination of three vectors.
{-# INLINE zipWith3 #-}
zipWith3 :: forall x v a b c d. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d)
         => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d)
         -> v a -> v b -> v c -> v d
zipWith3 f = \as bs cs -> unstream (Stream.zipWith3D (f @x) (coerce (f @Identity)) (stream as) (stream bs) (stream cs))

-- | Element-wise combination of three vectors.
{-# INLINE zipWith3D #-}
zipWith3D :: forall x v a b c d. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, LiftConstructor x)
          => (x a -> x b -> x c -> x d) -> (a -> b -> c -> d)
          -> v a -> v b -> v c -> v d
zipWith3D vf f = \as bs cs -> unstream (Stream.zipWith3D vf f (stream as) (stream bs) (stream cs))

-- | Element-wise combination of four vectors.
{-# INLINE zipWith4 #-}
zipWith4 :: forall x v a b c d e. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e)
         => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d -> xx e)
         -> v a -> v b -> v c -> v d -> v e
zipWith4 f = \as bs cs ds -> unstream (Stream.zipWith4D (f @x) (coerce (f @Identity)) (stream as) (stream bs) (stream cs) (stream ds))

-- | Element-wise combination of four vectors.
{-# INLINE zipWith4D #-}
zipWith4D :: forall x v a b c d e. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, LiftConstructor x)
          => (x a -> x b -> x c -> x d -> x e) -> (a -> b -> c -> d -> e)
          -> v a -> v b -> v c -> v d -> v e
zipWith4D vf f = \as bs cs ds -> unstream (Stream.zipWith4D vf f (stream as) (stream bs) (stream cs) (stream ds))

-- | Element-wise combination of five vectors.
{-# INLINE zipWith5 #-}
zipWith5 :: forall x v a b c d e f. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f)
         => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d -> xx e -> xx f)
         -> v a -> v b -> v c -> v d -> v e -> v f
zipWith5 f = \as bs cs ds es -> unstream (Stream.zipWith5D (f @x) (coerce (f @Identity)) (stream as) (stream bs) (stream cs) (stream ds) (stream es))

-- | Element-wise combination of five vectors.
{-# INLINE zipWith5D #-}
zipWith5D :: forall x v a b c d e f. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, LiftConstructor x)
         => (x a -> x b -> x c -> x d -> x e -> x f) -> (a -> b -> c -> d -> e -> f)
         -> v a -> v b -> v c -> v d -> v e -> v f
zipWith5D vf f = \as bs cs ds es -> unstream (Stream.zipWith5D vf f (stream as) (stream bs) (stream cs) (stream ds) (stream es))

-- | Element-wise combination of six vectors.
{-# INLINE zipWith6 #-}
zipWith6 :: forall x v a b c d e f g. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, SIMDVector v x g)
         => (forall xx. SIMD xx => xx a -> xx b -> xx c -> xx d -> xx e -> xx f -> xx g)
         -> v a -> v b -> v c -> v d -> v e -> v f -> v g
zipWith6 f = \as bs cs ds es fs -> unstream (Stream.zipWith6D (f @x) (coerce (f @Identity)) (stream as) (stream bs) (stream cs) (stream ds) (stream es) (stream fs))

-- | Element-wise combination of six vectors.
{-# INLINE zipWith6D #-}
zipWith6D :: forall x v a b c d e f g. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, SIMDVector v x g, LiftConstructor x)
          => (x a -> x b -> x c -> x d -> x e -> x f -> x g) -> (a -> b -> c -> d -> e -> f -> g)
          -> v a -> v b -> v c -> v d -> v e -> v f -> v g
zipWith6D vf f = \as bs cs ds es fs -> unstream (Stream.zipWith6D vf f (stream as) (stream bs) (stream cs) (stream ds) (stream es) (stream fs))

-- | Indexed element-wise combination of two vectors.
{-# INLINE izipWith #-}
izipWith :: forall x v i a b c. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDEnumFromZero i)
         => (forall xx. SIMD xx => xx i -> xx a -> xx b -> xx c)
         -> v a -> v b -> v c
izipWith f = \as bs -> unstream (Stream.zipWithD (\t -> case deconstructTuple2 t of (i, a) -> f @x i a) (\(i, a) -> coerce (f @Identity) i a) (Stream.indexed $ stream as) (stream bs))

-- | Indexed element-wise combination of two vectors.
{-# INLINE izipWithD #-}
izipWithD :: forall x v i a b c. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, EnumFromZero x i, LiftConstructor x)
          => (x i -> x a -> x b -> x c) -> (i -> a -> b -> c)
          -> v a -> v b -> v c
izipWithD vf f = \as bs -> unstream (Stream.zipWithD (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Stream.indexed $ stream as) (stream bs))

-- | Indexed element-wise combination of three vectors.
{-# INLINE izipWith3 #-}
izipWith3 :: forall x v i a b c d. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, EnumFromZero x i)
          => (forall xx. SIMD xx => xx i -> xx a -> xx b -> xx c -> xx d)
          -> v a -> v b -> v c -> v d
izipWith3 f = \as bs cs -> unstream (Stream.zipWith3D (\t -> case deconstructTuple2 t of (i, a) -> f @x i a) (\(i, a) -> coerce (f @Identity) i a) (Stream.indexed $ stream as) (stream bs) (stream cs))

-- | Indexed element-wise combination of three vectors.
{-# INLINE izipWith3D #-}
izipWith3D :: forall x v i a b c d. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, EnumFromZero x i, LiftConstructor x)
           => (x i -> x a -> x b -> x c -> x d) -> (i -> a -> b -> c -> d)
           -> v a -> v b -> v c -> v d
izipWith3D vf f = \as bs cs -> unstream (Stream.zipWith3D (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Stream.indexed $ stream as) (stream bs) (stream cs))

-- | Indexed element-wise combination of four vectors.
{-# INLINE izipWith4 #-}
izipWith4 :: forall x v i a b c d e. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDEnumFromZero i)
          => (forall xx. SIMD xx => xx i -> xx a -> xx b -> xx c -> xx d -> xx e)
          -> v a -> v b -> v c -> v d -> v e
izipWith4 f = \as bs cs ds -> unstream (Stream.zipWith4D (\t -> case deconstructTuple2 t of (i, a) -> f @x i a) (\(i, a) -> coerce (f @Identity) i a) (Stream.indexed $ stream as) (stream bs) (stream cs) (stream ds))

-- | Indexed element-wise combination of four vectors.
{-# INLINE izipWith4D #-}
izipWith4D :: forall x v i a b c d e. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, EnumFromZero x i, LiftConstructor x)
           => (x i -> x a -> x b -> x c -> x d -> x e) -> (i -> a -> b -> c -> d -> e)
           -> v a -> v b -> v c -> v d -> v e
izipWith4D vf f = \as bs cs ds -> unstream (Stream.zipWith4D (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Stream.indexed $ stream as) (stream bs) (stream cs) (stream ds))

-- | Indexed element-wise combination of five vectors.
{-# INLINE izipWith5 #-}
izipWith5 :: forall x v i a b c d e f. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, SIMDEnumFromZero i)
          => (forall xx. SIMD xx => xx i -> xx a -> xx b -> xx c -> xx d -> xx e -> xx f)
          -> v a -> v b -> v c -> v d -> v e -> v f
izipWith5 f = \as bs cs ds es -> unstream (Stream.zipWith5D (\t -> case deconstructTuple2 t of (i, a) -> f @x i a) (\(i, a) -> coerce (f @Identity) i a) (Stream.indexed $ stream as) (stream bs) (stream cs) (stream ds) (stream es))

-- | Indexed element-wise combination of five vectors.
{-# INLINE izipWith5D #-}
izipWith5D :: forall x v i a b c d e f. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, EnumFromZero x i, LiftConstructor x)
           => (x i -> x a -> x b -> x c -> x d -> x e -> x f) -> (i -> a -> b -> c -> d -> e ->  f)
           -> v a -> v b -> v c -> v d -> v e -> v f
izipWith5D vf f = \as bs cs ds es -> unstream (Stream.zipWith5D (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Stream.indexed $ stream as) (stream bs) (stream cs) (stream ds) (stream es))

-- | Indexed element-wise combination of six vectors.
{-# INLINE izipWith6 #-}
izipWith6 :: forall x v i a b c d e f g. (SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, SIMDVector v x g, SIMDEnumFromZero i)
          => (forall xx. SIMD xx => xx i -> xx a -> xx b -> xx c -> xx d -> xx e -> xx f -> xx g)
          -> v a -> v b -> v c -> v d -> v e -> v f -> v g
izipWith6 f = \as bs cs ds es fs -> unstream (Stream.zipWith6D (\t -> case deconstructTuple2 t of (i, a) -> f @x i a) (\(i, a) -> coerce (f @Identity) i a) (Stream.indexed $ stream as) (stream bs) (stream cs) (stream ds) (stream es) (stream fs))

-- | Indexed element-wise combination of six vectors.
{-# INLINE izipWith6D #-}
izipWith6D :: forall x v i a b c d e f g. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, SIMDVector v x g, EnumFromZero x i, LiftConstructor x)
           => (x i -> x a -> x b -> x c -> x d -> x e -> x f -> x g) -> (i -> a -> b -> c -> d -> e -> f -> g)
           -> v a -> v b -> v c -> v d -> v e -> v f -> v g
izipWith6D vf f = \as bs cs ds es fs -> unstream (Stream.zipWith6D (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Stream.indexed $ stream as) (stream bs) (stream cs) (stream ds) (stream es) (stream fs))

-- | Zip two vectors into a vector of pairs.
{-# INLINE zip #-}
zip :: forall x v a b. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x (a, b), LiftConstructor x) => v a -> v b -> v (a, b)
zip = zipWithD (mkTuple2 @x) (,)

-- | Zip three vectors into a vector of 3-tuples.
{-# INLINE zip3 #-}
zip3 :: forall x v a b c. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x (a, b, c), LiftConstructor x) => v a -> v b -> v c -> v (a, b, c)
zip3 = zipWith3D (mkTuple3 @x) (,,)

-- | Zip four vectors into a vector of 4-tuples.
{-# INLINE zip4 #-}
zip4 :: forall x v a b c d. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x (a, b, c, d), LiftConstructor x) => v a -> v b -> v c -> v d -> v (a, b, c, d)
zip4 = zipWith4D (mkTuple4 @x) (,,,)

-- | Zip five vectors into a vector of 5-tuples.
{-# INLINE zip5 #-}
zip5 :: forall x v a b c d e. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x (a, b, c, d, e), LiftConstructor x) => v a -> v b -> v c -> v d -> v e -> v (a, b, c, d, e)
zip5 = zipWith5D (mkTuple5 @x) (,,,,)

-- | Zip six vectors into a vector of 6-tuples.
{-# INLINE zip6 #-}
zip6 :: forall x v a b c d e f. (SIMDVector v x a, SIMDVector v x b, SIMDVector v x c, SIMDVector v x d, SIMDVector v x e, SIMDVector v x f, SIMDVector v x (a, b, c, d, e, f), LiftConstructor x) => v a -> v b -> v c -> v d -> v e -> v f -> v (a, b, c, d, e, f)
zip6 = zipWith6D (mkTuple6 @x) (,,,,,)

-- zipWithM
-- izipWithM

-- | Monadic element-wise combination, discarding results.
{-# INLINE zipWithM_ #-}
zipWithM_ :: forall x v m a b c. (Monad m, SIMD x, SIMDVector v x a, SIMDVector v x b)
          => (forall xx. SIMD xx => xx a -> xx b -> m (xx c))
          -> v a -> v b -> m ()
zipWithM_ f = \as bs -> Stream.zipWithMD_ (f @x) (\a -> coerce (fmap runIdentity . f (Identity a))) (stream as) (stream bs)

-- | Monadic element-wise combination, discarding results.
{-# INLINE zipWithMD_ #-}
zipWithMD_ :: forall x v m a b c. (Monad m, SIMDVector v x a, SIMDVector v x b)
           => (x a -> x b -> m (x c)) -> (a -> b -> m c)
           -> v a -> v b -> m ()
zipWithMD_ vf f = \as bs -> Stream.zipWithMD_ vf f (stream as) (stream bs)

-- | Indexed monadic element-wise combination, discarding results.
{-# INLINE izipWithM_ #-}
izipWithM_ :: forall x v m i a b c. (Monad m, SIMD x, SIMDVector v x a, SIMDVector v x b, SIMDEnumFromZero i)
            => (forall xx. SIMD xx => xx i -> xx a -> xx b -> m (xx c))
            -> v a -> v b -> m ()
izipWithM_ f = \as bs -> Stream.zipWithMD_ (\t -> case deconstructTuple2 t of (i, a) -> f @x i a) (\(i, a) -> coerce (fmap runIdentity . f (Identity i) (Identity a))) (Stream.indexed $ stream as) (stream bs)

-- | Indexed monadic element-wise combination, discarding results.
{-# INLINE izipWithMD_ #-}
izipWithMD_ :: forall x v m i a b c. (Monad m, SIMDVector v x a, SIMDVector v x b, EnumFromZero x i, LiftConstructor x)
            => (x i -> x a -> x b -> m (x c)) -> (i -> a -> b -> m c)
            -> v a -> v b -> m ()
izipWithMD_ vf f = \as bs -> Stream.zipWithMD_ (\t -> case deconstructTuple2 t of (i, a) -> vf i a) (\(i, a) -> f i a) (Stream.indexed $ stream as) (stream bs)

-- unzip
-- unzip3
-- unzip4
-- unzip5
-- unzip6

-- | Left fold over a vector using a rank-2 combining function.
-- The function is applied at SIMD width for the bulk, then reduced horizontally.
{-# INLINE fold #-}
fold :: forall x v a. (SIMD x, SIMDElement a, SIMDVector v x a) => (forall xx. SIMD xx => xx a -> xx a -> xx a) -> x a -> v a -> a
fold append initial = runIdentity . Stream.fold append initial . stream

-- | Strict left fold. Like 'fold' but forces the accumulator at each step.
{-# INLINE fold' #-}
fold' :: forall x v a. (SIMD x, SIMDElement a, SIMDVector v x a) => (forall xx. SIMD xx => xx a -> xx a -> xx a) -> x a -> v a -> a
fold' append initial = runIdentity . Stream.fold' append initial . stream

-- | Convert between vector representations (e.g. primitive to unboxed) using stream fusion.
{-# INLINE convert #-}
convert :: forall x v w a. (SIMDVector v x a, SIMDVector w x a) => v a -> w a
convert = unstream . stream @x
