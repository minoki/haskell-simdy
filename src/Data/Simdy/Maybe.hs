module Data.Simdy.Maybe where
import           Data.Default (Default)
import qualified Data.Simdy.Internal.Class as Internal
import           Data.Simdy.Internal.Default (SIMD, SIMDElement)

mkMaybe :: (SIMD x, SIMDElement a) => x Bool -> x a -> x (Maybe a)
mkMaybe = Internal.mkMaybe
{-# INLINE mkMaybe #-}

mkJust :: (SIMD x, SIMDElement a) => x a -> x (Maybe a)
mkJust = Internal.mkJust
{-# INLINE mkJust #-}

mkNothing :: (SIMD x, Default a, SIMDElement a) => x (Maybe a)
mkNothing = Internal.mkNothing
{-# INLINE mkNothing #-}

isJust' :: (SIMD x, SIMDElement a) => x (Maybe a) -> x Bool
isJust' = Internal.isJust'
{-# INLINE isJust' #-}

isNothing' :: (SIMD x, SIMDElement a) => x (Maybe a) -> x Bool
isNothing' = Internal.isNothing'
{-# INLINE isNothing' #-}

fromJust' :: (SIMD x, Default a, SIMDElement a) => x (Maybe a) -> x a
fromJust' = Internal.fromJust'
{-# INLINE fromJust' #-}

fromMaybe' :: (SIMD x, SIMDElement a) => x a -> x (Maybe a) -> x a
fromMaybe' = Internal.fromMaybe'
{-# INLINE fromMaybe' #-}

maybe' :: (SIMD x, SIMDElement a, SIMDElement b) => x b -> (x a -> x b) -> x (Maybe a) -> x b
maybe' = Internal.maybe'
{-# INLINE maybe' #-}

fmapMaybe :: (SIMD x, SIMDElement a, SIMDElement b) => (x a -> x b) -> x (Maybe a) -> x (Maybe b)
fmapMaybe = Internal.fmapMaybe
{-# INLINE fmapMaybe #-}

lift2Maybe :: (SIMD x, {- Default a, Default b, -} SIMDElement a, SIMDElement b, SIMDElement c) => (x a -> x b -> x c) -> x (Maybe a) -> x (Maybe b) -> x (Maybe c)
lift2Maybe = Internal.lift2Maybe
-- lift2Maybe f x y = Internal.mkMaybe (Internal.isJust' x .&. Internal.isJust' y) (f (Internal.fromJust' x) (Internal.fromJust' y))
{-# INLINE lift2Maybe #-}

bindMaybe :: (SIMD x, {- Default a, Default b, -} SIMDElement a, SIMDElement b) => x (Maybe a) -> (x a -> x (Maybe b)) -> x (Maybe b)
bindMaybe = Internal.bindMaybe
-- bindMaybe x f = let y = f (Internal.fromJust' x)
--                 in Internal.mkMaybe (Internal.isJust' x .&. Internal.isJust' y) (Internal.fromJust' y)
{-# INLINE bindMaybe #-}

joinMaybe :: (SIMD x, SIMDElement a) => x (Maybe (Maybe a)) -> x (Maybe a)
joinMaybe = Internal.joinMaybe
{-# INLINE joinMaybe #-}
