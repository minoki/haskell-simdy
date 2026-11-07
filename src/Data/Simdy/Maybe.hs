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
