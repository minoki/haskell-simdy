module Data.Simdy.Mask
  ( Boolean (..)
  , Mask
  , selectSIMD
  , Selectable (..)
  , Equatable (..)
  , (/=)
  , (==*)
  , (/=*)
  , Ordered (..)
  {-
  , (<*)
  , (<=*)
  , (>*)
  , (>=*)
  -}
  ) where
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Default
import           Prelude (Bool)

selectSIMD :: (SIMD f, SIMDElement a)
           => f Bool -- ^ condition
           -> f a -- ^ then-expression
           -> f a -- ^ else-expression
           -> f a
selectSIMD = selectF
{-# INLINE selectSIMD #-}

{-
infix 4 ==*, /=*

-- TODO: Why is 'EquatableF f a' needed?
(==*) :: (SIMD f, SIMDEq a, EquatableF f a) => f a -> f a -> f Bool
(==*) = eqF
{-# INLINE (==*) #-}

-- TODO: Why is 'EquatableF f a' needed?
(/=*) :: (SIMD f, SIMDEq a, EquatableF f a) => f a -> f a -> f Bool
x /=* y = not (eqF x y)
{-# INLINE (/=*) #-}
-}

{-
infix 4 <*, <=*, >*, >=*

(<*) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool
(<*) = ltF
{-# INLINE (<*) #-}

(<=*) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool
(<=*) = leF
{-# INLINE (<=*) #-}

(>*) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool
(>*) = gtF
{-# INLINE (>*) #-}

(>=*) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool
(>=*) = geF
{-# INLINE (>=*) #-}
-}
