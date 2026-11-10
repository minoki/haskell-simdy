{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Simdy.Internal.MinMax where
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity (Identity))
import           Data.Int
import           Data.Kind (Constraint)
import           Data.Word
import           GHC.TypeError (ErrorMessage (Text), Unsatisfiable)
import           Prelude hiding (max, min)
import qualified Prelude

-- | Lane-wise minimum and maximum.
--
-- For floating-point types, 'min' and 'max' follow IEEE 754-2019 semantics
-- (propagating NaN), while 'minimumNumber' and 'maximumNumber' prefer numeric
-- values over NaN.
class MinMax a where
  type MinMaxIsCompatibleWithOrd a :: Constraint
  type MinMaxIsCompatibleWithOrd a = ()
  min :: a -> a -> a
  max :: a -> a -> a
  -- | Like 'min', but returns the numeric value if one argument is NaN.
  minimumNumber :: a -> a -> a
  -- | Like 'max', but returns the numeric value if one argument is NaN.
  maximumNumber :: a -> a -> a

newtype Scalar a = MkScalar a

instance Ord a => MinMax (Scalar a) where
  type MinMaxIsCompatibleWithOrd (Scalar a) = ()
  min = coerce (Prelude.min @a)
  max = coerce (Prelude.max @a)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}

deriving via Scalar Int instance MinMax Int
deriving via Scalar Int8 instance MinMax Int8
deriving via Scalar Int16 instance MinMax Int16
deriving via Scalar Int32 instance MinMax Int32
deriving via Scalar Int64 instance MinMax Int64
deriving via Scalar Word instance MinMax Word
deriving via Scalar Word8 instance MinMax Word8
deriving via Scalar Word16 instance MinMax Word16
deriving via Scalar Word32 instance MinMax Word32
deriving via Scalar Word64 instance MinMax Word64

foreign import ccall unsafe "hs_simdy_minimumFloat"
  minimumFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_maximumFloat"
  maximumFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_minimumNumberFloat"
  minimumNumberFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_maximumNumberFloat"
  maximumNumberFloat :: Float -> Float -> Float

foreign import ccall unsafe "hs_simdy_minimumDouble"
  minimumDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_maximumDouble"
  maximumDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_minimumNumberDouble"
  minimumNumberDouble :: Double -> Double -> Double

foreign import ccall unsafe "hs_simdy_maximumNumberDouble"
  maximumNumberDouble :: Double -> Double -> Double

-- | IEEE 754-2019 compliant instance
instance MinMax Float where
  type MinMaxIsCompatibleWithOrd Float = Unsatisfiable (Text "This instance is not compatible with the min/max functions in Ord")
  min = minimumFloat
  max = maximumFloat
  minimumNumber = minimumNumberFloat
  maximumNumber = maximumNumberFloat

-- | IEEE 754-2019 compliant instance
instance MinMax Double where
  type MinMaxIsCompatibleWithOrd Double = Unsatisfiable (Text "This instance is not compatible with the min/max functions in Ord")
  min = minimumDouble
  max = maximumDouble
  minimumNumber = minimumNumberDouble
  maximumNumber = maximumNumberDouble

deriving newtype instance MinMax a => MinMax (Identity a)

newtype IEEEMin a = IEEEMin { getIEEEMin :: a }
  deriving (Eq, Show, Functor)

instance MinMax a => Semigroup (IEEEMin a) where
  (<>) = coerce (min @a)
  {-# INLINE (<>) #-}

newtype IEEEMax a = IEEEMax { getIEEEMax :: a }
  deriving (Eq, Show, Functor)

instance MinMax a => Semigroup (IEEEMax a) where
  (<>) = coerce (max @a)
  {-# INLINE (<>) #-}

newtype MinimumNumber a = MinimumNumber { getMinimumNumber :: a }
  deriving (Eq, Show, Functor)

instance MinMax a => Semigroup (MinimumNumber a) where
  (<>) = coerce (minimumNumber @a)
  {-# INLINE (<>) #-}

newtype MaximumNumber a = MaximumNumber { getMaximumNumber :: a }
  deriving (Eq, Show, Functor)

instance MinMax a => Semigroup (MaximumNumber a) where
  (<>) = coerce (maximumNumber @a)
  {-# INLINE (<>) #-}
