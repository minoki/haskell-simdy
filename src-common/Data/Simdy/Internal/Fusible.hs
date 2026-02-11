{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
-- |
-- Arithmetic operators that enable automatic FMA fusion via rewrite rules.
--
-- Import this module instead of using 'Prelude' arithmetic operators to allow
-- GHC rewrite rules to fuse @a * b + c@ into 'Data.Simdy.Internal.FMA.fusedMultiplyAdd'.
-- The operators are only fused when FMA hardware support is enabled at compile time;
-- otherwise they behave identically to their 'Prelude' counterparts.
module Data.Simdy.Internal.Fusible where
import           Prelude hiding ((+), (-), (*))
import qualified Prelude
#if defined(USE_FMA)
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Simdy.Internal.FMA
import           GHC.Float
#endif

infixl 6 +, -
infixl 7 *

(+) :: Num a => a -> a -> a
(+) = (Prelude.+)

(-) :: Num a => a -> a -> a
(-) = (Prelude.-)

(*) :: Num a => a -> a -> a
(*) = (Prelude.*)

#if defined(USE_FMA)

{-# INLINE [0] (+) #-}
{-# INLINE [0] (-) #-}
{-# INLINE [0] (*) #-}

{-# RULES
"Fusible/*+/Float" forall a b c.
  a * b + c = fusedMultiplyAdd a b c :: Float
"Fusible/*-/Float" forall a b c.
  a * b - c = fusedMultiplyAdd a b (-c) :: Float
"Fusible/-*+/Float" forall a b c.
  negateFloat (a * b) + c = fusedMultiplyAdd (-a) b c :: Float
"Fusible/-*-/Float" forall a b c.
  negateFloat (a * b) - c = fusedMultiplyAdd (-a) b (-c) :: Float
"Fusible/+*/Float" forall a b c.
  a + b * c = fusedMultiplyAdd b c a :: Float
"Fusible/-*/Float" forall a b c.
  a - b * c = fusedMultiplyAdd (-b) c a :: Float

"Fusible/*+/Double" forall a b c.
  a * b + c = fusedMultiplyAdd a b c :: Double
"Fusible/*-/Double" forall a b c.
  a * b - c = fusedMultiplyAdd a b (-c) :: Double
"Fusible/-*+/Double" forall a b c.
  negateDouble (a * b) + c = fusedMultiplyAdd (-a) b c :: Double
"Fusible/-*-/Double" forall a b c.
  negateDouble (a * b) - c = fusedMultiplyAdd (-a) b (-c) :: Double
"Fusible/+*/Double" forall a b c.
  a + b * c = fusedMultiplyAdd b c a :: Double
"Fusible/-*/Double" forall a b c.
  a - b * c = fusedMultiplyAdd (-b) c a :: Double

"Fusible/*+/Identity Float" forall a b c.
  a * b + c = fusedMultiplyAdd a b c :: Identity Float
"Fusible/*-/Identity Float" forall a b c.
  a * b - c = fusedMultiplyAdd a b (-c) :: Identity Float
"Fusible/-*+/Identity Float" forall a b c.
  coerce negateFloat (a * b) + c = fusedMultiplyAdd (-a) b c :: Identity Float
"Fusible/-*-/Identity Float" forall a b c.
  coerce negateFloat (a * b) - c = fusedMultiplyAdd (-a) b (-c) :: Identity Float
"Fusible/+*/Identity Float" forall a b c.
  a + b * c = fusedMultiplyAdd b c a :: Identity Float
"Fusible/-*/Identity Float" forall a b c.
  a - b * c = fusedMultiplyAdd (-b) c a :: Identity Float

"Fusible/*+/Identity Double" forall a b c.
  a * b + c = fusedMultiplyAdd a b c :: Identity Double
"Fusible/*-/Identity Double" forall a b c.
  a * b - c = fusedMultiplyAdd a b (-c) :: Identity Double
"Fusible/-*+/Identity Double" forall a b c.
  coerce negateDouble (a * b) + c = fusedMultiplyAdd (-a) b c :: Identity Double
"Fusible/-*-/Identity Double" forall a b c.
  coerce negateDouble (a * b) - c = fusedMultiplyAdd (-a) b (-c) :: Identity Double
"Fusible/+*/Identity Double" forall a b c.
  a + b * c = fusedMultiplyAdd b c a :: Identity Double
"Fusible/-*/Identity Double" forall a b c.
  a - b * c = fusedMultiplyAdd (-b) c a :: Identity Double
  #-}

#else

{-# INLINE (+) #-}
{-# INLINE (-) #-}
{-# INLINE (*) #-}

#endif
