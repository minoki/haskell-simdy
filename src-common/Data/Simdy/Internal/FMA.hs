{-# LANGUAGE CPP #-}
{-# LANGUAGE MonoLocalBinds #-}
#if MIN_VERSION_base(4, 19, 0)
{-# LANGUAGE MagicHash #-}
#endif
-- |
-- Fused multiply-add (FMA) support.
--
-- FMA computes @x * y + z@ in a single operation with only one rounding step,
-- which is both faster and more accurate than separate multiply and add.
module Data.Simdy.Internal.FMA where
import           Data.Functor.Identity (Identity (Identity))
import           Data.Coerce
#if MIN_VERSION_base(4, 19, 0)
import           GHC.Exts
#endif

-- | Types that support fused multiply-add: @fusedMultiplyAdd x y z = x * y + z@
-- with a single rounding step.
class Num a => FusedMultiplyAdd a where
  -- | @fusedMultiplyAdd x y z@ computes @x * y + z@ with a single rounding.
  fusedMultiplyAdd :: a -> a -> a -> a

-- | Constraint that is satisfiable only when FMA support was enabled at compile time
-- via Cabal package flags (@haswell@, @avx512@) or target architecture (@aarch64@).
-- This is __not__ a runtime CPUID check.
class HasFMA
-- | A witness that FMA support is available. Pattern match on 'MkFMAWitness'
-- to bring the 'HasFMA' constraint into scope.
data FMAWitness = HasFMA => MkFMAWitness
-- | Returns @'Just' 'MkFMAWitness'@ if FMA support was enabled at compile time,
-- 'Nothing' otherwise.  This is determined by Cabal package flags, not by runtime
-- CPU feature detection.
isFMAAvailable :: Maybe FMAWitness

#if defined(USE_FMA)
instance HasFMA
isFMAAvailable = Just MkFMAWitness
#else
isFMAAvailable = Nothing

fmaIsDisabled :: HasFMA => a
fmaIsDisabled = error "simdy: FMA is disabled"
#endif

#if MIN_VERSION_base(4, 19, 0)
-- GHC 9.8 or later

{-
-- | @x * y + z@
fmaddFloat# :: Float# -> Float# -> Float# -> Float#
fmaddFloat# = GHC.Exts.fmaddFloat#
{-# INLINE [0] fmaddFloat# #-}

-- | @x * y - z@
fmsubFloat# :: Float# -> Float# -> Float# -> Float#
fmsubFloat# = GHC.Exts.fmsubFloat#
{-# INLINE [0] fmsubFloat# #-}

-- | @-x * y + z@
fnmaddFloat# :: Float# -> Float# -> Float# -> Float#
fnmaddFloat# = GHC.Exts.fnmaddFloat#
{-# INLINE [0] fnmaddFloat# #-}

-- | @-x * y - z@
fnmsubFloat# :: Float# -> Float# -> Float# -> Float#
fnmsubFloat# = GHC.Exts.fnmsubFloat#
{-# INLINE [0] fnmsubFloat# #-}

{-# RULES
"fmaddFloat#/negate x" forall x.
  fmaddFloat# (negateFloat# x) = fnmaddFloat# x
"fmaddFloat#/negate y" forall x y.
  fmaddFloat# x (negateFloat# y) = fnmaddFloat# x y
"fmaddFloat#/negate z" forall x y z.
  fmaddFloat# x y (negateFloat# z) = fmsubFloat# x y z
"fmsubFloat#/negate x" forall x.
  fmsubFloat# (negateFloat# x) = fnmsubFloat# x
"fmsubFloat#/negate y" forall x y.
  fmsubFloat# x (negateFloat# y) = fnmsubFloat# x y
"fmsubFloat#/negate z" forall x y z.
  fmsubFloat# x y (negateFloat# z) = fmaddFloat# x y z
"fnmaddFloat#/negate x" forall x.
  fnmaddFloat# (negateFloat# x) = fmaddFloat# x
"fnmaddFloat#/negate y" forall x y.
  fnmaddFloat# x (negateFloat# y) = fmaddFloat# x y
"fnmaddFloat#/negate z" forall x y z.
  fnmaddFloat# x y (negateFloat# z) = fnmsubFloat# x y z
"fnmsubFloat#/negate x" forall x.
  fnmsubFloat# (negateFloat# x) = fmsubFloat# x
"fnmsubFloat#/negate y" forall x y.
  fnmsubFloat# x (negateFloat# y) = fmsubFloat# x y
"fnmsubFloat#/negate z" forall x y z.
  fnmsubFloat# x y (negateFloat# z) = fnmaddFloat# x y z
  #-}

-- | @x * y + z@
fmaddDouble# :: Double# -> Double# -> Double# -> Double#
fmaddDouble# = GHC.Exts.fmaddDouble#
{-# INLINE [0] fmaddDouble# #-}

-- | @x * y - z@
fmsubDouble# :: Double# -> Double# -> Double# -> Double#
fmsubDouble# = GHC.Exts.fmsubDouble#
{-# INLINE [0] fmsubDouble# #-}

-- | @-x * y + z@
fnmaddDouble# :: Double# -> Double# -> Double# -> Double#
fnmaddDouble# = GHC.Exts.fnmaddDouble#
{-# INLINE [0] fnmaddDouble# #-}

-- | @-x * y - z@
fnmsubDouble# :: Double# -> Double# -> Double# -> Double#
fnmsubDouble# = GHC.Exts.fnmsubDouble#
{-# INLINE [0] fnmsubDouble# #-}

{-# RULES
"fmaddDouble#/negate x" forall x.
  fmaddDouble# (negateDouble# x) = fnmaddDouble# x
"fmaddDouble#/negate y" forall x y.
  fmaddDouble# x (negateDouble# y) = fnmaddDouble# x y
"fmaddDouble#/negate z" forall x y z.
  fmaddDouble# x y (negateDouble# z) = fmsubDouble# x y z
"fmsubDouble#/negate x" forall x.
  fmsubDouble# (negateDouble# x) = fnmsubDouble# x
"fmsubDouble#/negate y" forall x y.
  fmsubDouble# x (negateDouble# y) = fnmsubDouble# x y
"fmsubDouble#/negate z" forall x y z.
  fmsubDouble# x y (negateDouble# z) = fmaddDouble# x y z
"fnmaddDouble#/negate x" forall x.
  fnmaddDouble# (negateDouble# x) = fmaddDouble# x
"fnmaddDouble#/negate y" forall x y.
  fnmaddDouble# x (negateDouble# y) = fmaddDouble# x y
"fnmaddDouble#/negate z" forall x y z.
  fnmaddDouble# x y (negateDouble# z) = fnmsubDouble# x y z
"fnmsubDouble#/negate x" forall x.
  fnmsubDouble# (negateDouble# x) = fmsubDouble# x
"fnmsubDouble#/negate y" forall x y.
  fnmsubDouble# x (negateDouble# y) = fmsubDouble# x y
"fnmsubDouble#/negate z" forall x y z.
  fnmsubDouble# x y (negateDouble# z) = fnmaddDouble# x y z
  #-}
-}

instance FusedMultiplyAdd Float where
  fusedMultiplyAdd (F# x) (F# y) (F# z) = F# (fmaddFloat# x y z)
  {-# INLINE fusedMultiplyAdd #-}

instance FusedMultiplyAdd Double where
  fusedMultiplyAdd (D# x) (D# y) (D# z) = D# (fmaddDouble# x y z)
  {-# INLINE fusedMultiplyAdd #-}
#else
-- Should we depend on fp-ieee?
foreign import ccall unsafe "fmaf"
  fmaFloat :: Float -> Float -> Float -> Float

foreign import ccall unsafe "fma"
  fmaDouble :: Double -> Double -> Double -> Double

instance FusedMultiplyAdd Float where
  fusedMultiplyAdd = fmaFloat
  {-# INLINE fusedMultiplyAdd #-}

instance FusedMultiplyAdd Double where
  fusedMultiplyAdd = fmaDouble
  {-# INLINE fusedMultiplyAdd #-}
#endif

instance FusedMultiplyAdd a => FusedMultiplyAdd (Identity a) where
  fusedMultiplyAdd = coerce (fusedMultiplyAdd @a)
  {-# INLINE fusedMultiplyAdd #-}
