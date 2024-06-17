{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.ShortVector.Class (module M, module Data.ShortVector.Class) where
import Data.ShortVector.Class.Generated as M -- PackXn, UnpackXn, MkTuple, DeconstructTuple

class SplitShortVector f g a | f -> g, g -> f where
  splitShortVector :: g a -> (f a, f a)
  unsplitShortVector :: f a -> f a -> g a

class Broadcast f a where
  broadcast :: a -> f a

class MonoMap f a where
  monoMap :: (a -> a) -> f a -> f a
  monoZipWith :: (a -> a -> a) -> f a -> f a -> f a

class NumF f a where
  addF :: f a -> f a -> f a
  subF :: f a -> f a -> f a
  mulF :: f a -> f a -> f a
  negateF :: f a -> f a
  absF :: f a -> f a
  signumF :: f a -> f a
  fromIntegerF :: Integer -> f a
  default fromIntegerF :: (Num a, Broadcast f a) => Integer -> f a
  fromIntegerF = broadcast . fromInteger

class NumF f a => FractionalF f a where
  divF :: f a -> f a -> f a
  recipF :: f a -> f a
  fromRationalF :: Rational -> f a
  default fromRationalF :: (Fractional a, Broadcast f a) => Rational -> f a
  fromRationalF = broadcast . fromRational

class FractionalF f a => FloatingF f a where
  piF :: f a
  default piF :: (Floating a, Broadcast f a) => f a
  piF = broadcast pi
  expF :: f a -> f a
  default expF :: (Floating a, MonoMap f a) => f a -> f a
  expF = monoMap exp
  logF :: f a -> f a
  default logF :: (Floating a, MonoMap f a) => f a -> f a
  logF = monoMap log
  sqrtF :: f a -> f a
  default sqrtF :: (Floating a, MonoMap f a) => f a -> f a
  sqrtF = monoMap sqrt
  powF :: f a -> f a -> f a
  default powF :: (Floating a, MonoMap f a) => f a -> f a -> f a
  powF = monoZipWith (**)
  logBaseF :: f a -> f a -> f a
  default logBaseF :: (Floating a, MonoMap f a) => f a -> f a -> f a
  logBaseF = monoZipWith logBase
  sinF :: f a -> f a
  default sinF :: (Floating a, MonoMap f a) => f a -> f a
  sinF = monoMap sin
  cosF :: f a -> f a
  default cosF :: (Floating a, MonoMap f a) => f a -> f a
  cosF = monoMap cos
  tanF :: f a -> f a
  default tanF :: (Floating a, MonoMap f a) => f a -> f a
  tanF = monoMap tan
  asinF :: f a -> f a
  default asinF :: (Floating a, MonoMap f a) => f a -> f a
  asinF = monoMap asin
  acosF :: f a -> f a
  default acosF :: (Floating a, MonoMap f a) => f a -> f a
  acosF = monoMap acos
  atanF :: f a -> f a
  default atanF :: (Floating a, MonoMap f a) => f a -> f a
  atanF = monoMap atan
  sinhF :: f a -> f a
  default sinhF :: (Floating a, MonoMap f a) => f a -> f a
  sinhF = monoMap sinh
  coshF :: f a -> f a
  default coshF :: (Floating a, MonoMap f a) => f a -> f a
  coshF = monoMap cosh
  tanhF :: f a -> f a
  default tanhF :: (Floating a, MonoMap f a) => f a -> f a
  tanhF = monoMap tanh
  asinhF :: f a -> f a
  default asinhF :: (Floating a, MonoMap f a) => f a -> f a
  asinhF = monoMap asinh
  acoshF :: f a -> f a
  default acoshF :: (Floating a, MonoMap f a) => f a -> f a
  acoshF = monoMap acosh
  atanhF :: f a -> f a
  default atanhF :: (Floating a, MonoMap f a) => f a -> f a
  atanhF = monoMap atanh

-- TODO: Add Data.Bits counterpart
