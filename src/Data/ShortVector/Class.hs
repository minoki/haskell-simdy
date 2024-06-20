{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.ShortVector.Class (module M, module Data.ShortVector.Class) where
import Data.Kind
import Data.Coerce
import Data.ShortVector.Class.Generated as M -- PackXn, UnpackXn, MkTuple, DeconstructTuple
import GHC.Exts
import Data.Primitive
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed.Base as VUB
import Control.Monad.ST
import GHC.ST
import Data.Int
import Data.Word
import Data.Semigroup
import Data.Monoid
import Data.Functor.Identity
import Foreign.Storable

{-
class SplitShortVector f g a | g -> f where
  splitShortVector :: g a -> (f a, f a)
  unsplitShortVector :: f a -> f a -> g a
-}

class Broadcast f a where
  broadcast :: a -> f a

instance Broadcast Identity a where
  broadcast = Identity

class MonoMap f a where
  monoMap :: (a -> a) -> f a -> f a

instance MonoMap Identity a where
  monoMap = coerce

class MonoZipWith f a where
  monoZipWith :: (a -> a -> a) -> f a -> f a -> f a

instance MonoZipWith Identity a where
  monoZipWith = coerce

type ShortVectorLength :: (Type -> Type) -> Constraint
class ShortVectorLength f where
  shortVectorLength :: Int

instance ShortVectorLength Identity where
  shortVectorLength = 1

class ShortVectorLength f => PrimSV f a where
  indexByteArraySV# :: ByteArray# -> Int# -> f a
  readByteArraySV# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, f a #)
  writeByteArraySV# :: MutableByteArray# s -> Int# -> f a -> State# s -> State# s

instance Prim a => PrimSV Identity a where
  indexByteArraySV# = coerce (indexByteArray# @a)
  readByteArraySV# = coerce (readByteArray# @a)
  writeByteArraySV# = coerce (writeByteArray# @a)

unsafeIndexPrimSV :: PrimSV f a => VP.Vector a -> Int -> f a
unsafeIndexPrimSV (VP.Vector (I# offset) _ (ByteArray ba)) (I# i) = indexByteArraySV# ba (offset +# i)

unsafeReadPrimSV :: PrimSV f a => VP.MVector s a -> Int -> ST s (f a)
unsafeReadPrimSV (VP.MVector (I# offset) _ (MutableByteArray ba)) (I# i) = ST (readByteArraySV# ba (offset +# i))

unsafeWritePrimSV :: PrimSV f a => VP.MVector s a -> Int -> f a -> ST s ()
unsafeWritePrimSV (VP.MVector (I# offset) _ (MutableByteArray ba)) (I# i) !v = ST (\s -> (# writeByteArraySV# ba (offset +# i) v s, () #))

class ShortVectorLength f => StorableSV f a where
  peekElemOffSV :: Ptr a -> Int -> IO (f a)
  pokeElemOffSV :: Ptr a -> Int -> f a -> IO ()

instance Storable a => StorableSV Identity a where
  peekElemOffSV = coerce (peekElemOff @a)
  pokeElemOffSV = coerce (pokeElemOff @a)

class ShortVectorLength f => UnboxSV f a where
  unsafeIndexUnboxedSV :: VU.Vector a -> Int -> f a
  unsafeReadUnboxedSV :: VUM.MVector s a -> Int -> ST s (f a)
  unsafeWriteUnboxedSV :: VUM.MVector s a -> Int -> f a -> ST s ()

instance (ShortVectorLength f, PrimSV f Float) => UnboxSV f Float where
  unsafeIndexUnboxedSV (VUB.V_Float v) = unsafeIndexPrimSV v
  unsafeReadUnboxedSV (VUB.MV_Float mv) = unsafeReadPrimSV mv
  unsafeWriteUnboxedSV (VUB.MV_Float mv) = unsafeWritePrimSV mv

instance (ShortVectorLength f, PrimSV f Double) => UnboxSV f Double where
  unsafeIndexUnboxedSV (VUB.V_Double v) = unsafeIndexPrimSV v
  unsafeReadUnboxedSV (VUB.MV_Double mv) = unsafeReadPrimSV mv
  unsafeWriteUnboxedSV (VUB.MV_Double mv) = unsafeWritePrimSV mv

instance (ShortVectorLength f, PrimSV f Int8) => UnboxSV f Int8 where
  unsafeIndexUnboxedSV (VUB.V_Int8 v) = unsafeIndexPrimSV v
  unsafeReadUnboxedSV (VUB.MV_Int8 mv) = unsafeReadPrimSV mv
  unsafeWriteUnboxedSV (VUB.MV_Int8 mv) = unsafeWritePrimSV mv

instance (ShortVectorLength f, PrimSV f Int16) => UnboxSV f Int16 where
  unsafeIndexUnboxedSV (VUB.V_Int16 v) = unsafeIndexPrimSV v
  unsafeReadUnboxedSV (VUB.MV_Int16 mv) = unsafeReadPrimSV mv
  unsafeWriteUnboxedSV (VUB.MV_Int16 mv) = unsafeWritePrimSV mv

instance (ShortVectorLength f, PrimSV f Int32) => UnboxSV f Int32 where
  unsafeIndexUnboxedSV (VUB.V_Int32 v) = unsafeIndexPrimSV v
  unsafeReadUnboxedSV (VUB.MV_Int32 mv) = unsafeReadPrimSV mv
  unsafeWriteUnboxedSV (VUB.MV_Int32 mv) = unsafeWritePrimSV mv

instance (ShortVectorLength f, PrimSV f Int64) => UnboxSV f Int64 where
  unsafeIndexUnboxedSV (VUB.V_Int64 v) = unsafeIndexPrimSV v
  unsafeReadUnboxedSV (VUB.MV_Int64 mv) = unsafeReadPrimSV mv
  unsafeWriteUnboxedSV (VUB.MV_Int64 mv) = unsafeWritePrimSV mv

instance (ShortVectorLength f, PrimSV f Word8) => UnboxSV f Word8 where
  unsafeIndexUnboxedSV (VUB.V_Word8 v) = unsafeIndexPrimSV v
  unsafeReadUnboxedSV (VUB.MV_Word8 mv) = unsafeReadPrimSV mv
  unsafeWriteUnboxedSV (VUB.MV_Word8 mv) = unsafeWritePrimSV mv

instance (ShortVectorLength f, PrimSV f Word16) => UnboxSV f Word16 where
  unsafeIndexUnboxedSV (VUB.V_Word16 v) = unsafeIndexPrimSV v
  unsafeReadUnboxedSV (VUB.MV_Word16 mv) = unsafeReadPrimSV mv
  unsafeWriteUnboxedSV (VUB.MV_Word16 mv) = unsafeWritePrimSV mv

instance (ShortVectorLength f, PrimSV f Word32) => UnboxSV f Word32 where
  unsafeIndexUnboxedSV (VUB.V_Word32 v) = unsafeIndexPrimSV v
  unsafeReadUnboxedSV (VUB.MV_Word32 mv) = unsafeReadPrimSV mv
  unsafeWriteUnboxedSV (VUB.MV_Word32 mv) = unsafeWritePrimSV mv

instance (ShortVectorLength f, PrimSV f Word64) => UnboxSV f Word64 where
  unsafeIndexUnboxedSV (VUB.V_Word64 v) = unsafeIndexPrimSV v
  unsafeReadUnboxedSV (VUB.MV_Word64 mv) = unsafeReadPrimSV mv
  unsafeWriteUnboxedSV (VUB.MV_Word64 mv) = unsafeWritePrimSV mv

instance (ShortVectorLength f, Broadcast f ()) => UnboxSV f () where
  unsafeIndexUnboxedSV (VUB.V_Unit _) !_ = broadcast ()
  unsafeReadUnboxedSV (VUB.MV_Unit _) !_ = pure (broadcast ())
  unsafeWriteUnboxedSV (VUB.MV_Unit _) !_ !_ = pure ()

type WrappedMulti :: (Type -> Type) -> Type -> Type
newtype WrappedMulti f a = MkWrappedMulti (f a)

class NumF f a where
  addF :: f a -> f a -> f a
  -- default addF :: (Num a, MonoZipWith f a) => f a -> f a -> f a
  -- addF = monoZipWith (+)
  subF :: f a -> f a -> f a
  -- default subF :: (Num a, MonoZipWith f a) => f a -> f a -> f a
  -- subF = monoZipWith (-)
  mulF :: f a -> f a -> f a
  -- default mulF :: (Num a, MonoZipWith f a) => f a -> f a -> f a
  -- mulF = monoZipWith (*)
  negateF :: f a -> f a
  default negateF :: (Num a, MonoMap f a) => f a -> f a
  negateF = monoMap negate
  absF :: f a -> f a
  default absF :: (Num a, MonoMap f a) => f a -> f a
  absF = monoMap abs
  signumF :: f a -> f a
  default signumF :: (Num a, MonoMap f a) => f a -> f a
  signumF = monoMap signum
  fromIntegerF :: Integer -> f a
  default fromIntegerF :: (Num a, Broadcast f a) => Integer -> f a
  fromIntegerF = broadcast . fromInteger

instance NumF f a => Num (WrappedMulti f a) where
  (+) = coerce (addF @f @a)
  (-) = coerce (subF @f @a)
  (*) = coerce (mulF @f @a)
  negate = coerce (negateF @f @a)
  abs = coerce (absF @f @a)
  signum = coerce (signumF @f @a)
  fromInteger = coerce (fromIntegerF @f @a)

class NumF f a => FractionalF f a where
  divF :: f a -> f a -> f a
  -- default divF :: (Num a, MonoZipWith f a) => f a -> f a -> f a
  -- divF = monoZipWith (/)
  recipF :: f a -> f a
  default recipF :: (Num a, Broadcast f a) => f a -> f a
  recipF = divF (broadcast 1)
  fromRationalF :: Rational -> f a
  default fromRationalF :: (Fractional a, Broadcast f a) => Rational -> f a
  fromRationalF = broadcast . fromRational

instance FractionalF f a => Fractional (WrappedMulti f a) where
  (/) = coerce (divF @f @a)
  recip = coerce (recipF @f @a)
  fromRational = coerce (fromRationalF @f @a)

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
  default powF :: (Floating a, MonoZipWith f a) => f a -> f a -> f a
  powF = monoZipWith (**)
  logBaseF :: f a -> f a -> f a
  default logBaseF :: (Floating a, MonoZipWith f a) => f a -> f a -> f a
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

instance FloatingF f a => Floating (WrappedMulti f a) where
  pi = coerce (piF @f @a)
  exp = coerce (expF @f @a)
  log = coerce (logF @f @a)
  sqrt = coerce (sqrtF @f @a)
  (**) = coerce (powF @f @a)
  logBase = coerce (logBaseF @f @a)
  sin = coerce (sinF @f @a)
  cos = coerce (cosF @f @a)
  tan = coerce (tanF @f @a)
  asin = coerce (asinF @f @a)
  acos = coerce (acosF @f @a)
  atan = coerce (atanF @f @a)
  sinh = coerce (sinhF @f @a)
  cosh = coerce (coshF @f @a)
  tanh = coerce (tanhF @f @a)
  asinh = coerce (asinhF @f @a)
  acosh = coerce (acoshF @f @a)
  atanh = coerce (atanhF @f @a)

-- TODO: Add Data.Bits counterpart
-- TODO: Add Data.Semigroup and Data.Monoid counterparts
