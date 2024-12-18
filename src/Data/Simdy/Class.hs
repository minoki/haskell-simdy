{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Simdy.Class (module M, module Data.Simdy.Class) where
import           Control.Monad.ST
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Int
import           Data.Kind
import           Data.Monoid
import           Data.Primitive
import           Data.Semigroup
import           Data.Simdy.Class.Generated as M
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Base as VUB
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Foreign.Storable
import           GHC.Exts
import           GHC.ST

type HalfVector :: (Type -> Type) -> Type -> Type
type family HalfVector f

class SplitShortVector f a where
  splitShortVector :: f a -> (HalfVector f a, HalfVector f a)
  joinShortVector :: HalfVector f a -> HalfVector f a -> f a

class Broadcast f a where
  broadcast :: a -> f a

instance Broadcast Identity a where
  broadcast = Identity
  {-# INLINE broadcast #-}

class SIMDFunctor f a b where
  simdMap :: (a -> b) -> f a -> f b

instance SIMDFunctor Identity a b where
  simdMap = coerce
  {-# INLINE simdMap #-}

class SIMDZipWith f a b c where
  simdZipWith :: (a -> b -> c) -> f a -> f b -> f c

instance SIMDZipWith Identity a b c where
  simdZipWith = coerce
  {-# INLINE simdZipWith #-}

type ShortVectorLength :: (Type -> Type) -> Constraint
class ShortVectorLength f where
  shortVectorLength :: Int

instance ShortVectorLength Identity where
  shortVectorLength = 1
  {-# INLINE shortVectorLength #-}

class ShortVectorLength f => PrimSIMD f a where
  indexByteArraySIMD# :: ByteArray# -> Int# -> f a
  readByteArraySIMD# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, f a #)
  writeByteArraySIMD# :: MutableByteArray# s -> Int# -> f a -> State# s -> State# s

instance Prim a => PrimSIMD Identity a where
  indexByteArraySIMD# = coerce (indexByteArray# @a)
  readByteArraySIMD# = coerce (readByteArray# @a)
  writeByteArraySIMD# = coerce (writeByteArray# @a)
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}

unsafeIndexPrimSIMD :: PrimSIMD f a => VP.Vector a -> Int -> f a
unsafeIndexPrimSIMD (VP.Vector (I# offset) _ (ByteArray ba)) (I# i) = indexByteArraySIMD# ba (offset +# i)
{-# INLINE unsafeIndexPrimSIMD #-}

unsafeReadPrimSIMD :: PrimSIMD f a => VP.MVector s a -> Int -> ST s (f a)
unsafeReadPrimSIMD (VP.MVector (I# offset) _ (MutableByteArray ba)) (I# i) = ST (readByteArraySIMD# ba (offset +# i))
{-# INLINE unsafeReadPrimSIMD #-}

unsafeWritePrimSIMD :: PrimSIMD f a => VP.MVector s a -> Int -> f a -> ST s ()
unsafeWritePrimSIMD (VP.MVector (I# offset) _ (MutableByteArray ba)) (I# i) !v = ST (\s -> (# writeByteArraySIMD# ba (offset +# i) v s, () #))
{-# INLINE unsafeWritePrimSIMD #-}

class ShortVectorLength f => StorableSIMD f a where
  peekElemOffSIMD :: Ptr a -> Int -> IO (f a)
  pokeElemOffSIMD :: Ptr a -> Int -> f a -> IO ()

instance Storable a => StorableSIMD Identity a where
  peekElemOffSIMD = coerce (peekElemOff @a)
  pokeElemOffSIMD = coerce (pokeElemOff @a)
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}

class ShortVectorLength f => UnboxSIMD f a where
  unsafeIndexUnboxedSIMD :: VU.Vector a -> Int -> f a
  unsafeReadUnboxedSIMD :: VUM.MVector s a -> Int -> ST s (f a)
  unsafeWriteUnboxedSIMD :: VUM.MVector s a -> Int -> f a -> ST s ()

instance (ShortVectorLength f, PrimSIMD f Float) => UnboxSIMD f Float where
  unsafeIndexUnboxedSIMD (VUB.V_Float v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Float mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Float mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (ShortVectorLength f, PrimSIMD f Double) => UnboxSIMD f Double where
  unsafeIndexUnboxedSIMD (VUB.V_Double v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Double mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Double mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (ShortVectorLength f, PrimSIMD f Int8) => UnboxSIMD f Int8 where
  unsafeIndexUnboxedSIMD (VUB.V_Int8 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Int8 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Int8 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (ShortVectorLength f, PrimSIMD f Int16) => UnboxSIMD f Int16 where
  unsafeIndexUnboxedSIMD (VUB.V_Int16 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Int16 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Int16 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (ShortVectorLength f, PrimSIMD f Int32) => UnboxSIMD f Int32 where
  unsafeIndexUnboxedSIMD (VUB.V_Int32 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Int32 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Int32 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (ShortVectorLength f, PrimSIMD f Int64) => UnboxSIMD f Int64 where
  unsafeIndexUnboxedSIMD (VUB.V_Int64 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Int64 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Int64 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (ShortVectorLength f, PrimSIMD f Word8) => UnboxSIMD f Word8 where
  unsafeIndexUnboxedSIMD (VUB.V_Word8 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Word8 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Word8 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (ShortVectorLength f, PrimSIMD f Word16) => UnboxSIMD f Word16 where
  unsafeIndexUnboxedSIMD (VUB.V_Word16 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Word16 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Word16 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (ShortVectorLength f, PrimSIMD f Word32) => UnboxSIMD f Word32 where
  unsafeIndexUnboxedSIMD (VUB.V_Word32 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Word32 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Word32 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (ShortVectorLength f, PrimSIMD f Word64) => UnboxSIMD f Word64 where
  unsafeIndexUnboxedSIMD (VUB.V_Word64 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Word64 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Word64 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (ShortVectorLength f, Broadcast f ()) => UnboxSIMD f () where
  unsafeIndexUnboxedSIMD (VUB.V_Unit _) !_ = broadcast ()
  unsafeReadUnboxedSIMD (VUB.MV_Unit _) !_ = pure (broadcast ())
  unsafeWriteUnboxedSIMD (VUB.MV_Unit _) !_ !_ = pure ()
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

type WrappedMulti :: (Type -> Type) -> Type -> Type
newtype WrappedMulti f a = MkWrappedMulti (f a)

class NumF f a where
  addF :: f a -> f a -> f a
  -- default addF :: (Num a, SIMDZipWith f a a a) => f a -> f a -> f a
  -- addF = simdZipWith (+)
  subF :: f a -> f a -> f a
  -- default subF :: (Num a, SIMDZipWith f a a a) => f a -> f a -> f a
  -- subF = simdZipWith (-)
  mulF :: f a -> f a -> f a
  -- default mulF :: (Num a, SIMDZipWith f a a a) => f a -> f a -> f a
  -- mulF = simdZipWith (*)
  negateF :: f a -> f a
  default negateF :: (Num a, Broadcast f a) => f a -> f a
  negateF = subF (broadcast 0) -- For Word-like instances
  absF :: f a -> f a
  default absF :: (Num a, SIMDFunctor f a a) => f a -> f a
  absF = simdMap abs
  signumF :: f a -> f a
  default signumF :: (Num a, SIMDFunctor f a a) => f a -> f a
  signumF = simdMap signum
  fromIntegerF :: Integer -> f a
  default fromIntegerF :: (Num a, Broadcast f a) => Integer -> f a
  fromIntegerF = broadcast . fromInteger
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
  {-# INLINE signumF #-}
  {-# INLINE fromIntegerF #-}

instance NumF f a => Num (WrappedMulti f a) where
  (+) = coerce (addF @f @a)
  (-) = coerce (subF @f @a)
  (*) = coerce (mulF @f @a)
  negate = coerce (negateF @f @a)
  abs = coerce (absF @f @a)
  signum = coerce (signumF @f @a)
  fromInteger = coerce (fromIntegerF @f @a)
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE (*) #-}
  {-# INLINE negate #-}
  {-# INLINE abs #-}
  {-# INLINE signum #-}
  {-# INLINE fromInteger #-}

class NumF f a => FractionalF f a where
  divF :: f a -> f a -> f a
  -- default divF :: (Num a, SIMDZipWith f a a a) => f a -> f a -> f a
  -- divF = simdZipWith (/)
  recipF :: f a -> f a
  default recipF :: (Num a, Broadcast f a) => f a -> f a
  recipF = divF (broadcast 1)
  fromRationalF :: Rational -> f a
  default fromRationalF :: (Fractional a, Broadcast f a) => Rational -> f a
  fromRationalF = broadcast . fromRational
  {-# INLINE recipF #-}
  {-# INLINE fromRationalF #-}

instance FractionalF f a => Fractional (WrappedMulti f a) where
  (/) = coerce (divF @f @a)
  recip = coerce (recipF @f @a)
  fromRational = coerce (fromRationalF @f @a)
  {-# INLINE (/) #-}
  {-# INLINE recip #-}
  {-# INLINE fromRational #-}

class FractionalF f a => FloatingF f a where
  piF :: f a
  default piF :: (Floating a, Broadcast f a) => f a
  piF = broadcast pi
  expF :: f a -> f a
  default expF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  expF = simdMap exp
  logF :: f a -> f a
  default logF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  logF = simdMap log
  sqrtF :: f a -> f a
  default sqrtF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  sqrtF = simdMap sqrt
  powF :: f a -> f a -> f a
  default powF :: (Floating a, SIMDZipWith f a a a) => f a -> f a -> f a
  powF = simdZipWith (**)
  logBaseF :: f a -> f a -> f a
  default logBaseF :: (Floating a, SIMDZipWith f a a a) => f a -> f a -> f a
  logBaseF = simdZipWith logBase
  sinF :: f a -> f a
  default sinF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  sinF = simdMap sin
  cosF :: f a -> f a
  default cosF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  cosF = simdMap cos
  tanF :: f a -> f a
  default tanF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  tanF = simdMap tan
  asinF :: f a -> f a
  default asinF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  asinF = simdMap asin
  acosF :: f a -> f a
  default acosF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  acosF = simdMap acos
  atanF :: f a -> f a
  default atanF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  atanF = simdMap atan
  sinhF :: f a -> f a
  default sinhF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  sinhF = simdMap sinh
  coshF :: f a -> f a
  default coshF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  coshF = simdMap cosh
  tanhF :: f a -> f a
  default tanhF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  tanhF = simdMap tanh
  asinhF :: f a -> f a
  default asinhF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  asinhF = simdMap asinh
  acoshF :: f a -> f a
  default acoshF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  acoshF = simdMap acosh
  atanhF :: f a -> f a
  default atanhF :: (Floating a, SIMDFunctor f a a) => f a -> f a
  atanhF = simdMap atanh
  {-# INLINE piF #-}
  {-# INLINE expF #-}
  {-# INLINE logF #-}
  {-# INLINE sqrtF #-}
  {-# INLINE powF #-}
  {-# INLINE logBaseF #-}
  {-# INLINE sinF #-}
  {-# INLINE cosF #-}
  {-# INLINE tanF #-}
  {-# INLINE asinF #-}
  {-# INLINE acosF #-}
  {-# INLINE atanF #-}
  {-# INLINE sinhF #-}
  {-# INLINE coshF #-}
  {-# INLINE tanhF #-}
  {-# INLINE asinhF #-}
  {-# INLINE acoshF #-}
  {-# INLINE atanhF #-}

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
  {-# INLINE pi #-}
  {-# INLINE exp #-}
  {-# INLINE log #-}
  {-# INLINE sqrt #-}
  {-# INLINE (**) #-}
  {-# INLINE logBase #-}
  {-# INLINE sin #-}
  {-# INLINE cos #-}
  {-# INLINE tan #-}
  {-# INLINE asin #-}
  {-# INLINE acos #-}
  {-# INLINE atan #-}
  {-# INLINE sinh #-}
  {-# INLINE cosh #-}
  {-# INLINE tanh #-}
  {-# INLINE asinh #-}
  {-# INLINE acosh #-}
  {-# INLINE atanh #-}

-- TODO: Add Data.Bits counterpart
-- TODO: Add Data.Semigroup and Data.Monoid counterparts
