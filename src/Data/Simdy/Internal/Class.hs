{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Simdy.Internal.Class (module M, module Data.Simdy.Internal.Class) where
import           Control.Monad.ST
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Int
import           Data.Kind
import           Data.Monoid
import           Data.Primitive
import           Data.Semigroup
import           Data.Simdy.Internal.Class.Generated as M
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Base as VUB
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word
import           Foreign.Storable
import           GHC.Exts
import           GHC.ST
import           GHC.TypeNats (KnownNat, Natural)
import           Prelude hiding (not, (&&), (||), (==), (/=), (<), (<=), (>), (>=))
import qualified Prelude

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

class LiftSIMD f a b where
  liftSIMD :: (a -> b) -> f a -> f b

instance LiftSIMD Identity a b where
  liftSIMD = coerce
  {-# INLINE liftSIMD #-}

class LiftSIMD2 f a b c where
  liftSIMD2 :: (a -> b -> c) -> f a -> f b -> f c

instance LiftSIMD2 Identity a b c where
  liftSIMD2 = coerce
  {-# INLINE liftSIMD2 #-}

type KnownSIMDLength :: (Type -> Type) -> Constraint
class KnownNat (SIMDLength f) => KnownSIMDLength f where
  type SIMDLength f :: Natural
  simdLength :: Int

instance KnownSIMDLength Identity where
  type SIMDLength Identity = 1
  simdLength = 1
  {-# INLINE simdLength #-}

class KnownSIMDLength f => PrimSIMD f a where
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

class KnownSIMDLength f => StorableSIMD f a where
  peekElemOffSIMD :: Ptr a -> Int -> IO (f a)
  pokeElemOffSIMD :: Ptr a -> Int -> f a -> IO ()

instance Storable a => StorableSIMD Identity a where
  peekElemOffSIMD = coerce (peekElemOff @a)
  pokeElemOffSIMD = coerce (pokeElemOff @a)
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}

class KnownSIMDLength f => UnboxSIMD f a where
  unsafeIndexUnboxedSIMD :: VU.Vector a -> Int -> f a
  unsafeReadUnboxedSIMD :: VUM.MVector s a -> Int -> ST s (f a)
  unsafeWriteUnboxedSIMD :: VUM.MVector s a -> Int -> f a -> ST s ()

instance (KnownSIMDLength f, PrimSIMD f Float) => UnboxSIMD f Float where
  unsafeIndexUnboxedSIMD (VUB.V_Float v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Float mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Float mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (KnownSIMDLength f, PrimSIMD f Double) => UnboxSIMD f Double where
  unsafeIndexUnboxedSIMD (VUB.V_Double v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Double mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Double mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (KnownSIMDLength f, PrimSIMD f Int8) => UnboxSIMD f Int8 where
  unsafeIndexUnboxedSIMD (VUB.V_Int8 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Int8 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Int8 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (KnownSIMDLength f, PrimSIMD f Int16) => UnboxSIMD f Int16 where
  unsafeIndexUnboxedSIMD (VUB.V_Int16 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Int16 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Int16 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (KnownSIMDLength f, PrimSIMD f Int32) => UnboxSIMD f Int32 where
  unsafeIndexUnboxedSIMD (VUB.V_Int32 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Int32 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Int32 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (KnownSIMDLength f, PrimSIMD f Int64) => UnboxSIMD f Int64 where
  unsafeIndexUnboxedSIMD (VUB.V_Int64 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Int64 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Int64 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (KnownSIMDLength f, PrimSIMD f Word8) => UnboxSIMD f Word8 where
  unsafeIndexUnboxedSIMD (VUB.V_Word8 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Word8 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Word8 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (KnownSIMDLength f, PrimSIMD f Word16) => UnboxSIMD f Word16 where
  unsafeIndexUnboxedSIMD (VUB.V_Word16 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Word16 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Word16 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (KnownSIMDLength f, PrimSIMD f Word32) => UnboxSIMD f Word32 where
  unsafeIndexUnboxedSIMD (VUB.V_Word32 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Word32 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Word32 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (KnownSIMDLength f, PrimSIMD f Word64) => UnboxSIMD f Word64 where
  unsafeIndexUnboxedSIMD (VUB.V_Word64 v) = unsafeIndexPrimSIMD v
  unsafeReadUnboxedSIMD (VUB.MV_Word64 mv) = unsafeReadPrimSIMD mv
  unsafeWriteUnboxedSIMD (VUB.MV_Word64 mv) = unsafeWritePrimSIMD mv
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

instance (KnownSIMDLength f, Broadcast f ()) => UnboxSIMD f () where
  unsafeIndexUnboxedSIMD (VUB.V_Unit _) !_ = broadcast ()
  unsafeReadUnboxedSIMD (VUB.MV_Unit _) !_ = pure (broadcast ())
  unsafeWriteUnboxedSIMD (VUB.MV_Unit _) !_ !_ = pure ()
  {-# INLINE unsafeIndexUnboxedSIMD #-}
  {-# INLINE unsafeReadUnboxedSIMD #-}
  {-# INLINE unsafeWriteUnboxedSIMD #-}

type WrappedMulti :: (Type -> Type) -> Type -> Type
newtype WrappedMulti f a = MkWrappedMulti (f a)

type Mask :: Type -> Type
type family Mask a
type instance Mask (Identity a) = Identity Bool
type instance Mask (WrappedMulti f a) = f Bool

class Selectable a where
  select :: Mask a -> a -> a -> a

instance Selectable (Identity a) where
  select (Identity True) !x !_ = x
  select (Identity False) !_ !y = y
  {-# INLINE select #-}

class SelectableF f a where
  selectF :: f Bool -> f a -> f a -> f a

instance SelectableF f a => Selectable (WrappedMulti f a) where
  select = coerce (selectF @f @a)
  {-# INLINE select #-}

infixr 3 &&
infixr 2 ||

class Boolean a where
  true :: a
  false :: a
  not :: a -> a
  (&&) :: a -> a -> a
  (||) :: a -> a -> a

instance Boolean Bool where
  true = True
  false = False
  not = Prelude.not
  (&&) = (Prelude.&&)
  (||) = (Prelude.||)
  {-# INLINE true #-}
  {-# INLINE false #-}
  {-# INLINE not #-}
  {-# INLINE (&&) #-}
  {-# INLINE (||) #-}

deriving via Bool instance Boolean (Identity Bool)

class BooleanF f where
  trueF :: f Bool
  falseF :: f Bool
  notF :: f Bool -> f Bool
  andF :: f Bool -> f Bool -> f Bool
  orF :: f Bool -> f Bool -> f Bool

instance BooleanF Identity where
  trueF = Identity True
  falseF = Identity False
  notF = coerce Prelude.not
  andF = coerce (Prelude.&&)
  orF = coerce (Prelude.||)
  {-# INLINE trueF #-}
  {-# INLINE falseF #-}
  {-# INLINE notF #-}
  {-# INLINE andF #-}
  {-# INLINE orF #-}

instance BooleanF f => Boolean (WrappedMulti f Bool) where
  true = coerce (trueF @f)
  false = coerce (falseF @f)
  not = coerce (notF @f)
  (&&) = coerce (andF @f)
  (||) = coerce (orF @f)
  {-# INLINE true #-}
  {-# INLINE false #-}
  {-# INLINE not #-}
  {-# INLINE (&&) #-}
  {-# INLINE (||) #-}

infix 4 ==, /=, <, <=, >, >=

class Equatable a where
  (==) :: a -> a -> Mask a

(/=) :: (Boolean (Mask a), Equatable a) => a -> a -> Mask a
x /= y = not (x == y)
{-# INLINE (/=) #-}

newtype Scalar a = MkScalar a

type instance Mask (Scalar a) = Bool
type instance Mask Int = Bool
type instance Mask Int8 = Bool
type instance Mask Int16 = Bool
type instance Mask Int32 = Bool
type instance Mask Int64 = Bool
type instance Mask Word = Bool
type instance Mask Word8 = Bool
type instance Mask Word16 = Bool
type instance Mask Word32 = Bool
type instance Mask Word64 = Bool
type instance Mask Float = Bool
type instance Mask Double = Bool

instance Eq a => Equatable (Scalar a) where
  (==) = coerce ((Prelude.==) @a)
  {-# INLINE (==) #-}

deriving via Scalar Int instance Equatable Int
deriving via Scalar Int8 instance Equatable Int8
deriving via Scalar Int16 instance Equatable Int16
deriving via Scalar Int32 instance Equatable Int32
deriving via Scalar Int64 instance Equatable Int64
deriving via Scalar Word instance Equatable Word
deriving via Scalar Word8 instance Equatable Word8
deriving via Scalar Word16 instance Equatable Word16
deriving via Scalar Word32 instance Equatable Word32
deriving via Scalar Word64 instance Equatable Word64
deriving via Scalar Float instance Equatable Float
deriving via Scalar Double instance Equatable Double

instance Eq a => Equatable (Identity a) where
  (==) = coerce ((Prelude.==) @a)
  {-# INLINE (==) #-}

class EquatableF f a where
  eqF :: f a -> f a -> f Bool

instance EquatableF f a => Equatable (WrappedMulti f a) where
  (==) = coerce (eqF @f @a)
  {-# INLINE (==) #-}

class Equatable a => Ordered a where
  (<) :: a -> a -> Mask a
  (<=) :: a -> a -> Mask a
  (>) :: a -> a -> Mask a
  (>=) :: a -> a -> Mask a

instance Ord a => Ordered (Scalar a) where
  (<) = coerce ((Prelude.<) @a)
  (<=) = coerce ((Prelude.<=) @a)
  (>) = coerce ((Prelude.>) @a)
  (>=) = coerce ((Prelude.>=) @a)
  {-# INLINE (<) #-}
  {-# INLINE (<=) #-}
  {-# INLINE (>) #-}
  {-# INLINE (>=) #-}

deriving via Scalar Int instance Ordered Int
deriving via Scalar Int8 instance Ordered Int8
deriving via Scalar Int16 instance Ordered Int16
deriving via Scalar Int32 instance Ordered Int32
deriving via Scalar Int64 instance Ordered Int64
deriving via Scalar Word instance Ordered Word
deriving via Scalar Word8 instance Ordered Word8
deriving via Scalar Word16 instance Ordered Word16
deriving via Scalar Word32 instance Ordered Word32
deriving via Scalar Word64 instance Ordered Word64
deriving via Scalar Float instance Ordered Float
deriving via Scalar Double instance Ordered Double

instance Ord a => Ordered (Identity a) where
  (<) = coerce ((Prelude.<) @a)
  (<=) = coerce ((Prelude.<=) @a)
  (>) = coerce ((Prelude.>) @a)
  (>=) = coerce ((Prelude.>=) @a)
  {-# INLINE (<) #-}
  {-# INLINE (<=) #-}
  {-# INLINE (>) #-}
  {-# INLINE (>=) #-}

class EquatableF f a => OrderedF f a where
  ltF :: f a -> f a -> f Bool
  leF :: f a -> f a -> f Bool
  gtF :: f a -> f a -> f Bool
  geF :: f a -> f a -> f Bool

instance OrderedF f a => Ordered (WrappedMulti f a) where
  (<) = coerce (ltF @f @a)
  (<=) = coerce (leF @f @a)
  (>) = coerce (gtF @f @a)
  (>=) = coerce (geF @f @a)
  {-# INLINE (<) #-}
  {-# INLINE (<=) #-}
  {-# INLINE (>) #-}
  {-# INLINE (>=) #-}

class NumF f a where
  plusF :: f a -> f a -> f a
  -- default plusF :: (Num a, LiftSIMD2 f a a a) => f a -> f a -> f a
  -- plusF = liftSIMD2 (+)
  minusF :: f a -> f a -> f a
  -- default minusF :: (Num a, LiftSIMD2 f a a a) => f a -> f a -> f a
  -- minusF = liftSIMD2 (-)
  timesF :: f a -> f a -> f a
  -- default timesF :: (Num a, LiftSIMD2 f a a a) => f a -> f a -> f a
  -- timesF = liftSIMD2 (*)
  negateF :: f a -> f a
  default negateF :: (Num a, Broadcast f a) => f a -> f a
  negateF = minusF (broadcast 0) -- For Word-like instances
  absF :: f a -> f a
  default absF :: (Num a, LiftSIMD f a a) => f a -> f a
  absF = liftSIMD abs
  signumF :: f a -> f a
  default signumF :: (Num a, LiftSIMD f a a) => f a -> f a
  signumF = liftSIMD signum
  fromIntegerF :: Integer -> f a
  default fromIntegerF :: (Num a, Broadcast f a) => Integer -> f a
  fromIntegerF = broadcast . fromInteger
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
  {-# INLINE signumF #-}
  {-# INLINE fromIntegerF #-}

instance NumF f a => Num (WrappedMulti f a) where
  (+) = coerce (plusF @f @a)
  (-) = coerce (minusF @f @a)
  (*) = coerce (timesF @f @a)
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
  -- default divF :: (Num a, LiftSIMD2 f a a a) => f a -> f a -> f a
  -- divF = liftSIMD2 (/)
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
  default expF :: (Floating a, LiftSIMD f a a) => f a -> f a
  expF = liftSIMD exp
  logF :: f a -> f a
  default logF :: (Floating a, LiftSIMD f a a) => f a -> f a
  logF = liftSIMD log
  sqrtF :: f a -> f a
  default sqrtF :: (Floating a, LiftSIMD f a a) => f a -> f a
  sqrtF = liftSIMD sqrt
  powF :: f a -> f a -> f a
  default powF :: (Floating a, LiftSIMD2 f a a a) => f a -> f a -> f a
  powF = liftSIMD2 (**)
  logBaseF :: f a -> f a -> f a
  default logBaseF :: (Floating a, LiftSIMD2 f a a a) => f a -> f a -> f a
  logBaseF = liftSIMD2 logBase
  sinF :: f a -> f a
  default sinF :: (Floating a, LiftSIMD f a a) => f a -> f a
  sinF = liftSIMD sin
  cosF :: f a -> f a
  default cosF :: (Floating a, LiftSIMD f a a) => f a -> f a
  cosF = liftSIMD cos
  tanF :: f a -> f a
  default tanF :: (Floating a, LiftSIMD f a a) => f a -> f a
  tanF = liftSIMD tan
  asinF :: f a -> f a
  default asinF :: (Floating a, LiftSIMD f a a) => f a -> f a
  asinF = liftSIMD asin
  acosF :: f a -> f a
  default acosF :: (Floating a, LiftSIMD f a a) => f a -> f a
  acosF = liftSIMD acos
  atanF :: f a -> f a
  default atanF :: (Floating a, LiftSIMD f a a) => f a -> f a
  atanF = liftSIMD atan
  sinhF :: f a -> f a
  default sinhF :: (Floating a, LiftSIMD f a a) => f a -> f a
  sinhF = liftSIMD sinh
  coshF :: f a -> f a
  default coshF :: (Floating a, LiftSIMD f a a) => f a -> f a
  coshF = liftSIMD cosh
  tanhF :: f a -> f a
  default tanhF :: (Floating a, LiftSIMD f a a) => f a -> f a
  tanhF = liftSIMD tanh
  asinhF :: f a -> f a
  default asinhF :: (Floating a, LiftSIMD f a a) => f a -> f a
  asinhF = liftSIMD asinh
  acoshF :: f a -> f a
  default acoshF :: (Floating a, LiftSIMD f a a) => f a -> f a
  acoshF = liftSIMD acosh
  atanhF :: f a -> f a
  default atanhF :: (Floating a, LiftSIMD f a a) => f a -> f a
  atanhF = liftSIMD atanh
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
