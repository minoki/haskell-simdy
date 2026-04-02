{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK not-home #-}
module Data.Simdy.Internal.Class (module M, module Data.Simdy.Internal.Class) where
import           Data.Coerce (coerce)
import           Data.Complex (Complex (..))
import           Data.Functor.Identity (Identity (Identity))
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Kind (Constraint, Type)
import           Data.Monoid (Product (..), Sum (..))
import           Data.Primitive (Prim (..))
import           Data.Proxy (Proxy)
import           Data.Semigroup (Max (..), Min (..))
import           Data.Simdy.Internal.Bits
import           Data.Simdy.Internal.Class.Generated as M
import           Data.Simdy.Internal.FMA as M
import           Data.Word (Word16, Word32, Word64, Word8)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable
import           GHC.Exts (ByteArray#, Int#, MutableByteArray#, State#)
import           GHC.TypeNats (KnownNat, Natural)
import           Prelude hiding (max, min, not, (&&), (/=), (<), (<=), (==),
                          (>), (>=), (||))
import qualified Prelude

-- | Provides a human-readable description of the SIMD backend (e.g. @\"X8;maxBits=128\"@).
type ImplementationDescription :: (Type -> Type) -> Constraint
class ImplementationDescription f where
  implementationDescription :: Proxy f -> String

instance ImplementationDescription Identity where
  implementationDescription _ = "Identity (scalar)"

-- | Maps a SIMD vector type to its half-width counterpart.
--
-- For example, @HalfVector X8 = X4@ and @HalfVector X4 = X2@.
-- Used by 'SplitShortVector' and 'horizontalFold'.
type HalfVector :: (Type -> Type) -> Type -> Type
type family HalfVector f

-- | Split a SIMD vector into two halves or join two halves into one.
class SplitShortVector f a where
  -- | Split a vector into its low and high halves.
  splitShortVector :: f a -> (HalfVector f a, HalfVector f a)
  -- | Join two half-width vectors into a full-width vector.
  joinShortVector :: HalfVector f a -> HalfVector f a -> f a

-- | Create a SIMD vector with all lanes set to the same value.
--
-- @broadcast x@ fills every element of the vector with @x@.
class Broadcast f a where
  broadcast :: a -> f a

instance Broadcast Identity a where
  broadcast = Identity
  {-# INLINE broadcast #-}

-- | Apply a scalar function element-wise to a SIMD vector.
class LiftSIMD f a b where
  liftSIMD :: (a -> b) -> f a -> f b

instance LiftSIMD Identity a b where
  liftSIMD = coerce
  {-# INLINE liftSIMD #-}

-- | Apply a binary scalar function element-wise to two SIMD vectors.
class LiftSIMD2 f a b c where
  liftSIMD2 :: (a -> b -> c) -> f a -> f b -> f c

instance LiftSIMD2 Identity a b c where
  liftSIMD2 = coerce
  {-# INLINE liftSIMD2 #-}

-- | Lift newtype constructors and tuple constructors over SIMD vectors.
--
-- This class enables working with tuples, 'Data.Monoid.Sum', 'Data.Monoid.Product',
-- 'Data.Semigroup.Min', 'Data.Semigroup.Max', and 'Data.Complex.Complex' inside SIMD vectors.
class LiftConstructor f where
  mkTuple2 :: f a0 -> f a1 -> f (a0, a1)
  mkTuple3 :: f a0 -> f a1 -> f a2 -> f (a0, a1, a2)
  mkTuple4 :: f a0 -> f a1 -> f a2 -> f a3 -> f (a0, a1, a2, a3)
  mkTuple5 :: f a0 -> f a1 -> f a2 -> f a3 -> f a4 -> f (a0, a1, a2, a3, a4)
  mkTuple6 :: f a0 -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f (a0, a1, a2, a3, a4, a5)
  deconstructTuple2 :: f (a0, a1) -> (f a0, f a1)
  deconstructTuple3 :: f (a0, a1, a2) -> (f a0, f a1, f a2)
  deconstructTuple4 :: f (a0, a1, a2, a3) -> (f a0, f a1, f a2, f a3)
  deconstructTuple5 :: f (a0, a1, a2, a3, a4) -> (f a0, f a1, f a2, f a3, f a4)
  deconstructTuple6 :: f (a0, a1, a2, a3, a4, a5) -> (f a0, f a1, f a2, f a3, f a4, f a5)
  mkSum :: f a -> f (Sum a)
  getSum' :: f (Sum a) -> f a
  mkProduct :: f a -> f (Product a)
  getProduct' :: f (Product a) -> f a
  mkMin :: f a -> f (Min a)
  getMin' :: f (Min a) -> f a
  mkMax :: f a -> f (Max a)
  getMax' :: f (Max a) -> f a
  {-
  mkAll :: f Bool -> f All
  getAll' :: f All -> f Bool
  mkAny :: f Bool -> f Any
  getAny' :: f Any -> f Bool
  -}
  mkComplex :: f a -> f a -> f (Complex a)
  deconstructComplex :: f (Complex a) -> (f a, f a)

instance LiftConstructor Identity where
  mkTuple2 (Identity x0) (Identity x1) = Identity (x0, x1)
  mkTuple3 (Identity x0) (Identity x1) (Identity x2) = Identity (x0, x1, x2)
  mkTuple4 (Identity x0) (Identity x1) (Identity x2) (Identity x3) = Identity (x0, x1, x2, x3)
  mkTuple5 (Identity x0) (Identity x1) (Identity x2) (Identity x3) (Identity x4) = Identity (x0, x1, x2, x3, x4)
  mkTuple6 (Identity x0) (Identity x1) (Identity x2) (Identity x3) (Identity x4) (Identity x6) = Identity (x0, x1, x2, x3, x4, x6)
  deconstructTuple2 = coerce
  deconstructTuple3 = coerce
  deconstructTuple4 = coerce
  deconstructTuple5 = coerce
  deconstructTuple6 = coerce
  mkSum = coerce
  getSum' = coerce
  mkProduct = coerce
  getProduct' = coerce
  mkMin = coerce
  getMin' = coerce
  mkMax = coerce
  getMax' = coerce
  mkComplex (Identity x) (Identity y) = Identity (x :+ y)
  deconstructComplex (Identity (x :+ y)) = (Identity x, Identity y)
  {-# INLINE mkTuple2 #-}
  {-# INLINE mkTuple3 #-}
  {-# INLINE mkTuple4 #-}
  {-# INLINE mkTuple5 #-}
  {-# INLINE mkTuple6 #-}
  {-# INLINE deconstructTuple2 #-}
  {-# INLINE deconstructTuple3 #-}
  {-# INLINE deconstructTuple4 #-}
  {-# INLINE deconstructTuple5 #-}
  {-# INLINE deconstructTuple6 #-}
  {-# INLINE mkSum #-}
  {-# INLINE getSum' #-}
  {-# INLINE mkProduct #-}
  {-# INLINE getProduct' #-}
  {-# INLINE mkMin #-}
  {-# INLINE getMin' #-}
  {-# INLINE mkMax #-}
  {-# INLINE getMax' #-}
  {-# INLINE mkComplex #-}
  {-# INLINE deconstructComplex #-}

pattern MkTuple2 :: LiftConstructor f => f a0 -> f a1 -> f (a0, a1)
pattern MkTuple2 x0 x1 <- (deconstructTuple2 -> (x0, x1)) where
  MkTuple2 = mkTuple2

pattern MkTuple3 :: LiftConstructor f => f a0 -> f a1 -> f a2 -> f (a0, a1, a2)
pattern MkTuple3 x0 x1 x2 <- (deconstructTuple3 -> (x0, x1, x2)) where
  MkTuple3 = mkTuple3

pattern MkTuple4 :: LiftConstructor f => f a0 -> f a1 -> f a2 -> f a3 -> f (a0, a1, a2, a3)
pattern MkTuple4 x0 x1 x2 x3 <- (deconstructTuple4 -> (x0, x1, x2, x3)) where
  MkTuple4 = mkTuple4

pattern MkTuple5 :: LiftConstructor f => f a0 -> f a1 -> f a2 -> f a3 -> f a4 -> f (a0, a1, a2, a3, a4)
pattern MkTuple5 x0 x1 x2 x3 x4 <- (deconstructTuple5 -> (x0, x1, x2, x3, x4)) where
  MkTuple5 = mkTuple5

pattern MkTuple6 :: LiftConstructor f => f a0 -> f a1 -> f a2 -> f a3 -> f a4 -> f a5 -> f (a0, a1, a2, a3, a4, a5)
pattern MkTuple6 x0 x1 x2 x3 x4 x5 <- (deconstructTuple6 -> (x0, x1, x2, x3, x4, x5)) where
  MkTuple6 = mkTuple6

pattern MkSum :: LiftConstructor f => f a -> f (Sum a)
pattern MkSum x <- (getSum' -> x) where
  MkSum = mkSum

pattern MkProduct :: LiftConstructor f => f a -> f (Product a)
pattern MkProduct x <- (getProduct' -> x) where
  MkProduct = mkProduct

pattern MkMin :: LiftConstructor f => f a -> f (Min a)
pattern MkMin x <- (getMin' -> x) where
  MkMin = mkMin

pattern MkMax :: LiftConstructor f => f a -> f (Max a)
pattern MkMax x <- (getMax' -> x) where
  MkMax = mkMax

{-
pattern MkAll :: LiftConstructor f => f Bool -> f All
pattern MkAll x <- (getAll' -> x) where
  MkAll = mkAll

pattern MkAny :: LiftConstructor f => f Bool -> f Any
pattern MkAny x <- (getAny' -> x) where
  MkAny = mkAny
-}

pattern MkComplex :: LiftConstructor f => f a -> f a -> f (Complex a)
pattern MkComplex x0 x1 <- (deconstructComplex -> (x0, x1)) where
  MkComplex = mkComplex

-- | SIMD vector types with a statically known number of lanes.
--
-- @'simdLength' \@f@ returns the number of elements in a vector of type @f a@.
type KnownSIMDLength :: (Type -> Type) -> Constraint
class KnownNat (SIMDLength f) => KnownSIMDLength f where
  -- | The number of lanes as a type-level natural.
  type SIMDLength f :: Natural
  -- | The number of lanes as a term-level 'Int'. Use with @TypeApplications@: @simdLength \@X4 == 4@.
  simdLength :: Int

instance KnownSIMDLength Identity where
  type SIMDLength Identity = 1
  simdLength = 1
  {-# INLINE simdLength #-}

-- | Low-level interface for reading\/writing SIMD vectors from\/to byte arrays.
class (KnownSIMDLength f, Prim a) => MultiPrim f a where
  indexByteArraySIMD# :: ByteArray# -> Int# -> f a
  readByteArraySIMD# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, f a #)
  writeByteArraySIMD# :: MutableByteArray# s -> Int# -> f a -> State# s -> State# s

instance Prim a => MultiPrim Identity a where
  indexByteArraySIMD# = coerce (indexByteArray# @a)
  readByteArraySIMD# = coerce (readByteArray# @a)
  writeByteArraySIMD# = coerce (writeByteArray# @a)
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}

-- | Low-level interface for reading\/writing SIMD vectors via 'Ptr'.
class (KnownSIMDLength f, Storable a) => MultiStorable f a where
  peekElemOffSIMD :: Ptr a -> Int -> IO (f a)
  pokeElemOffSIMD :: Ptr a -> Int -> f a -> IO ()

instance Storable a => MultiStorable Identity a where
  peekElemOffSIMD = coerce (peekElemOff @a)
  pokeElemOffSIMD = coerce (pokeElemOff @a)
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}

-- | Newtype wrapper that bridges the F-suffixed type classes (e.g. 'NumF', 'EquatableF')
-- to standard Haskell type classes (e.g. 'Num', 'Eq') via @DerivingVia@.
type WrappedMulti :: (Type -> Type) -> Type -> Type
newtype WrappedMulti f a = MkWrappedMulti (f a)

-- | Maps a type to its corresponding mask type for comparison results.
--
-- For scalar types, @Mask a = Bool@.
-- For SIMD vector types, @Mask (f a) = f Bool@ (a vector of booleans).
type Mask :: Type -> Type
type family Mask a
type instance Mask (Identity a) = Identity Bool
type instance Mask (WrappedMulti f a) = f Bool

class Mask (f a) ~ f Bool => MaskIsLiftedBool f a
instance MaskIsLiftedBool Identity a

-- | Lane-wise conditional selection using a mask.
--
-- @select mask trueVal falseVal@ picks from @trueVal@ where the mask is true
-- and from @falseVal@ where the mask is false.
class Selectable a where
  select :: Mask a -> a -> a -> a

instance Selectable (Identity a) where
  select (Identity True) !x !_  = x
  select (Identity False) !_ !y = y
  {-# INLINE select #-}

class SelectableF f a where
  selectF :: f Bool -> f a -> f a -> f a

instance SelectableF Identity a where
  selectF = select
  {-# INLINE selectF #-}

instance SelectableF f a => Selectable (WrappedMulti f a) where
  select = coerce (selectF @f @a)
  {-# INLINE select #-}

infix 4 ==, /=, <, <=, >, >=

-- | Lane-wise equality comparison, returning a 'Mask'.
--
-- For scalar types this behaves like 'Prelude.Eq'.
-- For SIMD vectors, comparison is performed element-wise and returns a vector of booleans.
class Equatable a where
  (==) :: a -> a -> Mask a
  (/=) :: a -> a -> Mask a
  default (/=) :: Boolean (Mask a) => a -> a -> Mask a
  x /= y = complement (x == y)
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
  (/=) = coerce ((Prelude./=) @a)
  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}

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
  (/=) = coerce ((Prelude./=) @a)
  {-# INLINE (==) #-}
  {-# INLINE (/=) #-}

class Boolean (f Bool) => EquatableF f a where
  eqF :: f a -> f a -> f Bool

instance Eq a => EquatableF Identity a where
  eqF = coerce ((Prelude.==) @a)
  {-# INLINE eqF #-}

instance EquatableF f a => Equatable (WrappedMulti f a) where
  (==) = coerce (eqF @f @a)
  {-# INLINE (==) #-}

-- | Lane-wise ordering comparison, returning a 'Mask'.
--
-- For scalar types this behaves like 'Prelude.Ord'.
-- For SIMD vectors, comparison is performed element-wise.
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

instance Ord a => OrderedF Identity a where
  ltF = coerce ((Prelude.<) @a)
  leF = coerce ((Prelude.<=) @a)
  gtF = coerce ((Prelude.>) @a)
  geF = coerce ((Prelude.>=) @a)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}

instance OrderedF f a => Ordered (WrappedMulti f a) where
  (<) = coerce (ltF @f @a)
  (<=) = coerce (leF @f @a)
  (>) = coerce (gtF @f @a)
  (>=) = coerce (geF @f @a)
  {-# INLINE (<) #-}
  {-# INLINE (<=) #-}
  {-# INLINE (>) #-}
  {-# INLINE (>=) #-}

-- | Lane-wise minimum and maximum.
--
-- For floating-point types, 'min' and 'max' follow IEEE 754-2019 semantics
-- (propagating NaN), while 'minimumNumber' and 'maximumNumber' prefer numeric
-- values over NaN.
class MinMax a where
  min :: a -> a -> a
  max :: a -> a -> a
  -- | Like 'min', but returns the numeric value if one argument is NaN.
  minimumNumber :: a -> a -> a
  -- | Like 'max', but returns the numeric value if one argument is NaN.
  maximumNumber :: a -> a -> a

instance Ord a => MinMax (Scalar a) where
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
  min = minimumFloat
  max = maximumFloat
  minimumNumber = minimumNumberFloat
  maximumNumber = maximumNumberFloat

-- | IEEE 754-2019 compliant instance
instance MinMax Double where
  min = minimumDouble
  max = maximumDouble
  minimumNumber = minimumNumberDouble
  maximumNumber = maximumNumberDouble

class MinMaxF f a where
  minF :: f a -> f a -> f a
  maxF :: f a -> f a -> f a
  minimumNumberF :: f a -> f a -> f a
  maximumNumberF :: f a -> f a -> f a

deriving instance MinMax a => MinMax (Identity a)

instance MinMaxF f a => MinMax (WrappedMulti f a) where
  min = coerce (minF @f @a)
  max = coerce (maxF @f @a)
  minimumNumber = coerce (minimumNumberF @f @a)
  maximumNumber = coerce (maximumNumberF @f @a)
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}

-- | Lane-wise numeric operations on SIMD vectors.
-- This is the internal (F-suffixed) version; standard 'Num' instances are
-- derived via 'WrappedMulti'.
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

instance Num a => NumF Identity a where
  plusF = (Prelude.+)
  minusF = (Prelude.-)
  timesF = (Prelude.*)
  negateF = Prelude.negate
  absF = Prelude.abs
  signumF = Prelude.signum
  fromIntegerF = Prelude.fromInteger
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
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

-- | Lane-wise fractional operations. Derives 'Fractional' via 'WrappedMulti'.
class NumF f a => FractionalF f a where
  divideF :: f a -> f a -> f a
  -- default divideF :: (Num a, LiftSIMD2 f a a a) => f a -> f a -> f a
  -- divideF = liftSIMD2 (/)
  recipF :: f a -> f a
  default recipF :: (Num a, Broadcast f a) => f a -> f a
  recipF = divideF (broadcast 1)
  fromRationalF :: Rational -> f a
  default fromRationalF :: (Fractional a, Broadcast f a) => Rational -> f a
  fromRationalF = broadcast . fromRational
  {-# INLINE recipF #-}
  {-# INLINE fromRationalF #-}

instance FractionalF f a => Fractional (WrappedMulti f a) where
  (/) = coerce (divideF @f @a)
  recip = coerce (recipF @f @a)
  fromRational = coerce (fromRationalF @f @a)
  {-# INLINE (/) #-}
  {-# INLINE recip #-}
  {-# INLINE fromRational #-}

-- | Lane-wise floating-point operations. Derives 'Floating' via 'WrappedMulti'.
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

-- | Lane-wise bitwise logic on SIMD vectors. Derives 'Boolean' via 'WrappedMulti'.
class BooleanF f a where
  andF :: f a -> f a -> f a
  orF :: f a -> f a -> f a
  xorF :: f a -> f a -> f a
  complementF :: f a -> f a

-- | Lane-wise bitwise shifts on SIMD vectors. Derives 'BitShift' via 'WrappedMulti'.
class BooleanF f a => BitShiftF f a where
  shiftLF :: f a -> Int -> f a
  unsafeShiftLF :: f a -> Int -> f a
  shiftRF :: f a -> Int -> f a
  unsafeShiftRF :: f a -> Int -> f a
  unsafeShiftLF = shiftLF
  unsafeShiftRF = shiftRF
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE unsafeShiftRF #-}

instance BooleanF f a => Boolean (WrappedMulti f a) where
  (.&.) = coerce (andF @f @a)
  (.|.) = coerce (orF @f @a)
  xor = coerce (xorF @f @a)
  complement = coerce (complementF @f @a)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}

instance BitShiftF f a => BitShift (WrappedMulti f a) where
  shiftL = coerce (shiftLF @f @a)
  unsafeShiftL = coerce (unsafeShiftLF @f @a)
  shiftR = coerce (shiftRF @f @a)
  unsafeShiftR = coerce (unsafeShiftRF @f @a)
  {-# INLINE shiftL #-}
  {-# INLINE unsafeShiftL #-}
  {-# INLINE shiftR #-}
  {-# INLINE unsafeShiftR #-}

-- | Lane-wise fused multiply-add on SIMD vectors.
class NumF f a => FusedMultiplyAddF f a where
  -- | @fusedMultiplyAddF x y z = x * y + z@ (single rounding per lane).
  fusedMultiplyAddF :: f a -> f a -> f a -> f a

instance FusedMultiplyAddF f a => FusedMultiplyAdd (WrappedMulti f a) where
  fusedMultiplyAdd = coerce (fusedMultiplyAddF @f @a)
  {-# INLINE fusedMultiplyAdd #-}

class KnownSIMDLength f => EnumFromZero_ f a where
  enumFromZero :: f a

instance Num a => EnumFromZero_ Identity a where
  enumFromZero = Identity 0
  {-# INLINE enumFromZero #-}

-- | Constraint alias for types that support generating @[0, 1, 2, ...]@ SIMD vectors.
-- Used by 'indexed' and related functions.
type EnumFromZero f a = (EnumFromZero_ f a, Num a, NumF f a, Broadcast f a)

-- TODO: Add Data.Semigroup and Data.Monoid counterparts
