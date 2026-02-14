-- This file was created by script/GenVL.hs. Do not edit by hand!
{-|
This module contains types and classes that use 256-bit vectors (x86 AVX/AVX2).

In general, the types and classes exported from this module are not compatible with other modules with different vector lengths (i.e. "Data.Simdy.Internal.NoSIMD", "Data.Simdy.Internal.SIMD128", "Data.Simdy.Internal.SIMD512").
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 0, 0)
{-# LANGUAGE RequiredTypeArguments #-}
#endif
module Data.Simdy.Internal.SIMD256
  ( module M
  , SIMD (horizontalFold)
  , SIMDElement
  , broadcast
  , liftSIMD
  , liftSIMD2
  , selectSIMD
  , SIMDEq
  , (==^)
  , (/=^)
  , SIMDOrd
  , (<^)
  , (<=^)
  , (>^)
  , (>=^)
  , SIMDNum
  , SIMDFractional
  , SIMDFloating
  , SIMDBoolean
  , SIMDBits
  , SIMDMinMax
  , SIMDFMA
  , SIMDEnumFromZero
  , SIMDPrim
  , SIMDStorable
#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 0, 0)
  , unaryShuffleX2
  , unaryShuffleX4
  , unaryShuffleX8
  , unaryShuffleX16
  , unaryShuffleX32
  , unaryShuffleX64
  , binaryShuffleX2
  , binaryShuffleX4
  , binaryShuffleX8
  , binaryShuffleX16
  , binaryShuffleX32
  , binaryShuffleX64
#endif
  , unaryShuffleWithX2
  , unaryShuffleWithX4
  , unaryShuffleWithX8
  , unaryShuffleWithX16
  , unaryShuffleWithX32
  , unaryShuffleWithX64
  , binaryShuffleWithX2
  , binaryShuffleWithX4
  , binaryShuffleWithX8
  , binaryShuffleWithX16
  , binaryShuffleWithX32
  , binaryShuffleWithX64
  ) where
import           Data.Bits (Bits)
import           Data.Coerce (coerce)
import           Data.Complex
import           Data.Functor.Identity
import           Data.Int
import           Data.Primitive
import           Data.Proxy (Proxy)
import           Data.Semigroup
import           Data.Simdy.Internal.Bits (Boolean, BitShift)
import           Data.Simdy.Internal.Class hiding (broadcast, liftSIMD, liftSIMD2)
import qualified Data.Simdy.Internal.Class as I
import           Data.Simdy.Internal.Shuffle
import           Data.Simdy.Internal.SIMD128.X2 as M
import           Data.Simdy.Internal.SIMD256.HalfVector ()
import           Data.Simdy.Internal.SIMD256.X16 as M
import           Data.Simdy.Internal.SIMD256.X32 as M
import           Data.Simdy.Internal.SIMD256.X4 as M
import           Data.Simdy.Internal.SIMD256.X64 as M
import           Data.Simdy.Internal.SIMD256.X8 as M
import           Data.Word
import           Foreign.Storable
import           Prelude hiding (not, (==), (/=), (<), (<=), (>), (>=))

-- | Constraint on element types that can be stored in SIMD vectors
-- (e.g. 'Int32', 'Float', 'Double').
--
-- An instance of 'SIMDElement' supports basic SIMD operations (pack\/unpack\/broadcast)
class ( PackX2 X2 a
      , PackX4 X4 a
      , PackX8 X8 a
      , PackX16 X16 a
      , PackX32 X32 a
      , PackX64 X64 a
      , Broadcast X2 a
      , Broadcast X4 a
      , Broadcast X8 a
      , Broadcast X16 a
      , Broadcast X32 a
      , Broadcast X64 a
      , SplitShortVector X2 a
      , SplitShortVector X4 a
      , SplitShortVector X8 a
      , SplitShortVector X16 a
      , SplitShortVector X32 a
      , SplitShortVector X64 a
      , SelectableF X2 a
      , SelectableF X4 a
      , SelectableF X8 a
      , SelectableF X16 a
      , SelectableF X32 a
      , SelectableF X64 a
      ) => SIMDElement a
instance SIMDElement Bool
instance SIMDElement Int8
instance SIMDElement Int16
instance SIMDElement Int32
instance SIMDElement Int64
instance SIMDElement Word8
instance SIMDElement Word16
instance SIMDElement Word32
instance SIMDElement Word64
instance SIMDElement Float
instance SIMDElement Double
instance SIMDElement a => SIMDElement (Sum a)
instance SIMDElement a => SIMDElement (Product a)
instance SIMDElement a => SIMDElement (Min a)
instance SIMDElement a => SIMDElement (Max a)
instance SIMDElement a => SIMDElement (Complex a)
instance SIMDElement ()
instance (SIMDElement a0, SIMDElement a1) => SIMDElement (a0, a1)
instance (SIMDElement a0, SIMDElement a1, SIMDElement a2) => SIMDElement (a0, a1, a2)
instance (SIMDElement a0, SIMDElement a1, SIMDElement a2, SIMDElement a3) => SIMDElement (a0, a1, a2, a3)
instance (SIMDElement a0, SIMDElement a1, SIMDElement a2, SIMDElement a3, SIMDElement a4) => SIMDElement (a0, a1, a2, a3, a4)
instance (SIMDElement a0, SIMDElement a1, SIMDElement a2, SIMDElement a3, SIMDElement a4, SIMDElement a5) => SIMDElement (a0, a1, a2, a3, a4, a5)

-- | Constraint for element types that support lane-wise equality comparison.
--
-- @('SIMD' f, 'SIMDEq' a)@ implies @'Equatable' (f a)@.
class ( Eq a
      , SIMDElement a
      , EquatableF X2 a
      , EquatableF X4 a
      , EquatableF X8 a
      , EquatableF X16 a
      , EquatableF X32 a
      , EquatableF X64 a
      ) => SIMDEq a
instance SIMDEq Int8
instance SIMDEq Int16
instance SIMDEq Int32
instance SIMDEq Int64
instance SIMDEq Word8
instance SIMDEq Word16
instance SIMDEq Word32
instance SIMDEq Word64
instance SIMDEq Float
instance SIMDEq Double

-- | Constraint for element types that support lane-wise ordering comparison.
--
-- @('SIMD' f, 'SIMDOrd' a)@ implies @'Ordered' (f a)@.
class ( Ord a
      , SIMDEq a
      , OrderedF X2 a
      , OrderedF X4 a
      , OrderedF X8 a
      , OrderedF X16 a
      , OrderedF X32 a
      , OrderedF X64 a
      ) => SIMDOrd a
instance SIMDOrd Int8
instance SIMDOrd Int16
instance SIMDOrd Int32
instance SIMDOrd Int64
instance SIMDOrd Word8
instance SIMDOrd Word16
instance SIMDOrd Word32
instance SIMDOrd Word64
instance SIMDOrd Float
instance SIMDOrd Double

-- | Constraint for element types that support lane-wise arithmetic ('Num').
--
-- @('SIMD' f, 'SIMDNum' a)@ implies @'Num' (f a)@.
class ( Num a
      , SIMDElement a
      , NumF X2 a
      , NumF X4 a
      , NumF X8 a
      , NumF X16 a
      , NumF X32 a
      , NumF X64 a
      ) => SIMDNum a
instance SIMDNum Int8
instance SIMDNum Int16
instance SIMDNum Int32
instance SIMDNum Int64
instance SIMDNum Word8
instance SIMDNum Word16
instance SIMDNum Word32
instance SIMDNum Word64
instance SIMDNum Float
instance SIMDNum Double
-- instance (RealFloat a, SIMDNum a) => SIMDNum (Complex a)

-- | Constraint for element types that support lane-wise 'Fractional' operations.
--
-- @('SIMD' f, 'SIMDFractional' a)@ implies @'Fractional' (f a)@.
class ( Fractional a
      , SIMDNum a
      , FractionalF X2 a
      , FractionalF X4 a
      , FractionalF X8 a
      , FractionalF X16 a
      , FractionalF X32 a
      , FractionalF X64 a
      ) => SIMDFractional a
instance SIMDFractional Float
instance SIMDFractional Double
-- instance (RealFloat a, SIMDFractional a) => SIMDFractional (Complex a)

-- | Constraint for element types that support lane-wise 'Floating' operations.
--
-- @('SIMD' f, 'SIMDFloating' a)@ implies @'Floating' (f a)@.
class ( Floating a
      , SIMDFractional a
      , FloatingF X2 a
      , FloatingF X4 a
      , FloatingF X8 a
      , FloatingF X16 a
      , FloatingF X32 a
      , FloatingF X64 a
      ) => SIMDFloating a
instance SIMDFloating Float
instance SIMDFloating Double
-- instance (RealFloat a, SIMDFloating a) => SIMDFloating (Complex a)

-- | Constraint for element types that support lane-wise bitwise logic ('Boolean').
--
-- @('SIMD' f, 'SIMDBoolean' a)@ implies @'Boolean' (f a)@.
class ( Bits a
      , Boolean a
      , SIMDElement a
      , BooleanF X2 a
      , BooleanF X4 a
      , BooleanF X8 a
      , BooleanF X16 a
      , BooleanF X32 a
      , BooleanF X64 a
      ) => SIMDBoolean a
instance SIMDBoolean Bool
instance SIMDBoolean Int8
instance SIMDBoolean Int16
instance SIMDBoolean Int32
instance SIMDBoolean Int64
instance SIMDBoolean Word8
instance SIMDBoolean Word16
instance SIMDBoolean Word32
instance SIMDBoolean Word64

-- | Constraint for element types that support lane-wise bit shifts ('BitShift').
--
-- @('SIMD' f, 'SIMDBits' a)@ implies @'BitShift' (f a)@.
class ( Bits a
      , BitShift a
      , SIMDBoolean a
      , BitShiftF X2 a
      , BitShiftF X4 a
      , BitShiftF X8 a
      , BitShiftF X16 a
      , BitShiftF X32 a
      , BitShiftF X64 a
      ) => SIMDBits a
instance SIMDBits Int8
instance SIMDBits Int16
instance SIMDBits Int32
instance SIMDBits Int64
instance SIMDBits Word8
instance SIMDBits Word16
instance SIMDBits Word32
instance SIMDBits Word64

-- | Constraint for element types that support lane-wise 'MinMax'.
--
-- @('SIMD' f, 'SIMDMinMax' a)@ implies @'MinMax' (f a)@.
class ( MinMax a
      , SIMDElement a
      , MinMaxF X2 a
      , MinMaxF X4 a
      , MinMaxF X8 a
      , MinMaxF X16 a
      , MinMaxF X32 a
      , MinMaxF X64 a
      ) => SIMDMinMax a
instance SIMDMinMax Int8
instance SIMDMinMax Int16
instance SIMDMinMax Int32
instance SIMDMinMax Int64
instance SIMDMinMax Word8
instance SIMDMinMax Word16
instance SIMDMinMax Word32
instance SIMDMinMax Word64
instance SIMDMinMax Float
instance SIMDMinMax Double

-- | Constraint for element types that support lane-wise FMA in SIMD vectors.
class ( FusedMultiplyAdd a
      , SIMDNum a
      , FusedMultiplyAddF X2 a
      , FusedMultiplyAddF X4 a
      , FusedMultiplyAddF X8 a
      , FusedMultiplyAddF X16 a
      , FusedMultiplyAddF X32 a
      , FusedMultiplyAddF X64 a
      ) => SIMDFMA a
instance HasFMA => SIMDFMA Float
instance HasFMA => SIMDFMA Double

class ( Num a
      , SIMDElement a
      , EnumFromZero X2 a
      , EnumFromZero X4 a
      , EnumFromZero X8 a
      , EnumFromZero X16 a
      , EnumFromZero X32 a
      , EnumFromZero X64 a
      ) => SIMDEnumFromZero a
instance SIMDEnumFromZero Int8
instance SIMDEnumFromZero Int16
instance SIMDEnumFromZero Int32
instance SIMDEnumFromZero Int64
instance SIMDEnumFromZero Word8
instance SIMDEnumFromZero Word16
instance SIMDEnumFromZero Word32
instance SIMDEnumFromZero Word64
instance SIMDEnumFromZero Float
instance SIMDEnumFromZero Double

-- | Constraint for element types that support reading\/writing via 'Data.Primitive.Prim'.
class ( Prim a
      , SIMDElement a
      , MultiPrim X2 a
      , MultiPrim X4 a
      , MultiPrim X8 a
      , MultiPrim X16 a
      , MultiPrim X32 a
      , MultiPrim X64 a
      ) => SIMDPrim a
instance SIMDPrim Int8
instance SIMDPrim Int16
instance SIMDPrim Int32
instance SIMDPrim Int64
instance SIMDPrim Word8
instance SIMDPrim Word16
instance SIMDPrim Word32
instance SIMDPrim Word64
instance SIMDPrim Float
instance SIMDPrim Double

-- | Constraint for element types that support reading\/writing via 'Foreign.Storable.Storable'.
class ( Storable a
      , SIMDElement a
      , MultiStorable X2 a
      , MultiStorable X4 a
      , MultiStorable X8 a
      , MultiStorable X16 a
      , MultiStorable X32 a
      , MultiStorable X64 a
      ) => SIMDStorable a
instance SIMDStorable Int8
instance SIMDStorable Int16
instance SIMDStorable Int32
instance SIMDStorable Int64
instance SIMDStorable Word8
instance SIMDStorable Word16
instance SIMDStorable Word32
instance SIMDStorable Word64
instance SIMDStorable Float
instance SIMDStorable Double

-- | SIMD vector types. @SIMD f@ implies that @f@
-- supports broadcasting, element-wise lifting, comparison, arithmetic, etc.
class ( KnownSIMDLength f
      , LiftConstructor f
      , forall a. SIMDElement a => Broadcast f a
      , forall a b. (SIMDElement a, SIMDElement b) => LiftSIMD f a b
      , forall a b c. (SIMDElement a, SIMDElement b, SIMDElement c) => LiftSIMD2 f a b c
      , forall a. MaskIsLiftedBool f a
      , forall a. SIMDElement a => Selectable (f a)
      , forall a. SIMDEq a => Equatable (f a)
      , forall a. SIMDOrd a => Ordered (f a)
      , forall a. SIMDNum a => Num (f a)
      , forall a. SIMDFractional a => Fractional (f a)
      , forall a. SIMDFloating a => Floating (f a)
      , forall a. SIMDBoolean a => Boolean (f a)
      , forall a. SIMDBits a => BitShift (f a)
      , forall a. SIMDMinMax a => MinMax (f a)
      , forall a. SIMDFMA a => FusedMultiplyAdd (f a)
      , forall a. SIMDEnumFromZero a => EnumFromZero_ f a
      , forall a. SIMDPrim a => MultiPrim f a
      , forall a. SIMDStorable a => MultiStorable f a
      ) => SIMD f where
  -- | Reduce all lanes of a SIMD vector using a binary combining function.
  -- The function is applied via recursive halving (splitting the vector in half
  -- and combining until a scalar remains).
  horizontalFold :: SIMDElement a => (forall g. SIMD g => g a -> g a -> g a) -> f a -> a
instance SIMD Identity where
  horizontalFold _ = runIdentity
  {-# INLINE horizontalFold #-}
instance SIMD X2 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> runIdentity (op low high)
  {-# INLINE horizontalFold #-}
instance SIMD X4 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> horizontalFold op (op low high)
  {-# INLINE horizontalFold #-}
instance SIMD X8 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> horizontalFold op (op low high)
  {-# INLINE horizontalFold #-}
instance SIMD X16 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> horizontalFold op (op low high)
  {-# INLINE horizontalFold #-}
instance SIMD X32 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> horizontalFold op (op low high)
  {-# INLINE horizontalFold #-}
instance SIMD X64 where
  horizontalFold op !v = case splitShortVector v of (low, high) -> horizontalFold op (op low high)
  {-# INLINE horizontalFold #-}

-- | Create a SIMD vector with all lanes set to the same value.
--
-- Conceptually, @'broadcast' x = mkX/N/ x x x ... x@.
broadcast :: (SIMD f, SIMDElement a) => a -> f a
broadcast = I.broadcast
{-# INLINE broadcast #-}

-- | Apply a scalar function element-wise to a SIMD vector.
--
-- In general, the resulting function does not use SIMD instructions.
liftSIMD :: (SIMD f, SIMDElement a, SIMDElement b) => (a -> b) -> f a -> f b
liftSIMD = I.liftSIMD
{-# INLINE [1] liftSIMD #-}

-- | Apply a binary scalar function element-wise to two SIMD vectors.
--
-- In general, the resulting function does not use SIMD instructions.
liftSIMD2 :: (SIMD f, SIMDElement a, SIMDElement b, SIMDElement c) => (a -> b -> c) -> f a -> f b -> f c
liftSIMD2 = I.liftSIMD2
{-# INLINE [1] liftSIMD2 #-}

-- | Lane-wise conditional selection
--
-- @selectSIMD mask trueVec falseVec@ picks lanes from @trueVec@ where
-- the mask is true and from @falseVec@ where it is false.
selectSIMD :: (SIMD f, SIMDElement a)
           => f Bool -- ^ condition
           -> f a -- ^ then-expression
           -> f a -- ^ else-expression
           -> f a
selectSIMD = select
{-# INLINE selectSIMD #-}

infix 4 ==^, /=^

(==^) :: (SIMD f, SIMDEq a) => f a -> f a -> f Bool
(==^) = (==)
{-# INLINE (==^) #-}

(/=^) :: (SIMD f, SIMDEq a) => f a -> f a -> f Bool
(/=^) = (/=)
{-# INLINE (/=^) #-}

infix 4 <^, <=^, >^, >=^

(<^) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool
(<^) = (<)
{-# INLINE (<^) #-}

(<=^) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool
(<=^) = (<=)
{-# INLINE (<=^) #-}

(>^) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool
(>^) = (>)
{-# INLINE (>^) #-}

(>=^) :: (SIMD f, SIMDOrd a) => f a -> f a -> f Bool
(>=^) = (>=)
{-# INLINE (>=^) #-}

#if MIN_VERSION_GLASGOW_HASKELL(9, 10, 0, 0)
unaryShuffleX2 :: X2 a -> forall t -> UnaryShuffle (Tuple2ToList t) X2 a => X2 a
unaryShuffleX2 v t = unaryShuffle @(Tuple2ToList t) v
{-# INLINE unaryShuffleX2 #-}

unaryShuffleX4 :: X4 a -> forall t -> UnaryShuffle (Tuple4ToList t) X4 a => X4 a
unaryShuffleX4 v t = unaryShuffle @(Tuple4ToList t) v
{-# INLINE unaryShuffleX4 #-}

unaryShuffleX8 :: X8 a -> forall t -> UnaryShuffle (Tuple8ToList t) X8 a => X8 a
unaryShuffleX8 v t = unaryShuffle @(Tuple8ToList t) v
{-# INLINE unaryShuffleX8 #-}

unaryShuffleX16 :: X16 a -> forall t -> UnaryShuffle (Tuple16ToList t) X16 a => X16 a
unaryShuffleX16 v t = unaryShuffle @(Tuple16ToList t) v
{-# INLINE unaryShuffleX16 #-}

unaryShuffleX32 :: X32 a -> forall t -> UnaryShuffle (Tuple32ToList t) X32 a => X32 a
unaryShuffleX32 v t = unaryShuffle @(Tuple32ToList t) v
{-# INLINE unaryShuffleX32 #-}

unaryShuffleX64 :: X64 a -> forall t -> UnaryShuffle (Tuple64ToList t) X64 a => X64 a
unaryShuffleX64 v t = unaryShuffle @(Tuple64ToList t) v
{-# INLINE unaryShuffleX64 #-}

binaryShuffleX2 :: X2 a -> X2 a -> forall t -> BinaryShuffle (Tuple2ToList t) X2 a => X2 a
binaryShuffleX2 u v t = binaryShuffle @(Tuple2ToList t) u v
{-# INLINE binaryShuffleX2 #-}

binaryShuffleX4 :: X4 a -> X4 a -> forall t -> BinaryShuffle (Tuple4ToList t) X4 a => X4 a
binaryShuffleX4 u v t = binaryShuffle @(Tuple4ToList t) u v
{-# INLINE binaryShuffleX4 #-}

binaryShuffleX8 :: X8 a -> X8 a -> forall t -> BinaryShuffle (Tuple8ToList t) X8 a => X8 a
binaryShuffleX8 u v t = binaryShuffle @(Tuple8ToList t) u v
{-# INLINE binaryShuffleX8 #-}

binaryShuffleX16 :: X16 a -> X16 a -> forall t -> BinaryShuffle (Tuple16ToList t) X16 a => X16 a
binaryShuffleX16 u v t = binaryShuffle @(Tuple16ToList t) u v
{-# INLINE binaryShuffleX16 #-}

binaryShuffleX32 :: X32 a -> X32 a -> forall t -> BinaryShuffle (Tuple32ToList t) X32 a => X32 a
binaryShuffleX32 u v t = binaryShuffle @(Tuple32ToList t) u v
{-# INLINE binaryShuffleX32 #-}

binaryShuffleX64 :: X64 a -> X64 a -> forall t -> BinaryShuffle (Tuple64ToList t) X64 a => X64 a
binaryShuffleX64 u v t = binaryShuffle @(Tuple64ToList t) u v
{-# INLINE binaryShuffleX64 #-}
#endif

unaryShuffleWithX2 :: forall i0 i1 a. UnaryShuffle '[i0, i1] X2 a => ((Proxy 0, Proxy 1) -> (Proxy i0, Proxy i1)) -> X2 a -> X2 a
unaryShuffleWithX2 _ = unaryShuffle @'[i0, i1]
{-# INLINE unaryShuffleWithX2 #-}

unaryShuffleWithX4 :: forall i0 i1 i2 i3 a. UnaryShuffle '[i0, i1, i2, i3] X4 a => ((Proxy 0, Proxy 1, Proxy 2, Proxy 3) -> (Proxy i0, Proxy i1, Proxy i2, Proxy i3)) -> X4 a -> X4 a
unaryShuffleWithX4 _ = unaryShuffle @'[i0, i1, i2, i3]
{-# INLINE unaryShuffleWithX4 #-}

unaryShuffleWithX8 :: forall i0 i1 i2 i3 i4 i5 i6 i7 a. UnaryShuffle '[i0, i1, i2, i3, i4, i5, i6, i7] X8 a => ((Proxy 0, Proxy 1, Proxy 2, Proxy 3, Proxy 4, Proxy 5, Proxy 6, Proxy 7) -> (Proxy i0, Proxy i1, Proxy i2, Proxy i3, Proxy i4, Proxy i5, Proxy i6, Proxy i7)) -> X8 a -> X8 a
unaryShuffleWithX8 _ = unaryShuffle @'[i0, i1, i2, i3, i4, i5, i6, i7]
{-# INLINE unaryShuffleWithX8 #-}

unaryShuffleWithX16 :: forall i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 a. UnaryShuffle '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 a => ((Proxy 0, Proxy 1, Proxy 2, Proxy 3, Proxy 4, Proxy 5, Proxy 6, Proxy 7, Proxy 8, Proxy 9, Proxy 10, Proxy 11, Proxy 12, Proxy 13, Proxy 14, Proxy 15) -> (Proxy i0, Proxy i1, Proxy i2, Proxy i3, Proxy i4, Proxy i5, Proxy i6, Proxy i7, Proxy i8, Proxy i9, Proxy i10, Proxy i11, Proxy i12, Proxy i13, Proxy i14, Proxy i15)) -> X16 a -> X16 a
unaryShuffleWithX16 _ = unaryShuffle @'[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15]
{-# INLINE unaryShuffleWithX16 #-}

unaryShuffleWithX32 :: forall i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18 i19 i20 i21 i22 i23 i24 i25 i26 i27 i28 i29 i30 i31 a. UnaryShuffle '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 a => ((Proxy 0, Proxy 1, Proxy 2, Proxy 3, Proxy 4, Proxy 5, Proxy 6, Proxy 7, Proxy 8, Proxy 9, Proxy 10, Proxy 11, Proxy 12, Proxy 13, Proxy 14, Proxy 15, Proxy 16, Proxy 17, Proxy 18, Proxy 19, Proxy 20, Proxy 21, Proxy 22, Proxy 23, Proxy 24, Proxy 25, Proxy 26, Proxy 27, Proxy 28, Proxy 29, Proxy 30, Proxy 31) -> (Proxy i0, Proxy i1, Proxy i2, Proxy i3, Proxy i4, Proxy i5, Proxy i6, Proxy i7, Proxy i8, Proxy i9, Proxy i10, Proxy i11, Proxy i12, Proxy i13, Proxy i14, Proxy i15, Proxy i16, Proxy i17, Proxy i18, Proxy i19, Proxy i20, Proxy i21, Proxy i22, Proxy i23, Proxy i24, Proxy i25, Proxy i26, Proxy i27, Proxy i28, Proxy i29, Proxy i30, Proxy i31)) -> X32 a -> X32 a
unaryShuffleWithX32 _ = unaryShuffle @'[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31]
{-# INLINE unaryShuffleWithX32 #-}

unaryShuffleWithX64 :: forall i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18 i19 i20 i21 i22 i23 i24 i25 i26 i27 i28 i29 i30 i31 i32 i33 i34 i35 i36 i37 i38 i39 i40 i41 i42 i43 i44 i45 i46 i47 i48 i49 i50 i51 i52 i53 i54 i55 i56 i57 i58 i59 i60 i61 i62 i63 a. UnaryShuffle '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 a => ((Proxy 0, Proxy 1, Proxy 2, Proxy 3, Proxy 4, Proxy 5, Proxy 6, Proxy 7, Proxy 8, Proxy 9, Proxy 10, Proxy 11, Proxy 12, Proxy 13, Proxy 14, Proxy 15, Proxy 16, Proxy 17, Proxy 18, Proxy 19, Proxy 20, Proxy 21, Proxy 22, Proxy 23, Proxy 24, Proxy 25, Proxy 26, Proxy 27, Proxy 28, Proxy 29, Proxy 30, Proxy 31, Proxy 32, Proxy 33, Proxy 34, Proxy 35, Proxy 36, Proxy 37, Proxy 38, Proxy 39, Proxy 40, Proxy 41, Proxy 42, Proxy 43, Proxy 44, Proxy 45, Proxy 46, Proxy 47, Proxy 48, Proxy 49, Proxy 50, Proxy 51, Proxy 52, Proxy 53, Proxy 54, Proxy 55, Proxy 56, Proxy 57, Proxy 58, Proxy 59, Proxy 60, Proxy 61, Proxy 62, Proxy 63) -> (Proxy i0, Proxy i1, Proxy i2, Proxy i3, Proxy i4, Proxy i5, Proxy i6, Proxy i7, Proxy i8, Proxy i9, Proxy i10, Proxy i11, Proxy i12, Proxy i13, Proxy i14, Proxy i15, Proxy i16, Proxy i17, Proxy i18, Proxy i19, Proxy i20, Proxy i21, Proxy i22, Proxy i23, Proxy i24, Proxy i25, Proxy i26, Proxy i27, Proxy i28, Proxy i29, Proxy i30, Proxy i31, Proxy i32, Proxy i33, Proxy i34, Proxy i35, Proxy i36, Proxy i37, Proxy i38, Proxy i39, Proxy i40, Proxy i41, Proxy i42, Proxy i43, Proxy i44, Proxy i45, Proxy i46, Proxy i47, Proxy i48, Proxy i49, Proxy i50, Proxy i51, Proxy i52, Proxy i53, Proxy i54, Proxy i55, Proxy i56, Proxy i57, Proxy i58, Proxy i59, Proxy i60, Proxy i61, Proxy i62, Proxy i63)) -> X64 a -> X64 a
unaryShuffleWithX64 _ = unaryShuffle @'[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]
{-# INLINE unaryShuffleWithX64 #-}

binaryShuffleWithX2 :: forall i0 i1 a. BinaryShuffle '[i0, i1] X2 a => ((Proxy 0, Proxy 1) -> (Proxy 2, Proxy 3) -> (Proxy i0, Proxy i1)) -> X2 a -> X2 a -> X2 a
binaryShuffleWithX2 _ = binaryShuffle @'[i0, i1]
{-# INLINE binaryShuffleWithX2 #-}

binaryShuffleWithX4 :: forall i0 i1 i2 i3 a. BinaryShuffle '[i0, i1, i2, i3] X4 a => ((Proxy 0, Proxy 1, Proxy 2, Proxy 3) -> (Proxy 4, Proxy 5, Proxy 6, Proxy 7) -> (Proxy i0, Proxy i1, Proxy i2, Proxy i3)) -> X4 a -> X4 a -> X4 a
binaryShuffleWithX4 _ = binaryShuffle @'[i0, i1, i2, i3]
{-# INLINE binaryShuffleWithX4 #-}

binaryShuffleWithX8 :: forall i0 i1 i2 i3 i4 i5 i6 i7 a. BinaryShuffle '[i0, i1, i2, i3, i4, i5, i6, i7] X8 a => ((Proxy 0, Proxy 1, Proxy 2, Proxy 3, Proxy 4, Proxy 5, Proxy 6, Proxy 7) -> (Proxy 8, Proxy 9, Proxy 10, Proxy 11, Proxy 12, Proxy 13, Proxy 14, Proxy 15) -> (Proxy i0, Proxy i1, Proxy i2, Proxy i3, Proxy i4, Proxy i5, Proxy i6, Proxy i7)) -> X8 a -> X8 a -> X8 a
binaryShuffleWithX8 _ = binaryShuffle @'[i0, i1, i2, i3, i4, i5, i6, i7]
{-# INLINE binaryShuffleWithX8 #-}

binaryShuffleWithX16 :: forall i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 a. BinaryShuffle '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15] X16 a => ((Proxy 0, Proxy 1, Proxy 2, Proxy 3, Proxy 4, Proxy 5, Proxy 6, Proxy 7, Proxy 8, Proxy 9, Proxy 10, Proxy 11, Proxy 12, Proxy 13, Proxy 14, Proxy 15) -> (Proxy 16, Proxy 17, Proxy 18, Proxy 19, Proxy 20, Proxy 21, Proxy 22, Proxy 23, Proxy 24, Proxy 25, Proxy 26, Proxy 27, Proxy 28, Proxy 29, Proxy 30, Proxy 31) -> (Proxy i0, Proxy i1, Proxy i2, Proxy i3, Proxy i4, Proxy i5, Proxy i6, Proxy i7, Proxy i8, Proxy i9, Proxy i10, Proxy i11, Proxy i12, Proxy i13, Proxy i14, Proxy i15)) -> X16 a -> X16 a -> X16 a
binaryShuffleWithX16 _ = binaryShuffle @'[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15]
{-# INLINE binaryShuffleWithX16 #-}

binaryShuffleWithX32 :: forall i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18 i19 i20 i21 i22 i23 i24 i25 i26 i27 i28 i29 i30 i31 a. BinaryShuffle '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31] X32 a => ((Proxy 0, Proxy 1, Proxy 2, Proxy 3, Proxy 4, Proxy 5, Proxy 6, Proxy 7, Proxy 8, Proxy 9, Proxy 10, Proxy 11, Proxy 12, Proxy 13, Proxy 14, Proxy 15, Proxy 16, Proxy 17, Proxy 18, Proxy 19, Proxy 20, Proxy 21, Proxy 22, Proxy 23, Proxy 24, Proxy 25, Proxy 26, Proxy 27, Proxy 28, Proxy 29, Proxy 30, Proxy 31) -> (Proxy 32, Proxy 33, Proxy 34, Proxy 35, Proxy 36, Proxy 37, Proxy 38, Proxy 39, Proxy 40, Proxy 41, Proxy 42, Proxy 43, Proxy 44, Proxy 45, Proxy 46, Proxy 47, Proxy 48, Proxy 49, Proxy 50, Proxy 51, Proxy 52, Proxy 53, Proxy 54, Proxy 55, Proxy 56, Proxy 57, Proxy 58, Proxy 59, Proxy 60, Proxy 61, Proxy 62, Proxy 63) -> (Proxy i0, Proxy i1, Proxy i2, Proxy i3, Proxy i4, Proxy i5, Proxy i6, Proxy i7, Proxy i8, Proxy i9, Proxy i10, Proxy i11, Proxy i12, Proxy i13, Proxy i14, Proxy i15, Proxy i16, Proxy i17, Proxy i18, Proxy i19, Proxy i20, Proxy i21, Proxy i22, Proxy i23, Proxy i24, Proxy i25, Proxy i26, Proxy i27, Proxy i28, Proxy i29, Proxy i30, Proxy i31)) -> X32 a -> X32 a -> X32 a
binaryShuffleWithX32 _ = binaryShuffle @'[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31]
{-# INLINE binaryShuffleWithX32 #-}

binaryShuffleWithX64 :: forall i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 i16 i17 i18 i19 i20 i21 i22 i23 i24 i25 i26 i27 i28 i29 i30 i31 i32 i33 i34 i35 i36 i37 i38 i39 i40 i41 i42 i43 i44 i45 i46 i47 i48 i49 i50 i51 i52 i53 i54 i55 i56 i57 i58 i59 i60 i61 i62 i63 a. BinaryShuffle '[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63] X64 a => ((Proxy 0, Proxy 1, Proxy 2, Proxy 3, Proxy 4, Proxy 5, Proxy 6, Proxy 7, Proxy 8, Proxy 9, Proxy 10, Proxy 11, Proxy 12, Proxy 13, Proxy 14, Proxy 15, Proxy 16, Proxy 17, Proxy 18, Proxy 19, Proxy 20, Proxy 21, Proxy 22, Proxy 23, Proxy 24, Proxy 25, Proxy 26, Proxy 27, Proxy 28, Proxy 29, Proxy 30, Proxy 31, Proxy 32, Proxy 33, Proxy 34, Proxy 35, Proxy 36, Proxy 37, Proxy 38, Proxy 39, Proxy 40, Proxy 41, Proxy 42, Proxy 43, Proxy 44, Proxy 45, Proxy 46, Proxy 47, Proxy 48, Proxy 49, Proxy 50, Proxy 51, Proxy 52, Proxy 53, Proxy 54, Proxy 55, Proxy 56, Proxy 57, Proxy 58, Proxy 59, Proxy 60, Proxy 61, Proxy 62, Proxy 63) -> (Proxy 64, Proxy 65, Proxy 66, Proxy 67, Proxy 68, Proxy 69, Proxy 70, Proxy 71, Proxy 72, Proxy 73, Proxy 74, Proxy 75, Proxy 76, Proxy 77, Proxy 78, Proxy 79, Proxy 80, Proxy 81, Proxy 82, Proxy 83, Proxy 84, Proxy 85, Proxy 86, Proxy 87, Proxy 88, Proxy 89, Proxy 90, Proxy 91, Proxy 92, Proxy 93, Proxy 94, Proxy 95, Proxy 96, Proxy 97, Proxy 98, Proxy 99, Proxy 100, Proxy 101, Proxy 102, Proxy 103, Proxy 104, Proxy 105, Proxy 106, Proxy 107, Proxy 108, Proxy 109, Proxy 110, Proxy 111, Proxy 112, Proxy 113, Proxy 114, Proxy 115, Proxy 116, Proxy 117, Proxy 118, Proxy 119, Proxy 120, Proxy 121, Proxy 122, Proxy 123, Proxy 124, Proxy 125, Proxy 126, Proxy 127) -> (Proxy i0, Proxy i1, Proxy i2, Proxy i3, Proxy i4, Proxy i5, Proxy i6, Proxy i7, Proxy i8, Proxy i9, Proxy i10, Proxy i11, Proxy i12, Proxy i13, Proxy i14, Proxy i15, Proxy i16, Proxy i17, Proxy i18, Proxy i19, Proxy i20, Proxy i21, Proxy i22, Proxy i23, Proxy i24, Proxy i25, Proxy i26, Proxy i27, Proxy i28, Proxy i29, Proxy i30, Proxy i31, Proxy i32, Proxy i33, Proxy i34, Proxy i35, Proxy i36, Proxy i37, Proxy i38, Proxy i39, Proxy i40, Proxy i41, Proxy i42, Proxy i43, Proxy i44, Proxy i45, Proxy i46, Proxy i47, Proxy i48, Proxy i49, Proxy i50, Proxy i51, Proxy i52, Proxy i53, Proxy i54, Proxy i55, Proxy i56, Proxy i57, Proxy i58, Proxy i59, Proxy i60, Proxy i61, Proxy i62, Proxy i63)) -> X64 a -> X64 a -> X64 a
binaryShuffleWithX64 _ = binaryShuffle @'[i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22, i23, i24, i25, i26, i27, i28, i29, i30, i31, i32, i33, i34, i35, i36, i37, i38, i39, i40, i41, i42, i43, i44, i45, i46, i47, i48, i49, i50, i51, i52, i53, i54, i55, i56, i57, i58, i59, i60, i61, i62, i63]
{-# INLINE binaryShuffleWithX64 #-}

{-# RULES
"liftSIMD/Sum/X2"
  liftSIMD coerce = mkSum @X2
"liftSIMD/Sum/X4"
  liftSIMD coerce = mkSum @X4
"liftSIMD/Sum/X8"
  liftSIMD coerce = mkSum @X8
"liftSIMD/Sum/X16"
  liftSIMD coerce = mkSum @X16
"liftSIMD/Sum/X32"
  liftSIMD coerce = mkSum @X32
"liftSIMD/Sum/X64"
  liftSIMD coerce = mkSum @X64
"liftSIMD/getSum/X2"
  liftSIMD coerce = getSum' @X2
"liftSIMD/getSum/X4"
  liftSIMD coerce = getSum' @X4
"liftSIMD/getSum/X8"
  liftSIMD coerce = getSum' @X8
"liftSIMD/getSum/X16"
  liftSIMD coerce = getSum' @X16
"liftSIMD/getSum/X32"
  liftSIMD coerce = getSum' @X32
"liftSIMD/getSum/X64"
  liftSIMD coerce = getSum' @X64
"liftSIMD/Product/X2"
  liftSIMD coerce = mkProduct @X2
"liftSIMD/Product/X4"
  liftSIMD coerce = mkProduct @X4
"liftSIMD/Product/X8"
  liftSIMD coerce = mkProduct @X8
"liftSIMD/Product/X16"
  liftSIMD coerce = mkProduct @X16
"liftSIMD/Product/X32"
  liftSIMD coerce = mkProduct @X32
"liftSIMD/Product/X64"
  liftSIMD coerce = mkProduct @X64
"liftSIMD/getProduct/X2"
  liftSIMD coerce = getProduct' @X2
"liftSIMD/getProduct/X4"
  liftSIMD coerce = getProduct' @X4
"liftSIMD/getProduct/X8"
  liftSIMD coerce = getProduct' @X8
"liftSIMD/getProduct/X16"
  liftSIMD coerce = getProduct' @X16
"liftSIMD/getProduct/X32"
  liftSIMD coerce = getProduct' @X32
"liftSIMD/getProduct/X64"
  liftSIMD coerce = getProduct' @X64
"liftSIMD/Min/X2"
  liftSIMD coerce = mkMin @X2
"liftSIMD/Min/X4"
  liftSIMD coerce = mkMin @X4
"liftSIMD/Min/X8"
  liftSIMD coerce = mkMin @X8
"liftSIMD/Min/X16"
  liftSIMD coerce = mkMin @X16
"liftSIMD/Min/X32"
  liftSIMD coerce = mkMin @X32
"liftSIMD/Min/X64"
  liftSIMD coerce = mkMin @X64
"liftSIMD/getMin/X2"
  liftSIMD coerce = getMin' @X2
"liftSIMD/getMin/X4"
  liftSIMD coerce = getMin' @X4
"liftSIMD/getMin/X8"
  liftSIMD coerce = getMin' @X8
"liftSIMD/getMin/X16"
  liftSIMD coerce = getMin' @X16
"liftSIMD/getMin/X32"
  liftSIMD coerce = getMin' @X32
"liftSIMD/getMin/X64"
  liftSIMD coerce = getMin' @X64
"liftSIMD/Max/X2"
  liftSIMD coerce = mkMax @X2
"liftSIMD/Max/X4"
  liftSIMD coerce = mkMax @X4
"liftSIMD/Max/X8"
  liftSIMD coerce = mkMax @X8
"liftSIMD/Max/X16"
  liftSIMD coerce = mkMax @X16
"liftSIMD/Max/X32"
  liftSIMD coerce = mkMax @X32
"liftSIMD/Max/X64"
  liftSIMD coerce = mkMax @X64
"liftSIMD/getMax/X2"
  liftSIMD coerce = getMax' @X2
"liftSIMD/getMax/X4"
  liftSIMD coerce = getMax' @X4
"liftSIMD/getMax/X8"
  liftSIMD coerce = getMax' @X8
"liftSIMD/getMax/X16"
  liftSIMD coerce = getMax' @X16
"liftSIMD/getMax/X32"
  liftSIMD coerce = getMax' @X32
"liftSIMD/getMax/X64"
  liftSIMD coerce = getMax' @X64
"liftSIMD2/Complex/X2"
  liftSIMD2 (:+) = mkComplex @X2
"liftSIMD2/Complex/X4"
  liftSIMD2 (:+) = mkComplex @X4
"liftSIMD2/Complex/X8"
  liftSIMD2 (:+) = mkComplex @X8
"liftSIMD2/Complex/X16"
  liftSIMD2 (:+) = mkComplex @X16
"liftSIMD2/Complex/X32"
  liftSIMD2 (:+) = mkComplex @X32
"liftSIMD2/Complex/X64"
  liftSIMD2 (:+) = mkComplex @X64
"liftSIMD2/(,)/X2"
  liftSIMD2 (,) = mkTuple2 @X2
"liftSIMD2/(,)/X4"
  liftSIMD2 (,) = mkTuple2 @X4
"liftSIMD2/(,)/X8"
  liftSIMD2 (,) = mkTuple2 @X8
"liftSIMD2/(,)/X16"
  liftSIMD2 (,) = mkTuple2 @X16
"liftSIMD2/(,)/X32"
  liftSIMD2 (,) = mkTuple2 @X32
"liftSIMD2/(,)/X64"
  liftSIMD2 (,) = mkTuple2 @X64
  #-}
