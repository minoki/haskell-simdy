-- This file was created by script/Gen.hs. Do not edit by hand!
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Simdy.Internal.VL256.X4 where
import           Data.Bits
import           Data.Coerce (coerce)
import           Data.Complex
import           Data.Monoid
import           Data.Semigroup
import qualified Data.Simdy.Fusible as F
import           Data.Simdy.Internal.Bits (Boolean, BitShift)
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Shuffle
import           Data.Type.Ord (type (<))
import           Data.Simdy.Internal.VL256.Prim
import           Data.Simdy.Internal.VL128.PrimExtra
import           Data.Simdy.Internal.VL256.PrimExtra
import qualified GHC.Exts
import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..))
import           GHC.Int
import           GHC.IO
import           GHC.Word
import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=), min, max)
-- | @'X4' a@ is a fixed-length vector of length 4.
--
-- Conceptually, @data 'X4' a = MkX4 !a !a !a !a@.
--
-- You can access the elements by 'mkX4', 'packX4' and 'unpackX4'.
data family X4 a
instance KnownSIMDLength X4 where
  type SIMDLength X4 = 4
  simdLength = 4
  {-# INLINE simdLength #-}
newtype instance X4 Bool = MkBoolX4 Word8
type instance Mask (X4 a) = X4 Bool
instance MaskIsLiftedBool X4 a
instance BooleanF X4 Bool where
  andF (MkBoolX4 x) (MkBoolX4 y) = MkBoolX4 (x .&. y)
  orF (MkBoolX4 x) (MkBoolX4 y) = MkBoolX4 (x .|. y)
  xorF (MkBoolX4 x) (MkBoolX4 y) = MkBoolX4 (xor x y)
  complementF (MkBoolX4 x) = MkBoolX4 (0xf - x)
deriving via WrappedMulti X4 a instance EquatableF X4 a => Equatable (X4 a)
deriving via WrappedMulti X4 a instance OrderedF X4 a => Ordered (X4 a)
instance PackX4 X4 a => IsList (X4 a) where
  type Item (X4 a) = a
  toList = toListX4
  fromList = fromListX4
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX4 X4 Bool where
  mkX4 !x0 !x1 !x2 !x3 = MkBoolX4 ((if x0 then 0x1 else 0) .|. (if x1 then 0x2 else 0) .|. (if x2 then 0x4 else 0) .|. (if x3 then 0x8 else 0))
  unpackX4 (MkBoolX4 !x) = (testBit x 0, testBit x 1, testBit x 2, testBit x 3)
instance Broadcast X4 Bool where
  broadcast False = MkBoolX4 0
  broadcast True = MkBoolX4 0xf
  {-# INLINE broadcast #-}
instance SelectableF X4 Bool where
  selectF (MkBoolX4 !cond) (MkBoolX4 !x) (MkBoolX4 !y) = MkBoolX4 ((cond .&. x) .|. (complement cond .&. y))
  {-# INLINE selectF #-}
data instance X4 Float = MkFloatX4 FloatX4#
instance PackX4 X4 Float where
  mkX4 (F# x0) (F# x1) (F# x2) (F# x3) = MkFloatX4 (packFloatX4# (# x0, x1, x2, x3 #))
  unpackX4 (MkFloatX4 v0) = case unpackFloatX4# v0 of (# x0, x1, x2, x3 #) -> (F# x0, F# x1, F# x2, F# x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Float where
  broadcast (F# x) = MkFloatX4 (broadcastFloatX4# x)
  {-# INLINE broadcast #-}
instance SelectableF X4 Float where
  selectF (MkBoolX4 !cond) (MkFloatX4 x0) (MkFloatX4 y0) = MkFloatX4 (selectFloatX4# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 4, ShuffleMany FloatX4# indices) => UnaryShuffle indices X4 Float where
  unaryShuffle (MkFloatX4 x) = MkFloatX4 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 8, ShuffleMany FloatX4# indices) => BinaryShuffle indices X4 Float where
  binaryShuffle (MkFloatX4 x0) (MkFloatX4 x1) = MkFloatX4 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Float where
  eqF (MkFloatX4 u0) (MkFloatX4 v0) = MkBoolX4 $ (fromIntegral (eqFloatX4# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X4 Float where
  ltF (MkFloatX4 u0) (MkFloatX4 v0) = MkBoolX4 $ (fromIntegral (ltFloatX4# u0 v0))
  leF (MkFloatX4 u0) (MkFloatX4 v0) = MkBoolX4 $ (fromIntegral (leFloatX4# u0 v0))
  gtF (MkFloatX4 u0) (MkFloatX4 v0) = MkBoolX4 $ (fromIntegral (gtFloatX4# u0 v0))
  geF (MkFloatX4 u0) (MkFloatX4 v0) = MkBoolX4 $ (fromIntegral (geFloatX4# u0 v0))
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Float where
  minF (MkFloatX4 u0) (MkFloatX4 v0) = MkFloatX4 (minimumFloatX4# u0 v0)
  maxF (MkFloatX4 u0) (MkFloatX4 v0) = MkFloatX4 (maximumFloatX4# u0 v0)
  minimumNumberF (MkFloatX4 u0) (MkFloatX4 v0) = MkFloatX4 (minimumNumberFloatX4# u0 v0)
  maximumNumberF (MkFloatX4 u0) (MkFloatX4 v0) = MkFloatX4 (maximumNumberFloatX4# u0 v0)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX4Float :: X4 Float -> X4 Float
negateX4Float (MkFloatX4 u0) = MkFloatX4 (negateFloatX4# u0)
#if defined(USE_FMA)
{-# INLINE [0] negateX4Float #-}
#else
{-# INLINE negateX4Float #-}
#endif
instance NumF X4 Float where
  plusF (MkFloatX4 u0) (MkFloatX4 v0) = MkFloatX4 (plusFloatX4# u0 v0)
  minusF (MkFloatX4 u0) (MkFloatX4 v0) = MkFloatX4 (minusFloatX4# u0 v0)
  timesF (MkFloatX4 u0) (MkFloatX4 v0) = MkFloatX4 (timesFloatX4# u0 v0)
  negateF = negateX4Float
  absF (MkFloatX4 u0) = MkFloatX4 (absFloatX4# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X4 Float where
  divideF (MkFloatX4 u0) (MkFloatX4 v0) = MkFloatX4 (divideFloatX4# u0 v0)
  {-# INLINE divideF #-}
instance FloatingF X4 Float where
  sqrtF (MkFloatX4 u0) = MkFloatX4 (sqrtFloatX4# u0)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X4 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX4 u0) (MkFloatX4 v0) (MkFloatX4 w0) = MkFloatX4 (fmaddFloatX4# u0 v0 w0)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X4 Float" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X4 Float
"Fusible/*-/X4 Float" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX4Float c) :: X4 Float
"Fusible/-*+/X4 Float" forall a b c.
  negateX4Float (a F.* b) F.+ c = fusedMultiplyAdd (negateX4Float a) b c :: X4 Float
"Fusible/-*-/X4 Float" forall a b c.
  negateX4Float (a F.* b) F.- c = fusedMultiplyAdd (negateX4Float a) b (negateX4Float c) :: X4 Float
"Fusible/+*/X4 Float" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X4 Float
"Fusible/-*/X4 Float" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX4Float b) c a :: X4 Float
  #-}
#endif
instance EnumFromZero_ X4 Float where
  enumFromZero = MkFloatX4 (packFloatX4# (# 0.0#, 1.0#, 2.0#, 3.0# #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Float where
  indexByteArraySIMD# ba i = MkFloatX4 (indexFloatArrayAsFloatX4# ba i)
  readByteArraySIMD# mba i s0 = case readFloatArrayAsFloatX4# mba i s0 of (# s1, v0 #) -> (# s1, MkFloatX4 v0 #)
  writeByteArraySIMD# mba i (MkFloatX4 v0) s0 = writeFloatArrayAsFloatX4# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readFloatOffAddrAsFloatX4# addr i s0 of (# s1, v0 #) -> (# s1, MkFloatX4 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX4 v0) = IO (\s0 -> case writeFloatOffAddrAsFloatX4# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Double = MkDoubleX4 DoubleX4#
instance PackX4 X4 Double where
  mkX4 (D# x0) (D# x1) (D# x2) (D# x3) = MkDoubleX4 (packDoubleX4# (# x0, x1, x2, x3 #))
  unpackX4 (MkDoubleX4 v0) = case unpackDoubleX4# v0 of (# x0, x1, x2, x3 #) -> (D# x0, D# x1, D# x2, D# x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Double where
  broadcast (D# x) = MkDoubleX4 (broadcastDoubleX4# x)
  {-# INLINE broadcast #-}
instance SelectableF X4 Double where
  selectF (MkBoolX4 !cond) (MkDoubleX4 x0) (MkDoubleX4 y0) = MkDoubleX4 (selectDoubleX4# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 4, ShuffleMany DoubleX4# indices) => UnaryShuffle indices X4 Double where
  unaryShuffle (MkDoubleX4 x) = MkDoubleX4 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 8, ShuffleMany DoubleX4# indices) => BinaryShuffle indices X4 Double where
  binaryShuffle (MkDoubleX4 x0) (MkDoubleX4 x1) = MkDoubleX4 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Double where
  eqF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkBoolX4 $ (fromIntegral (eqDoubleX4# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X4 Double where
  ltF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkBoolX4 $ (fromIntegral (ltDoubleX4# u0 v0))
  leF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkBoolX4 $ (fromIntegral (leDoubleX4# u0 v0))
  gtF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkBoolX4 $ (fromIntegral (gtDoubleX4# u0 v0))
  geF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkBoolX4 $ (fromIntegral (geDoubleX4# u0 v0))
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Double where
  minF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkDoubleX4 (minimumDoubleX4# u0 v0)
  maxF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkDoubleX4 (maximumDoubleX4# u0 v0)
  minimumNumberF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkDoubleX4 (minimumNumberDoubleX4# u0 v0)
  maximumNumberF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkDoubleX4 (maximumNumberDoubleX4# u0 v0)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX4Double :: X4 Double -> X4 Double
negateX4Double (MkDoubleX4 u0) = MkDoubleX4 (negateDoubleX4# u0)
#if defined(USE_FMA)
{-# INLINE [0] negateX4Double #-}
#else
{-# INLINE negateX4Double #-}
#endif
instance NumF X4 Double where
  plusF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkDoubleX4 (plusDoubleX4# u0 v0)
  minusF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkDoubleX4 (minusDoubleX4# u0 v0)
  timesF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkDoubleX4 (timesDoubleX4# u0 v0)
  negateF = negateX4Double
  absF (MkDoubleX4 u0) = MkDoubleX4 (absDoubleX4# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X4 Double where
  divideF (MkDoubleX4 u0) (MkDoubleX4 v0) = MkDoubleX4 (divideDoubleX4# u0 v0)
  {-# INLINE divideF #-}
instance FloatingF X4 Double where
  sqrtF (MkDoubleX4 u0) = MkDoubleX4 (sqrtDoubleX4# u0)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X4 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX4 u0) (MkDoubleX4 v0) (MkDoubleX4 w0) = MkDoubleX4 (fmaddDoubleX4# u0 v0 w0)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X4 Double" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X4 Double
"Fusible/*-/X4 Double" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX4Double c) :: X4 Double
"Fusible/-*+/X4 Double" forall a b c.
  negateX4Double (a F.* b) F.+ c = fusedMultiplyAdd (negateX4Double a) b c :: X4 Double
"Fusible/-*-/X4 Double" forall a b c.
  negateX4Double (a F.* b) F.- c = fusedMultiplyAdd (negateX4Double a) b (negateX4Double c) :: X4 Double
"Fusible/+*/X4 Double" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X4 Double
"Fusible/-*/X4 Double" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX4Double b) c a :: X4 Double
  #-}
#endif
instance EnumFromZero_ X4 Double where
  enumFromZero = MkDoubleX4 (packDoubleX4# (# 0.0##, 1.0##, 2.0##, 3.0## #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Double where
  indexByteArraySIMD# ba i = MkDoubleX4 (indexDoubleArrayAsDoubleX4# ba i)
  readByteArraySIMD# mba i s0 = case readDoubleArrayAsDoubleX4# mba i s0 of (# s1, v0 #) -> (# s1, MkDoubleX4 v0 #)
  writeByteArraySIMD# mba i (MkDoubleX4 v0) s0 = writeDoubleArrayAsDoubleX4# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readDoubleOffAddrAsDoubleX4# addr i s0 of (# s1, v0 #) -> (# s1, MkDoubleX4 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX4 v0) = IO (\s0 -> case writeDoubleOffAddrAsDoubleX4# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
instance ImplementationDescription X4 where
  implementationDescription _ = "X4;maxBits=256"
data instance X4 Int8 = MkInt8X4 Int8X16#
instance PackX4 X4 Int8 where
  mkX4 (I8# x0) (I8# x1) (I8# x2) (I8# x3) = MkInt8X4 (packInt8X16# (# x0, x1, x2, x3, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  unpackX4 (MkInt8X4 v0) = case unpackInt8X16# v0 of (# x0, x1, x2, x3, _, _, _, _, _, _, _, _, _, _, _, _ #) -> (I8# x0, I8# x1, I8# x2, I8# x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Int8 where
  broadcast (I8# x) = MkInt8X4 (broadcastInt8X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X4 Int8 where
  selectF (MkBoolX4 !cond) (MkInt8X4 x0) (MkInt8X4 y0) = MkInt8X4 (selectInt8X16# (fromIntegral cond) x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 4, ShuffleMany Int8X16# indices) => UnaryShuffle indices X4 Int8 where
  unaryShuffle (MkInt8X4 x) = MkInt8X4 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 8, ShuffleMany Int8X16# indices) => BinaryShuffle indices X4 Int8 where
  binaryShuffle (MkInt8X4 x0) (MkInt8X4 x1) = MkInt8X4 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Int8 where
  eqF (MkInt8X4 u0) (MkInt8X4 v0) = MkBoolX4 $ (fromIntegral (eqInt8X16# u0 v0 .&. 0xf))
  {-# INLINE eqF #-}
instance OrderedF X4 Int8 where
  ltF (MkInt8X4 u0) (MkInt8X4 v0) = MkBoolX4 $ (fromIntegral (ltInt8X16# u0 v0 .&. 0xf))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Int8 where
  minF (MkInt8X4 u0) (MkInt8X4 v0) = MkInt8X4 (minInt8X16# u0 v0)
  maxF (MkInt8X4 u0) (MkInt8X4 v0) = MkInt8X4 (maxInt8X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Int8 where
  plusF (MkInt8X4 u0) (MkInt8X4 v0) = MkInt8X4 (plusInt8X16# u0 v0)
  minusF (MkInt8X4 u0) (MkInt8X4 v0) = MkInt8X4 (minusInt8X16# u0 v0)
  timesF (MkInt8X4 u0) (MkInt8X4 v0) = MkInt8X4 (timesInt8X16# u0 v0)
  negateF (MkInt8X4 u0) = MkInt8X4 (negateInt8X16# u0)
  absF (MkInt8X4 u0) = MkInt8X4 (absInt8X16# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X4 Int8 where
  andF (MkInt8X4 u0) (MkInt8X4 v0) = MkInt8X4 (andInt8X16# u0 v0)
  orF (MkInt8X4 u0) (MkInt8X4 v0) = MkInt8X4 (orInt8X16# u0 v0)
  xorF (MkInt8X4 u0) (MkInt8X4 v0) = MkInt8X4 (xorInt8X16# u0 v0)
  complementF (MkInt8X4 u0) = MkInt8X4 (complementInt8X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Int8 where
  shiftLF (MkInt8X4 u0) (I# i) = MkInt8X4 (shiftLInt8X16# u0 i)
  shiftRF (MkInt8X4 u0) (I# i) = MkInt8X4 (shiftRInt8X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X4 Int8 where
  enumFromZero = MkInt8X4 (packInt8X16# (# 0#Int8, 1#Int8, 2#Int8, 3#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Int8 where
  indexByteArraySIMD# ba i = MkInt8X4 (packInt8X16# (# GHC.Exts.indexInt8Array# ba i, GHC.Exts.indexInt8Array# ba (i +# 1#), GHC.Exts.indexInt8Array# ba (i +# 2#), GHC.Exts.indexInt8Array# ba (i +# 3#), 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt8X4 (packInt8X16# (# x0, x1, x2, x3, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #)) #)
  writeByteArraySIMD# mba i (MkInt8X4 v0) s0 = case unpackInt8X16# v0 of (# x0, x1, x2, x3, _, _, _, _, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeInt8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeInt8Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt8X4 (packInt8X16# (# x0, x1, x2, x3, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X4 v0) = IO (\s0 -> case unpackInt8X16# v0 of (# x0, x1, x2, x3, _, _, _, _, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeInt8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 3#) x3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Int16 = MkInt16X4 Int16X8#
instance PackX4 X4 Int16 where
  mkX4 (I16# x0) (I16# x1) (I16# x2) (I16# x3) = MkInt16X4 (packInt16X8# (# x0, x1, x2, x3, 0#Int16, 0#Int16, 0#Int16, 0#Int16 #))
  unpackX4 (MkInt16X4 v0) = case unpackInt16X8# v0 of (# x0, x1, x2, x3, _, _, _, _ #) -> (I16# x0, I16# x1, I16# x2, I16# x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Int16 where
  broadcast (I16# x) = MkInt16X4 (broadcastInt16X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X4 Int16 where
  selectF (MkBoolX4 !cond) (MkInt16X4 x0) (MkInt16X4 y0) = MkInt16X4 (selectInt16X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 4, ShuffleMany Int16X8# indices) => UnaryShuffle indices X4 Int16 where
  unaryShuffle (MkInt16X4 x) = MkInt16X4 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 8, ShuffleMany Int16X8# indices) => BinaryShuffle indices X4 Int16 where
  binaryShuffle (MkInt16X4 x0) (MkInt16X4 x1) = MkInt16X4 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Int16 where
  eqF (MkInt16X4 u0) (MkInt16X4 v0) = MkBoolX4 $ (fromIntegral (eqInt16X8# u0 v0 .&. 0xf))
  {-# INLINE eqF #-}
instance OrderedF X4 Int16 where
  ltF (MkInt16X4 u0) (MkInt16X4 v0) = MkBoolX4 $ (fromIntegral (ltInt16X8# u0 v0 .&. 0xf))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Int16 where
  minF (MkInt16X4 u0) (MkInt16X4 v0) = MkInt16X4 (minInt16X8# u0 v0)
  maxF (MkInt16X4 u0) (MkInt16X4 v0) = MkInt16X4 (maxInt16X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Int16 where
  plusF (MkInt16X4 u0) (MkInt16X4 v0) = MkInt16X4 (plusInt16X8# u0 v0)
  minusF (MkInt16X4 u0) (MkInt16X4 v0) = MkInt16X4 (minusInt16X8# u0 v0)
  timesF (MkInt16X4 u0) (MkInt16X4 v0) = MkInt16X4 (timesInt16X8# u0 v0)
  negateF (MkInt16X4 u0) = MkInt16X4 (negateInt16X8# u0)
  absF (MkInt16X4 u0) = MkInt16X4 (absInt16X8# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X4 Int16 where
  andF (MkInt16X4 u0) (MkInt16X4 v0) = MkInt16X4 (andInt16X8# u0 v0)
  orF (MkInt16X4 u0) (MkInt16X4 v0) = MkInt16X4 (orInt16X8# u0 v0)
  xorF (MkInt16X4 u0) (MkInt16X4 v0) = MkInt16X4 (xorInt16X8# u0 v0)
  complementF (MkInt16X4 u0) = MkInt16X4 (complementInt16X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Int16 where
  shiftLF (MkInt16X4 u0) (I# i) = MkInt16X4 (shiftLInt16X8# u0 i)
  shiftRF (MkInt16X4 u0) (I# i) = MkInt16X4 (shiftRInt16X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X4 Int16 where
  enumFromZero = MkInt16X4 (packInt16X8# (# 0#Int16, 1#Int16, 2#Int16, 3#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Int16 where
  indexByteArraySIMD# ba i = MkInt16X4 (packInt16X8# (# GHC.Exts.indexInt16Array# ba i, GHC.Exts.indexInt16Array# ba (i +# 1#), GHC.Exts.indexInt16Array# ba (i +# 2#), GHC.Exts.indexInt16Array# ba (i +# 3#), 0#Int16, 0#Int16, 0#Int16, 0#Int16 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt16Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt16Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt16X4 (packInt16X8# (# x0, x1, x2, x3, 0#Int16, 0#Int16, 0#Int16, 0#Int16 #)) #)
  writeByteArraySIMD# mba i (MkInt16X4 v0) s0 = case unpackInt16X8# v0 of (# x0, x1, x2, x3, _, _, _, _ #) -> case GHC.Exts.writeInt16Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt16Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt16Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeInt16Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt16X4 (packInt16X8# (# x0, x1, x2, x3, 0#Int16, 0#Int16, 0#Int16, 0#Int16 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X4 v0) = IO (\s0 -> case unpackInt16X8# v0 of (# x0, x1, x2, x3, _, _, _, _ #) -> case GHC.Exts.writeInt16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 3#) x3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Int32 = MkInt32X4 Int32X4#
instance PackX4 X4 Int32 where
  mkX4 (I32# x0) (I32# x1) (I32# x2) (I32# x3) = MkInt32X4 (packInt32X4# (# x0, x1, x2, x3 #))
  unpackX4 (MkInt32X4 v0) = case unpackInt32X4# v0 of (# x0, x1, x2, x3 #) -> (I32# x0, I32# x1, I32# x2, I32# x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Int32 where
  broadcast (I32# x) = MkInt32X4 (broadcastInt32X4# x)
  {-# INLINE broadcast #-}
instance SelectableF X4 Int32 where
  selectF (MkBoolX4 !cond) (MkInt32X4 x0) (MkInt32X4 y0) = MkInt32X4 (selectInt32X4# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 4, ShuffleMany Int32X4# indices) => UnaryShuffle indices X4 Int32 where
  unaryShuffle (MkInt32X4 x) = MkInt32X4 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 8, ShuffleMany Int32X4# indices) => BinaryShuffle indices X4 Int32 where
  binaryShuffle (MkInt32X4 x0) (MkInt32X4 x1) = MkInt32X4 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Int32 where
  eqF (MkInt32X4 u0) (MkInt32X4 v0) = MkBoolX4 $ (fromIntegral (eqInt32X4# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X4 Int32 where
  ltF (MkInt32X4 u0) (MkInt32X4 v0) = MkBoolX4 $ (fromIntegral (ltInt32X4# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Int32 where
  minF (MkInt32X4 u0) (MkInt32X4 v0) = MkInt32X4 (minInt32X4# u0 v0)
  maxF (MkInt32X4 u0) (MkInt32X4 v0) = MkInt32X4 (maxInt32X4# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Int32 where
  plusF (MkInt32X4 u0) (MkInt32X4 v0) = MkInt32X4 (plusInt32X4# u0 v0)
  minusF (MkInt32X4 u0) (MkInt32X4 v0) = MkInt32X4 (minusInt32X4# u0 v0)
  timesF (MkInt32X4 u0) (MkInt32X4 v0) = MkInt32X4 (timesInt32X4# u0 v0)
  negateF (MkInt32X4 u0) = MkInt32X4 (negateInt32X4# u0)
  absF (MkInt32X4 u0) = MkInt32X4 (absInt32X4# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X4 Int32 where
  andF (MkInt32X4 u0) (MkInt32X4 v0) = MkInt32X4 (andInt32X4# u0 v0)
  orF (MkInt32X4 u0) (MkInt32X4 v0) = MkInt32X4 (orInt32X4# u0 v0)
  xorF (MkInt32X4 u0) (MkInt32X4 v0) = MkInt32X4 (xorInt32X4# u0 v0)
  complementF (MkInt32X4 u0) = MkInt32X4 (complementInt32X4# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Int32 where
  shiftLF (MkInt32X4 u0) (I# i) = MkInt32X4 (shiftLInt32X4# u0 i)
  shiftRF (MkInt32X4 u0) (I# i) = MkInt32X4 (shiftRInt32X4# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X4 Int32 where
  enumFromZero = MkInt32X4 (packInt32X4# (# 0#Int32, 1#Int32, 2#Int32, 3#Int32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Int32 where
  indexByteArraySIMD# ba i = MkInt32X4 (indexInt32ArrayAsInt32X4# ba i)
  readByteArraySIMD# mba i s0 = case readInt32ArrayAsInt32X4# mba i s0 of (# s1, v0 #) -> (# s1, MkInt32X4 v0 #)
  writeByteArraySIMD# mba i (MkInt32X4 v0) s0 = writeInt32ArrayAsInt32X4# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt32OffAddrAsInt32X4# addr i s0 of (# s1, v0 #) -> (# s1, MkInt32X4 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X4 v0) = IO (\s0 -> case writeInt32OffAddrAsInt32X4# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Int64 = MkInt64X4 Int64X4#
instance PackX4 X4 Int64 where
  mkX4 (I64# x0) (I64# x1) (I64# x2) (I64# x3) = MkInt64X4 (packInt64X4# (# x0, x1, x2, x3 #))
  unpackX4 (MkInt64X4 v0) = case unpackInt64X4# v0 of (# x0, x1, x2, x3 #) -> (I64# x0, I64# x1, I64# x2, I64# x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Int64 where
  broadcast (I64# x) = MkInt64X4 (broadcastInt64X4# x)
  {-# INLINE broadcast #-}
instance SelectableF X4 Int64 where
  selectF (MkBoolX4 !cond) (MkInt64X4 x0) (MkInt64X4 y0) = MkInt64X4 (selectInt64X4# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 4, ShuffleMany Int64X4# indices) => UnaryShuffle indices X4 Int64 where
  unaryShuffle (MkInt64X4 x) = MkInt64X4 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 8, ShuffleMany Int64X4# indices) => BinaryShuffle indices X4 Int64 where
  binaryShuffle (MkInt64X4 x0) (MkInt64X4 x1) = MkInt64X4 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Int64 where
  eqF (MkInt64X4 u0) (MkInt64X4 v0) = MkBoolX4 $ (fromIntegral (eqInt64X4# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X4 Int64 where
  ltF (MkInt64X4 u0) (MkInt64X4 v0) = MkBoolX4 $ (fromIntegral (ltInt64X4# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Int64 where
  minF (MkInt64X4 u0) (MkInt64X4 v0) = MkInt64X4 (minInt64X4# u0 v0)
  maxF (MkInt64X4 u0) (MkInt64X4 v0) = MkInt64X4 (maxInt64X4# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Int64 where
  plusF (MkInt64X4 u0) (MkInt64X4 v0) = MkInt64X4 (plusInt64X4# u0 v0)
  minusF (MkInt64X4 u0) (MkInt64X4 v0) = MkInt64X4 (minusInt64X4# u0 v0)
  timesF (MkInt64X4 u0) (MkInt64X4 v0) = MkInt64X4 (timesInt64X4# u0 v0)
  negateF (MkInt64X4 u0) = MkInt64X4 (negateInt64X4# u0)
  absF (MkInt64X4 u0) = MkInt64X4 (absInt64X4# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X4 Int64 where
  andF (MkInt64X4 u0) (MkInt64X4 v0) = MkInt64X4 (andInt64X4# u0 v0)
  orF (MkInt64X4 u0) (MkInt64X4 v0) = MkInt64X4 (orInt64X4# u0 v0)
  xorF (MkInt64X4 u0) (MkInt64X4 v0) = MkInt64X4 (xorInt64X4# u0 v0)
  complementF (MkInt64X4 u0) = MkInt64X4 (complementInt64X4# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Int64 where
  shiftLF (MkInt64X4 u0) (I# i) = MkInt64X4 (shiftLInt64X4# u0 i)
  shiftRF (MkInt64X4 u0) (I# i) = MkInt64X4 (shiftRInt64X4# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X4 Int64 where
  enumFromZero = MkInt64X4 (packInt64X4# (# 0#Int64, 1#Int64, 2#Int64, 3#Int64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Int64 where
  indexByteArraySIMD# ba i = MkInt64X4 (indexInt64ArrayAsInt64X4# ba i)
  readByteArraySIMD# mba i s0 = case readInt64ArrayAsInt64X4# mba i s0 of (# s1, v0 #) -> (# s1, MkInt64X4 v0 #)
  writeByteArraySIMD# mba i (MkInt64X4 v0) s0 = writeInt64ArrayAsInt64X4# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt64OffAddrAsInt64X4# addr i s0 of (# s1, v0 #) -> (# s1, MkInt64X4 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X4 v0) = IO (\s0 -> case writeInt64OffAddrAsInt64X4# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Word8 = MkWord8X4 Word8X16#
instance PackX4 X4 Word8 where
  mkX4 (W8# x0) (W8# x1) (W8# x2) (W8# x3) = MkWord8X4 (packWord8X16# (# x0, x1, x2, x3, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  unpackX4 (MkWord8X4 v0) = case unpackWord8X16# v0 of (# x0, x1, x2, x3, _, _, _, _, _, _, _, _, _, _, _, _ #) -> (W8# x0, W8# x1, W8# x2, W8# x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Word8 where
  broadcast (W8# x) = MkWord8X4 (broadcastWord8X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X4 Word8 where
  selectF (MkBoolX4 !cond) (MkWord8X4 x0) (MkWord8X4 y0) = MkWord8X4 (selectWord8X16# (fromIntegral cond) x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 4, ShuffleMany Word8X16# indices) => UnaryShuffle indices X4 Word8 where
  unaryShuffle (MkWord8X4 x) = MkWord8X4 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 8, ShuffleMany Word8X16# indices) => BinaryShuffle indices X4 Word8 where
  binaryShuffle (MkWord8X4 x0) (MkWord8X4 x1) = MkWord8X4 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Word8 where
  eqF (MkWord8X4 u0) (MkWord8X4 v0) = MkBoolX4 $ (fromIntegral (eqWord8X16# u0 v0 .&. 0xf))
  {-# INLINE eqF #-}
instance OrderedF X4 Word8 where
  ltF (MkWord8X4 u0) (MkWord8X4 v0) = MkBoolX4 $ (fromIntegral (ltWord8X16# u0 v0 .&. 0xf))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Word8 where
  minF (MkWord8X4 u0) (MkWord8X4 v0) = MkWord8X4 (minWord8X16# u0 v0)
  maxF (MkWord8X4 u0) (MkWord8X4 v0) = MkWord8X4 (maxWord8X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Word8 where
  plusF (MkWord8X4 u0) (MkWord8X4 v0) = MkWord8X4 (plusWord8X16# u0 v0)
  minusF (MkWord8X4 u0) (MkWord8X4 v0) = MkWord8X4 (minusWord8X16# u0 v0)
  timesF (MkWord8X4 u0) (MkWord8X4 v0) = MkWord8X4 (timesWord8X16# u0 v0)
  -- Currently, there is no negateWord8X16#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X4 Word8 where
  andF (MkWord8X4 u0) (MkWord8X4 v0) = MkWord8X4 (andWord8X16# u0 v0)
  orF (MkWord8X4 u0) (MkWord8X4 v0) = MkWord8X4 (orWord8X16# u0 v0)
  xorF (MkWord8X4 u0) (MkWord8X4 v0) = MkWord8X4 (xorWord8X16# u0 v0)
  complementF (MkWord8X4 u0) = MkWord8X4 (complementWord8X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Word8 where
  shiftLF (MkWord8X4 u0) (I# i) = MkWord8X4 (shiftLWord8X16# u0 i)
  shiftRF (MkWord8X4 u0) (I# i) = MkWord8X4 (shiftRWord8X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X4 Word8 where
  enumFromZero = MkWord8X4 (packWord8X16# (# 0#Word8, 1#Word8, 2#Word8, 3#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Word8 where
  indexByteArraySIMD# ba i = MkWord8X4 (packWord8X16# (# GHC.Exts.indexWord8Array# ba i, GHC.Exts.indexWord8Array# ba (i +# 1#), GHC.Exts.indexWord8Array# ba (i +# 2#), GHC.Exts.indexWord8Array# ba (i +# 3#), 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord8X4 (packWord8X16# (# x0, x1, x2, x3, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #)) #)
  writeByteArraySIMD# mba i (MkWord8X4 v0) s0 = case unpackWord8X16# v0 of (# x0, x1, x2, x3, _, _, _, _, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeWord8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeWord8Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord8X4 (packWord8X16# (# x0, x1, x2, x3, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X4 v0) = IO (\s0 -> case unpackWord8X16# v0 of (# x0, x1, x2, x3, _, _, _, _, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeWord8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 3#) x3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Word16 = MkWord16X4 Word16X8#
instance PackX4 X4 Word16 where
  mkX4 (W16# x0) (W16# x1) (W16# x2) (W16# x3) = MkWord16X4 (packWord16X8# (# x0, x1, x2, x3, 0#Word16, 0#Word16, 0#Word16, 0#Word16 #))
  unpackX4 (MkWord16X4 v0) = case unpackWord16X8# v0 of (# x0, x1, x2, x3, _, _, _, _ #) -> (W16# x0, W16# x1, W16# x2, W16# x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Word16 where
  broadcast (W16# x) = MkWord16X4 (broadcastWord16X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X4 Word16 where
  selectF (MkBoolX4 !cond) (MkWord16X4 x0) (MkWord16X4 y0) = MkWord16X4 (selectWord16X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 4, ShuffleMany Word16X8# indices) => UnaryShuffle indices X4 Word16 where
  unaryShuffle (MkWord16X4 x) = MkWord16X4 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 8, ShuffleMany Word16X8# indices) => BinaryShuffle indices X4 Word16 where
  binaryShuffle (MkWord16X4 x0) (MkWord16X4 x1) = MkWord16X4 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Word16 where
  eqF (MkWord16X4 u0) (MkWord16X4 v0) = MkBoolX4 $ (fromIntegral (eqWord16X8# u0 v0 .&. 0xf))
  {-# INLINE eqF #-}
instance OrderedF X4 Word16 where
  ltF (MkWord16X4 u0) (MkWord16X4 v0) = MkBoolX4 $ (fromIntegral (ltWord16X8# u0 v0 .&. 0xf))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Word16 where
  minF (MkWord16X4 u0) (MkWord16X4 v0) = MkWord16X4 (minWord16X8# u0 v0)
  maxF (MkWord16X4 u0) (MkWord16X4 v0) = MkWord16X4 (maxWord16X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Word16 where
  plusF (MkWord16X4 u0) (MkWord16X4 v0) = MkWord16X4 (plusWord16X8# u0 v0)
  minusF (MkWord16X4 u0) (MkWord16X4 v0) = MkWord16X4 (minusWord16X8# u0 v0)
  timesF (MkWord16X4 u0) (MkWord16X4 v0) = MkWord16X4 (timesWord16X8# u0 v0)
  -- Currently, there is no negateWord16X8#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X4 Word16 where
  andF (MkWord16X4 u0) (MkWord16X4 v0) = MkWord16X4 (andWord16X8# u0 v0)
  orF (MkWord16X4 u0) (MkWord16X4 v0) = MkWord16X4 (orWord16X8# u0 v0)
  xorF (MkWord16X4 u0) (MkWord16X4 v0) = MkWord16X4 (xorWord16X8# u0 v0)
  complementF (MkWord16X4 u0) = MkWord16X4 (complementWord16X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Word16 where
  shiftLF (MkWord16X4 u0) (I# i) = MkWord16X4 (shiftLWord16X8# u0 i)
  shiftRF (MkWord16X4 u0) (I# i) = MkWord16X4 (shiftRWord16X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X4 Word16 where
  enumFromZero = MkWord16X4 (packWord16X8# (# 0#Word16, 1#Word16, 2#Word16, 3#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Word16 where
  indexByteArraySIMD# ba i = MkWord16X4 (packWord16X8# (# GHC.Exts.indexWord16Array# ba i, GHC.Exts.indexWord16Array# ba (i +# 1#), GHC.Exts.indexWord16Array# ba (i +# 2#), GHC.Exts.indexWord16Array# ba (i +# 3#), 0#Word16, 0#Word16, 0#Word16, 0#Word16 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord16Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord16Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord16X4 (packWord16X8# (# x0, x1, x2, x3, 0#Word16, 0#Word16, 0#Word16, 0#Word16 #)) #)
  writeByteArraySIMD# mba i (MkWord16X4 v0) s0 = case unpackWord16X8# v0 of (# x0, x1, x2, x3, _, _, _, _ #) -> case GHC.Exts.writeWord16Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord16Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord16Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeWord16Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord16X4 (packWord16X8# (# x0, x1, x2, x3, 0#Word16, 0#Word16, 0#Word16, 0#Word16 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X4 v0) = IO (\s0 -> case unpackWord16X8# v0 of (# x0, x1, x2, x3, _, _, _, _ #) -> case GHC.Exts.writeWord16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 3#) x3 s3 of s4 -> (# s4, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Word32 = MkWord32X4 Word32X4#
instance PackX4 X4 Word32 where
  mkX4 (W32# x0) (W32# x1) (W32# x2) (W32# x3) = MkWord32X4 (packWord32X4# (# x0, x1, x2, x3 #))
  unpackX4 (MkWord32X4 v0) = case unpackWord32X4# v0 of (# x0, x1, x2, x3 #) -> (W32# x0, W32# x1, W32# x2, W32# x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Word32 where
  broadcast (W32# x) = MkWord32X4 (broadcastWord32X4# x)
  {-# INLINE broadcast #-}
instance SelectableF X4 Word32 where
  selectF (MkBoolX4 !cond) (MkWord32X4 x0) (MkWord32X4 y0) = MkWord32X4 (selectWord32X4# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 4, ShuffleMany Word32X4# indices) => UnaryShuffle indices X4 Word32 where
  unaryShuffle (MkWord32X4 x) = MkWord32X4 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 8, ShuffleMany Word32X4# indices) => BinaryShuffle indices X4 Word32 where
  binaryShuffle (MkWord32X4 x0) (MkWord32X4 x1) = MkWord32X4 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Word32 where
  eqF (MkWord32X4 u0) (MkWord32X4 v0) = MkBoolX4 $ (fromIntegral (eqWord32X4# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X4 Word32 where
  ltF (MkWord32X4 u0) (MkWord32X4 v0) = MkBoolX4 $ (fromIntegral (ltWord32X4# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Word32 where
  minF (MkWord32X4 u0) (MkWord32X4 v0) = MkWord32X4 (minWord32X4# u0 v0)
  maxF (MkWord32X4 u0) (MkWord32X4 v0) = MkWord32X4 (maxWord32X4# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Word32 where
  plusF (MkWord32X4 u0) (MkWord32X4 v0) = MkWord32X4 (plusWord32X4# u0 v0)
  minusF (MkWord32X4 u0) (MkWord32X4 v0) = MkWord32X4 (minusWord32X4# u0 v0)
  timesF (MkWord32X4 u0) (MkWord32X4 v0) = MkWord32X4 (timesWord32X4# u0 v0)
  -- Currently, there is no negateWord32X4#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X4 Word32 where
  andF (MkWord32X4 u0) (MkWord32X4 v0) = MkWord32X4 (andWord32X4# u0 v0)
  orF (MkWord32X4 u0) (MkWord32X4 v0) = MkWord32X4 (orWord32X4# u0 v0)
  xorF (MkWord32X4 u0) (MkWord32X4 v0) = MkWord32X4 (xorWord32X4# u0 v0)
  complementF (MkWord32X4 u0) = MkWord32X4 (complementWord32X4# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Word32 where
  shiftLF (MkWord32X4 u0) (I# i) = MkWord32X4 (shiftLWord32X4# u0 i)
  shiftRF (MkWord32X4 u0) (I# i) = MkWord32X4 (shiftRWord32X4# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X4 Word32 where
  enumFromZero = MkWord32X4 (packWord32X4# (# 0#Word32, 1#Word32, 2#Word32, 3#Word32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Word32 where
  indexByteArraySIMD# ba i = MkWord32X4 (indexWord32ArrayAsWord32X4# ba i)
  readByteArraySIMD# mba i s0 = case readWord32ArrayAsWord32X4# mba i s0 of (# s1, v0 #) -> (# s1, MkWord32X4 v0 #)
  writeByteArraySIMD# mba i (MkWord32X4 v0) s0 = writeWord32ArrayAsWord32X4# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord32OffAddrAsWord32X4# addr i s0 of (# s1, v0 #) -> (# s1, MkWord32X4 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X4 v0) = IO (\s0 -> case writeWord32OffAddrAsWord32X4# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Word64 = MkWord64X4 Word64X4#
instance PackX4 X4 Word64 where
  mkX4 (W64# x0) (W64# x1) (W64# x2) (W64# x3) = MkWord64X4 (packWord64X4# (# x0, x1, x2, x3 #))
  unpackX4 (MkWord64X4 v0) = case unpackWord64X4# v0 of (# x0, x1, x2, x3 #) -> (W64# x0, W64# x1, W64# x2, W64# x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Word64 where
  broadcast (W64# x) = MkWord64X4 (broadcastWord64X4# x)
  {-# INLINE broadcast #-}
instance SelectableF X4 Word64 where
  selectF (MkBoolX4 !cond) (MkWord64X4 x0) (MkWord64X4 y0) = MkWord64X4 (selectWord64X4# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 4, ShuffleMany Word64X4# indices) => UnaryShuffle indices X4 Word64 where
  unaryShuffle (MkWord64X4 x) = MkWord64X4 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 8, ShuffleMany Word64X4# indices) => BinaryShuffle indices X4 Word64 where
  binaryShuffle (MkWord64X4 x0) (MkWord64X4 x1) = MkWord64X4 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Word64 where
  eqF (MkWord64X4 u0) (MkWord64X4 v0) = MkBoolX4 $ (fromIntegral (eqWord64X4# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X4 Word64 where
  ltF (MkWord64X4 u0) (MkWord64X4 v0) = MkBoolX4 $ (fromIntegral (ltWord64X4# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Word64 where
  minF (MkWord64X4 u0) (MkWord64X4 v0) = MkWord64X4 (minWord64X4# u0 v0)
  maxF (MkWord64X4 u0) (MkWord64X4 v0) = MkWord64X4 (maxWord64X4# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Word64 where
  plusF (MkWord64X4 u0) (MkWord64X4 v0) = MkWord64X4 (plusWord64X4# u0 v0)
  minusF (MkWord64X4 u0) (MkWord64X4 v0) = MkWord64X4 (minusWord64X4# u0 v0)
  timesF (MkWord64X4 u0) (MkWord64X4 v0) = MkWord64X4 (timesWord64X4# u0 v0)
  -- Currently, there is no negateWord64X4#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X4 Word64 where
  andF (MkWord64X4 u0) (MkWord64X4 v0) = MkWord64X4 (andWord64X4# u0 v0)
  orF (MkWord64X4 u0) (MkWord64X4 v0) = MkWord64X4 (orWord64X4# u0 v0)
  xorF (MkWord64X4 u0) (MkWord64X4 v0) = MkWord64X4 (xorWord64X4# u0 v0)
  complementF (MkWord64X4 u0) = MkWord64X4 (complementWord64X4# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Word64 where
  shiftLF (MkWord64X4 u0) (I# i) = MkWord64X4 (shiftLWord64X4# u0 i)
  shiftRF (MkWord64X4 u0) (I# i) = MkWord64X4 (shiftRWord64X4# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X4 Word64 where
  enumFromZero = MkWord64X4 (packWord64X4# (# 0#Word64, 1#Word64, 2#Word64, 3#Word64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Word64 where
  indexByteArraySIMD# ba i = MkWord64X4 (indexWord64ArrayAsWord64X4# ba i)
  readByteArraySIMD# mba i s0 = case readWord64ArrayAsWord64X4# mba i s0 of (# s1, v0 #) -> (# s1, MkWord64X4 v0 #)
  writeByteArraySIMD# mba i (MkWord64X4 v0) s0 = writeWord64ArrayAsWord64X4# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord64OffAddrAsWord64X4# addr i s0 of (# s1, v0 #) -> (# s1, MkWord64X4 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X4 v0) = IO (\s0 -> case writeWord64OffAddrAsWord64X4# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
newtype instance X4 (Sum a) = MkSumX4 (X4 a)
instance PackX4 X4 a => PackX4 X4 (Sum a) where
  mkX4 = coerce (mkX4 @X4 @a)
  unpackX4 = coerce (unpackX4 @X4 @a)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 a => Broadcast X4 (Sum a) where
  broadcast = coerce (broadcast @X4 @a)
  {-# INLINE broadcast #-}
instance SelectableF X4 a => SelectableF X4 (Sum a) where
  selectF = coerce (selectF @X4 @a)
  {-# INLINE selectF #-}
newtype instance X4 (Product a) = MkProductX4 (X4 a)
instance PackX4 X4 a => PackX4 X4 (Product a) where
  mkX4 = coerce (mkX4 @X4 @a)
  unpackX4 = coerce (unpackX4 @X4 @a)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 a => Broadcast X4 (Product a) where
  broadcast = coerce (broadcast @X4 @a)
  {-# INLINE broadcast #-}
instance SelectableF X4 a => SelectableF X4 (Product a) where
  selectF = coerce (selectF @X4 @a)
  {-# INLINE selectF #-}
newtype instance X4 (Min a) = MkMinX4 (X4 a)
instance PackX4 X4 a => PackX4 X4 (Min a) where
  mkX4 = coerce (mkX4 @X4 @a)
  unpackX4 = coerce (unpackX4 @X4 @a)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 a => Broadcast X4 (Min a) where
  broadcast = coerce (broadcast @X4 @a)
  {-# INLINE broadcast #-}
instance SelectableF X4 a => SelectableF X4 (Min a) where
  selectF = coerce (selectF @X4 @a)
  {-# INLINE selectF #-}
newtype instance X4 (Max a) = MkMaxX4 (X4 a)
instance PackX4 X4 a => PackX4 X4 (Max a) where
  mkX4 = coerce (mkX4 @X4 @a)
  unpackX4 = coerce (unpackX4 @X4 @a)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 a => Broadcast X4 (Max a) where
  broadcast = coerce (broadcast @X4 @a)
  {-# INLINE broadcast #-}
instance SelectableF X4 a => SelectableF X4 (Max a) where
  selectF = coerce (selectF @X4 @a)
  {-# INLINE selectF #-}
data instance X4 (Complex a) = MkComplexX4 !(X4 a) !(X4 a)
instance PackX4 X4 a => PackX4 X4 (Complex a) where
  mkX4 (x0 :+ y0) (x1 :+ y1) (x2 :+ y2) (x3 :+ y3) = MkComplexX4 (mkX4 x0 x1 x2 x3) (mkX4 y0 y1 y2 y3)
  unpackX4 (MkComplexX4 s t) = case unpackX4 s of (x0, x1, x2, x3) -> case unpackX4 t of (y0, y1, y2, y3) -> (x0 :+ y0, x1 :+ y1, x2 :+ y2, x3 :+ y3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 a => Broadcast X4 (Complex a) where
  broadcast (x :+ y) = MkComplexX4 (broadcast x) (broadcast y)
  {-# INLINE broadcast #-}
instance SelectableF X4 a => SelectableF X4 (Complex a) where
  selectF !cond (MkComplexX4 x y) (MkComplexX4 x' y') = MkComplexX4 (selectF cond x x') (selectF cond y y')
  {-# INLINE selectF #-}
data instance X4 () = MkUnitX4
instance PackX4 X4 () where
  mkX4 _ _ _ _ = MkUnitX4
  unpackX4 MkUnitX4 = ((), (), (), ())
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 () where
  broadcast _ = MkUnitX4
  {-# INLINE broadcast #-}
instance SelectableF X4 () where
  selectF _ _ _ = MkUnitX4
  {-# INLINE selectF #-}
data instance X4 (a0, a1) = MkTuple2X4 !(X4 a0) !(X4 a1)
instance (PackX4 X4 a0, PackX4 X4 a1) => PackX4 X4 (a0, a1) where
  mkX4 (x0_0, x0_1) (x1_0, x1_1) (x2_0, x2_1) (x3_0, x3_1) = MkTuple2X4 (mkX4 x0_0 x1_0 x2_0 x3_0) (mkX4 x0_1 x1_1 x2_1 x3_1)
  unpackX4 (MkTuple2X4 v0 v1) = case unpackX4 v0 of (x0_0, x1_0, x2_0, x3_0) -> case unpackX4 v1 of (x0_1, x1_1, x2_1, x3_1) -> ((x0_0, x0_1), (x1_0, x1_1), (x2_0, x2_1), (x3_0, x3_1))
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance (Broadcast X4 a0, Broadcast X4 a1) => Broadcast X4 (a0, a1) where
  broadcast (x0, x1) = MkTuple2X4 (broadcast x0) (broadcast x1)
  {-# INLINE broadcast #-}
instance (SelectableF X4 a0, SelectableF X4 a1) => SelectableF X4 (a0, a1) where
  selectF !cond (MkTuple2X4 x0 x1) (MkTuple2X4 y0 y1) = MkTuple2X4 (selectF cond x0 y0) (selectF cond x1 y1)
  {-# INLINE selectF #-}
data instance X4 (a0, a1, a2) = MkTuple3X4 !(X4 a0) !(X4 a1) !(X4 a2)
instance (PackX4 X4 a0, PackX4 X4 a1, PackX4 X4 a2) => PackX4 X4 (a0, a1, a2) where
  mkX4 (x0_0, x0_1, x0_2) (x1_0, x1_1, x1_2) (x2_0, x2_1, x2_2) (x3_0, x3_1, x3_2) = MkTuple3X4 (mkX4 x0_0 x1_0 x2_0 x3_0) (mkX4 x0_1 x1_1 x2_1 x3_1) (mkX4 x0_2 x1_2 x2_2 x3_2)
  unpackX4 (MkTuple3X4 v0 v1 v2) = case unpackX4 v0 of (x0_0, x1_0, x2_0, x3_0) -> case unpackX4 v1 of (x0_1, x1_1, x2_1, x3_1) -> case unpackX4 v2 of (x0_2, x1_2, x2_2, x3_2) -> ((x0_0, x0_1, x0_2), (x1_0, x1_1, x1_2), (x2_0, x2_1, x2_2), (x3_0, x3_1, x3_2))
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance (Broadcast X4 a0, Broadcast X4 a1, Broadcast X4 a2) => Broadcast X4 (a0, a1, a2) where
  broadcast (x0, x1, x2) = MkTuple3X4 (broadcast x0) (broadcast x1) (broadcast x2)
  {-# INLINE broadcast #-}
instance (SelectableF X4 a0, SelectableF X4 a1, SelectableF X4 a2) => SelectableF X4 (a0, a1, a2) where
  selectF !cond (MkTuple3X4 x0 x1 x2) (MkTuple3X4 y0 y1 y2) = MkTuple3X4 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2)
  {-# INLINE selectF #-}
data instance X4 (a0, a1, a2, a3) = MkTuple4X4 !(X4 a0) !(X4 a1) !(X4 a2) !(X4 a3)
instance (PackX4 X4 a0, PackX4 X4 a1, PackX4 X4 a2, PackX4 X4 a3) => PackX4 X4 (a0, a1, a2, a3) where
  mkX4 (x0_0, x0_1, x0_2, x0_3) (x1_0, x1_1, x1_2, x1_3) (x2_0, x2_1, x2_2, x2_3) (x3_0, x3_1, x3_2, x3_3) = MkTuple4X4 (mkX4 x0_0 x1_0 x2_0 x3_0) (mkX4 x0_1 x1_1 x2_1 x3_1) (mkX4 x0_2 x1_2 x2_2 x3_2) (mkX4 x0_3 x1_3 x2_3 x3_3)
  unpackX4 (MkTuple4X4 v0 v1 v2 v3) = case unpackX4 v0 of (x0_0, x1_0, x2_0, x3_0) -> case unpackX4 v1 of (x0_1, x1_1, x2_1, x3_1) -> case unpackX4 v2 of (x0_2, x1_2, x2_2, x3_2) -> case unpackX4 v3 of (x0_3, x1_3, x2_3, x3_3) -> ((x0_0, x0_1, x0_2, x0_3), (x1_0, x1_1, x1_2, x1_3), (x2_0, x2_1, x2_2, x2_3), (x3_0, x3_1, x3_2, x3_3))
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance (Broadcast X4 a0, Broadcast X4 a1, Broadcast X4 a2, Broadcast X4 a3) => Broadcast X4 (a0, a1, a2, a3) where
  broadcast (x0, x1, x2, x3) = MkTuple4X4 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3)
  {-# INLINE broadcast #-}
instance (SelectableF X4 a0, SelectableF X4 a1, SelectableF X4 a2, SelectableF X4 a3) => SelectableF X4 (a0, a1, a2, a3) where
  selectF !cond (MkTuple4X4 x0 x1 x2 x3) (MkTuple4X4 y0 y1 y2 y3) = MkTuple4X4 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3)
  {-# INLINE selectF #-}
data instance X4 (a0, a1, a2, a3, a4) = MkTuple5X4 !(X4 a0) !(X4 a1) !(X4 a2) !(X4 a3) !(X4 a4)
instance (PackX4 X4 a0, PackX4 X4 a1, PackX4 X4 a2, PackX4 X4 a3, PackX4 X4 a4) => PackX4 X4 (a0, a1, a2, a3, a4) where
  mkX4 (x0_0, x0_1, x0_2, x0_3, x0_4) (x1_0, x1_1, x1_2, x1_3, x1_4) (x2_0, x2_1, x2_2, x2_3, x2_4) (x3_0, x3_1, x3_2, x3_3, x3_4) = MkTuple5X4 (mkX4 x0_0 x1_0 x2_0 x3_0) (mkX4 x0_1 x1_1 x2_1 x3_1) (mkX4 x0_2 x1_2 x2_2 x3_2) (mkX4 x0_3 x1_3 x2_3 x3_3) (mkX4 x0_4 x1_4 x2_4 x3_4)
  unpackX4 (MkTuple5X4 v0 v1 v2 v3 v4) = case unpackX4 v0 of (x0_0, x1_0, x2_0, x3_0) -> case unpackX4 v1 of (x0_1, x1_1, x2_1, x3_1) -> case unpackX4 v2 of (x0_2, x1_2, x2_2, x3_2) -> case unpackX4 v3 of (x0_3, x1_3, x2_3, x3_3) -> case unpackX4 v4 of (x0_4, x1_4, x2_4, x3_4) -> ((x0_0, x0_1, x0_2, x0_3, x0_4), (x1_0, x1_1, x1_2, x1_3, x1_4), (x2_0, x2_1, x2_2, x2_3, x2_4), (x3_0, x3_1, x3_2, x3_3, x3_4))
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance (Broadcast X4 a0, Broadcast X4 a1, Broadcast X4 a2, Broadcast X4 a3, Broadcast X4 a4) => Broadcast X4 (a0, a1, a2, a3, a4) where
  broadcast (x0, x1, x2, x3, x4) = MkTuple5X4 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4)
  {-# INLINE broadcast #-}
instance (SelectableF X4 a0, SelectableF X4 a1, SelectableF X4 a2, SelectableF X4 a3, SelectableF X4 a4) => SelectableF X4 (a0, a1, a2, a3, a4) where
  selectF !cond (MkTuple5X4 x0 x1 x2 x3 x4) (MkTuple5X4 y0 y1 y2 y3 y4) = MkTuple5X4 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4)
  {-# INLINE selectF #-}
data instance X4 (a0, a1, a2, a3, a4, a5) = MkTuple6X4 !(X4 a0) !(X4 a1) !(X4 a2) !(X4 a3) !(X4 a4) !(X4 a5)
instance (PackX4 X4 a0, PackX4 X4 a1, PackX4 X4 a2, PackX4 X4 a3, PackX4 X4 a4, PackX4 X4 a5) => PackX4 X4 (a0, a1, a2, a3, a4, a5) where
  mkX4 (x0_0, x0_1, x0_2, x0_3, x0_4, x0_5) (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5) (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5) (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5) = MkTuple6X4 (mkX4 x0_0 x1_0 x2_0 x3_0) (mkX4 x0_1 x1_1 x2_1 x3_1) (mkX4 x0_2 x1_2 x2_2 x3_2) (mkX4 x0_3 x1_3 x2_3 x3_3) (mkX4 x0_4 x1_4 x2_4 x3_4) (mkX4 x0_5 x1_5 x2_5 x3_5)
  unpackX4 (MkTuple6X4 v0 v1 v2 v3 v4 v5) = case unpackX4 v0 of (x0_0, x1_0, x2_0, x3_0) -> case unpackX4 v1 of (x0_1, x1_1, x2_1, x3_1) -> case unpackX4 v2 of (x0_2, x1_2, x2_2, x3_2) -> case unpackX4 v3 of (x0_3, x1_3, x2_3, x3_3) -> case unpackX4 v4 of (x0_4, x1_4, x2_4, x3_4) -> case unpackX4 v5 of (x0_5, x1_5, x2_5, x3_5) -> ((x0_0, x0_1, x0_2, x0_3, x0_4, x0_5), (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5), (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5), (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5))
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance (Broadcast X4 a0, Broadcast X4 a1, Broadcast X4 a2, Broadcast X4 a3, Broadcast X4 a4, Broadcast X4 a5) => Broadcast X4 (a0, a1, a2, a3, a4, a5) where
  broadcast (x0, x1, x2, x3, x4, x5) = MkTuple6X4 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4) (broadcast x5)
  {-# INLINE broadcast #-}
instance (SelectableF X4 a0, SelectableF X4 a1, SelectableF X4 a2, SelectableF X4 a3, SelectableF X4 a4, SelectableF X4 a5) => SelectableF X4 (a0, a1, a2, a3, a4, a5) where
  selectF !cond (MkTuple6X4 x0 x1 x2 x3 x4 x5) (MkTuple6X4 y0 y1 y2 y3 y4 y5) = MkTuple6X4 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4) (selectF cond x5 y5)
  {-# INLINE selectF #-}
instance (PackX4 X4 a, PackX4 X4 b) => LiftSIMD X4 a b where
  liftSIMD f !v = case unpackX4 v of (x0, x1, x2, x3) -> mkX4 (f x0) (f x1) (f x2) (f x3)
  {-# INLINE liftSIMD #-}
instance (PackX4 X4 a, PackX4 X4 b, PackX4 X4 c) => LiftSIMD2 X4 a b c where
  liftSIMD2 f !u !v = case unpackX4 u of (x0, x1, x2, x3) -> case unpackX4 v of (y0, y1, y2, y3) -> mkX4 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X4 where
  mkTuple2 = MkTuple2X4
  mkTuple3 = MkTuple3X4
  mkTuple4 = MkTuple4X4
  mkTuple5 = MkTuple5X4
  mkTuple6 = MkTuple6X4
  deconstructTuple2 (MkTuple2X4 v0 v1) = (v0, v1)
  deconstructTuple3 (MkTuple3X4 v0 v1 v2) = (v0, v1, v2)
  deconstructTuple4 (MkTuple4X4 v0 v1 v2 v3) = (v0, v1, v2, v3)
  deconstructTuple5 (MkTuple5X4 v0 v1 v2 v3 v4) = (v0, v1, v2, v3, v4)
  deconstructTuple6 (MkTuple6X4 v0 v1 v2 v3 v4 v5) = (v0, v1, v2, v3, v4, v5)
  mkSum = coerce
  getSum' = coerce
  mkProduct = coerce
  getProduct' = coerce
  mkMin = coerce
  getMin' = coerce
  mkMax = coerce
  getMax' = coerce
  mkComplex = MkComplexX4
  deconstructComplex (MkComplexX4 x y) = (x, y)
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
deriving via WrappedMulti X4 a instance SelectableF X4 a => Selectable (X4 a)
deriving via WrappedMulti X4 a instance NumF X4 a => Num (X4 a)
deriving via WrappedMulti X4 a instance FractionalF X4 a => Fractional (X4 a)
deriving via WrappedMulti X4 a instance FloatingF X4 a => Floating (X4 a)
deriving via WrappedMulti X4 a instance BooleanF X4 a => Boolean (X4 a)
deriving via WrappedMulti X4 a instance BitShiftF X4 a => BitShift (X4 a)
deriving via WrappedMulti X4 a instance MinMaxF X4 a => MinMax (X4 a)
deriving via WrappedMulti X4 a instance FusedMultiplyAddF X4 a => FusedMultiplyAdd (X4 a)
