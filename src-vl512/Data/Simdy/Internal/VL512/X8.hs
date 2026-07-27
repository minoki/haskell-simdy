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
module Data.Simdy.Internal.VL512.X8 where
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
import           Data.Simdy.Internal.VL512.Prim
import           Data.Simdy.Internal.VL128.PrimExtra
import           Data.Simdy.Internal.VL256.PrimExtra
import           Data.Simdy.Internal.VL512.PrimExtra
import qualified GHC.Exts
import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..))
import           GHC.Int
import           GHC.IO
import           GHC.Word
import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=), min, max)
import qualified Prelude
-- | @'X8' a@ is a fixed-length vector of length 8.
--
-- Conceptually, @data 'X8' a = MkX8 !a !a !a !a !a !a !a !a@.
--
-- You can access the elements by 'mkX8', 'packX8' and 'unpackX8'.
data family X8 a
instance KnownSIMDLength X8 where
  type SIMDLength X8 = 8
  simdLength = 8
  {-# INLINE simdLength #-}
newtype instance X8 Bool = MkBoolX8 Word8
type instance Mask (X8 a) = X8 Bool
instance MaskIsLiftedBool X8 a
instance BooleanF X8 Bool where
  andF (MkBoolX8 x) (MkBoolX8 y) = MkBoolX8 (x .&. y)
  orF (MkBoolX8 x) (MkBoolX8 y) = MkBoolX8 (x .|. y)
  xorF (MkBoolX8 x) (MkBoolX8 y) = MkBoolX8 (xor x y)
  complementF (MkBoolX8 x) = MkBoolX8 (0xff - x)
deriving via WrappedMulti X8 a instance EquatableF X8 a => Equatable (X8 a)
deriving via WrappedMulti X8 a instance OrderedF X8 a => Ordered (X8 a)
instance PackX8 X8 a => IsList (X8 a) where
  type Item (X8 a) = a
  toList = toListX8
  fromList = fromListX8
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX8 X8 Bool where
  mkX8 !x0 !x1 !x2 !x3 !x4 !x5 !x6 !x7 = MkBoolX8 ((if x0 then 0x1 else 0) .|. (if x1 then 0x2 else 0) .|. (if x2 then 0x4 else 0) .|. (if x3 then 0x8 else 0) .|. (if x4 then 0x10 else 0) .|. (if x5 then 0x20 else 0) .|. (if x6 then 0x40 else 0) .|. (if x7 then 0x80 else 0))
  unpackX8 (MkBoolX8 !x) = (testBit x 0, testBit x 1, testBit x 2, testBit x 3, testBit x 4, testBit x 5, testBit x 6, testBit x 7)
instance Broadcast X8 Bool where
  broadcast False = MkBoolX8 0
  broadcast True = MkBoolX8 0xff
  {-# INLINE broadcast #-}
instance SelectableF X8 Bool where
  selectF (MkBoolX8 !cond) (MkBoolX8 !x) (MkBoolX8 !y) = MkBoolX8 ((cond .&. x) .|. (complement cond .&. y))
  {-# INLINE selectF #-}
instance BooleanReduction X8 where
  horizontalAndBool (MkBoolX8 !cond) = cond Prelude.== 0xff
  horizontalOrBool (MkBoolX8 !cond) = cond Prelude./= 0
  {-# INLINE horizontalAndBool #-}
  {-# INLINE horizontalOrBool #-}
instance UnaryShuffleT indices X8 Bool where
  unaryShuffle = error "not implemented yet"
  {-# NOINLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 Bool where
  binaryShuffle = error "not implemented yet"
  {-# NOINLINE binaryShuffle #-}
data instance X8 Float = MkFloatX8 FloatX8#
instance PackX8 X8 Float where
  mkX8 (F# x0) (F# x1) (F# x2) (F# x3) (F# x4) (F# x5) (F# x6) (F# x7) = MkFloatX8 (packFloatX8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))
  unpackX8 (MkFloatX8 v0) = case unpackFloatX8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (F# x0, F# x1, F# x2, F# x3, F# x4, F# x5, F# x6, F# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Float where
  broadcast (F# x) = MkFloatX8 (broadcastFloatX8# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Float where
  selectF (MkBoolX8 !cond) (MkFloatX8 x0) (MkFloatX8 y0) = MkFloatX8 (selectFloatX8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany FloatX8# indices) => UnaryShuffleT indices X8 Float where
  unaryShuffle (MkFloatX8 x) = MkFloatX8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany FloatX8# indices) => BinaryShuffleT indices X8 Float where
  binaryShuffle (MkFloatX8 x0) (MkFloatX8 x1) = MkFloatX8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Float where
  eqF (MkFloatX8 u0) (MkFloatX8 v0) = MkBoolX8 $ (fromIntegral (eqFloatX8# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X8 Float where
  ltF (MkFloatX8 u0) (MkFloatX8 v0) = MkBoolX8 $ (fromIntegral (ltFloatX8# u0 v0))
  leF (MkFloatX8 u0) (MkFloatX8 v0) = MkBoolX8 $ (fromIntegral (leFloatX8# u0 v0))
  gtF (MkFloatX8 u0) (MkFloatX8 v0) = MkBoolX8 $ (fromIntegral (gtFloatX8# u0 v0))
  geF (MkFloatX8 u0) (MkFloatX8 v0) = MkBoolX8 $ (fromIntegral (geFloatX8# u0 v0))
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Float where
  minF (MkFloatX8 u0) (MkFloatX8 v0) = MkFloatX8 (minimumFloatX8# u0 v0)
  maxF (MkFloatX8 u0) (MkFloatX8 v0) = MkFloatX8 (maximumFloatX8# u0 v0)
  minimumNumberF (MkFloatX8 u0) (MkFloatX8 v0) = MkFloatX8 (minimumNumberFloatX8# u0 v0)
  maximumNumberF (MkFloatX8 u0) (MkFloatX8 v0) = MkFloatX8 (maximumNumberFloatX8# u0 v0)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX8Float :: X8 Float -> X8 Float
negateX8Float (MkFloatX8 u0) = MkFloatX8 (negateFloatX8# u0)
#if defined(USE_FMA)
{-# INLINE [0] negateX8Float #-}
#else
{-# INLINE negateX8Float #-}
#endif
instance NumF X8 Float where
  plusF (MkFloatX8 u0) (MkFloatX8 v0) = MkFloatX8 (plusFloatX8# u0 v0)
  minusF (MkFloatX8 u0) (MkFloatX8 v0) = MkFloatX8 (minusFloatX8# u0 v0)
  timesF (MkFloatX8 u0) (MkFloatX8 v0) = MkFloatX8 (timesFloatX8# u0 v0)
  negateF = negateX8Float
  absF (MkFloatX8 u0) = MkFloatX8 (absFloatX8# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X8 Float where
  divideF (MkFloatX8 u0) (MkFloatX8 v0) = MkFloatX8 (divideFloatX8# u0 v0)
  {-# INLINE divideF #-}
instance FloatingF X8 Float where
  sqrtF (MkFloatX8 u0) = MkFloatX8 (sqrtFloatX8# u0)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X8 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX8 u0) (MkFloatX8 v0) (MkFloatX8 w0) = MkFloatX8 (fmaddFloatX8# u0 v0 w0)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X8 Float" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X8 Float
"Fusible/*-/X8 Float" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX8Float c) :: X8 Float
"Fusible/-*+/X8 Float" forall a b c.
  negateX8Float (a F.* b) F.+ c = fusedMultiplyAdd (negateX8Float a) b c :: X8 Float
"Fusible/-*-/X8 Float" forall a b c.
  negateX8Float (a F.* b) F.- c = fusedMultiplyAdd (negateX8Float a) b (negateX8Float c) :: X8 Float
"Fusible/+*/X8 Float" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X8 Float
"Fusible/-*/X8 Float" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX8Float b) c a :: X8 Float
  #-}
#endif
instance EnumFromZero_ X8 Float where
  enumFromZero = MkFloatX8 (packFloatX8# (# 0.0#, 1.0#, 2.0#, 3.0#, 4.0#, 5.0#, 6.0#, 7.0# #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Float where
  indexByteArraySIMD# ba i = MkFloatX8 (indexFloatArrayAsFloatX8# ba i)
  readByteArraySIMD# mba i s0 = case readFloatArrayAsFloatX8# mba i s0 of (# s1, v0 #) -> (# s1, MkFloatX8 v0 #)
  writeByteArraySIMD# mba i (MkFloatX8 v0) s0 = writeFloatArrayAsFloatX8# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readFloatOffAddrAsFloatX8# addr i s0 of (# s1, v0 #) -> (# s1, MkFloatX8 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX8 v0) = IO (\s0 -> case writeFloatOffAddrAsFloatX8# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Double = MkDoubleX8 DoubleX8#
instance PackX8 X8 Double where
  mkX8 (D# x0) (D# x1) (D# x2) (D# x3) (D# x4) (D# x5) (D# x6) (D# x7) = MkDoubleX8 (packDoubleX8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))
  unpackX8 (MkDoubleX8 v0) = case unpackDoubleX8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (D# x0, D# x1, D# x2, D# x3, D# x4, D# x5, D# x6, D# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Double where
  broadcast (D# x) = MkDoubleX8 (broadcastDoubleX8# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Double where
  selectF (MkBoolX8 !cond) (MkDoubleX8 x0) (MkDoubleX8 y0) = MkDoubleX8 (selectDoubleX8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany DoubleX8# indices) => UnaryShuffleT indices X8 Double where
  unaryShuffle (MkDoubleX8 x) = MkDoubleX8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany DoubleX8# indices) => BinaryShuffleT indices X8 Double where
  binaryShuffle (MkDoubleX8 x0) (MkDoubleX8 x1) = MkDoubleX8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Double where
  eqF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkBoolX8 $ (fromIntegral (eqDoubleX8# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X8 Double where
  ltF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkBoolX8 $ (fromIntegral (ltDoubleX8# u0 v0))
  leF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkBoolX8 $ (fromIntegral (leDoubleX8# u0 v0))
  gtF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkBoolX8 $ (fromIntegral (gtDoubleX8# u0 v0))
  geF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkBoolX8 $ (fromIntegral (geDoubleX8# u0 v0))
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Double where
  minF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkDoubleX8 (minimumDoubleX8# u0 v0)
  maxF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkDoubleX8 (maximumDoubleX8# u0 v0)
  minimumNumberF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkDoubleX8 (minimumNumberDoubleX8# u0 v0)
  maximumNumberF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkDoubleX8 (maximumNumberDoubleX8# u0 v0)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX8Double :: X8 Double -> X8 Double
negateX8Double (MkDoubleX8 u0) = MkDoubleX8 (negateDoubleX8# u0)
#if defined(USE_FMA)
{-# INLINE [0] negateX8Double #-}
#else
{-# INLINE negateX8Double #-}
#endif
instance NumF X8 Double where
  plusF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkDoubleX8 (plusDoubleX8# u0 v0)
  minusF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkDoubleX8 (minusDoubleX8# u0 v0)
  timesF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkDoubleX8 (timesDoubleX8# u0 v0)
  negateF = negateX8Double
  absF (MkDoubleX8 u0) = MkDoubleX8 (absDoubleX8# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X8 Double where
  divideF (MkDoubleX8 u0) (MkDoubleX8 v0) = MkDoubleX8 (divideDoubleX8# u0 v0)
  {-# INLINE divideF #-}
instance FloatingF X8 Double where
  sqrtF (MkDoubleX8 u0) = MkDoubleX8 (sqrtDoubleX8# u0)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X8 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX8 u0) (MkDoubleX8 v0) (MkDoubleX8 w0) = MkDoubleX8 (fmaddDoubleX8# u0 v0 w0)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X8 Double" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X8 Double
"Fusible/*-/X8 Double" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX8Double c) :: X8 Double
"Fusible/-*+/X8 Double" forall a b c.
  negateX8Double (a F.* b) F.+ c = fusedMultiplyAdd (negateX8Double a) b c :: X8 Double
"Fusible/-*-/X8 Double" forall a b c.
  negateX8Double (a F.* b) F.- c = fusedMultiplyAdd (negateX8Double a) b (negateX8Double c) :: X8 Double
"Fusible/+*/X8 Double" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X8 Double
"Fusible/-*/X8 Double" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX8Double b) c a :: X8 Double
  #-}
#endif
instance EnumFromZero_ X8 Double where
  enumFromZero = MkDoubleX8 (packDoubleX8# (# 0.0##, 1.0##, 2.0##, 3.0##, 4.0##, 5.0##, 6.0##, 7.0## #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Double where
  indexByteArraySIMD# ba i = MkDoubleX8 (indexDoubleArrayAsDoubleX8# ba i)
  readByteArraySIMD# mba i s0 = case readDoubleArrayAsDoubleX8# mba i s0 of (# s1, v0 #) -> (# s1, MkDoubleX8 v0 #)
  writeByteArraySIMD# mba i (MkDoubleX8 v0) s0 = writeDoubleArrayAsDoubleX8# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readDoubleOffAddrAsDoubleX8# addr i s0 of (# s1, v0 #) -> (# s1, MkDoubleX8 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX8 v0) = IO (\s0 -> case writeDoubleOffAddrAsDoubleX8# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
instance ImplementationDescription X8 where
  implementationDescription _ = "X8;maxBits=512"
data instance X8 Int8 = MkInt8X8 Int8X16#
instance PackX8 X8 Int8 where
  mkX8 (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) = MkInt8X8 (packInt8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  unpackX8 (MkInt8X8 v0) = case unpackInt8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> (I8# x0, I8# x1, I8# x2, I8# x3, I8# x4, I8# x5, I8# x6, I8# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int8 where
  broadcast (I8# x) = MkInt8X8 (broadcastInt8X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Int8 where
  selectF (MkBoolX8 !cond) (MkInt8X8 x0) (MkInt8X8 y0) = MkInt8X8 (selectInt8X16# (fromIntegral cond) x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Int8X16# indices) => UnaryShuffleT indices X8 Int8 where
  unaryShuffle (MkInt8X8 x) = MkInt8X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Int8X16# indices) => BinaryShuffleT indices X8 Int8 where
  binaryShuffle (MkInt8X8 x0) (MkInt8X8 x1) = MkInt8X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int8 where
  eqF (MkInt8X8 u0) (MkInt8X8 v0) = MkBoolX8 $ (fromIntegral (eqInt8X16# u0 v0 .&. 0xff))
  {-# INLINE eqF #-}
instance OrderedF X8 Int8 where
  ltF (MkInt8X8 u0) (MkInt8X8 v0) = MkBoolX8 $ (fromIntegral (ltInt8X16# u0 v0 .&. 0xff))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int8 where
  minF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (minInt8X16# u0 v0)
  maxF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (maxInt8X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int8 where
  plusF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (plusInt8X16# u0 v0)
  minusF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (minusInt8X16# u0 v0)
  timesF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (timesInt8X16# u0 v0)
  negateF (MkInt8X8 u0) = MkInt8X8 (negateInt8X16# u0)
  absF (MkInt8X8 u0) = MkInt8X8 (absInt8X16# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Int8 where
  andF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (andInt8X16# u0 v0)
  orF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (orInt8X16# u0 v0)
  xorF (MkInt8X8 u0) (MkInt8X8 v0) = MkInt8X8 (xorInt8X16# u0 v0)
  complementF (MkInt8X8 u0) = MkInt8X8 (complementInt8X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int8 where
  shiftLF (MkInt8X8 u0) (I# i) = MkInt8X8 (shiftLInt8X16# u0 i)
  shiftRF (MkInt8X8 u0) (I# i) = MkInt8X8 (shiftRInt8X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Int8 where
  enumFromZero = MkInt8X8 (packInt8X16# (# 0#Int8, 1#Int8, 2#Int8, 3#Int8, 4#Int8, 5#Int8, 6#Int8, 7#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int8 where
  indexByteArraySIMD# ba i = MkInt8X8 (packInt8X16# (# GHC.Exts.indexInt8Array# ba i, GHC.Exts.indexInt8Array# ba (i +# 1#), GHC.Exts.indexInt8Array# ba (i +# 2#), GHC.Exts.indexInt8Array# ba (i +# 3#), GHC.Exts.indexInt8Array# ba (i +# 4#), GHC.Exts.indexInt8Array# ba (i +# 5#), GHC.Exts.indexInt8Array# ba (i +# 6#), GHC.Exts.indexInt8Array# ba (i +# 7#), 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt8Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt8Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt8Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt8Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt8X8 (packInt8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #)) #)
  writeByteArraySIMD# mba i (MkInt8X8 v0) s0 = case unpackInt8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeInt8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt8Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt8Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt8Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeInt8Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt8X8 (packInt8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X8 v0) = IO (\s0 -> case unpackInt8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeInt8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 7#) x7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int16 = MkInt16X8 Int16X8#
instance PackX8 X8 Int16 where
  mkX8 (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) = MkInt16X8 (packInt16X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))
  unpackX8 (MkInt16X8 v0) = case unpackInt16X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (I16# x0, I16# x1, I16# x2, I16# x3, I16# x4, I16# x5, I16# x6, I16# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int16 where
  broadcast (I16# x) = MkInt16X8 (broadcastInt16X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Int16 where
  selectF (MkBoolX8 !cond) (MkInt16X8 x0) (MkInt16X8 y0) = MkInt16X8 (selectInt16X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Int16X8# indices) => UnaryShuffleT indices X8 Int16 where
  unaryShuffle (MkInt16X8 x) = MkInt16X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Int16X8# indices) => BinaryShuffleT indices X8 Int16 where
  binaryShuffle (MkInt16X8 x0) (MkInt16X8 x1) = MkInt16X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int16 where
  eqF (MkInt16X8 u0) (MkInt16X8 v0) = MkBoolX8 $ (fromIntegral (eqInt16X8# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X8 Int16 where
  ltF (MkInt16X8 u0) (MkInt16X8 v0) = MkBoolX8 $ (fromIntegral (ltInt16X8# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int16 where
  minF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (minInt16X8# u0 v0)
  maxF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (maxInt16X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int16 where
  plusF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (plusInt16X8# u0 v0)
  minusF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (minusInt16X8# u0 v0)
  timesF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (timesInt16X8# u0 v0)
  negateF (MkInt16X8 u0) = MkInt16X8 (negateInt16X8# u0)
  absF (MkInt16X8 u0) = MkInt16X8 (absInt16X8# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Int16 where
  andF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (andInt16X8# u0 v0)
  orF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (orInt16X8# u0 v0)
  xorF (MkInt16X8 u0) (MkInt16X8 v0) = MkInt16X8 (xorInt16X8# u0 v0)
  complementF (MkInt16X8 u0) = MkInt16X8 (complementInt16X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int16 where
  shiftLF (MkInt16X8 u0) (I# i) = MkInt16X8 (shiftLInt16X8# u0 i)
  shiftRF (MkInt16X8 u0) (I# i) = MkInt16X8 (shiftRInt16X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Int16 where
  enumFromZero = MkInt16X8 (packInt16X8# (# 0#Int16, 1#Int16, 2#Int16, 3#Int16, 4#Int16, 5#Int16, 6#Int16, 7#Int16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int16 where
  indexByteArraySIMD# ba i = MkInt16X8 (indexInt16ArrayAsInt16X8# ba i)
  readByteArraySIMD# mba i s0 = case readInt16ArrayAsInt16X8# mba i s0 of (# s1, v0 #) -> (# s1, MkInt16X8 v0 #)
  writeByteArraySIMD# mba i (MkInt16X8 v0) s0 = writeInt16ArrayAsInt16X8# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt16OffAddrAsInt16X8# addr i s0 of (# s1, v0 #) -> (# s1, MkInt16X8 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X8 v0) = IO (\s0 -> case writeInt16OffAddrAsInt16X8# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int32 = MkInt32X8 Int32X8#
instance PackX8 X8 Int32 where
  mkX8 (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) = MkInt32X8 (packInt32X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))
  unpackX8 (MkInt32X8 v0) = case unpackInt32X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (I32# x0, I32# x1, I32# x2, I32# x3, I32# x4, I32# x5, I32# x6, I32# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int32 where
  broadcast (I32# x) = MkInt32X8 (broadcastInt32X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Int32 where
  selectF (MkBoolX8 !cond) (MkInt32X8 x0) (MkInt32X8 y0) = MkInt32X8 (selectInt32X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Int32X8# indices) => UnaryShuffleT indices X8 Int32 where
  unaryShuffle (MkInt32X8 x) = MkInt32X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Int32X8# indices) => BinaryShuffleT indices X8 Int32 where
  binaryShuffle (MkInt32X8 x0) (MkInt32X8 x1) = MkInt32X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int32 where
  eqF (MkInt32X8 u0) (MkInt32X8 v0) = MkBoolX8 $ (fromIntegral (eqInt32X8# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X8 Int32 where
  ltF (MkInt32X8 u0) (MkInt32X8 v0) = MkBoolX8 $ (fromIntegral (ltInt32X8# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int32 where
  minF (MkInt32X8 u0) (MkInt32X8 v0) = MkInt32X8 (minInt32X8# u0 v0)
  maxF (MkInt32X8 u0) (MkInt32X8 v0) = MkInt32X8 (maxInt32X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int32 where
  plusF (MkInt32X8 u0) (MkInt32X8 v0) = MkInt32X8 (plusInt32X8# u0 v0)
  minusF (MkInt32X8 u0) (MkInt32X8 v0) = MkInt32X8 (minusInt32X8# u0 v0)
  timesF (MkInt32X8 u0) (MkInt32X8 v0) = MkInt32X8 (timesInt32X8# u0 v0)
  negateF (MkInt32X8 u0) = MkInt32X8 (negateInt32X8# u0)
  absF (MkInt32X8 u0) = MkInt32X8 (absInt32X8# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Int32 where
  andF (MkInt32X8 u0) (MkInt32X8 v0) = MkInt32X8 (andInt32X8# u0 v0)
  orF (MkInt32X8 u0) (MkInt32X8 v0) = MkInt32X8 (orInt32X8# u0 v0)
  xorF (MkInt32X8 u0) (MkInt32X8 v0) = MkInt32X8 (xorInt32X8# u0 v0)
  complementF (MkInt32X8 u0) = MkInt32X8 (complementInt32X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int32 where
  shiftLF (MkInt32X8 u0) (I# i) = MkInt32X8 (shiftLInt32X8# u0 i)
  shiftRF (MkInt32X8 u0) (I# i) = MkInt32X8 (shiftRInt32X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Int32 where
  enumFromZero = MkInt32X8 (packInt32X8# (# 0#Int32, 1#Int32, 2#Int32, 3#Int32, 4#Int32, 5#Int32, 6#Int32, 7#Int32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int32 where
  indexByteArraySIMD# ba i = MkInt32X8 (indexInt32ArrayAsInt32X8# ba i)
  readByteArraySIMD# mba i s0 = case readInt32ArrayAsInt32X8# mba i s0 of (# s1, v0 #) -> (# s1, MkInt32X8 v0 #)
  writeByteArraySIMD# mba i (MkInt32X8 v0) s0 = writeInt32ArrayAsInt32X8# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt32OffAddrAsInt32X8# addr i s0 of (# s1, v0 #) -> (# s1, MkInt32X8 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X8 v0) = IO (\s0 -> case writeInt32OffAddrAsInt32X8# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int64 = MkInt64X8 Int64X8#
instance PackX8 X8 Int64 where
  mkX8 (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) = MkInt64X8 (packInt64X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))
  unpackX8 (MkInt64X8 v0) = case unpackInt64X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (I64# x0, I64# x1, I64# x2, I64# x3, I64# x4, I64# x5, I64# x6, I64# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int64 where
  broadcast (I64# x) = MkInt64X8 (broadcastInt64X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Int64 where
  selectF (MkBoolX8 !cond) (MkInt64X8 x0) (MkInt64X8 y0) = MkInt64X8 (selectInt64X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Int64X8# indices) => UnaryShuffleT indices X8 Int64 where
  unaryShuffle (MkInt64X8 x) = MkInt64X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Int64X8# indices) => BinaryShuffleT indices X8 Int64 where
  binaryShuffle (MkInt64X8 x0) (MkInt64X8 x1) = MkInt64X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int64 where
  eqF (MkInt64X8 u0) (MkInt64X8 v0) = MkBoolX8 $ (fromIntegral (eqInt64X8# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X8 Int64 where
  ltF (MkInt64X8 u0) (MkInt64X8 v0) = MkBoolX8 $ (fromIntegral (ltInt64X8# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int64 where
  minF (MkInt64X8 u0) (MkInt64X8 v0) = MkInt64X8 (minInt64X8# u0 v0)
  maxF (MkInt64X8 u0) (MkInt64X8 v0) = MkInt64X8 (maxInt64X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int64 where
  plusF (MkInt64X8 u0) (MkInt64X8 v0) = MkInt64X8 (plusInt64X8# u0 v0)
  minusF (MkInt64X8 u0) (MkInt64X8 v0) = MkInt64X8 (minusInt64X8# u0 v0)
  timesF (MkInt64X8 u0) (MkInt64X8 v0) = MkInt64X8 (timesInt64X8# u0 v0)
  negateF (MkInt64X8 u0) = MkInt64X8 (negateInt64X8# u0)
  absF (MkInt64X8 u0) = MkInt64X8 (absInt64X8# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Int64 where
  andF (MkInt64X8 u0) (MkInt64X8 v0) = MkInt64X8 (andInt64X8# u0 v0)
  orF (MkInt64X8 u0) (MkInt64X8 v0) = MkInt64X8 (orInt64X8# u0 v0)
  xorF (MkInt64X8 u0) (MkInt64X8 v0) = MkInt64X8 (xorInt64X8# u0 v0)
  complementF (MkInt64X8 u0) = MkInt64X8 (complementInt64X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int64 where
  shiftLF (MkInt64X8 u0) (I# i) = MkInt64X8 (shiftLInt64X8# u0 i)
  shiftRF (MkInt64X8 u0) (I# i) = MkInt64X8 (shiftRInt64X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Int64 where
  enumFromZero = MkInt64X8 (packInt64X8# (# 0#Int64, 1#Int64, 2#Int64, 3#Int64, 4#Int64, 5#Int64, 6#Int64, 7#Int64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int64 where
  indexByteArraySIMD# ba i = MkInt64X8 (indexInt64ArrayAsInt64X8# ba i)
  readByteArraySIMD# mba i s0 = case readInt64ArrayAsInt64X8# mba i s0 of (# s1, v0 #) -> (# s1, MkInt64X8 v0 #)
  writeByteArraySIMD# mba i (MkInt64X8 v0) s0 = writeInt64ArrayAsInt64X8# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt64OffAddrAsInt64X8# addr i s0 of (# s1, v0 #) -> (# s1, MkInt64X8 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X8 v0) = IO (\s0 -> case writeInt64OffAddrAsInt64X8# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word8 = MkWord8X8 Word8X16#
instance PackX8 X8 Word8 where
  mkX8 (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) = MkWord8X8 (packWord8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  unpackX8 (MkWord8X8 v0) = case unpackWord8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> (W8# x0, W8# x1, W8# x2, W8# x3, W8# x4, W8# x5, W8# x6, W8# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word8 where
  broadcast (W8# x) = MkWord8X8 (broadcastWord8X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Word8 where
  selectF (MkBoolX8 !cond) (MkWord8X8 x0) (MkWord8X8 y0) = MkWord8X8 (selectWord8X16# (fromIntegral cond) x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Word8X16# indices) => UnaryShuffleT indices X8 Word8 where
  unaryShuffle (MkWord8X8 x) = MkWord8X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Word8X16# indices) => BinaryShuffleT indices X8 Word8 where
  binaryShuffle (MkWord8X8 x0) (MkWord8X8 x1) = MkWord8X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word8 where
  eqF (MkWord8X8 u0) (MkWord8X8 v0) = MkBoolX8 $ (fromIntegral (eqWord8X16# u0 v0 .&. 0xff))
  {-# INLINE eqF #-}
instance OrderedF X8 Word8 where
  ltF (MkWord8X8 u0) (MkWord8X8 v0) = MkBoolX8 $ (fromIntegral (ltWord8X16# u0 v0 .&. 0xff))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word8 where
  minF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (minWord8X16# u0 v0)
  maxF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (maxWord8X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word8 where
  plusF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (plusWord8X16# u0 v0)
  minusF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (minusWord8X16# u0 v0)
  timesF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (timesWord8X16# u0 v0)
  -- Currently, there is no negateWord8X16#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Word8 where
  andF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (andWord8X16# u0 v0)
  orF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (orWord8X16# u0 v0)
  xorF (MkWord8X8 u0) (MkWord8X8 v0) = MkWord8X8 (xorWord8X16# u0 v0)
  complementF (MkWord8X8 u0) = MkWord8X8 (complementWord8X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word8 where
  shiftLF (MkWord8X8 u0) (I# i) = MkWord8X8 (shiftLWord8X16# u0 i)
  shiftRF (MkWord8X8 u0) (I# i) = MkWord8X8 (shiftRWord8X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Word8 where
  enumFromZero = MkWord8X8 (packWord8X16# (# 0#Word8, 1#Word8, 2#Word8, 3#Word8, 4#Word8, 5#Word8, 6#Word8, 7#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word8 where
  indexByteArraySIMD# ba i = MkWord8X8 (packWord8X16# (# GHC.Exts.indexWord8Array# ba i, GHC.Exts.indexWord8Array# ba (i +# 1#), GHC.Exts.indexWord8Array# ba (i +# 2#), GHC.Exts.indexWord8Array# ba (i +# 3#), GHC.Exts.indexWord8Array# ba (i +# 4#), GHC.Exts.indexWord8Array# ba (i +# 5#), GHC.Exts.indexWord8Array# ba (i +# 6#), GHC.Exts.indexWord8Array# ba (i +# 7#), 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord8Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord8Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord8Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord8Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord8X8 (packWord8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #)) #)
  writeByteArraySIMD# mba i (MkWord8X8 v0) s0 = case unpackWord8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeWord8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord8Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord8Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord8Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeWord8Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord8X8 (packWord8X16# (# x0, x1, x2, x3, x4, x5, x6, x7, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X8 v0) = IO (\s0 -> case unpackWord8X16# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeWord8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 6#) x6 s6 of s7 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 7#) x7 s7 of s8 -> (# s8, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word16 = MkWord16X8 Word16X8#
instance PackX8 X8 Word16 where
  mkX8 (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) = MkWord16X8 (packWord16X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))
  unpackX8 (MkWord16X8 v0) = case unpackWord16X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (W16# x0, W16# x1, W16# x2, W16# x3, W16# x4, W16# x5, W16# x6, W16# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word16 where
  broadcast (W16# x) = MkWord16X8 (broadcastWord16X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Word16 where
  selectF (MkBoolX8 !cond) (MkWord16X8 x0) (MkWord16X8 y0) = MkWord16X8 (selectWord16X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Word16X8# indices) => UnaryShuffleT indices X8 Word16 where
  unaryShuffle (MkWord16X8 x) = MkWord16X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Word16X8# indices) => BinaryShuffleT indices X8 Word16 where
  binaryShuffle (MkWord16X8 x0) (MkWord16X8 x1) = MkWord16X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word16 where
  eqF (MkWord16X8 u0) (MkWord16X8 v0) = MkBoolX8 $ (fromIntegral (eqWord16X8# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X8 Word16 where
  ltF (MkWord16X8 u0) (MkWord16X8 v0) = MkBoolX8 $ (fromIntegral (ltWord16X8# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word16 where
  minF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (minWord16X8# u0 v0)
  maxF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (maxWord16X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word16 where
  plusF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (plusWord16X8# u0 v0)
  minusF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (minusWord16X8# u0 v0)
  timesF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (timesWord16X8# u0 v0)
  -- Currently, there is no negateWord16X8#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Word16 where
  andF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (andWord16X8# u0 v0)
  orF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (orWord16X8# u0 v0)
  xorF (MkWord16X8 u0) (MkWord16X8 v0) = MkWord16X8 (xorWord16X8# u0 v0)
  complementF (MkWord16X8 u0) = MkWord16X8 (complementWord16X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word16 where
  shiftLF (MkWord16X8 u0) (I# i) = MkWord16X8 (shiftLWord16X8# u0 i)
  shiftRF (MkWord16X8 u0) (I# i) = MkWord16X8 (shiftRWord16X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Word16 where
  enumFromZero = MkWord16X8 (packWord16X8# (# 0#Word16, 1#Word16, 2#Word16, 3#Word16, 4#Word16, 5#Word16, 6#Word16, 7#Word16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word16 where
  indexByteArraySIMD# ba i = MkWord16X8 (indexWord16ArrayAsWord16X8# ba i)
  readByteArraySIMD# mba i s0 = case readWord16ArrayAsWord16X8# mba i s0 of (# s1, v0 #) -> (# s1, MkWord16X8 v0 #)
  writeByteArraySIMD# mba i (MkWord16X8 v0) s0 = writeWord16ArrayAsWord16X8# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord16OffAddrAsWord16X8# addr i s0 of (# s1, v0 #) -> (# s1, MkWord16X8 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X8 v0) = IO (\s0 -> case writeWord16OffAddrAsWord16X8# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word32 = MkWord32X8 Word32X8#
instance PackX8 X8 Word32 where
  mkX8 (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) = MkWord32X8 (packWord32X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))
  unpackX8 (MkWord32X8 v0) = case unpackWord32X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (W32# x0, W32# x1, W32# x2, W32# x3, W32# x4, W32# x5, W32# x6, W32# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word32 where
  broadcast (W32# x) = MkWord32X8 (broadcastWord32X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Word32 where
  selectF (MkBoolX8 !cond) (MkWord32X8 x0) (MkWord32X8 y0) = MkWord32X8 (selectWord32X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Word32X8# indices) => UnaryShuffleT indices X8 Word32 where
  unaryShuffle (MkWord32X8 x) = MkWord32X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Word32X8# indices) => BinaryShuffleT indices X8 Word32 where
  binaryShuffle (MkWord32X8 x0) (MkWord32X8 x1) = MkWord32X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word32 where
  eqF (MkWord32X8 u0) (MkWord32X8 v0) = MkBoolX8 $ (fromIntegral (eqWord32X8# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X8 Word32 where
  ltF (MkWord32X8 u0) (MkWord32X8 v0) = MkBoolX8 $ (fromIntegral (ltWord32X8# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word32 where
  minF (MkWord32X8 u0) (MkWord32X8 v0) = MkWord32X8 (minWord32X8# u0 v0)
  maxF (MkWord32X8 u0) (MkWord32X8 v0) = MkWord32X8 (maxWord32X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word32 where
  plusF (MkWord32X8 u0) (MkWord32X8 v0) = MkWord32X8 (plusWord32X8# u0 v0)
  minusF (MkWord32X8 u0) (MkWord32X8 v0) = MkWord32X8 (minusWord32X8# u0 v0)
  timesF (MkWord32X8 u0) (MkWord32X8 v0) = MkWord32X8 (timesWord32X8# u0 v0)
  -- Currently, there is no negateWord32X8#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Word32 where
  andF (MkWord32X8 u0) (MkWord32X8 v0) = MkWord32X8 (andWord32X8# u0 v0)
  orF (MkWord32X8 u0) (MkWord32X8 v0) = MkWord32X8 (orWord32X8# u0 v0)
  xorF (MkWord32X8 u0) (MkWord32X8 v0) = MkWord32X8 (xorWord32X8# u0 v0)
  complementF (MkWord32X8 u0) = MkWord32X8 (complementWord32X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word32 where
  shiftLF (MkWord32X8 u0) (I# i) = MkWord32X8 (shiftLWord32X8# u0 i)
  shiftRF (MkWord32X8 u0) (I# i) = MkWord32X8 (shiftRWord32X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Word32 where
  enumFromZero = MkWord32X8 (packWord32X8# (# 0#Word32, 1#Word32, 2#Word32, 3#Word32, 4#Word32, 5#Word32, 6#Word32, 7#Word32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word32 where
  indexByteArraySIMD# ba i = MkWord32X8 (indexWord32ArrayAsWord32X8# ba i)
  readByteArraySIMD# mba i s0 = case readWord32ArrayAsWord32X8# mba i s0 of (# s1, v0 #) -> (# s1, MkWord32X8 v0 #)
  writeByteArraySIMD# mba i (MkWord32X8 v0) s0 = writeWord32ArrayAsWord32X8# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord32OffAddrAsWord32X8# addr i s0 of (# s1, v0 #) -> (# s1, MkWord32X8 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X8 v0) = IO (\s0 -> case writeWord32OffAddrAsWord32X8# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word64 = MkWord64X8 Word64X8#
instance PackX8 X8 Word64 where
  mkX8 (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) = MkWord64X8 (packWord64X8# (# x0, x1, x2, x3, x4, x5, x6, x7 #))
  unpackX8 (MkWord64X8 v0) = case unpackWord64X8# v0 of (# x0, x1, x2, x3, x4, x5, x6, x7 #) -> (W64# x0, W64# x1, W64# x2, W64# x3, W64# x4, W64# x5, W64# x6, W64# x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word64 where
  broadcast (W64# x) = MkWord64X8 (broadcastWord64X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X8 Word64 where
  selectF (MkBoolX8 !cond) (MkWord64X8 x0) (MkWord64X8 y0) = MkWord64X8 (selectWord64X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 8, ShuffleMany Word64X8# indices) => UnaryShuffleT indices X8 Word64 where
  unaryShuffle (MkWord64X8 x) = MkWord64X8 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 16, ShuffleMany Word64X8# indices) => BinaryShuffleT indices X8 Word64 where
  binaryShuffle (MkWord64X8 x0) (MkWord64X8 x1) = MkWord64X8 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word64 where
  eqF (MkWord64X8 u0) (MkWord64X8 v0) = MkBoolX8 $ (fromIntegral (eqWord64X8# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X8 Word64 where
  ltF (MkWord64X8 u0) (MkWord64X8 v0) = MkBoolX8 $ (fromIntegral (ltWord64X8# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word64 where
  minF (MkWord64X8 u0) (MkWord64X8 v0) = MkWord64X8 (minWord64X8# u0 v0)
  maxF (MkWord64X8 u0) (MkWord64X8 v0) = MkWord64X8 (maxWord64X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word64 where
  plusF (MkWord64X8 u0) (MkWord64X8 v0) = MkWord64X8 (plusWord64X8# u0 v0)
  minusF (MkWord64X8 u0) (MkWord64X8 v0) = MkWord64X8 (minusWord64X8# u0 v0)
  timesF (MkWord64X8 u0) (MkWord64X8 v0) = MkWord64X8 (timesWord64X8# u0 v0)
  -- Currently, there is no negateWord64X8#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X8 Word64 where
  andF (MkWord64X8 u0) (MkWord64X8 v0) = MkWord64X8 (andWord64X8# u0 v0)
  orF (MkWord64X8 u0) (MkWord64X8 v0) = MkWord64X8 (orWord64X8# u0 v0)
  xorF (MkWord64X8 u0) (MkWord64X8 v0) = MkWord64X8 (xorWord64X8# u0 v0)
  complementF (MkWord64X8 u0) = MkWord64X8 (complementWord64X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word64 where
  shiftLF (MkWord64X8 u0) (I# i) = MkWord64X8 (shiftLWord64X8# u0 i)
  shiftRF (MkWord64X8 u0) (I# i) = MkWord64X8 (shiftRWord64X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X8 Word64 where
  enumFromZero = MkWord64X8 (packWord64X8# (# 0#Word64, 1#Word64, 2#Word64, 3#Word64, 4#Word64, 5#Word64, 6#Word64, 7#Word64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word64 where
  indexByteArraySIMD# ba i = MkWord64X8 (indexWord64ArrayAsWord64X8# ba i)
  readByteArraySIMD# mba i s0 = case readWord64ArrayAsWord64X8# mba i s0 of (# s1, v0 #) -> (# s1, MkWord64X8 v0 #)
  writeByteArraySIMD# mba i (MkWord64X8 v0) s0 = writeWord64ArrayAsWord64X8# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord64OffAddrAsWord64X8# addr i s0 of (# s1, v0 #) -> (# s1, MkWord64X8 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X8 v0) = IO (\s0 -> case writeWord64OffAddrAsWord64X8# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
newtype instance X8 (Sum a) = MkSumX8 (X8 a)
instance PackX8 X8 a => PackX8 X8 (Sum a) where
  mkX8 = coerce (mkX8 @X8 @a)
  unpackX8 = coerce (unpackX8 @X8 @a)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 a => Broadcast X8 (Sum a) where
  broadcast = coerce (broadcast @X8 @a)
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X8 (Sum a) where
  selectF = coerce (selectF @X8 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 a => UnaryShuffleT indices X8 (Sum a) where
  unaryShuffle = coerce (unaryShuffle @indices @X8 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 a => BinaryShuffleT indices X8 (Sum a) where
  binaryShuffle = coerce (binaryShuffle @indices @X8 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X8 (Product a) = MkProductX8 (X8 a)
instance PackX8 X8 a => PackX8 X8 (Product a) where
  mkX8 = coerce (mkX8 @X8 @a)
  unpackX8 = coerce (unpackX8 @X8 @a)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 a => Broadcast X8 (Product a) where
  broadcast = coerce (broadcast @X8 @a)
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X8 (Product a) where
  selectF = coerce (selectF @X8 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 a => UnaryShuffleT indices X8 (Product a) where
  unaryShuffle = coerce (unaryShuffle @indices @X8 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 a => BinaryShuffleT indices X8 (Product a) where
  binaryShuffle = coerce (binaryShuffle @indices @X8 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X8 (Min a) = MkMinX8 (X8 a)
instance PackX8 X8 a => PackX8 X8 (Min a) where
  mkX8 = coerce (mkX8 @X8 @a)
  unpackX8 = coerce (unpackX8 @X8 @a)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 a => Broadcast X8 (Min a) where
  broadcast = coerce (broadcast @X8 @a)
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X8 (Min a) where
  selectF = coerce (selectF @X8 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 a => UnaryShuffleT indices X8 (Min a) where
  unaryShuffle = coerce (unaryShuffle @indices @X8 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 a => BinaryShuffleT indices X8 (Min a) where
  binaryShuffle = coerce (binaryShuffle @indices @X8 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X8 (Max a) = MkMaxX8 (X8 a)
instance PackX8 X8 a => PackX8 X8 (Max a) where
  mkX8 = coerce (mkX8 @X8 @a)
  unpackX8 = coerce (unpackX8 @X8 @a)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 a => Broadcast X8 (Max a) where
  broadcast = coerce (broadcast @X8 @a)
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X8 (Max a) where
  selectF = coerce (selectF @X8 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 a => UnaryShuffleT indices X8 (Max a) where
  unaryShuffle = coerce (unaryShuffle @indices @X8 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 a => BinaryShuffleT indices X8 (Max a) where
  binaryShuffle = coerce (binaryShuffle @indices @X8 @a)
  {-# INLINE binaryShuffle #-}
data instance X8 (Complex a) = MkComplexX8 !(X8 a) !(X8 a)
instance PackX8 X8 a => PackX8 X8 (Complex a) where
  mkX8 (x0 :+ y0) (x1 :+ y1) (x2 :+ y2) (x3 :+ y3) (x4 :+ y4) (x5 :+ y5) (x6 :+ y6) (x7 :+ y7) = MkComplexX8 (mkX8 x0 x1 x2 x3 x4 x5 x6 x7) (mkX8 y0 y1 y2 y3 y4 y5 y6 y7)
  unpackX8 (MkComplexX8 s t) = case unpackX8 s of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 t of (y0, y1, y2, y3, y4, y5, y6, y7) -> (x0 :+ y0, x1 :+ y1, x2 :+ y2, x3 :+ y3, x4 :+ y4, x5 :+ y5, x6 :+ y6, x7 :+ y7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 a => Broadcast X8 (Complex a) where
  broadcast (x :+ y) = MkComplexX8 (broadcast x) (broadcast y)
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X8 (Complex a) where
  selectF !cond (MkComplexX8 x y) (MkComplexX8 x' y') = MkComplexX8 (selectF cond x x') (selectF cond y y')
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X8 a => UnaryShuffleT indices X8 (Complex a) where
  unaryShuffle (MkComplexX8 x y) = MkComplexX8 (unaryShuffle @indices x) (unaryShuffle @indices y)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X8 a => BinaryShuffleT indices X8 (Complex a) where
  binaryShuffle (MkComplexX8 x y) (MkComplexX8 u v) = MkComplexX8 (binaryShuffle @indices x u) (binaryShuffle @indices y v)
  {-# INLINE binaryShuffle #-}
data instance X8 () = MkUnitX8
instance PackX8 X8 () where
  mkX8 _ _ _ _ _ _ _ _ = MkUnitX8
  unpackX8 MkUnitX8 = ((), (), (), (), (), (), (), ())
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 () where
  broadcast _ = MkUnitX8
  {-# INLINE broadcast #-}
instance SelectableF X8 () where
  selectF _ _ _ = MkUnitX8
  {-# INLINE selectF #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8) => UnaryShuffleT '[i0, i1, i2, i3, i4, i5, i6, i7] X8 () where
  unaryShuffle _ = MkUnitX8
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16) => BinaryShuffleT '[i0, i1, i2, i3, i4, i5, i6, i7] X8 () where
  binaryShuffle _ _ = MkUnitX8
  {-# INLINE binaryShuffle #-}
data instance X8 (a0, a1) = MkTuple2X8 !(X8 a0) !(X8 a1)
instance (PackX8 X8 a0, PackX8 X8 a1) => PackX8 X8 (a0, a1) where
  mkX8 (x0_0, x0_1) (x1_0, x1_1) (x2_0, x2_1) (x3_0, x3_1) (x4_0, x4_1) (x5_0, x5_1) (x6_0, x6_1) (x7_0, x7_1) = MkTuple2X8 (mkX8 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0) (mkX8 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1)
  unpackX8 (MkTuple2X8 v0 v1) = case unpackX8 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0) -> case unpackX8 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1) -> ((x0_0, x0_1), (x1_0, x1_1), (x2_0, x2_1), (x3_0, x3_1), (x4_0, x4_1), (x5_0, x5_1), (x6_0, x6_1), (x7_0, x7_1))
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance (Broadcast X8 a0, Broadcast X8 a1) => Broadcast X8 (a0, a1) where
  broadcast (x0, x1) = MkTuple2X8 (broadcast x0) (broadcast x1)
  {-# INLINE broadcast #-}
instance (SelectableF X8 a0, SelectableF X8 a1) => SelectableF X8 (a0, a1) where
  selectF !cond (MkTuple2X8 x0 x1) (MkTuple2X8 y0 y1) = MkTuple2X8 (selectF cond x0 y0) (selectF cond x1 y1)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X8 a0, UnaryShuffleT indices X8 a1) => UnaryShuffleT indices X8 (a0, a1) where
  unaryShuffle (MkTuple2X8 x0 x1) = MkTuple2X8 (unaryShuffle @indices x0) (unaryShuffle @indices x1)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X8 a0, BinaryShuffleT indices X8 a1) => BinaryShuffleT indices X8 (a0, a1) where
  binaryShuffle (MkTuple2X8 x0 x1) (MkTuple2X8 y0 y1) = MkTuple2X8 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1)
  {-# INLINE binaryShuffle #-}
data instance X8 (a0, a1, a2) = MkTuple3X8 !(X8 a0) !(X8 a1) !(X8 a2)
instance (PackX8 X8 a0, PackX8 X8 a1, PackX8 X8 a2) => PackX8 X8 (a0, a1, a2) where
  mkX8 (x0_0, x0_1, x0_2) (x1_0, x1_1, x1_2) (x2_0, x2_1, x2_2) (x3_0, x3_1, x3_2) (x4_0, x4_1, x4_2) (x5_0, x5_1, x5_2) (x6_0, x6_1, x6_2) (x7_0, x7_1, x7_2) = MkTuple3X8 (mkX8 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0) (mkX8 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1) (mkX8 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2)
  unpackX8 (MkTuple3X8 v0 v1 v2) = case unpackX8 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0) -> case unpackX8 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1) -> case unpackX8 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2) -> ((x0_0, x0_1, x0_2), (x1_0, x1_1, x1_2), (x2_0, x2_1, x2_2), (x3_0, x3_1, x3_2), (x4_0, x4_1, x4_2), (x5_0, x5_1, x5_2), (x6_0, x6_1, x6_2), (x7_0, x7_1, x7_2))
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance (Broadcast X8 a0, Broadcast X8 a1, Broadcast X8 a2) => Broadcast X8 (a0, a1, a2) where
  broadcast (x0, x1, x2) = MkTuple3X8 (broadcast x0) (broadcast x1) (broadcast x2)
  {-# INLINE broadcast #-}
instance (SelectableF X8 a0, SelectableF X8 a1, SelectableF X8 a2) => SelectableF X8 (a0, a1, a2) where
  selectF !cond (MkTuple3X8 x0 x1 x2) (MkTuple3X8 y0 y1 y2) = MkTuple3X8 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X8 a0, UnaryShuffleT indices X8 a1, UnaryShuffleT indices X8 a2) => UnaryShuffleT indices X8 (a0, a1, a2) where
  unaryShuffle (MkTuple3X8 x0 x1 x2) = MkTuple3X8 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X8 a0, BinaryShuffleT indices X8 a1, BinaryShuffleT indices X8 a2) => BinaryShuffleT indices X8 (a0, a1, a2) where
  binaryShuffle (MkTuple3X8 x0 x1 x2) (MkTuple3X8 y0 y1 y2) = MkTuple3X8 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2)
  {-# INLINE binaryShuffle #-}
data instance X8 (a0, a1, a2, a3) = MkTuple4X8 !(X8 a0) !(X8 a1) !(X8 a2) !(X8 a3)
instance (PackX8 X8 a0, PackX8 X8 a1, PackX8 X8 a2, PackX8 X8 a3) => PackX8 X8 (a0, a1, a2, a3) where
  mkX8 (x0_0, x0_1, x0_2, x0_3) (x1_0, x1_1, x1_2, x1_3) (x2_0, x2_1, x2_2, x2_3) (x3_0, x3_1, x3_2, x3_3) (x4_0, x4_1, x4_2, x4_3) (x5_0, x5_1, x5_2, x5_3) (x6_0, x6_1, x6_2, x6_3) (x7_0, x7_1, x7_2, x7_3) = MkTuple4X8 (mkX8 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0) (mkX8 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1) (mkX8 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2) (mkX8 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3)
  unpackX8 (MkTuple4X8 v0 v1 v2 v3) = case unpackX8 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0) -> case unpackX8 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1) -> case unpackX8 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2) -> case unpackX8 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3) -> ((x0_0, x0_1, x0_2, x0_3), (x1_0, x1_1, x1_2, x1_3), (x2_0, x2_1, x2_2, x2_3), (x3_0, x3_1, x3_2, x3_3), (x4_0, x4_1, x4_2, x4_3), (x5_0, x5_1, x5_2, x5_3), (x6_0, x6_1, x6_2, x6_3), (x7_0, x7_1, x7_2, x7_3))
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance (Broadcast X8 a0, Broadcast X8 a1, Broadcast X8 a2, Broadcast X8 a3) => Broadcast X8 (a0, a1, a2, a3) where
  broadcast (x0, x1, x2, x3) = MkTuple4X8 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3)
  {-# INLINE broadcast #-}
instance (SelectableF X8 a0, SelectableF X8 a1, SelectableF X8 a2, SelectableF X8 a3) => SelectableF X8 (a0, a1, a2, a3) where
  selectF !cond (MkTuple4X8 x0 x1 x2 x3) (MkTuple4X8 y0 y1 y2 y3) = MkTuple4X8 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X8 a0, UnaryShuffleT indices X8 a1, UnaryShuffleT indices X8 a2, UnaryShuffleT indices X8 a3) => UnaryShuffleT indices X8 (a0, a1, a2, a3) where
  unaryShuffle (MkTuple4X8 x0 x1 x2 x3) = MkTuple4X8 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X8 a0, BinaryShuffleT indices X8 a1, BinaryShuffleT indices X8 a2, BinaryShuffleT indices X8 a3) => BinaryShuffleT indices X8 (a0, a1, a2, a3) where
  binaryShuffle (MkTuple4X8 x0 x1 x2 x3) (MkTuple4X8 y0 y1 y2 y3) = MkTuple4X8 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3)
  {-# INLINE binaryShuffle #-}
data instance X8 (a0, a1, a2, a3, a4) = MkTuple5X8 !(X8 a0) !(X8 a1) !(X8 a2) !(X8 a3) !(X8 a4)
instance (PackX8 X8 a0, PackX8 X8 a1, PackX8 X8 a2, PackX8 X8 a3, PackX8 X8 a4) => PackX8 X8 (a0, a1, a2, a3, a4) where
  mkX8 (x0_0, x0_1, x0_2, x0_3, x0_4) (x1_0, x1_1, x1_2, x1_3, x1_4) (x2_0, x2_1, x2_2, x2_3, x2_4) (x3_0, x3_1, x3_2, x3_3, x3_4) (x4_0, x4_1, x4_2, x4_3, x4_4) (x5_0, x5_1, x5_2, x5_3, x5_4) (x6_0, x6_1, x6_2, x6_3, x6_4) (x7_0, x7_1, x7_2, x7_3, x7_4) = MkTuple5X8 (mkX8 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0) (mkX8 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1) (mkX8 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2) (mkX8 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3) (mkX8 x0_4 x1_4 x2_4 x3_4 x4_4 x5_4 x6_4 x7_4)
  unpackX8 (MkTuple5X8 v0 v1 v2 v3 v4) = case unpackX8 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0) -> case unpackX8 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1) -> case unpackX8 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2) -> case unpackX8 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3) -> case unpackX8 v4 of (x0_4, x1_4, x2_4, x3_4, x4_4, x5_4, x6_4, x7_4) -> ((x0_0, x0_1, x0_2, x0_3, x0_4), (x1_0, x1_1, x1_2, x1_3, x1_4), (x2_0, x2_1, x2_2, x2_3, x2_4), (x3_0, x3_1, x3_2, x3_3, x3_4), (x4_0, x4_1, x4_2, x4_3, x4_4), (x5_0, x5_1, x5_2, x5_3, x5_4), (x6_0, x6_1, x6_2, x6_3, x6_4), (x7_0, x7_1, x7_2, x7_3, x7_4))
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance (Broadcast X8 a0, Broadcast X8 a1, Broadcast X8 a2, Broadcast X8 a3, Broadcast X8 a4) => Broadcast X8 (a0, a1, a2, a3, a4) where
  broadcast (x0, x1, x2, x3, x4) = MkTuple5X8 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4)
  {-# INLINE broadcast #-}
instance (SelectableF X8 a0, SelectableF X8 a1, SelectableF X8 a2, SelectableF X8 a3, SelectableF X8 a4) => SelectableF X8 (a0, a1, a2, a3, a4) where
  selectF !cond (MkTuple5X8 x0 x1 x2 x3 x4) (MkTuple5X8 y0 y1 y2 y3 y4) = MkTuple5X8 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X8 a0, UnaryShuffleT indices X8 a1, UnaryShuffleT indices X8 a2, UnaryShuffleT indices X8 a3, UnaryShuffleT indices X8 a4) => UnaryShuffleT indices X8 (a0, a1, a2, a3, a4) where
  unaryShuffle (MkTuple5X8 x0 x1 x2 x3 x4) = MkTuple5X8 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X8 a0, BinaryShuffleT indices X8 a1, BinaryShuffleT indices X8 a2, BinaryShuffleT indices X8 a3, BinaryShuffleT indices X8 a4) => BinaryShuffleT indices X8 (a0, a1, a2, a3, a4) where
  binaryShuffle (MkTuple5X8 x0 x1 x2 x3 x4) (MkTuple5X8 y0 y1 y2 y3 y4) = MkTuple5X8 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4)
  {-# INLINE binaryShuffle #-}
data instance X8 (a0, a1, a2, a3, a4, a5) = MkTuple6X8 !(X8 a0) !(X8 a1) !(X8 a2) !(X8 a3) !(X8 a4) !(X8 a5)
instance (PackX8 X8 a0, PackX8 X8 a1, PackX8 X8 a2, PackX8 X8 a3, PackX8 X8 a4, PackX8 X8 a5) => PackX8 X8 (a0, a1, a2, a3, a4, a5) where
  mkX8 (x0_0, x0_1, x0_2, x0_3, x0_4, x0_5) (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5) (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5) (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5) (x4_0, x4_1, x4_2, x4_3, x4_4, x4_5) (x5_0, x5_1, x5_2, x5_3, x5_4, x5_5) (x6_0, x6_1, x6_2, x6_3, x6_4, x6_5) (x7_0, x7_1, x7_2, x7_3, x7_4, x7_5) = MkTuple6X8 (mkX8 x0_0 x1_0 x2_0 x3_0 x4_0 x5_0 x6_0 x7_0) (mkX8 x0_1 x1_1 x2_1 x3_1 x4_1 x5_1 x6_1 x7_1) (mkX8 x0_2 x1_2 x2_2 x3_2 x4_2 x5_2 x6_2 x7_2) (mkX8 x0_3 x1_3 x2_3 x3_3 x4_3 x5_3 x6_3 x7_3) (mkX8 x0_4 x1_4 x2_4 x3_4 x4_4 x5_4 x6_4 x7_4) (mkX8 x0_5 x1_5 x2_5 x3_5 x4_5 x5_5 x6_5 x7_5)
  unpackX8 (MkTuple6X8 v0 v1 v2 v3 v4 v5) = case unpackX8 v0 of (x0_0, x1_0, x2_0, x3_0, x4_0, x5_0, x6_0, x7_0) -> case unpackX8 v1 of (x0_1, x1_1, x2_1, x3_1, x4_1, x5_1, x6_1, x7_1) -> case unpackX8 v2 of (x0_2, x1_2, x2_2, x3_2, x4_2, x5_2, x6_2, x7_2) -> case unpackX8 v3 of (x0_3, x1_3, x2_3, x3_3, x4_3, x5_3, x6_3, x7_3) -> case unpackX8 v4 of (x0_4, x1_4, x2_4, x3_4, x4_4, x5_4, x6_4, x7_4) -> case unpackX8 v5 of (x0_5, x1_5, x2_5, x3_5, x4_5, x5_5, x6_5, x7_5) -> ((x0_0, x0_1, x0_2, x0_3, x0_4, x0_5), (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5), (x2_0, x2_1, x2_2, x2_3, x2_4, x2_5), (x3_0, x3_1, x3_2, x3_3, x3_4, x3_5), (x4_0, x4_1, x4_2, x4_3, x4_4, x4_5), (x5_0, x5_1, x5_2, x5_3, x5_4, x5_5), (x6_0, x6_1, x6_2, x6_3, x6_4, x6_5), (x7_0, x7_1, x7_2, x7_3, x7_4, x7_5))
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance (Broadcast X8 a0, Broadcast X8 a1, Broadcast X8 a2, Broadcast X8 a3, Broadcast X8 a4, Broadcast X8 a5) => Broadcast X8 (a0, a1, a2, a3, a4, a5) where
  broadcast (x0, x1, x2, x3, x4, x5) = MkTuple6X8 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4) (broadcast x5)
  {-# INLINE broadcast #-}
instance (SelectableF X8 a0, SelectableF X8 a1, SelectableF X8 a2, SelectableF X8 a3, SelectableF X8 a4, SelectableF X8 a5) => SelectableF X8 (a0, a1, a2, a3, a4, a5) where
  selectF !cond (MkTuple6X8 x0 x1 x2 x3 x4 x5) (MkTuple6X8 y0 y1 y2 y3 y4 y5) = MkTuple6X8 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4) (selectF cond x5 y5)
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X8 a0, UnaryShuffleT indices X8 a1, UnaryShuffleT indices X8 a2, UnaryShuffleT indices X8 a3, UnaryShuffleT indices X8 a4, UnaryShuffleT indices X8 a5) => UnaryShuffleT indices X8 (a0, a1, a2, a3, a4, a5) where
  unaryShuffle (MkTuple6X8 x0 x1 x2 x3 x4 x5) = MkTuple6X8 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4) (unaryShuffle @indices x5)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X8 a0, BinaryShuffleT indices X8 a1, BinaryShuffleT indices X8 a2, BinaryShuffleT indices X8 a3, BinaryShuffleT indices X8 a4, BinaryShuffleT indices X8 a5) => BinaryShuffleT indices X8 (a0, a1, a2, a3, a4, a5) where
  binaryShuffle (MkTuple6X8 x0 x1 x2 x3 x4 x5) (MkTuple6X8 y0 y1 y2 y3 y4 y5) = MkTuple6X8 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4) (binaryShuffle @indices x5 y5)
  {-# INLINE binaryShuffle #-}
instance (PackX8 X8 a, PackX8 X8 b) => LiftSIMD X8 a b where
  liftSIMD f !v = case unpackX8 v of (x0, x1, x2, x3, x4, x5, x6, x7) -> mkX8 (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7)
  {-# INLINE liftSIMD #-}
instance (PackX8 X8 a, PackX8 X8 b, PackX8 X8 c) => LiftSIMD2 X8 a b c where
  liftSIMD2 f !u !v = case unpackX8 u of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 v of (y0, y1, y2, y3, y4, y5, y6, y7) -> mkX8 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4) (f x5 y5) (f x6 y6) (f x7 y7)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X8 where
  mkTuple2 = MkTuple2X8
  mkTuple3 = MkTuple3X8
  mkTuple4 = MkTuple4X8
  mkTuple5 = MkTuple5X8
  mkTuple6 = MkTuple6X8
  deconstructTuple2 (MkTuple2X8 v0 v1) = (v0, v1)
  deconstructTuple3 (MkTuple3X8 v0 v1 v2) = (v0, v1, v2)
  deconstructTuple4 (MkTuple4X8 v0 v1 v2 v3) = (v0, v1, v2, v3)
  deconstructTuple5 (MkTuple5X8 v0 v1 v2 v3 v4) = (v0, v1, v2, v3, v4)
  deconstructTuple6 (MkTuple6X8 v0 v1 v2 v3 v4 v5) = (v0, v1, v2, v3, v4, v5)
  mkSum = coerce
  getSum' = coerce
  mkProduct = coerce
  getProduct' = coerce
  mkMin = coerce
  getMin' = coerce
  mkMax = coerce
  getMax' = coerce
  mkComplex = MkComplexX8
  deconstructComplex (MkComplexX8 x y) = (x, y)
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
deriving via WrappedMulti X8 a instance SelectableF X8 a => Selectable (X8 a)
deriving via WrappedMulti X8 a instance NumF X8 a => Num (X8 a)
deriving via WrappedMulti X8 a instance FractionalF X8 a => Fractional (X8 a)
deriving via WrappedMulti X8 a instance FloatingF X8 a => Floating (X8 a)
deriving via WrappedMulti X8 a instance BooleanF X8 a => Boolean (X8 a)
deriving via WrappedMulti X8 a instance BitShiftF X8 a => BitShift (X8 a)
deriving via WrappedMulti X8 a instance MinMaxF X8 a => MinMax (X8 a)
deriving via WrappedMulti X8 a instance FusedMultiplyAddF X8 a => FusedMultiplyAdd (X8 a)
