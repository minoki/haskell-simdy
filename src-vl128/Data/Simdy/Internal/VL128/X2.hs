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
module Data.Simdy.Internal.VL128.X2 where
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
import           Data.Simdy.Internal.VL128.Prim
import           Data.Simdy.Internal.VL128.PrimExtra
import qualified GHC.Exts
import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..))
import           GHC.Int
import           GHC.IO
import           GHC.Word
import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=), min, max)
-- | @'X2' a@ is a fixed-length vector of length 2.
--
-- Conceptually, @data 'X2' a = MkX2 !a !a@.
--
-- You can access the elements by 'mkX2', 'packX2' and 'unpackX2'.
data family X2 a
instance KnownSIMDLength X2 where
  type SIMDLength X2 = 2
  simdLength = 2
  {-# INLINE simdLength #-}
newtype instance X2 Bool = MkBoolX2 Word8
type instance Mask (X2 a) = X2 Bool
instance MaskIsLiftedBool X2 a
instance BooleanF X2 Bool where
  andF (MkBoolX2 x) (MkBoolX2 y) = MkBoolX2 (x .&. y)
  orF (MkBoolX2 x) (MkBoolX2 y) = MkBoolX2 (x .|. y)
  xorF (MkBoolX2 x) (MkBoolX2 y) = MkBoolX2 (xor x y)
  complementF (MkBoolX2 x) = MkBoolX2 (0x3 - x)
deriving via WrappedMulti X2 a instance EquatableF X2 a => Equatable (X2 a)
deriving via WrappedMulti X2 a instance OrderedF X2 a => Ordered (X2 a)
instance PackX2 X2 a => IsList (X2 a) where
  type Item (X2 a) = a
  toList = toListX2
  fromList = fromListX2
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX2 X2 Bool where
  mkX2 !x0 !x1 = MkBoolX2 ((if x0 then 0x1 else 0) .|. (if x1 then 0x2 else 0))
  unpackX2 (MkBoolX2 !x) = (testBit x 0, testBit x 1)
instance Broadcast X2 Bool where
  broadcast False = MkBoolX2 0
  broadcast True = MkBoolX2 0x3
  {-# INLINE broadcast #-}
instance SelectableF X2 Bool where
  selectF (MkBoolX2 !cond) (MkBoolX2 !x) (MkBoolX2 !y) = MkBoolX2 ((cond .&. x) .|. (complement cond .&. y))
  {-# INLINE selectF #-}
data instance X2 Float = MkFloatX2 FloatX4#
instance PackX2 X2 Float where
  mkX2 (F# x0) (F# x1) = MkFloatX2 (packFloatX4# (# x0, x1, 0.0#, 0.0# #))
  unpackX2 (MkFloatX2 v0) = case unpackFloatX4# v0 of (# x0, x1, _, _ #) -> (F# x0, F# x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Float where
  broadcast (F# x) = MkFloatX2 (broadcastFloatX4# x)
  {-# INLINE broadcast #-}
instance SelectableF X2 Float where
  selectF (MkBoolX2 !cond) (MkFloatX2 x0) (MkFloatX2 y0) = MkFloatX2 (selectFloatX4# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 2, ShuffleMany FloatX4# indices) => UnaryShuffle indices X2 Float where
  unaryShuffle (MkFloatX2 x) = MkFloatX2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany FloatX4# indices) => BinaryShuffle indices X2 Float where
  binaryShuffle (MkFloatX2 x0) (MkFloatX2 x1) = MkFloatX2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Float where
  eqF (MkFloatX2 u0) (MkFloatX2 v0) = MkBoolX2 $ (fromIntegral (eqFloatX4# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Float where
  ltF (MkFloatX2 u0) (MkFloatX2 v0) = MkBoolX2 $ (fromIntegral (ltFloatX4# u0 v0 .&. 0x3))
  leF (MkFloatX2 u0) (MkFloatX2 v0) = MkBoolX2 $ (fromIntegral (leFloatX4# u0 v0 .&. 0x3))
  gtF (MkFloatX2 u0) (MkFloatX2 v0) = MkBoolX2 $ (fromIntegral (gtFloatX4# u0 v0 .&. 0x3))
  geF (MkFloatX2 u0) (MkFloatX2 v0) = MkBoolX2 $ (fromIntegral (geFloatX4# u0 v0 .&. 0x3))
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Float where
  minF (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (minimumFloatX4# u0 v0)
  maxF (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (maximumFloatX4# u0 v0)
  minimumNumberF (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (minimumNumberFloatX4# u0 v0)
  maximumNumberF (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (maximumNumberFloatX4# u0 v0)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX2Float :: X2 Float -> X2 Float
negateX2Float (MkFloatX2 u0) = MkFloatX2 (negateFloatX4# u0)
#if defined(USE_FMA)
{-# INLINE [0] negateX2Float #-}
#else
{-# INLINE negateX2Float #-}
#endif
instance NumF X2 Float where
  plusF (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (plusFloatX4# u0 v0)
  minusF (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (minusFloatX4# u0 v0)
  timesF (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (timesFloatX4# u0 v0)
  negateF = negateX2Float
  absF (MkFloatX2 u0) = MkFloatX2 (absFloatX4# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X2 Float where
  divideF (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (divideFloatX4# u0 v0)
  {-# INLINE divideF #-}
instance FloatingF X2 Float where
  sqrtF (MkFloatX2 u0) = MkFloatX2 (sqrtFloatX4# u0)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X2 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX2 u0) (MkFloatX2 v0) (MkFloatX2 w0) = MkFloatX2 (fmaddFloatX4# u0 v0 w0)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X2 Float" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X2 Float
"Fusible/*-/X2 Float" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX2Float c) :: X2 Float
"Fusible/-*+/X2 Float" forall a b c.
  negateX2Float (a F.* b) F.+ c = fusedMultiplyAdd (negateX2Float a) b c :: X2 Float
"Fusible/-*-/X2 Float" forall a b c.
  negateX2Float (a F.* b) F.- c = fusedMultiplyAdd (negateX2Float a) b (negateX2Float c) :: X2 Float
"Fusible/+*/X2 Float" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X2 Float
"Fusible/-*/X2 Float" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX2Float b) c a :: X2 Float
  #-}
#endif
instance EnumFromZero_ X2 Float where
  enumFromZero = MkFloatX2 (packFloatX4# (# 0.0#, 1.0#, 0.0#, 0.0# #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Float where
  indexByteArraySIMD# ba i = MkFloatX2 (packFloatX4# (# GHC.Exts.indexFloatArray# ba i, GHC.Exts.indexFloatArray# ba (i +# 1#), 0.0#, 0.0# #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readFloatArray# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readFloatArray# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkFloatX2 (packFloatX4# (# x0, x1, 0.0#, 0.0# #)) #)
  writeByteArraySIMD# mba i (MkFloatX2 v0) s0 = case unpackFloatX4# v0 of (# x0, x1, _, _ #) -> case GHC.Exts.writeFloatArray# mba i x0 s0 of s1 -> GHC.Exts.writeFloatArray# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readFloatOffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkFloatX2 (packFloatX4# (# x0, x1, 0.0#, 0.0# #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX2 v0) = IO (\s0 -> case unpackFloatX4# v0 of (# x0, x1, _, _ #) -> case GHC.Exts.writeFloatOffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeFloatOffAddr# addr (i +# 1#) x1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Double = MkDoubleX2 DoubleX2#
instance PackX2 X2 Double where
  mkX2 (D# x0) (D# x1) = MkDoubleX2 (packDoubleX2# (# x0, x1 #))
  unpackX2 (MkDoubleX2 v0) = case unpackDoubleX2# v0 of (# x0, x1 #) -> (D# x0, D# x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Double where
  broadcast (D# x) = MkDoubleX2 (broadcastDoubleX2# x)
  {-# INLINE broadcast #-}
instance SelectableF X2 Double where
  selectF (MkBoolX2 !cond) (MkDoubleX2 x0) (MkDoubleX2 y0) = MkDoubleX2 (selectDoubleX2# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 2, ShuffleMany DoubleX2# indices) => UnaryShuffle indices X2 Double where
  unaryShuffle (MkDoubleX2 x) = MkDoubleX2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany DoubleX2# indices) => BinaryShuffle indices X2 Double where
  binaryShuffle (MkDoubleX2 x0) (MkDoubleX2 x1) = MkDoubleX2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Double where
  eqF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkBoolX2 $ (fromIntegral (eqDoubleX2# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X2 Double where
  ltF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkBoolX2 $ (fromIntegral (ltDoubleX2# u0 v0))
  leF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkBoolX2 $ (fromIntegral (leDoubleX2# u0 v0))
  gtF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkBoolX2 $ (fromIntegral (gtDoubleX2# u0 v0))
  geF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkBoolX2 $ (fromIntegral (geDoubleX2# u0 v0))
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Double where
  minF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (minimumDoubleX2# u0 v0)
  maxF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (maximumDoubleX2# u0 v0)
  minimumNumberF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (minimumNumberDoubleX2# u0 v0)
  maximumNumberF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (maximumNumberDoubleX2# u0 v0)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX2Double :: X2 Double -> X2 Double
negateX2Double (MkDoubleX2 u0) = MkDoubleX2 (negateDoubleX2# u0)
#if defined(USE_FMA)
{-# INLINE [0] negateX2Double #-}
#else
{-# INLINE negateX2Double #-}
#endif
instance NumF X2 Double where
  plusF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (plusDoubleX2# u0 v0)
  minusF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (minusDoubleX2# u0 v0)
  timesF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (timesDoubleX2# u0 v0)
  negateF = negateX2Double
  absF (MkDoubleX2 u0) = MkDoubleX2 (absDoubleX2# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance FractionalF X2 Double where
  divideF (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (divideDoubleX2# u0 v0)
  {-# INLINE divideF #-}
instance FloatingF X2 Double where
  sqrtF (MkDoubleX2 u0) = MkDoubleX2 (sqrtDoubleX2# u0)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X2 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX2 u0) (MkDoubleX2 v0) (MkDoubleX2 w0) = MkDoubleX2 (fmaddDoubleX2# u0 v0 w0)
  {-# INLINE fusedMultiplyAddF #-}
#else
  fusedMultiplyAddF _ _ _ = fmaIsDisabled
#endif
#if defined(USE_FMA)
{-# RULES
"Fusible/*+/X2 Double" forall a b c.
  a F.* b F.+ c = fusedMultiplyAdd a b c :: X2 Double
"Fusible/*-/X2 Double" forall a b c.
  a F.* b F.- c = fusedMultiplyAdd a b (negateX2Double c) :: X2 Double
"Fusible/-*+/X2 Double" forall a b c.
  negateX2Double (a F.* b) F.+ c = fusedMultiplyAdd (negateX2Double a) b c :: X2 Double
"Fusible/-*-/X2 Double" forall a b c.
  negateX2Double (a F.* b) F.- c = fusedMultiplyAdd (negateX2Double a) b (negateX2Double c) :: X2 Double
"Fusible/+*/X2 Double" forall a b c.
  a F.+ b F.* c = fusedMultiplyAdd b c a :: X2 Double
"Fusible/-*/X2 Double" forall a b c.
  a F.- b F.* c = fusedMultiplyAdd (negateX2Double b) c a :: X2 Double
  #-}
#endif
instance EnumFromZero_ X2 Double where
  enumFromZero = MkDoubleX2 (packDoubleX2# (# 0.0##, 1.0## #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Double where
  indexByteArraySIMD# ba i = MkDoubleX2 (indexDoubleArrayAsDoubleX2# ba i)
  readByteArraySIMD# mba i s0 = case readDoubleArrayAsDoubleX2# mba i s0 of (# s1, v0 #) -> (# s1, MkDoubleX2 v0 #)
  writeByteArraySIMD# mba i (MkDoubleX2 v0) s0 = writeDoubleArrayAsDoubleX2# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readDoubleOffAddrAsDoubleX2# addr i s0 of (# s1, v0 #) -> (# s1, MkDoubleX2 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX2 v0) = IO (\s0 -> case writeDoubleOffAddrAsDoubleX2# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__)
instance ImplementationDescription X2 where
  implementationDescription _ = "X2;maxBits=128"
data instance X2 Int8 = MkInt8X2 Int8X16#
instance PackX2 X2 Int8 where
  mkX2 (I8# x0) (I8# x1) = MkInt8X2 (packInt8X16# (# x0, x1, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  unpackX2 (MkInt8X2 v0) = case unpackInt8X16# v0 of (# x0, x1, _, _, _, _, _, _, _, _, _, _, _, _, _, _ #) -> (I8# x0, I8# x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Int8 where
  broadcast (I8# x) = MkInt8X2 (broadcastInt8X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X2 Int8 where
  selectF (MkBoolX2 !cond) (MkInt8X2 x0) (MkInt8X2 y0) = MkInt8X2 (selectInt8X16# (fromIntegral cond) x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 2, ShuffleMany Int8X16# indices) => UnaryShuffle indices X2 Int8 where
  unaryShuffle (MkInt8X2 x) = MkInt8X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Int8X16# indices) => BinaryShuffle indices X2 Int8 where
  binaryShuffle (MkInt8X2 x0) (MkInt8X2 x1) = MkInt8X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int8 where
  eqF (MkInt8X2 u0) (MkInt8X2 v0) = MkBoolX2 $ (fromIntegral (eqInt8X16# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Int8 where
  ltF (MkInt8X2 u0) (MkInt8X2 v0) = MkBoolX2 $ (fromIntegral (ltInt8X16# u0 v0 .&. 0x3))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Int8 where
  minF (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (minInt8X16# u0 v0)
  maxF (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (maxInt8X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Int8 where
  plusF (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (plusInt8X16# u0 v0)
  minusF (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (minusInt8X16# u0 v0)
  timesF (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (timesInt8X16# u0 v0)
  negateF (MkInt8X2 u0) = MkInt8X2 (negateInt8X16# u0)
  absF (MkInt8X2 u0) = MkInt8X2 (absInt8X16# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X2 Int8 where
  andF (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (andInt8X16# u0 v0)
  orF (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (orInt8X16# u0 v0)
  xorF (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (xorInt8X16# u0 v0)
  complementF (MkInt8X2 u0) = MkInt8X2 (complementInt8X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Int8 where
  shiftLF (MkInt8X2 u0) (I# i) = MkInt8X2 (shiftLInt8X16# u0 i)
  shiftRF (MkInt8X2 u0) (I# i) = MkInt8X2 (shiftRInt8X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X2 Int8 where
  enumFromZero = MkInt8X2 (packInt8X16# (# 0#Int8, 1#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Int8 where
  indexByteArraySIMD# ba i = MkInt8X2 (packInt8X16# (# GHC.Exts.indexInt8Array# ba i, GHC.Exts.indexInt8Array# ba (i +# 1#), 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt8X2 (packInt8X16# (# x0, x1, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #)) #)
  writeByteArraySIMD# mba i (MkInt8X2 v0) s0 = case unpackInt8X16# v0 of (# x0, x1, _, _, _, _, _, _, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeInt8Array# mba i x0 s0 of s1 -> GHC.Exts.writeInt8Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt8X2 (packInt8X16# (# x0, x1, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8, 0#Int8 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X2 v0) = IO (\s0 -> case unpackInt8X16# v0 of (# x0, x1, _, _, _, _, _, _, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeInt8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 1#) x1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Int16 = MkInt16X2 Int16X8#
instance PackX2 X2 Int16 where
  mkX2 (I16# x0) (I16# x1) = MkInt16X2 (packInt16X8# (# x0, x1, 0#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16 #))
  unpackX2 (MkInt16X2 v0) = case unpackInt16X8# v0 of (# x0, x1, _, _, _, _, _, _ #) -> (I16# x0, I16# x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Int16 where
  broadcast (I16# x) = MkInt16X2 (broadcastInt16X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X2 Int16 where
  selectF (MkBoolX2 !cond) (MkInt16X2 x0) (MkInt16X2 y0) = MkInt16X2 (selectInt16X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 2, ShuffleMany Int16X8# indices) => UnaryShuffle indices X2 Int16 where
  unaryShuffle (MkInt16X2 x) = MkInt16X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Int16X8# indices) => BinaryShuffle indices X2 Int16 where
  binaryShuffle (MkInt16X2 x0) (MkInt16X2 x1) = MkInt16X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int16 where
  eqF (MkInt16X2 u0) (MkInt16X2 v0) = MkBoolX2 $ (fromIntegral (eqInt16X8# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Int16 where
  ltF (MkInt16X2 u0) (MkInt16X2 v0) = MkBoolX2 $ (fromIntegral (ltInt16X8# u0 v0 .&. 0x3))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Int16 where
  minF (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (minInt16X8# u0 v0)
  maxF (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (maxInt16X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Int16 where
  plusF (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (plusInt16X8# u0 v0)
  minusF (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (minusInt16X8# u0 v0)
  timesF (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (timesInt16X8# u0 v0)
  negateF (MkInt16X2 u0) = MkInt16X2 (negateInt16X8# u0)
  absF (MkInt16X2 u0) = MkInt16X2 (absInt16X8# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X2 Int16 where
  andF (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (andInt16X8# u0 v0)
  orF (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (orInt16X8# u0 v0)
  xorF (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (xorInt16X8# u0 v0)
  complementF (MkInt16X2 u0) = MkInt16X2 (complementInt16X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Int16 where
  shiftLF (MkInt16X2 u0) (I# i) = MkInt16X2 (shiftLInt16X8# u0 i)
  shiftRF (MkInt16X2 u0) (I# i) = MkInt16X2 (shiftRInt16X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X2 Int16 where
  enumFromZero = MkInt16X2 (packInt16X8# (# 0#Int16, 1#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Int16 where
  indexByteArraySIMD# ba i = MkInt16X2 (packInt16X8# (# GHC.Exts.indexInt16Array# ba i, GHC.Exts.indexInt16Array# ba (i +# 1#), 0#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt16X2 (packInt16X8# (# x0, x1, 0#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16 #)) #)
  writeByteArraySIMD# mba i (MkInt16X2 v0) s0 = case unpackInt16X8# v0 of (# x0, x1, _, _, _, _, _, _ #) -> case GHC.Exts.writeInt16Array# mba i x0 s0 of s1 -> GHC.Exts.writeInt16Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt16X2 (packInt16X8# (# x0, x1, 0#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16, 0#Int16 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X2 v0) = IO (\s0 -> case unpackInt16X8# v0 of (# x0, x1, _, _, _, _, _, _ #) -> case GHC.Exts.writeInt16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 1#) x1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Int32 = MkInt32X2 Int32X4#
instance PackX2 X2 Int32 where
  mkX2 (I32# x0) (I32# x1) = MkInt32X2 (packInt32X4# (# x0, x1, 0#Int32, 0#Int32 #))
  unpackX2 (MkInt32X2 v0) = case unpackInt32X4# v0 of (# x0, x1, _, _ #) -> (I32# x0, I32# x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Int32 where
  broadcast (I32# x) = MkInt32X2 (broadcastInt32X4# x)
  {-# INLINE broadcast #-}
instance SelectableF X2 Int32 where
  selectF (MkBoolX2 !cond) (MkInt32X2 x0) (MkInt32X2 y0) = MkInt32X2 (selectInt32X4# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 2, ShuffleMany Int32X4# indices) => UnaryShuffle indices X2 Int32 where
  unaryShuffle (MkInt32X2 x) = MkInt32X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Int32X4# indices) => BinaryShuffle indices X2 Int32 where
  binaryShuffle (MkInt32X2 x0) (MkInt32X2 x1) = MkInt32X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int32 where
  eqF (MkInt32X2 u0) (MkInt32X2 v0) = MkBoolX2 $ (fromIntegral (eqInt32X4# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Int32 where
  ltF (MkInt32X2 u0) (MkInt32X2 v0) = MkBoolX2 $ (fromIntegral (ltInt32X4# u0 v0 .&. 0x3))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Int32 where
  minF (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (minInt32X4# u0 v0)
  maxF (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (maxInt32X4# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Int32 where
  plusF (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (plusInt32X4# u0 v0)
  minusF (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (minusInt32X4# u0 v0)
  timesF (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (timesInt32X4# u0 v0)
  negateF (MkInt32X2 u0) = MkInt32X2 (negateInt32X4# u0)
  absF (MkInt32X2 u0) = MkInt32X2 (absInt32X4# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X2 Int32 where
  andF (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (andInt32X4# u0 v0)
  orF (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (orInt32X4# u0 v0)
  xorF (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (xorInt32X4# u0 v0)
  complementF (MkInt32X2 u0) = MkInt32X2 (complementInt32X4# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Int32 where
  shiftLF (MkInt32X2 u0) (I# i) = MkInt32X2 (shiftLInt32X4# u0 i)
  shiftRF (MkInt32X2 u0) (I# i) = MkInt32X2 (shiftRInt32X4# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X2 Int32 where
  enumFromZero = MkInt32X2 (packInt32X4# (# 0#Int32, 1#Int32, 0#Int32, 0#Int32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Int32 where
  indexByteArraySIMD# ba i = MkInt32X2 (packInt32X4# (# GHC.Exts.indexInt32Array# ba i, GHC.Exts.indexInt32Array# ba (i +# 1#), 0#Int32, 0#Int32 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt32X2 (packInt32X4# (# x0, x1, 0#Int32, 0#Int32 #)) #)
  writeByteArraySIMD# mba i (MkInt32X2 v0) s0 = case unpackInt32X4# v0 of (# x0, x1, _, _ #) -> case GHC.Exts.writeInt32Array# mba i x0 s0 of s1 -> GHC.Exts.writeInt32Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt32X2 (packInt32X4# (# x0, x1, 0#Int32, 0#Int32 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X2 v0) = IO (\s0 -> case unpackInt32X4# v0 of (# x0, x1, _, _ #) -> case GHC.Exts.writeInt32OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 1#) x1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Int64 = MkInt64X2 Int64X2#
instance PackX2 X2 Int64 where
  mkX2 (I64# x0) (I64# x1) = MkInt64X2 (packInt64X2# (# x0, x1 #))
  unpackX2 (MkInt64X2 v0) = case unpackInt64X2# v0 of (# x0, x1 #) -> (I64# x0, I64# x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Int64 where
  broadcast (I64# x) = MkInt64X2 (broadcastInt64X2# x)
  {-# INLINE broadcast #-}
instance SelectableF X2 Int64 where
  selectF (MkBoolX2 !cond) (MkInt64X2 x0) (MkInt64X2 y0) = MkInt64X2 (selectInt64X2# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 2, ShuffleMany Int64X2# indices) => UnaryShuffle indices X2 Int64 where
  unaryShuffle (MkInt64X2 x) = MkInt64X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Int64X2# indices) => BinaryShuffle indices X2 Int64 where
  binaryShuffle (MkInt64X2 x0) (MkInt64X2 x1) = MkInt64X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int64 where
  eqF (MkInt64X2 u0) (MkInt64X2 v0) = MkBoolX2 $ (fromIntegral (eqInt64X2# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X2 Int64 where
  ltF (MkInt64X2 u0) (MkInt64X2 v0) = MkBoolX2 $ (fromIntegral (ltInt64X2# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Int64 where
  minF (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (minInt64X2# u0 v0)
  maxF (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (maxInt64X2# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Int64 where
  plusF (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (plusInt64X2# u0 v0)
  minusF (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (minusInt64X2# u0 v0)
  timesF (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (timesInt64X2# u0 v0)
  negateF (MkInt64X2 u0) = MkInt64X2 (negateInt64X2# u0)
  absF (MkInt64X2 u0) = MkInt64X2 (absInt64X2# u0)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
  {-# INLINE absF #-}
instance BooleanF X2 Int64 where
  andF (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (andInt64X2# u0 v0)
  orF (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (orInt64X2# u0 v0)
  xorF (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (xorInt64X2# u0 v0)
  complementF (MkInt64X2 u0) = MkInt64X2 (complementInt64X2# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Int64 where
  shiftLF (MkInt64X2 u0) (I# i) = MkInt64X2 (shiftLInt64X2# u0 i)
  shiftRF (MkInt64X2 u0) (I# i) = MkInt64X2 (shiftRInt64X2# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X2 Int64 where
  enumFromZero = MkInt64X2 (packInt64X2# (# 0#Int64, 1#Int64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Int64 where
  indexByteArraySIMD# ba i = MkInt64X2 (indexInt64ArrayAsInt64X2# ba i)
  readByteArraySIMD# mba i s0 = case readInt64ArrayAsInt64X2# mba i s0 of (# s1, v0 #) -> (# s1, MkInt64X2 v0 #)
  writeByteArraySIMD# mba i (MkInt64X2 v0) s0 = writeInt64ArrayAsInt64X2# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readInt64OffAddrAsInt64X2# addr i s0 of (# s1, v0 #) -> (# s1, MkInt64X2 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X2 v0) = IO (\s0 -> case writeInt64OffAddrAsInt64X2# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Word8 = MkWord8X2 Word8X16#
instance PackX2 X2 Word8 where
  mkX2 (W8# x0) (W8# x1) = MkWord8X2 (packWord8X16# (# x0, x1, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  unpackX2 (MkWord8X2 v0) = case unpackWord8X16# v0 of (# x0, x1, _, _, _, _, _, _, _, _, _, _, _, _, _, _ #) -> (W8# x0, W8# x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Word8 where
  broadcast (W8# x) = MkWord8X2 (broadcastWord8X16# x)
  {-# INLINE broadcast #-}
instance SelectableF X2 Word8 where
  selectF (MkBoolX2 !cond) (MkWord8X2 x0) (MkWord8X2 y0) = MkWord8X2 (selectWord8X16# (fromIntegral cond) x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 2, ShuffleMany Word8X16# indices) => UnaryShuffle indices X2 Word8 where
  unaryShuffle (MkWord8X2 x) = MkWord8X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Word8X16# indices) => BinaryShuffle indices X2 Word8 where
  binaryShuffle (MkWord8X2 x0) (MkWord8X2 x1) = MkWord8X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word8 where
  eqF (MkWord8X2 u0) (MkWord8X2 v0) = MkBoolX2 $ (fromIntegral (eqWord8X16# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Word8 where
  ltF (MkWord8X2 u0) (MkWord8X2 v0) = MkBoolX2 $ (fromIntegral (ltWord8X16# u0 v0 .&. 0x3))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Word8 where
  minF (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (minWord8X16# u0 v0)
  maxF (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (maxWord8X16# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Word8 where
  plusF (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (plusWord8X16# u0 v0)
  minusF (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (minusWord8X16# u0 v0)
  timesF (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (timesWord8X16# u0 v0)
  -- Currently, there is no negateWord8X16#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X2 Word8 where
  andF (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (andWord8X16# u0 v0)
  orF (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (orWord8X16# u0 v0)
  xorF (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (xorWord8X16# u0 v0)
  complementF (MkWord8X2 u0) = MkWord8X2 (complementWord8X16# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Word8 where
  shiftLF (MkWord8X2 u0) (I# i) = MkWord8X2 (shiftLWord8X16# u0 i)
  shiftRF (MkWord8X2 u0) (I# i) = MkWord8X2 (shiftRWord8X16# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X2 Word8 where
  enumFromZero = MkWord8X2 (packWord8X16# (# 0#Word8, 1#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Word8 where
  indexByteArraySIMD# ba i = MkWord8X2 (packWord8X16# (# GHC.Exts.indexWord8Array# ba i, GHC.Exts.indexWord8Array# ba (i +# 1#), 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord8X2 (packWord8X16# (# x0, x1, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #)) #)
  writeByteArraySIMD# mba i (MkWord8X2 v0) s0 = case unpackWord8X16# v0 of (# x0, x1, _, _, _, _, _, _, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeWord8Array# mba i x0 s0 of s1 -> GHC.Exts.writeWord8Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord8X2 (packWord8X16# (# x0, x1, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8, 0#Word8 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X2 v0) = IO (\s0 -> case unpackWord8X16# v0 of (# x0, x1, _, _, _, _, _, _, _, _, _, _, _, _, _, _ #) -> case GHC.Exts.writeWord8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 1#) x1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Word16 = MkWord16X2 Word16X8#
instance PackX2 X2 Word16 where
  mkX2 (W16# x0) (W16# x1) = MkWord16X2 (packWord16X8# (# x0, x1, 0#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16 #))
  unpackX2 (MkWord16X2 v0) = case unpackWord16X8# v0 of (# x0, x1, _, _, _, _, _, _ #) -> (W16# x0, W16# x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Word16 where
  broadcast (W16# x) = MkWord16X2 (broadcastWord16X8# x)
  {-# INLINE broadcast #-}
instance SelectableF X2 Word16 where
  selectF (MkBoolX2 !cond) (MkWord16X2 x0) (MkWord16X2 y0) = MkWord16X2 (selectWord16X8# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 2, ShuffleMany Word16X8# indices) => UnaryShuffle indices X2 Word16 where
  unaryShuffle (MkWord16X2 x) = MkWord16X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Word16X8# indices) => BinaryShuffle indices X2 Word16 where
  binaryShuffle (MkWord16X2 x0) (MkWord16X2 x1) = MkWord16X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word16 where
  eqF (MkWord16X2 u0) (MkWord16X2 v0) = MkBoolX2 $ (fromIntegral (eqWord16X8# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Word16 where
  ltF (MkWord16X2 u0) (MkWord16X2 v0) = MkBoolX2 $ (fromIntegral (ltWord16X8# u0 v0 .&. 0x3))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Word16 where
  minF (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (minWord16X8# u0 v0)
  maxF (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (maxWord16X8# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Word16 where
  plusF (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (plusWord16X8# u0 v0)
  minusF (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (minusWord16X8# u0 v0)
  timesF (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (timesWord16X8# u0 v0)
  -- Currently, there is no negateWord16X8#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X2 Word16 where
  andF (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (andWord16X8# u0 v0)
  orF (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (orWord16X8# u0 v0)
  xorF (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (xorWord16X8# u0 v0)
  complementF (MkWord16X2 u0) = MkWord16X2 (complementWord16X8# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Word16 where
  shiftLF (MkWord16X2 u0) (I# i) = MkWord16X2 (shiftLWord16X8# u0 i)
  shiftRF (MkWord16X2 u0) (I# i) = MkWord16X2 (shiftRWord16X8# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X2 Word16 where
  enumFromZero = MkWord16X2 (packWord16X8# (# 0#Word16, 1#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Word16 where
  indexByteArraySIMD# ba i = MkWord16X2 (packWord16X8# (# GHC.Exts.indexWord16Array# ba i, GHC.Exts.indexWord16Array# ba (i +# 1#), 0#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord16X2 (packWord16X8# (# x0, x1, 0#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16 #)) #)
  writeByteArraySIMD# mba i (MkWord16X2 v0) s0 = case unpackWord16X8# v0 of (# x0, x1, _, _, _, _, _, _ #) -> case GHC.Exts.writeWord16Array# mba i x0 s0 of s1 -> GHC.Exts.writeWord16Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord16X2 (packWord16X8# (# x0, x1, 0#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16, 0#Word16 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X2 v0) = IO (\s0 -> case unpackWord16X8# v0 of (# x0, x1, _, _, _, _, _, _ #) -> case GHC.Exts.writeWord16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 1#) x1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Word32 = MkWord32X2 Word32X4#
instance PackX2 X2 Word32 where
  mkX2 (W32# x0) (W32# x1) = MkWord32X2 (packWord32X4# (# x0, x1, 0#Word32, 0#Word32 #))
  unpackX2 (MkWord32X2 v0) = case unpackWord32X4# v0 of (# x0, x1, _, _ #) -> (W32# x0, W32# x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Word32 where
  broadcast (W32# x) = MkWord32X2 (broadcastWord32X4# x)
  {-# INLINE broadcast #-}
instance SelectableF X2 Word32 where
  selectF (MkBoolX2 !cond) (MkWord32X2 x0) (MkWord32X2 y0) = MkWord32X2 (selectWord32X4# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 2, ShuffleMany Word32X4# indices) => UnaryShuffle indices X2 Word32 where
  unaryShuffle (MkWord32X2 x) = MkWord32X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Word32X4# indices) => BinaryShuffle indices X2 Word32 where
  binaryShuffle (MkWord32X2 x0) (MkWord32X2 x1) = MkWord32X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word32 where
  eqF (MkWord32X2 u0) (MkWord32X2 v0) = MkBoolX2 $ (fromIntegral (eqWord32X4# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Word32 where
  ltF (MkWord32X2 u0) (MkWord32X2 v0) = MkBoolX2 $ (fromIntegral (ltWord32X4# u0 v0 .&. 0x3))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Word32 where
  minF (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (minWord32X4# u0 v0)
  maxF (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (maxWord32X4# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Word32 where
  plusF (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (plusWord32X4# u0 v0)
  minusF (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (minusWord32X4# u0 v0)
  timesF (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (timesWord32X4# u0 v0)
  -- Currently, there is no negateWord32X4#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X2 Word32 where
  andF (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (andWord32X4# u0 v0)
  orF (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (orWord32X4# u0 v0)
  xorF (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (xorWord32X4# u0 v0)
  complementF (MkWord32X2 u0) = MkWord32X2 (complementWord32X4# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Word32 where
  shiftLF (MkWord32X2 u0) (I# i) = MkWord32X2 (shiftLWord32X4# u0 i)
  shiftRF (MkWord32X2 u0) (I# i) = MkWord32X2 (shiftRWord32X4# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X2 Word32 where
  enumFromZero = MkWord32X2 (packWord32X4# (# 0#Word32, 1#Word32, 0#Word32, 0#Word32 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Word32 where
  indexByteArraySIMD# ba i = MkWord32X2 (packWord32X4# (# GHC.Exts.indexWord32Array# ba i, GHC.Exts.indexWord32Array# ba (i +# 1#), 0#Word32, 0#Word32 #))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord32X2 (packWord32X4# (# x0, x1, 0#Word32, 0#Word32 #)) #)
  writeByteArraySIMD# mba i (MkWord32X2 v0) s0 = case unpackWord32X4# v0 of (# x0, x1, _, _ #) -> case GHC.Exts.writeWord32Array# mba i x0 s0 of s1 -> GHC.Exts.writeWord32Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord32X2 (packWord32X4# (# x0, x1, 0#Word32, 0#Word32 #)) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X2 v0) = IO (\s0 -> case unpackWord32X4# v0 of (# x0, x1, _, _ #) -> case GHC.Exts.writeWord32OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 1#) x1 s1 of s2 -> (# s2, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Word64 = MkWord64X2 Word64X2#
instance PackX2 X2 Word64 where
  mkX2 (W64# x0) (W64# x1) = MkWord64X2 (packWord64X2# (# x0, x1 #))
  unpackX2 (MkWord64X2 v0) = case unpackWord64X2# v0 of (# x0, x1 #) -> (W64# x0, W64# x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Word64 where
  broadcast (W64# x) = MkWord64X2 (broadcastWord64X2# x)
  {-# INLINE broadcast #-}
instance SelectableF X2 Word64 where
  selectF (MkBoolX2 !cond) (MkWord64X2 x0) (MkWord64X2 y0) = MkWord64X2 (selectWord64X2# cond x0 y0)
  {-# INLINE selectF #-}
instance (AllLessThan indices 2, ShuffleMany Word64X2# indices) => UnaryShuffle indices X2 Word64 where
  unaryShuffle (MkWord64X2 x) = MkWord64X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Word64X2# indices) => BinaryShuffle indices X2 Word64 where
  binaryShuffle (MkWord64X2 x0) (MkWord64X2 x1) = MkWord64X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word64 where
  eqF (MkWord64X2 u0) (MkWord64X2 v0) = MkBoolX2 $ (fromIntegral (eqWord64X2# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X2 Word64 where
  ltF (MkWord64X2 u0) (MkWord64X2 v0) = MkBoolX2 $ (fromIntegral (ltWord64X2# u0 v0))
  leF !x !y = complementF (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = complementF (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Word64 where
  minF (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (minWord64X2# u0 v0)
  maxF (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (maxWord64X2# u0 v0)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Word64 where
  plusF (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (plusWord64X2# u0 v0)
  minusF (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (minusWord64X2# u0 v0)
  timesF (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (timesWord64X2# u0 v0)
  -- Currently, there is no negateWord64X2#
  absF x = x
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE absF #-}
instance BooleanF X2 Word64 where
  andF (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (andWord64X2# u0 v0)
  orF (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (orWord64X2# u0 v0)
  xorF (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (xorWord64X2# u0 v0)
  complementF (MkWord64X2 u0) = MkWord64X2 (complementWord64X2# u0)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Word64 where
  shiftLF (MkWord64X2 u0) (I# i) = MkWord64X2 (shiftLWord64X2# u0 i)
  shiftRF (MkWord64X2 u0) (I# i) = MkWord64X2 (shiftRWord64X2# u0 i)
  {-# INLINE shiftLF #-}
  {-# INLINE shiftRF #-}
instance EnumFromZero_ X2 Word64 where
  enumFromZero = MkWord64X2 (packWord64X2# (# 0#Word64, 1#Word64 #))
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Word64 where
  indexByteArraySIMD# ba i = MkWord64X2 (indexWord64ArrayAsWord64X2# ba i)
  readByteArraySIMD# mba i s0 = case readWord64ArrayAsWord64X2# mba i s0 of (# s1, v0 #) -> (# s1, MkWord64X2 v0 #)
  writeByteArraySIMD# mba i (MkWord64X2 v0) s0 = writeWord64ArrayAsWord64X2# mba i v0 s0
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case readWord64OffAddrAsWord64X2# addr i s0 of (# s1, v0 #) -> (# s1, MkWord64X2 v0 #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X2 v0) = IO (\s0 -> case writeWord64OffAddrAsWord64X2# addr i v0 s0 of s1 -> (# s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
#else
-- The NCG of GHC 9.12 does not support integer vectors
instance ImplementationDescription X2 where
  implementationDescription _ = "X2;maxBits(Float,Double)=128,maxBits(other)=0"
data instance X2 Int8 = MkInt8X2WithElems !Int8 !Int8
instance PackX2 X2 Int8 where
  mkX2 = MkInt8X2WithElems
  unpackX2 (MkInt8X2WithElems x0 x1) = (x0, x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Int8 where
  broadcast !x = MkInt8X2WithElems x x
  {-# INLINE broadcast #-}
instance SelectableF X2 Int8 where
  selectF (MkBoolX2 !cond) (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1)
instance (i0 < 2, i1 < 2, Pick Int8 i0, Pick Int8 i1) => UnaryShuffle [i0, i1] X2 Int8 where
  unaryShuffle (MkInt8X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt8X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Int8 i0, Pick Int8 i1) => BinaryShuffle [i0, i1] X2 Int8 where
  binaryShuffle (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt8X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int8 where
  eqF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = mkX2 (x0 == y0) (x1 == y1)
  {-# INLINE eqF #-}
instance OrderedF X2 Int8 where
  ltF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = mkX2 (x0 < y0) (x1 < y1)
  leF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = mkX2 (x0 <= y0) (x1 <= y1)
  gtF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = mkX2 (x0 > y0) (x1 > y1)
  geF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = mkX2 (x0 >= y0) (x1 >= y1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Int8 where
  minF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (min x0 y0) (min x1 y1)
  maxF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (max x0 y0) (max x1 y1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Int8 where
  plusF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkInt8X2WithElems x0 x1) = MkInt8X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X2 Int8 where
  andF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (x0 .&. y0) (x1 .&. y1)
  orF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (x0 .|. y0) (x1 .|. y1)
  xorF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (xor x0 y0) (xor x1 y1)
  complementF (MkInt8X2WithElems x0 x1) = MkInt8X2WithElems (complement x0) (complement x1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Int8 where
  shiftLF (MkInt8X2WithElems x0 x1) !i = MkInt8X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftLF (MkInt8X2WithElems x0 x1) !i = MkInt8X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftRF (MkInt8X2WithElems x0 x1) !i = MkInt8X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftRF (MkInt8X2WithElems x0 x1) !i = MkInt8X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X2 Int8 where
  enumFromZero = MkInt8X2WithElems 0 1
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Int8 where
  indexByteArraySIMD# ba i = MkInt8X2WithElems (I8# (GHC.Exts.indexInt8Array# ba i)) (I8# (GHC.Exts.indexInt8Array# ba (i +# 1#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt8X2WithElems (I8# x0) (I8# x1) #)
  writeByteArraySIMD# mba i (MkInt8X2WithElems (I8# x0) (I8# x1)) s0 = case GHC.Exts.writeInt8Array# mba i x0 s0 of s1 -> GHC.Exts.writeInt8Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt8X2WithElems (I8# x0) (I8# x1) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X2WithElems (I8# x0) (I8# x1)) = IO (\s0 -> case GHC.Exts.writeInt8OffAddr# addr i x0 s0 of s1 -> (# GHC.Exts.writeInt8OffAddr# addr (i +# 1#) x1 s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Int16 = MkInt16X2WithElems !Int16 !Int16
instance PackX2 X2 Int16 where
  mkX2 = MkInt16X2WithElems
  unpackX2 (MkInt16X2WithElems x0 x1) = (x0, x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Int16 where
  broadcast !x = MkInt16X2WithElems x x
  {-# INLINE broadcast #-}
instance SelectableF X2 Int16 where
  selectF (MkBoolX2 !cond) (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1)
instance (i0 < 2, i1 < 2, Pick Int16 i0, Pick Int16 i1) => UnaryShuffle [i0, i1] X2 Int16 where
  unaryShuffle (MkInt16X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt16X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Int16 i0, Pick Int16 i1) => BinaryShuffle [i0, i1] X2 Int16 where
  binaryShuffle (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt16X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int16 where
  eqF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = mkX2 (x0 == y0) (x1 == y1)
  {-# INLINE eqF #-}
instance OrderedF X2 Int16 where
  ltF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = mkX2 (x0 < y0) (x1 < y1)
  leF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = mkX2 (x0 <= y0) (x1 <= y1)
  gtF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = mkX2 (x0 > y0) (x1 > y1)
  geF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = mkX2 (x0 >= y0) (x1 >= y1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Int16 where
  minF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (min x0 y0) (min x1 y1)
  maxF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (max x0 y0) (max x1 y1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Int16 where
  plusF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkInt16X2WithElems x0 x1) = MkInt16X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X2 Int16 where
  andF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (x0 .&. y0) (x1 .&. y1)
  orF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (x0 .|. y0) (x1 .|. y1)
  xorF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (xor x0 y0) (xor x1 y1)
  complementF (MkInt16X2WithElems x0 x1) = MkInt16X2WithElems (complement x0) (complement x1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Int16 where
  shiftLF (MkInt16X2WithElems x0 x1) !i = MkInt16X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftLF (MkInt16X2WithElems x0 x1) !i = MkInt16X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftRF (MkInt16X2WithElems x0 x1) !i = MkInt16X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftRF (MkInt16X2WithElems x0 x1) !i = MkInt16X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X2 Int16 where
  enumFromZero = MkInt16X2WithElems 0 1
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Int16 where
  indexByteArraySIMD# ba i = MkInt16X2WithElems (I16# (GHC.Exts.indexInt16Array# ba i)) (I16# (GHC.Exts.indexInt16Array# ba (i +# 1#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt16X2WithElems (I16# x0) (I16# x1) #)
  writeByteArraySIMD# mba i (MkInt16X2WithElems (I16# x0) (I16# x1)) s0 = case GHC.Exts.writeInt16Array# mba i x0 s0 of s1 -> GHC.Exts.writeInt16Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt16X2WithElems (I16# x0) (I16# x1) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X2WithElems (I16# x0) (I16# x1)) = IO (\s0 -> case GHC.Exts.writeInt16OffAddr# addr i x0 s0 of s1 -> (# GHC.Exts.writeInt16OffAddr# addr (i +# 1#) x1 s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Int32 = MkInt32X2WithElems !Int32 !Int32
instance PackX2 X2 Int32 where
  mkX2 = MkInt32X2WithElems
  unpackX2 (MkInt32X2WithElems x0 x1) = (x0, x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Int32 where
  broadcast !x = MkInt32X2WithElems x x
  {-# INLINE broadcast #-}
instance SelectableF X2 Int32 where
  selectF (MkBoolX2 !cond) (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1)
instance (i0 < 2, i1 < 2, Pick Int32 i0, Pick Int32 i1) => UnaryShuffle [i0, i1] X2 Int32 where
  unaryShuffle (MkInt32X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt32X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Int32 i0, Pick Int32 i1) => BinaryShuffle [i0, i1] X2 Int32 where
  binaryShuffle (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt32X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int32 where
  eqF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = mkX2 (x0 == y0) (x1 == y1)
  {-# INLINE eqF #-}
instance OrderedF X2 Int32 where
  ltF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = mkX2 (x0 < y0) (x1 < y1)
  leF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = mkX2 (x0 <= y0) (x1 <= y1)
  gtF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = mkX2 (x0 > y0) (x1 > y1)
  geF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = mkX2 (x0 >= y0) (x1 >= y1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Int32 where
  minF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (min x0 y0) (min x1 y1)
  maxF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (max x0 y0) (max x1 y1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Int32 where
  plusF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkInt32X2WithElems x0 x1) = MkInt32X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X2 Int32 where
  andF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (x0 .&. y0) (x1 .&. y1)
  orF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (x0 .|. y0) (x1 .|. y1)
  xorF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (xor x0 y0) (xor x1 y1)
  complementF (MkInt32X2WithElems x0 x1) = MkInt32X2WithElems (complement x0) (complement x1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Int32 where
  shiftLF (MkInt32X2WithElems x0 x1) !i = MkInt32X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftLF (MkInt32X2WithElems x0 x1) !i = MkInt32X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftRF (MkInt32X2WithElems x0 x1) !i = MkInt32X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftRF (MkInt32X2WithElems x0 x1) !i = MkInt32X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X2 Int32 where
  enumFromZero = MkInt32X2WithElems 0 1
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Int32 where
  indexByteArraySIMD# ba i = MkInt32X2WithElems (I32# (GHC.Exts.indexInt32Array# ba i)) (I32# (GHC.Exts.indexInt32Array# ba (i +# 1#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt32X2WithElems (I32# x0) (I32# x1) #)
  writeByteArraySIMD# mba i (MkInt32X2WithElems (I32# x0) (I32# x1)) s0 = case GHC.Exts.writeInt32Array# mba i x0 s0 of s1 -> GHC.Exts.writeInt32Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt32X2WithElems (I32# x0) (I32# x1) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X2WithElems (I32# x0) (I32# x1)) = IO (\s0 -> case GHC.Exts.writeInt32OffAddr# addr i x0 s0 of s1 -> (# GHC.Exts.writeInt32OffAddr# addr (i +# 1#) x1 s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Int64 = MkInt64X2WithElems !Int64 !Int64
instance PackX2 X2 Int64 where
  mkX2 = MkInt64X2WithElems
  unpackX2 (MkInt64X2WithElems x0 x1) = (x0, x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Int64 where
  broadcast !x = MkInt64X2WithElems x x
  {-# INLINE broadcast #-}
instance SelectableF X2 Int64 where
  selectF (MkBoolX2 !cond) (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1)
instance (i0 < 2, i1 < 2, Pick Int64 i0, Pick Int64 i1) => UnaryShuffle [i0, i1] X2 Int64 where
  unaryShuffle (MkInt64X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt64X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Int64 i0, Pick Int64 i1) => BinaryShuffle [i0, i1] X2 Int64 where
  binaryShuffle (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt64X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int64 where
  eqF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = mkX2 (x0 == y0) (x1 == y1)
  {-# INLINE eqF #-}
instance OrderedF X2 Int64 where
  ltF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = mkX2 (x0 < y0) (x1 < y1)
  leF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = mkX2 (x0 <= y0) (x1 <= y1)
  gtF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = mkX2 (x0 > y0) (x1 > y1)
  geF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = mkX2 (x0 >= y0) (x1 >= y1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Int64 where
  minF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (min x0 y0) (min x1 y1)
  maxF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (max x0 y0) (max x1 y1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Int64 where
  plusF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkInt64X2WithElems x0 x1) = MkInt64X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X2 Int64 where
  andF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (x0 .&. y0) (x1 .&. y1)
  orF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (x0 .|. y0) (x1 .|. y1)
  xorF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (xor x0 y0) (xor x1 y1)
  complementF (MkInt64X2WithElems x0 x1) = MkInt64X2WithElems (complement x0) (complement x1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Int64 where
  shiftLF (MkInt64X2WithElems x0 x1) !i = MkInt64X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftLF (MkInt64X2WithElems x0 x1) !i = MkInt64X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftRF (MkInt64X2WithElems x0 x1) !i = MkInt64X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftRF (MkInt64X2WithElems x0 x1) !i = MkInt64X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X2 Int64 where
  enumFromZero = MkInt64X2WithElems 0 1
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Int64 where
  indexByteArraySIMD# ba i = MkInt64X2WithElems (I64# (GHC.Exts.indexInt64Array# ba i)) (I64# (GHC.Exts.indexInt64Array# ba (i +# 1#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt64Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt64Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt64X2WithElems (I64# x0) (I64# x1) #)
  writeByteArraySIMD# mba i (MkInt64X2WithElems (I64# x0) (I64# x1)) s0 = case GHC.Exts.writeInt64Array# mba i x0 s0 of s1 -> GHC.Exts.writeInt64Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt64OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkInt64X2WithElems (I64# x0) (I64# x1) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X2WithElems (I64# x0) (I64# x1)) = IO (\s0 -> case GHC.Exts.writeInt64OffAddr# addr i x0 s0 of s1 -> (# GHC.Exts.writeInt64OffAddr# addr (i +# 1#) x1 s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Word8 = MkWord8X2WithElems !Word8 !Word8
instance PackX2 X2 Word8 where
  mkX2 = MkWord8X2WithElems
  unpackX2 (MkWord8X2WithElems x0 x1) = (x0, x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Word8 where
  broadcast !x = MkWord8X2WithElems x x
  {-# INLINE broadcast #-}
instance SelectableF X2 Word8 where
  selectF (MkBoolX2 !cond) (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1)
instance (i0 < 2, i1 < 2, Pick Word8 i0, Pick Word8 i1) => UnaryShuffle [i0, i1] X2 Word8 where
  unaryShuffle (MkWord8X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord8X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Word8 i0, Pick Word8 i1) => BinaryShuffle [i0, i1] X2 Word8 where
  binaryShuffle (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord8X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word8 where
  eqF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = mkX2 (x0 == y0) (x1 == y1)
  {-# INLINE eqF #-}
instance OrderedF X2 Word8 where
  ltF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = mkX2 (x0 < y0) (x1 < y1)
  leF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = mkX2 (x0 <= y0) (x1 <= y1)
  gtF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = mkX2 (x0 > y0) (x1 > y1)
  geF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = mkX2 (x0 >= y0) (x1 >= y1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Word8 where
  minF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (min x0 y0) (min x1 y1)
  maxF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (max x0 y0) (max x1 y1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Word8 where
  plusF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkWord8X2WithElems x0 x1) = MkWord8X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X2 Word8 where
  andF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (x0 .&. y0) (x1 .&. y1)
  orF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (x0 .|. y0) (x1 .|. y1)
  xorF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (xor x0 y0) (xor x1 y1)
  complementF (MkWord8X2WithElems x0 x1) = MkWord8X2WithElems (complement x0) (complement x1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Word8 where
  shiftLF (MkWord8X2WithElems x0 x1) !i = MkWord8X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftLF (MkWord8X2WithElems x0 x1) !i = MkWord8X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftRF (MkWord8X2WithElems x0 x1) !i = MkWord8X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftRF (MkWord8X2WithElems x0 x1) !i = MkWord8X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X2 Word8 where
  enumFromZero = MkWord8X2WithElems 0 1
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Word8 where
  indexByteArraySIMD# ba i = MkWord8X2WithElems (W8# (GHC.Exts.indexWord8Array# ba i)) (W8# (GHC.Exts.indexWord8Array# ba (i +# 1#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord8X2WithElems (W8# x0) (W8# x1) #)
  writeByteArraySIMD# mba i (MkWord8X2WithElems (W8# x0) (W8# x1)) s0 = case GHC.Exts.writeWord8Array# mba i x0 s0 of s1 -> GHC.Exts.writeWord8Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord8X2WithElems (W8# x0) (W8# x1) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X2WithElems (W8# x0) (W8# x1)) = IO (\s0 -> case GHC.Exts.writeWord8OffAddr# addr i x0 s0 of s1 -> (# GHC.Exts.writeWord8OffAddr# addr (i +# 1#) x1 s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Word16 = MkWord16X2WithElems !Word16 !Word16
instance PackX2 X2 Word16 where
  mkX2 = MkWord16X2WithElems
  unpackX2 (MkWord16X2WithElems x0 x1) = (x0, x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Word16 where
  broadcast !x = MkWord16X2WithElems x x
  {-# INLINE broadcast #-}
instance SelectableF X2 Word16 where
  selectF (MkBoolX2 !cond) (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1)
instance (i0 < 2, i1 < 2, Pick Word16 i0, Pick Word16 i1) => UnaryShuffle [i0, i1] X2 Word16 where
  unaryShuffle (MkWord16X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord16X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Word16 i0, Pick Word16 i1) => BinaryShuffle [i0, i1] X2 Word16 where
  binaryShuffle (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord16X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word16 where
  eqF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = mkX2 (x0 == y0) (x1 == y1)
  {-# INLINE eqF #-}
instance OrderedF X2 Word16 where
  ltF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = mkX2 (x0 < y0) (x1 < y1)
  leF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = mkX2 (x0 <= y0) (x1 <= y1)
  gtF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = mkX2 (x0 > y0) (x1 > y1)
  geF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = mkX2 (x0 >= y0) (x1 >= y1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Word16 where
  minF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (min x0 y0) (min x1 y1)
  maxF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (max x0 y0) (max x1 y1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Word16 where
  plusF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkWord16X2WithElems x0 x1) = MkWord16X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X2 Word16 where
  andF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (x0 .&. y0) (x1 .&. y1)
  orF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (x0 .|. y0) (x1 .|. y1)
  xorF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (xor x0 y0) (xor x1 y1)
  complementF (MkWord16X2WithElems x0 x1) = MkWord16X2WithElems (complement x0) (complement x1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Word16 where
  shiftLF (MkWord16X2WithElems x0 x1) !i = MkWord16X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftLF (MkWord16X2WithElems x0 x1) !i = MkWord16X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftRF (MkWord16X2WithElems x0 x1) !i = MkWord16X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftRF (MkWord16X2WithElems x0 x1) !i = MkWord16X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X2 Word16 where
  enumFromZero = MkWord16X2WithElems 0 1
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Word16 where
  indexByteArraySIMD# ba i = MkWord16X2WithElems (W16# (GHC.Exts.indexWord16Array# ba i)) (W16# (GHC.Exts.indexWord16Array# ba (i +# 1#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord16X2WithElems (W16# x0) (W16# x1) #)
  writeByteArraySIMD# mba i (MkWord16X2WithElems (W16# x0) (W16# x1)) s0 = case GHC.Exts.writeWord16Array# mba i x0 s0 of s1 -> GHC.Exts.writeWord16Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord16X2WithElems (W16# x0) (W16# x1) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X2WithElems (W16# x0) (W16# x1)) = IO (\s0 -> case GHC.Exts.writeWord16OffAddr# addr i x0 s0 of s1 -> (# GHC.Exts.writeWord16OffAddr# addr (i +# 1#) x1 s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Word32 = MkWord32X2WithElems !Word32 !Word32
instance PackX2 X2 Word32 where
  mkX2 = MkWord32X2WithElems
  unpackX2 (MkWord32X2WithElems x0 x1) = (x0, x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Word32 where
  broadcast !x = MkWord32X2WithElems x x
  {-# INLINE broadcast #-}
instance SelectableF X2 Word32 where
  selectF (MkBoolX2 !cond) (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1)
instance (i0 < 2, i1 < 2, Pick Word32 i0, Pick Word32 i1) => UnaryShuffle [i0, i1] X2 Word32 where
  unaryShuffle (MkWord32X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord32X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Word32 i0, Pick Word32 i1) => BinaryShuffle [i0, i1] X2 Word32 where
  binaryShuffle (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord32X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word32 where
  eqF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = mkX2 (x0 == y0) (x1 == y1)
  {-# INLINE eqF #-}
instance OrderedF X2 Word32 where
  ltF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = mkX2 (x0 < y0) (x1 < y1)
  leF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = mkX2 (x0 <= y0) (x1 <= y1)
  gtF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = mkX2 (x0 > y0) (x1 > y1)
  geF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = mkX2 (x0 >= y0) (x1 >= y1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Word32 where
  minF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (min x0 y0) (min x1 y1)
  maxF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (max x0 y0) (max x1 y1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Word32 where
  plusF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkWord32X2WithElems x0 x1) = MkWord32X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X2 Word32 where
  andF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (x0 .&. y0) (x1 .&. y1)
  orF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (x0 .|. y0) (x1 .|. y1)
  xorF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (xor x0 y0) (xor x1 y1)
  complementF (MkWord32X2WithElems x0 x1) = MkWord32X2WithElems (complement x0) (complement x1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Word32 where
  shiftLF (MkWord32X2WithElems x0 x1) !i = MkWord32X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftLF (MkWord32X2WithElems x0 x1) !i = MkWord32X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftRF (MkWord32X2WithElems x0 x1) !i = MkWord32X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftRF (MkWord32X2WithElems x0 x1) !i = MkWord32X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X2 Word32 where
  enumFromZero = MkWord32X2WithElems 0 1
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Word32 where
  indexByteArraySIMD# ba i = MkWord32X2WithElems (W32# (GHC.Exts.indexWord32Array# ba i)) (W32# (GHC.Exts.indexWord32Array# ba (i +# 1#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord32X2WithElems (W32# x0) (W32# x1) #)
  writeByteArraySIMD# mba i (MkWord32X2WithElems (W32# x0) (W32# x1)) s0 = case GHC.Exts.writeWord32Array# mba i x0 s0 of s1 -> GHC.Exts.writeWord32Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord32X2WithElems (W32# x0) (W32# x1) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X2WithElems (W32# x0) (W32# x1)) = IO (\s0 -> case GHC.Exts.writeWord32OffAddr# addr i x0 s0 of s1 -> (# GHC.Exts.writeWord32OffAddr# addr (i +# 1#) x1 s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Word64 = MkWord64X2WithElems !Word64 !Word64
instance PackX2 X2 Word64 where
  mkX2 = MkWord64X2WithElems
  unpackX2 (MkWord64X2WithElems x0 x1) = (x0, x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Word64 where
  broadcast !x = MkWord64X2WithElems x x
  {-# INLINE broadcast #-}
instance SelectableF X2 Word64 where
  selectF (MkBoolX2 !cond) (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1)
instance (i0 < 2, i1 < 2, Pick Word64 i0, Pick Word64 i1) => UnaryShuffle [i0, i1] X2 Word64 where
  unaryShuffle (MkWord64X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord64X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Word64 i0, Pick Word64 i1) => BinaryShuffle [i0, i1] X2 Word64 where
  binaryShuffle (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord64X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word64 where
  eqF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = mkX2 (x0 == y0) (x1 == y1)
  {-# INLINE eqF #-}
instance OrderedF X2 Word64 where
  ltF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = mkX2 (x0 < y0) (x1 < y1)
  leF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = mkX2 (x0 <= y0) (x1 <= y1)
  gtF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = mkX2 (x0 > y0) (x1 > y1)
  geF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = mkX2 (x0 >= y0) (x1 >= y1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Word64 where
  minF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (min x0 y0) (min x1 y1)
  maxF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (max x0 y0) (max x1 y1)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X2 Word64 where
  plusF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkWord64X2WithElems x0 x1) = MkWord64X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X2 Word64 where
  andF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (x0 .&. y0) (x1 .&. y1)
  orF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (x0 .|. y0) (x1 .|. y1)
  xorF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (xor x0 y0) (xor x1 y1)
  complementF (MkWord64X2WithElems x0 x1) = MkWord64X2WithElems (complement x0) (complement x1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X2 Word64 where
  shiftLF (MkWord64X2WithElems x0 x1) !i = MkWord64X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftLF (MkWord64X2WithElems x0 x1) !i = MkWord64X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftRF (MkWord64X2WithElems x0 x1) !i = MkWord64X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftRF (MkWord64X2WithElems x0 x1) !i = MkWord64X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X2 Word64 where
  enumFromZero = MkWord64X2WithElems 0 1
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Word64 where
  indexByteArraySIMD# ba i = MkWord64X2WithElems (W64# (GHC.Exts.indexWord64Array# ba i)) (W64# (GHC.Exts.indexWord64Array# ba (i +# 1#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord64Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord64Array# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord64X2WithElems (W64# x0) (W64# x1) #)
  writeByteArraySIMD# mba i (MkWord64X2WithElems (W64# x0) (W64# x1)) s0 = case GHC.Exts.writeWord64Array# mba i x0 s0 of s1 -> GHC.Exts.writeWord64Array# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord64OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkWord64X2WithElems (W64# x0) (W64# x1) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X2WithElems (W64# x0) (W64# x1)) = IO (\s0 -> case GHC.Exts.writeWord64OffAddr# addr i x0 s0 of s1 -> (# GHC.Exts.writeWord64OffAddr# addr (i +# 1#) x1 s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
#endif
newtype instance X2 (Sum a) = MkSumX2 (X2 a)
instance PackX2 X2 a => PackX2 X2 (Sum a) where
  mkX2 = coerce (mkX2 @X2 @a)
  unpackX2 = coerce (unpackX2 @X2 @a)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 a => Broadcast X2 (Sum a) where
  broadcast = coerce (broadcast @X2 @a)
  {-# INLINE broadcast #-}
instance SelectableF X2 a => SelectableF X2 (Sum a) where
  selectF = coerce (selectF @X2 @a)
  {-# INLINE selectF #-}
newtype instance X2 (Product a) = MkProductX2 (X2 a)
instance PackX2 X2 a => PackX2 X2 (Product a) where
  mkX2 = coerce (mkX2 @X2 @a)
  unpackX2 = coerce (unpackX2 @X2 @a)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 a => Broadcast X2 (Product a) where
  broadcast = coerce (broadcast @X2 @a)
  {-# INLINE broadcast #-}
instance SelectableF X2 a => SelectableF X2 (Product a) where
  selectF = coerce (selectF @X2 @a)
  {-# INLINE selectF #-}
newtype instance X2 (Min a) = MkMinX2 (X2 a)
instance PackX2 X2 a => PackX2 X2 (Min a) where
  mkX2 = coerce (mkX2 @X2 @a)
  unpackX2 = coerce (unpackX2 @X2 @a)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 a => Broadcast X2 (Min a) where
  broadcast = coerce (broadcast @X2 @a)
  {-# INLINE broadcast #-}
instance SelectableF X2 a => SelectableF X2 (Min a) where
  selectF = coerce (selectF @X2 @a)
  {-# INLINE selectF #-}
newtype instance X2 (Max a) = MkMaxX2 (X2 a)
instance PackX2 X2 a => PackX2 X2 (Max a) where
  mkX2 = coerce (mkX2 @X2 @a)
  unpackX2 = coerce (unpackX2 @X2 @a)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 a => Broadcast X2 (Max a) where
  broadcast = coerce (broadcast @X2 @a)
  {-# INLINE broadcast #-}
instance SelectableF X2 a => SelectableF X2 (Max a) where
  selectF = coerce (selectF @X2 @a)
  {-# INLINE selectF #-}
data instance X2 (Complex a) = MkComplexX2 !(X2 a) !(X2 a)
instance PackX2 X2 a => PackX2 X2 (Complex a) where
  mkX2 (x0 :+ y0) (x1 :+ y1) = MkComplexX2 (mkX2 x0 x1) (mkX2 y0 y1)
  unpackX2 (MkComplexX2 s t) = case unpackX2 s of (x0, x1) -> case unpackX2 t of (y0, y1) -> (x0 :+ y0, x1 :+ y1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 a => Broadcast X2 (Complex a) where
  broadcast (x :+ y) = MkComplexX2 (broadcast x) (broadcast y)
  {-# INLINE broadcast #-}
instance SelectableF X2 a => SelectableF X2 (Complex a) where
  selectF !cond (MkComplexX2 x y) (MkComplexX2 x' y') = MkComplexX2 (selectF cond x x') (selectF cond y y')
  {-# INLINE selectF #-}
data instance X2 () = MkUnitX2
instance PackX2 X2 () where
  mkX2 _ _ = MkUnitX2
  unpackX2 MkUnitX2 = ((), ())
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 () where
  broadcast _ = MkUnitX2
  {-# INLINE broadcast #-}
instance SelectableF X2 () where
  selectF _ _ _ = MkUnitX2
  {-# INLINE selectF #-}
data instance X2 (a0, a1) = MkTuple2X2 !(X2 a0) !(X2 a1)
instance (PackX2 X2 a0, PackX2 X2 a1) => PackX2 X2 (a0, a1) where
  mkX2 (x0_0, x0_1) (x1_0, x1_1) = MkTuple2X2 (mkX2 x0_0 x1_0) (mkX2 x0_1 x1_1)
  unpackX2 (MkTuple2X2 v0 v1) = case unpackX2 v0 of (x0_0, x1_0) -> case unpackX2 v1 of (x0_1, x1_1) -> ((x0_0, x0_1), (x1_0, x1_1))
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance (Broadcast X2 a0, Broadcast X2 a1) => Broadcast X2 (a0, a1) where
  broadcast (x0, x1) = MkTuple2X2 (broadcast x0) (broadcast x1)
  {-# INLINE broadcast #-}
instance (SelectableF X2 a0, SelectableF X2 a1) => SelectableF X2 (a0, a1) where
  selectF !cond (MkTuple2X2 x0 x1) (MkTuple2X2 y0 y1) = MkTuple2X2 (selectF cond x0 y0) (selectF cond x1 y1)
  {-# INLINE selectF #-}
data instance X2 (a0, a1, a2) = MkTuple3X2 !(X2 a0) !(X2 a1) !(X2 a2)
instance (PackX2 X2 a0, PackX2 X2 a1, PackX2 X2 a2) => PackX2 X2 (a0, a1, a2) where
  mkX2 (x0_0, x0_1, x0_2) (x1_0, x1_1, x1_2) = MkTuple3X2 (mkX2 x0_0 x1_0) (mkX2 x0_1 x1_1) (mkX2 x0_2 x1_2)
  unpackX2 (MkTuple3X2 v0 v1 v2) = case unpackX2 v0 of (x0_0, x1_0) -> case unpackX2 v1 of (x0_1, x1_1) -> case unpackX2 v2 of (x0_2, x1_2) -> ((x0_0, x0_1, x0_2), (x1_0, x1_1, x1_2))
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance (Broadcast X2 a0, Broadcast X2 a1, Broadcast X2 a2) => Broadcast X2 (a0, a1, a2) where
  broadcast (x0, x1, x2) = MkTuple3X2 (broadcast x0) (broadcast x1) (broadcast x2)
  {-# INLINE broadcast #-}
instance (SelectableF X2 a0, SelectableF X2 a1, SelectableF X2 a2) => SelectableF X2 (a0, a1, a2) where
  selectF !cond (MkTuple3X2 x0 x1 x2) (MkTuple3X2 y0 y1 y2) = MkTuple3X2 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2)
  {-# INLINE selectF #-}
data instance X2 (a0, a1, a2, a3) = MkTuple4X2 !(X2 a0) !(X2 a1) !(X2 a2) !(X2 a3)
instance (PackX2 X2 a0, PackX2 X2 a1, PackX2 X2 a2, PackX2 X2 a3) => PackX2 X2 (a0, a1, a2, a3) where
  mkX2 (x0_0, x0_1, x0_2, x0_3) (x1_0, x1_1, x1_2, x1_3) = MkTuple4X2 (mkX2 x0_0 x1_0) (mkX2 x0_1 x1_1) (mkX2 x0_2 x1_2) (mkX2 x0_3 x1_3)
  unpackX2 (MkTuple4X2 v0 v1 v2 v3) = case unpackX2 v0 of (x0_0, x1_0) -> case unpackX2 v1 of (x0_1, x1_1) -> case unpackX2 v2 of (x0_2, x1_2) -> case unpackX2 v3 of (x0_3, x1_3) -> ((x0_0, x0_1, x0_2, x0_3), (x1_0, x1_1, x1_2, x1_3))
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance (Broadcast X2 a0, Broadcast X2 a1, Broadcast X2 a2, Broadcast X2 a3) => Broadcast X2 (a0, a1, a2, a3) where
  broadcast (x0, x1, x2, x3) = MkTuple4X2 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3)
  {-# INLINE broadcast #-}
instance (SelectableF X2 a0, SelectableF X2 a1, SelectableF X2 a2, SelectableF X2 a3) => SelectableF X2 (a0, a1, a2, a3) where
  selectF !cond (MkTuple4X2 x0 x1 x2 x3) (MkTuple4X2 y0 y1 y2 y3) = MkTuple4X2 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3)
  {-# INLINE selectF #-}
data instance X2 (a0, a1, a2, a3, a4) = MkTuple5X2 !(X2 a0) !(X2 a1) !(X2 a2) !(X2 a3) !(X2 a4)
instance (PackX2 X2 a0, PackX2 X2 a1, PackX2 X2 a2, PackX2 X2 a3, PackX2 X2 a4) => PackX2 X2 (a0, a1, a2, a3, a4) where
  mkX2 (x0_0, x0_1, x0_2, x0_3, x0_4) (x1_0, x1_1, x1_2, x1_3, x1_4) = MkTuple5X2 (mkX2 x0_0 x1_0) (mkX2 x0_1 x1_1) (mkX2 x0_2 x1_2) (mkX2 x0_3 x1_3) (mkX2 x0_4 x1_4)
  unpackX2 (MkTuple5X2 v0 v1 v2 v3 v4) = case unpackX2 v0 of (x0_0, x1_0) -> case unpackX2 v1 of (x0_1, x1_1) -> case unpackX2 v2 of (x0_2, x1_2) -> case unpackX2 v3 of (x0_3, x1_3) -> case unpackX2 v4 of (x0_4, x1_4) -> ((x0_0, x0_1, x0_2, x0_3, x0_4), (x1_0, x1_1, x1_2, x1_3, x1_4))
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance (Broadcast X2 a0, Broadcast X2 a1, Broadcast X2 a2, Broadcast X2 a3, Broadcast X2 a4) => Broadcast X2 (a0, a1, a2, a3, a4) where
  broadcast (x0, x1, x2, x3, x4) = MkTuple5X2 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4)
  {-# INLINE broadcast #-}
instance (SelectableF X2 a0, SelectableF X2 a1, SelectableF X2 a2, SelectableF X2 a3, SelectableF X2 a4) => SelectableF X2 (a0, a1, a2, a3, a4) where
  selectF !cond (MkTuple5X2 x0 x1 x2 x3 x4) (MkTuple5X2 y0 y1 y2 y3 y4) = MkTuple5X2 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4)
  {-# INLINE selectF #-}
data instance X2 (a0, a1, a2, a3, a4, a5) = MkTuple6X2 !(X2 a0) !(X2 a1) !(X2 a2) !(X2 a3) !(X2 a4) !(X2 a5)
instance (PackX2 X2 a0, PackX2 X2 a1, PackX2 X2 a2, PackX2 X2 a3, PackX2 X2 a4, PackX2 X2 a5) => PackX2 X2 (a0, a1, a2, a3, a4, a5) where
  mkX2 (x0_0, x0_1, x0_2, x0_3, x0_4, x0_5) (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5) = MkTuple6X2 (mkX2 x0_0 x1_0) (mkX2 x0_1 x1_1) (mkX2 x0_2 x1_2) (mkX2 x0_3 x1_3) (mkX2 x0_4 x1_4) (mkX2 x0_5 x1_5)
  unpackX2 (MkTuple6X2 v0 v1 v2 v3 v4 v5) = case unpackX2 v0 of (x0_0, x1_0) -> case unpackX2 v1 of (x0_1, x1_1) -> case unpackX2 v2 of (x0_2, x1_2) -> case unpackX2 v3 of (x0_3, x1_3) -> case unpackX2 v4 of (x0_4, x1_4) -> case unpackX2 v5 of (x0_5, x1_5) -> ((x0_0, x0_1, x0_2, x0_3, x0_4, x0_5), (x1_0, x1_1, x1_2, x1_3, x1_4, x1_5))
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance (Broadcast X2 a0, Broadcast X2 a1, Broadcast X2 a2, Broadcast X2 a3, Broadcast X2 a4, Broadcast X2 a5) => Broadcast X2 (a0, a1, a2, a3, a4, a5) where
  broadcast (x0, x1, x2, x3, x4, x5) = MkTuple6X2 (broadcast x0) (broadcast x1) (broadcast x2) (broadcast x3) (broadcast x4) (broadcast x5)
  {-# INLINE broadcast #-}
instance (SelectableF X2 a0, SelectableF X2 a1, SelectableF X2 a2, SelectableF X2 a3, SelectableF X2 a4, SelectableF X2 a5) => SelectableF X2 (a0, a1, a2, a3, a4, a5) where
  selectF !cond (MkTuple6X2 x0 x1 x2 x3 x4 x5) (MkTuple6X2 y0 y1 y2 y3 y4 y5) = MkTuple6X2 (selectF cond x0 y0) (selectF cond x1 y1) (selectF cond x2 y2) (selectF cond x3 y3) (selectF cond x4 y4) (selectF cond x5 y5)
  {-# INLINE selectF #-}
instance (PackX2 X2 a, PackX2 X2 b) => LiftSIMD X2 a b where
  liftSIMD f !v = case unpackX2 v of (x0, x1) -> mkX2 (f x0) (f x1)
  {-# INLINE liftSIMD #-}
instance (PackX2 X2 a, PackX2 X2 b, PackX2 X2 c) => LiftSIMD2 X2 a b c where
  liftSIMD2 f !u !v = case unpackX2 u of (x0, x1) -> case unpackX2 v of (y0, y1) -> mkX2 (f x0 y0) (f x1 y1)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X2 where
  mkTuple2 = MkTuple2X2
  mkTuple3 = MkTuple3X2
  mkTuple4 = MkTuple4X2
  mkTuple5 = MkTuple5X2
  mkTuple6 = MkTuple6X2
  deconstructTuple2 (MkTuple2X2 v0 v1) = (v0, v1)
  deconstructTuple3 (MkTuple3X2 v0 v1 v2) = (v0, v1, v2)
  deconstructTuple4 (MkTuple4X2 v0 v1 v2 v3) = (v0, v1, v2, v3)
  deconstructTuple5 (MkTuple5X2 v0 v1 v2 v3 v4) = (v0, v1, v2, v3, v4)
  deconstructTuple6 (MkTuple6X2 v0 v1 v2 v3 v4 v5) = (v0, v1, v2, v3, v4, v5)
  mkSum = coerce
  getSum' = coerce
  mkProduct = coerce
  getProduct' = coerce
  mkMin = coerce
  getMin' = coerce
  mkMax = coerce
  getMax' = coerce
  mkComplex = MkComplexX2
  deconstructComplex (MkComplexX2 x y) = (x, y)
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
deriving via WrappedMulti X2 a instance SelectableF X2 a => Selectable (X2 a)
deriving via WrappedMulti X2 a instance NumF X2 a => Num (X2 a)
deriving via WrappedMulti X2 a instance FractionalF X2 a => Fractional (X2 a)
deriving via WrappedMulti X2 a instance FloatingF X2 a => Floating (X2 a)
deriving via WrappedMulti X2 a instance BooleanF X2 a => Boolean (X2 a)
deriving via WrappedMulti X2 a instance BitShiftF X2 a => BitShift (X2 a)
deriving via WrappedMulti X2 a instance MinMaxF X2 a => MinMax (X2 a)
deriving via WrappedMulti X2 a instance FusedMultiplyAddF X2 a => FusedMultiplyAdd (X2 a)
