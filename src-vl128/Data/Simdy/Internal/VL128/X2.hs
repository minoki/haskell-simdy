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
import           Data.Default (Default (def))
import           Data.Maybe (isJust, fromMaybe)
import           Data.Monoid
import           Data.Semigroup
import qualified Data.Simdy.Fusible as F
import           Data.Simdy.Internal.Bits (Boolean, BitShift)
import qualified Data.Simdy.Internal.Bits as B
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.Shuffle
import           Data.Type.Ord (type (<))
import           Data.Simdy.Internal.VL128.Prim
import           Data.Simdy.Internal.VL128.PrimExtra
import qualified GHC.Exts
import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..), Word8#, Word16#, Word32#, Word64#)
import           GHC.Int
import           GHC.IO
import           GHC.TypeError (Unsatisfiable, ErrorMessage (Text))
import           GHC.Word
import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=), min, max)
import qualified Prelude
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
instance Boolean (X2 Bool) where
  (MkBoolX2 x) .&. (MkBoolX2 y) = MkBoolX2 (x .&. y)
  (MkBoolX2 x) .|. (MkBoolX2 y) = MkBoolX2 (x .|. y)
  xor (MkBoolX2 x) (MkBoolX2 y) = MkBoolX2 (xor x y)
  complement (MkBoolX2 x) = MkBoolX2 (0x3 - x)
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
instance BooleanReduction X2 where
  horizontalAndBool (MkBoolX2 !cond) = cond Prelude.== 0x3
  horizontalOrBool (MkBoolX2 !cond) = cond Prelude./= 0
  {-# INLINE horizontalAndBool #-}
  {-# INLINE horizontalOrBool #-}
instance PackMask X2 where
  maskToNatural (MkBoolX2 x) = fromIntegral x
  maskFromNatural x = MkBoolX2 (fromIntegral x .&. 0x3)
  maskToInteger (MkBoolX2 x) = toInteger x
  maskFromInteger x = MkBoolX2 (fromInteger x .&. 0x3)
  {-# INLINE maskToNatural #-}
  {-# INLINE maskFromNatural #-}
  {-# INLINE maskToInteger #-}
  {-# INLINE maskFromInteger #-}
instance PackMask8 X2 where
  maskToWord8 = coerce
  maskFromWord8 x = MkBoolX2 (x .&. 0x3)
  {-# INLINE maskToWord8 #-}
  {-# INLINE maskFromWord8 #-}
instance (AllLessThan indices 2, ShuffleMany Word8# indices) => UnaryShuffleT indices X2 Bool where
  unaryShuffle (MkBoolX2 (W8# x)) = MkBoolX2 (W8# (shuffleMany# @_ @_ @indices (\_ -> x)))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Word8# indices) => BinaryShuffleT indices X2 Bool where
  binaryShuffle (MkBoolX2 (W8# x0)) (MkBoolX2 (W8# x1)) = MkBoolX2 (W8# (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 })))
  {-# INLINE binaryShuffle #-}
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
instance (AllLessThan indices 2, ShuffleMany FloatX4# indices) => UnaryShuffleT indices X2 Float where
  unaryShuffle (MkFloatX2 x) = MkFloatX2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany FloatX4# indices) => BinaryShuffleT indices X2 Float where
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
instance MinMax (X2 Float) where
  type MinMaxIsCompatibleWithOrd (X2 Float) = Unsatisfiable (Text "This instance is not compatible with the min/max functions in Ord")
  min (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (minimumFloatX4# u0 v0)
  max (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (maximumFloatX4# u0 v0)
  minimumNumber (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (minimumNumberFloatX4# u0 v0)
  maximumNumber (MkFloatX2 u0) (MkFloatX2 v0) = MkFloatX2 (maximumNumberFloatX4# u0 v0)
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
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
instance HasFMA => FusedMultiplyAdd (X2 Float) where
#if defined(USE_FMA)
  fusedMultiplyAdd (MkFloatX2 u0) (MkFloatX2 v0) (MkFloatX2 w0) = MkFloatX2 (fmaddFloatX4# u0 v0 w0)
  {-# INLINE fusedMultiplyAdd #-}
#else
  fusedMultiplyAdd _ _ _ = fmaIsDisabled
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
instance (AllLessThan indices 2, ShuffleMany DoubleX2# indices) => UnaryShuffleT indices X2 Double where
  unaryShuffle (MkDoubleX2 x) = MkDoubleX2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany DoubleX2# indices) => BinaryShuffleT indices X2 Double where
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
instance MinMax (X2 Double) where
  type MinMaxIsCompatibleWithOrd (X2 Double) = Unsatisfiable (Text "This instance is not compatible with the min/max functions in Ord")
  min (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (minimumDoubleX2# u0 v0)
  max (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (maximumDoubleX2# u0 v0)
  minimumNumber (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (minimumNumberDoubleX2# u0 v0)
  maximumNumber (MkDoubleX2 u0) (MkDoubleX2 v0) = MkDoubleX2 (maximumNumberDoubleX2# u0 v0)
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
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
instance HasFMA => FusedMultiplyAdd (X2 Double) where
#if defined(USE_FMA)
  fusedMultiplyAdd (MkDoubleX2 u0) (MkDoubleX2 v0) (MkDoubleX2 w0) = MkDoubleX2 (fmaddDoubleX2# u0 v0 w0)
  {-# INLINE fusedMultiplyAdd #-}
#else
  fusedMultiplyAdd _ _ _ = fmaIsDisabled
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
#if MIN_VERSION_GLASGOW_HASKELL(9, 14, 0, 0) || defined(__GLASGOW_HASKELL_LLVM__) || defined(USE_LLVM)
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
instance (AllLessThan indices 2, ShuffleMany Int8X16# indices) => UnaryShuffleT indices X2 Int8 where
  unaryShuffle (MkInt8X2 x) = MkInt8X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Int8X16# indices) => BinaryShuffleT indices X2 Int8 where
  binaryShuffle (MkInt8X2 x0) (MkInt8X2 x1) = MkInt8X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int8 where
  eqF (MkInt8X2 u0) (MkInt8X2 v0) = MkBoolX2 $ (fromIntegral (eqInt8X16# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Int8 where
  ltF (MkInt8X2 u0) (MkInt8X2 v0) = MkBoolX2 $ (fromIntegral (ltInt8X16# u0 v0 .&. 0x3))
  leF !x !y = B.complement (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = B.complement (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMax (X2 Int8) where
  type MinMaxIsCompatibleWithOrd (X2 Int8) = ()
  min (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (minInt8X16# u0 v0)
  max (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (maxInt8X16# u0 v0)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
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
instance Boolean (X2 Int8) where
  (MkInt8X2 u0) .&. (MkInt8X2 v0) = MkInt8X2 (andInt8X16# u0 v0)
  (MkInt8X2 u0) .|. (MkInt8X2 v0) = MkInt8X2 (orInt8X16# u0 v0)
  xor (MkInt8X2 u0) (MkInt8X2 v0) = MkInt8X2 (xorInt8X16# u0 v0)
  complement (MkInt8X2 u0) = MkInt8X2 (complementInt8X16# u0)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Int8) where
  shiftL (MkInt8X2 u0) (I# i) = MkInt8X2 (shiftLInt8X16# u0 i)
  shiftR (MkInt8X2 u0) (I# i) = MkInt8X2 (shiftRInt8X16# u0 i)
  {-# INLINE shiftL #-}
  {-# INLINE shiftR #-}
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
instance (AllLessThan indices 2, ShuffleMany Int16X8# indices) => UnaryShuffleT indices X2 Int16 where
  unaryShuffle (MkInt16X2 x) = MkInt16X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Int16X8# indices) => BinaryShuffleT indices X2 Int16 where
  binaryShuffle (MkInt16X2 x0) (MkInt16X2 x1) = MkInt16X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int16 where
  eqF (MkInt16X2 u0) (MkInt16X2 v0) = MkBoolX2 $ (fromIntegral (eqInt16X8# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Int16 where
  ltF (MkInt16X2 u0) (MkInt16X2 v0) = MkBoolX2 $ (fromIntegral (ltInt16X8# u0 v0 .&. 0x3))
  leF !x !y = B.complement (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = B.complement (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMax (X2 Int16) where
  type MinMaxIsCompatibleWithOrd (X2 Int16) = ()
  min (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (minInt16X8# u0 v0)
  max (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (maxInt16X8# u0 v0)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
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
instance Boolean (X2 Int16) where
  (MkInt16X2 u0) .&. (MkInt16X2 v0) = MkInt16X2 (andInt16X8# u0 v0)
  (MkInt16X2 u0) .|. (MkInt16X2 v0) = MkInt16X2 (orInt16X8# u0 v0)
  xor (MkInt16X2 u0) (MkInt16X2 v0) = MkInt16X2 (xorInt16X8# u0 v0)
  complement (MkInt16X2 u0) = MkInt16X2 (complementInt16X8# u0)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Int16) where
  shiftL (MkInt16X2 u0) (I# i) = MkInt16X2 (shiftLInt16X8# u0 i)
  shiftR (MkInt16X2 u0) (I# i) = MkInt16X2 (shiftRInt16X8# u0 i)
  {-# INLINE shiftL #-}
  {-# INLINE shiftR #-}
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
instance (AllLessThan indices 2, ShuffleMany Int32X4# indices) => UnaryShuffleT indices X2 Int32 where
  unaryShuffle (MkInt32X2 x) = MkInt32X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Int32X4# indices) => BinaryShuffleT indices X2 Int32 where
  binaryShuffle (MkInt32X2 x0) (MkInt32X2 x1) = MkInt32X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int32 where
  eqF (MkInt32X2 u0) (MkInt32X2 v0) = MkBoolX2 $ (fromIntegral (eqInt32X4# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Int32 where
  ltF (MkInt32X2 u0) (MkInt32X2 v0) = MkBoolX2 $ (fromIntegral (ltInt32X4# u0 v0 .&. 0x3))
  leF !x !y = B.complement (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = B.complement (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMax (X2 Int32) where
  type MinMaxIsCompatibleWithOrd (X2 Int32) = ()
  min (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (minInt32X4# u0 v0)
  max (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (maxInt32X4# u0 v0)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
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
instance Boolean (X2 Int32) where
  (MkInt32X2 u0) .&. (MkInt32X2 v0) = MkInt32X2 (andInt32X4# u0 v0)
  (MkInt32X2 u0) .|. (MkInt32X2 v0) = MkInt32X2 (orInt32X4# u0 v0)
  xor (MkInt32X2 u0) (MkInt32X2 v0) = MkInt32X2 (xorInt32X4# u0 v0)
  complement (MkInt32X2 u0) = MkInt32X2 (complementInt32X4# u0)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Int32) where
  shiftL (MkInt32X2 u0) (I# i) = MkInt32X2 (shiftLInt32X4# u0 i)
  shiftR (MkInt32X2 u0) (I# i) = MkInt32X2 (shiftRInt32X4# u0 i)
  {-# INLINE shiftL #-}
  {-# INLINE shiftR #-}
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
instance (AllLessThan indices 2, ShuffleMany Int64X2# indices) => UnaryShuffleT indices X2 Int64 where
  unaryShuffle (MkInt64X2 x) = MkInt64X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Int64X2# indices) => BinaryShuffleT indices X2 Int64 where
  binaryShuffle (MkInt64X2 x0) (MkInt64X2 x1) = MkInt64X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Int64 where
  eqF (MkInt64X2 u0) (MkInt64X2 v0) = MkBoolX2 $ (fromIntegral (eqInt64X2# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X2 Int64 where
  ltF (MkInt64X2 u0) (MkInt64X2 v0) = MkBoolX2 $ (fromIntegral (ltInt64X2# u0 v0))
  leF !x !y = B.complement (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = B.complement (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMax (X2 Int64) where
  type MinMaxIsCompatibleWithOrd (X2 Int64) = ()
  min (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (minInt64X2# u0 v0)
  max (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (maxInt64X2# u0 v0)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
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
instance Boolean (X2 Int64) where
  (MkInt64X2 u0) .&. (MkInt64X2 v0) = MkInt64X2 (andInt64X2# u0 v0)
  (MkInt64X2 u0) .|. (MkInt64X2 v0) = MkInt64X2 (orInt64X2# u0 v0)
  xor (MkInt64X2 u0) (MkInt64X2 v0) = MkInt64X2 (xorInt64X2# u0 v0)
  complement (MkInt64X2 u0) = MkInt64X2 (complementInt64X2# u0)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Int64) where
  shiftL (MkInt64X2 u0) (I# i) = MkInt64X2 (shiftLInt64X2# u0 i)
  shiftR (MkInt64X2 u0) (I# i) = MkInt64X2 (shiftRInt64X2# u0 i)
  {-# INLINE shiftL #-}
  {-# INLINE shiftR #-}
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
instance (AllLessThan indices 2, ShuffleMany Word8X16# indices) => UnaryShuffleT indices X2 Word8 where
  unaryShuffle (MkWord8X2 x) = MkWord8X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Word8X16# indices) => BinaryShuffleT indices X2 Word8 where
  binaryShuffle (MkWord8X2 x0) (MkWord8X2 x1) = MkWord8X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word8 where
  eqF (MkWord8X2 u0) (MkWord8X2 v0) = MkBoolX2 $ (fromIntegral (eqWord8X16# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Word8 where
  ltF (MkWord8X2 u0) (MkWord8X2 v0) = MkBoolX2 $ (fromIntegral (ltWord8X16# u0 v0 .&. 0x3))
  leF !x !y = B.complement (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = B.complement (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMax (X2 Word8) where
  type MinMaxIsCompatibleWithOrd (X2 Word8) = ()
  min (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (minWord8X16# u0 v0)
  max (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (maxWord8X16# u0 v0)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
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
instance Boolean (X2 Word8) where
  (MkWord8X2 u0) .&. (MkWord8X2 v0) = MkWord8X2 (andWord8X16# u0 v0)
  (MkWord8X2 u0) .|. (MkWord8X2 v0) = MkWord8X2 (orWord8X16# u0 v0)
  xor (MkWord8X2 u0) (MkWord8X2 v0) = MkWord8X2 (xorWord8X16# u0 v0)
  complement (MkWord8X2 u0) = MkWord8X2 (complementWord8X16# u0)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Word8) where
  shiftL (MkWord8X2 u0) (I# i) = MkWord8X2 (shiftLWord8X16# u0 i)
  shiftR (MkWord8X2 u0) (I# i) = MkWord8X2 (shiftRWord8X16# u0 i)
  {-# INLINE shiftL #-}
  {-# INLINE shiftR #-}
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
instance (AllLessThan indices 2, ShuffleMany Word16X8# indices) => UnaryShuffleT indices X2 Word16 where
  unaryShuffle (MkWord16X2 x) = MkWord16X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Word16X8# indices) => BinaryShuffleT indices X2 Word16 where
  binaryShuffle (MkWord16X2 x0) (MkWord16X2 x1) = MkWord16X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word16 where
  eqF (MkWord16X2 u0) (MkWord16X2 v0) = MkBoolX2 $ (fromIntegral (eqWord16X8# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Word16 where
  ltF (MkWord16X2 u0) (MkWord16X2 v0) = MkBoolX2 $ (fromIntegral (ltWord16X8# u0 v0 .&. 0x3))
  leF !x !y = B.complement (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = B.complement (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMax (X2 Word16) where
  type MinMaxIsCompatibleWithOrd (X2 Word16) = ()
  min (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (minWord16X8# u0 v0)
  max (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (maxWord16X8# u0 v0)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
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
instance Boolean (X2 Word16) where
  (MkWord16X2 u0) .&. (MkWord16X2 v0) = MkWord16X2 (andWord16X8# u0 v0)
  (MkWord16X2 u0) .|. (MkWord16X2 v0) = MkWord16X2 (orWord16X8# u0 v0)
  xor (MkWord16X2 u0) (MkWord16X2 v0) = MkWord16X2 (xorWord16X8# u0 v0)
  complement (MkWord16X2 u0) = MkWord16X2 (complementWord16X8# u0)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Word16) where
  shiftL (MkWord16X2 u0) (I# i) = MkWord16X2 (shiftLWord16X8# u0 i)
  shiftR (MkWord16X2 u0) (I# i) = MkWord16X2 (shiftRWord16X8# u0 i)
  {-# INLINE shiftL #-}
  {-# INLINE shiftR #-}
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
instance (AllLessThan indices 2, ShuffleMany Word32X4# indices) => UnaryShuffleT indices X2 Word32 where
  unaryShuffle (MkWord32X2 x) = MkWord32X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Word32X4# indices) => BinaryShuffleT indices X2 Word32 where
  binaryShuffle (MkWord32X2 x0) (MkWord32X2 x1) = MkWord32X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word32 where
  eqF (MkWord32X2 u0) (MkWord32X2 v0) = MkBoolX2 $ (fromIntegral (eqWord32X4# u0 v0 .&. 0x3))
  {-# INLINE eqF #-}
instance OrderedF X2 Word32 where
  ltF (MkWord32X2 u0) (MkWord32X2 v0) = MkBoolX2 $ (fromIntegral (ltWord32X4# u0 v0 .&. 0x3))
  leF !x !y = B.complement (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = B.complement (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMax (X2 Word32) where
  type MinMaxIsCompatibleWithOrd (X2 Word32) = ()
  min (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (minWord32X4# u0 v0)
  max (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (maxWord32X4# u0 v0)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
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
instance Boolean (X2 Word32) where
  (MkWord32X2 u0) .&. (MkWord32X2 v0) = MkWord32X2 (andWord32X4# u0 v0)
  (MkWord32X2 u0) .|. (MkWord32X2 v0) = MkWord32X2 (orWord32X4# u0 v0)
  xor (MkWord32X2 u0) (MkWord32X2 v0) = MkWord32X2 (xorWord32X4# u0 v0)
  complement (MkWord32X2 u0) = MkWord32X2 (complementWord32X4# u0)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Word32) where
  shiftL (MkWord32X2 u0) (I# i) = MkWord32X2 (shiftLWord32X4# u0 i)
  shiftR (MkWord32X2 u0) (I# i) = MkWord32X2 (shiftRWord32X4# u0 i)
  {-# INLINE shiftL #-}
  {-# INLINE shiftR #-}
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
instance (AllLessThan indices 2, ShuffleMany Word64X2# indices) => UnaryShuffleT indices X2 Word64 where
  unaryShuffle (MkWord64X2 x) = MkWord64X2 (shuffleMany# @_ @_ @indices (\_ -> x))
  {-# INLINE unaryShuffle #-}
instance (AllLessThan indices 4, ShuffleMany Word64X2# indices) => BinaryShuffleT indices X2 Word64 where
  binaryShuffle (MkWord64X2 x0) (MkWord64X2 x1) = MkWord64X2 (shuffleMany# @_ @_ @indices (\case { 0 -> x0; _ -> x1 }))
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Word64 where
  eqF (MkWord64X2 u0) (MkWord64X2 v0) = MkBoolX2 $ (fromIntegral (eqWord64X2# u0 v0))
  {-# INLINE eqF #-}
instance OrderedF X2 Word64 where
  ltF (MkWord64X2 u0) (MkWord64X2 v0) = MkBoolX2 $ (fromIntegral (ltWord64X2# u0 v0))
  leF !x !y = B.complement (ltF y x)
  gtF !x !y = ltF y x
  geF !x !y = B.complement (ltF x y)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMax (X2 Word64) where
  type MinMaxIsCompatibleWithOrd (X2 Word64) = ()
  min (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (minWord64X2# u0 v0)
  max (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (maxWord64X2# u0 v0)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
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
instance Boolean (X2 Word64) where
  (MkWord64X2 u0) .&. (MkWord64X2 v0) = MkWord64X2 (andWord64X2# u0 v0)
  (MkWord64X2 u0) .|. (MkWord64X2 v0) = MkWord64X2 (orWord64X2# u0 v0)
  xor (MkWord64X2 u0) (MkWord64X2 v0) = MkWord64X2 (xorWord64X2# u0 v0)
  complement (MkWord64X2 u0) = MkWord64X2 (complementWord64X2# u0)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Word64) where
  shiftL (MkWord64X2 u0) (I# i) = MkWord64X2 (shiftLWord64X2# u0 i)
  shiftR (MkWord64X2 u0) (I# i) = MkWord64X2 (shiftRWord64X2# u0 i)
  {-# INLINE shiftL #-}
  {-# INLINE shiftR #-}
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
instance (i0 < 2, i1 < 2, Pick Int8 i0, Pick Int8 i1) => UnaryShuffleT [i0, i1] X2 Int8 where
  unaryShuffle (MkInt8X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt8X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Int8 i0, Pick Int8 i1) => BinaryShuffleT [i0, i1] X2 Int8 where
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
instance MinMax (X2 Int8) where
  type MinMaxIsCompatibleWithOrd (X2 Int8) = ()
  min (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (min x0 y0) (min x1 y1)
  max (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (max x0 y0) (max x1 y1)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
instance NumF X2 Int8 where
  plusF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkInt8X2WithElems x0 x1) = MkInt8X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance Boolean (X2 Int8) where
  (MkInt8X2WithElems x0 x1) .&. (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (x0 .&. y0) (x1 .&. y1)
  (MkInt8X2WithElems x0 x1) .|. (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (x0 .|. y0) (x1 .|. y1)
  xor (MkInt8X2WithElems x0 x1) (MkInt8X2WithElems y0 y1) = MkInt8X2WithElems (xor x0 y0) (xor x1 y1)
  complement (MkInt8X2WithElems x0 x1) = MkInt8X2WithElems (complement x0) (complement x1)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Int8) where
  shiftL (MkInt8X2WithElems x0 x1) !i = MkInt8X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftL (MkInt8X2WithElems x0 x1) !i = MkInt8X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftR (MkInt8X2WithElems x0 x1) !i = MkInt8X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftR (MkInt8X2WithElems x0 x1) !i = MkInt8X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftL #-}
  {-# INLINE unsafeShiftL #-}
  {-# INLINE shiftR #-}
  {-# INLINE unsafeShiftR #-}
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
instance (i0 < 2, i1 < 2, Pick Int16 i0, Pick Int16 i1) => UnaryShuffleT [i0, i1] X2 Int16 where
  unaryShuffle (MkInt16X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt16X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Int16 i0, Pick Int16 i1) => BinaryShuffleT [i0, i1] X2 Int16 where
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
instance MinMax (X2 Int16) where
  type MinMaxIsCompatibleWithOrd (X2 Int16) = ()
  min (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (min x0 y0) (min x1 y1)
  max (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (max x0 y0) (max x1 y1)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
instance NumF X2 Int16 where
  plusF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkInt16X2WithElems x0 x1) = MkInt16X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance Boolean (X2 Int16) where
  (MkInt16X2WithElems x0 x1) .&. (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (x0 .&. y0) (x1 .&. y1)
  (MkInt16X2WithElems x0 x1) .|. (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (x0 .|. y0) (x1 .|. y1)
  xor (MkInt16X2WithElems x0 x1) (MkInt16X2WithElems y0 y1) = MkInt16X2WithElems (xor x0 y0) (xor x1 y1)
  complement (MkInt16X2WithElems x0 x1) = MkInt16X2WithElems (complement x0) (complement x1)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Int16) where
  shiftL (MkInt16X2WithElems x0 x1) !i = MkInt16X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftL (MkInt16X2WithElems x0 x1) !i = MkInt16X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftR (MkInt16X2WithElems x0 x1) !i = MkInt16X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftR (MkInt16X2WithElems x0 x1) !i = MkInt16X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftL #-}
  {-# INLINE unsafeShiftL #-}
  {-# INLINE shiftR #-}
  {-# INLINE unsafeShiftR #-}
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
instance (i0 < 2, i1 < 2, Pick Int32 i0, Pick Int32 i1) => UnaryShuffleT [i0, i1] X2 Int32 where
  unaryShuffle (MkInt32X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt32X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Int32 i0, Pick Int32 i1) => BinaryShuffleT [i0, i1] X2 Int32 where
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
instance MinMax (X2 Int32) where
  type MinMaxIsCompatibleWithOrd (X2 Int32) = ()
  min (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (min x0 y0) (min x1 y1)
  max (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (max x0 y0) (max x1 y1)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
instance NumF X2 Int32 where
  plusF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkInt32X2WithElems x0 x1) = MkInt32X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance Boolean (X2 Int32) where
  (MkInt32X2WithElems x0 x1) .&. (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (x0 .&. y0) (x1 .&. y1)
  (MkInt32X2WithElems x0 x1) .|. (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (x0 .|. y0) (x1 .|. y1)
  xor (MkInt32X2WithElems x0 x1) (MkInt32X2WithElems y0 y1) = MkInt32X2WithElems (xor x0 y0) (xor x1 y1)
  complement (MkInt32X2WithElems x0 x1) = MkInt32X2WithElems (complement x0) (complement x1)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Int32) where
  shiftL (MkInt32X2WithElems x0 x1) !i = MkInt32X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftL (MkInt32X2WithElems x0 x1) !i = MkInt32X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftR (MkInt32X2WithElems x0 x1) !i = MkInt32X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftR (MkInt32X2WithElems x0 x1) !i = MkInt32X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftL #-}
  {-# INLINE unsafeShiftL #-}
  {-# INLINE shiftR #-}
  {-# INLINE unsafeShiftR #-}
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
instance (i0 < 2, i1 < 2, Pick Int64 i0, Pick Int64 i1) => UnaryShuffleT [i0, i1] X2 Int64 where
  unaryShuffle (MkInt64X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkInt64X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Int64 i0, Pick Int64 i1) => BinaryShuffleT [i0, i1] X2 Int64 where
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
instance MinMax (X2 Int64) where
  type MinMaxIsCompatibleWithOrd (X2 Int64) = ()
  min (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (min x0 y0) (min x1 y1)
  max (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (max x0 y0) (max x1 y1)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
instance NumF X2 Int64 where
  plusF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkInt64X2WithElems x0 x1) = MkInt64X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance Boolean (X2 Int64) where
  (MkInt64X2WithElems x0 x1) .&. (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (x0 .&. y0) (x1 .&. y1)
  (MkInt64X2WithElems x0 x1) .|. (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (x0 .|. y0) (x1 .|. y1)
  xor (MkInt64X2WithElems x0 x1) (MkInt64X2WithElems y0 y1) = MkInt64X2WithElems (xor x0 y0) (xor x1 y1)
  complement (MkInt64X2WithElems x0 x1) = MkInt64X2WithElems (complement x0) (complement x1)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Int64) where
  shiftL (MkInt64X2WithElems x0 x1) !i = MkInt64X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftL (MkInt64X2WithElems x0 x1) !i = MkInt64X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftR (MkInt64X2WithElems x0 x1) !i = MkInt64X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftR (MkInt64X2WithElems x0 x1) !i = MkInt64X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftL #-}
  {-# INLINE unsafeShiftL #-}
  {-# INLINE shiftR #-}
  {-# INLINE unsafeShiftR #-}
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
instance (i0 < 2, i1 < 2, Pick Word8 i0, Pick Word8 i1) => UnaryShuffleT [i0, i1] X2 Word8 where
  unaryShuffle (MkWord8X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord8X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Word8 i0, Pick Word8 i1) => BinaryShuffleT [i0, i1] X2 Word8 where
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
instance MinMax (X2 Word8) where
  type MinMaxIsCompatibleWithOrd (X2 Word8) = ()
  min (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (min x0 y0) (min x1 y1)
  max (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (max x0 y0) (max x1 y1)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
instance NumF X2 Word8 where
  plusF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkWord8X2WithElems x0 x1) = MkWord8X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance Boolean (X2 Word8) where
  (MkWord8X2WithElems x0 x1) .&. (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (x0 .&. y0) (x1 .&. y1)
  (MkWord8X2WithElems x0 x1) .|. (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (x0 .|. y0) (x1 .|. y1)
  xor (MkWord8X2WithElems x0 x1) (MkWord8X2WithElems y0 y1) = MkWord8X2WithElems (xor x0 y0) (xor x1 y1)
  complement (MkWord8X2WithElems x0 x1) = MkWord8X2WithElems (complement x0) (complement x1)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Word8) where
  shiftL (MkWord8X2WithElems x0 x1) !i = MkWord8X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftL (MkWord8X2WithElems x0 x1) !i = MkWord8X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftR (MkWord8X2WithElems x0 x1) !i = MkWord8X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftR (MkWord8X2WithElems x0 x1) !i = MkWord8X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftL #-}
  {-# INLINE unsafeShiftL #-}
  {-# INLINE shiftR #-}
  {-# INLINE unsafeShiftR #-}
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
instance (i0 < 2, i1 < 2, Pick Word16 i0, Pick Word16 i1) => UnaryShuffleT [i0, i1] X2 Word16 where
  unaryShuffle (MkWord16X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord16X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Word16 i0, Pick Word16 i1) => BinaryShuffleT [i0, i1] X2 Word16 where
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
instance MinMax (X2 Word16) where
  type MinMaxIsCompatibleWithOrd (X2 Word16) = ()
  min (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (min x0 y0) (min x1 y1)
  max (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (max x0 y0) (max x1 y1)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
instance NumF X2 Word16 where
  plusF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkWord16X2WithElems x0 x1) = MkWord16X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance Boolean (X2 Word16) where
  (MkWord16X2WithElems x0 x1) .&. (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (x0 .&. y0) (x1 .&. y1)
  (MkWord16X2WithElems x0 x1) .|. (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (x0 .|. y0) (x1 .|. y1)
  xor (MkWord16X2WithElems x0 x1) (MkWord16X2WithElems y0 y1) = MkWord16X2WithElems (xor x0 y0) (xor x1 y1)
  complement (MkWord16X2WithElems x0 x1) = MkWord16X2WithElems (complement x0) (complement x1)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Word16) where
  shiftL (MkWord16X2WithElems x0 x1) !i = MkWord16X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftL (MkWord16X2WithElems x0 x1) !i = MkWord16X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftR (MkWord16X2WithElems x0 x1) !i = MkWord16X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftR (MkWord16X2WithElems x0 x1) !i = MkWord16X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftL #-}
  {-# INLINE unsafeShiftL #-}
  {-# INLINE shiftR #-}
  {-# INLINE unsafeShiftR #-}
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
instance (i0 < 2, i1 < 2, Pick Word32 i0, Pick Word32 i1) => UnaryShuffleT [i0, i1] X2 Word32 where
  unaryShuffle (MkWord32X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord32X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Word32 i0, Pick Word32 i1) => BinaryShuffleT [i0, i1] X2 Word32 where
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
instance MinMax (X2 Word32) where
  type MinMaxIsCompatibleWithOrd (X2 Word32) = ()
  min (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (min x0 y0) (min x1 y1)
  max (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (max x0 y0) (max x1 y1)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
instance NumF X2 Word32 where
  plusF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkWord32X2WithElems x0 x1) = MkWord32X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance Boolean (X2 Word32) where
  (MkWord32X2WithElems x0 x1) .&. (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (x0 .&. y0) (x1 .&. y1)
  (MkWord32X2WithElems x0 x1) .|. (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (x0 .|. y0) (x1 .|. y1)
  xor (MkWord32X2WithElems x0 x1) (MkWord32X2WithElems y0 y1) = MkWord32X2WithElems (xor x0 y0) (xor x1 y1)
  complement (MkWord32X2WithElems x0 x1) = MkWord32X2WithElems (complement x0) (complement x1)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Word32) where
  shiftL (MkWord32X2WithElems x0 x1) !i = MkWord32X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftL (MkWord32X2WithElems x0 x1) !i = MkWord32X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftR (MkWord32X2WithElems x0 x1) !i = MkWord32X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftR (MkWord32X2WithElems x0 x1) !i = MkWord32X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftL #-}
  {-# INLINE unsafeShiftL #-}
  {-# INLINE shiftR #-}
  {-# INLINE unsafeShiftR #-}
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
instance (i0 < 2, i1 < 2, Pick Word64 i0, Pick Word64 i1) => UnaryShuffleT [i0, i1] X2 Word64 where
  unaryShuffle (MkWord64X2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkWord64X2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Word64 i0, Pick Word64 i1) => BinaryShuffleT [i0, i1] X2 Word64 where
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
instance MinMax (X2 Word64) where
  type MinMaxIsCompatibleWithOrd (X2 Word64) = ()
  min (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (min x0 y0) (min x1 y1)
  max (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (max x0 y0) (max x1 y1)
  minimumNumber = min
  maximumNumber = max
  {-# INLINE min #-}
  {-# INLINE max #-}
  {-# INLINE minimumNumber #-}
  {-# INLINE maximumNumber #-}
instance NumF X2 Word64 where
  plusF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (x0 + y0) (x1 + y1)
  minusF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (x0 - y0) (x1 - y1)
  timesF (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (x0 * y0) (x1 * y1)
  negateF (MkWord64X2WithElems x0 x1) = MkWord64X2WithElems (- x0) (- x1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance Boolean (X2 Word64) where
  (MkWord64X2WithElems x0 x1) .&. (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (x0 .&. y0) (x1 .&. y1)
  (MkWord64X2WithElems x0 x1) .|. (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (x0 .|. y0) (x1 .|. y1)
  xor (MkWord64X2WithElems x0 x1) (MkWord64X2WithElems y0 y1) = MkWord64X2WithElems (xor x0 y0) (xor x1 y1)
  complement (MkWord64X2WithElems x0 x1) = MkWord64X2WithElems (complement x0) (complement x1)
  {-# INLINE (.&.) #-}
  {-# INLINE (.|.) #-}
  {-# INLINE xor #-}
  {-# INLINE complement #-}
instance BitShift (X2 Word64) where
  shiftL (MkWord64X2WithElems x0 x1) !i = MkWord64X2WithElems (shiftL x0 i) (shiftL x1 i)
  unsafeShiftL (MkWord64X2WithElems x0 x1) !i = MkWord64X2WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i)
  shiftR (MkWord64X2WithElems x0 x1) !i = MkWord64X2WithElems (shiftR x0 i) (shiftR x1 i)
  unsafeShiftR (MkWord64X2WithElems x0 x1) !i = MkWord64X2WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i)
  {-# INLINE shiftL #-}
  {-# INLINE unsafeShiftL #-}
  {-# INLINE shiftR #-}
  {-# INLINE unsafeShiftR #-}
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
instance UnaryShuffleT indices X2 a => UnaryShuffleT indices X2 (Sum a) where
  unaryShuffle = coerce (unaryShuffle @indices @X2 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 a => BinaryShuffleT indices X2 (Sum a) where
  binaryShuffle = coerce (binaryShuffle @indices @X2 @a)
  {-# INLINE binaryShuffle #-}
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
instance UnaryShuffleT indices X2 a => UnaryShuffleT indices X2 (Product a) where
  unaryShuffle = coerce (unaryShuffle @indices @X2 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 a => BinaryShuffleT indices X2 (Product a) where
  binaryShuffle = coerce (binaryShuffle @indices @X2 @a)
  {-# INLINE binaryShuffle #-}
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
instance UnaryShuffleT indices X2 a => UnaryShuffleT indices X2 (Min a) where
  unaryShuffle = coerce (unaryShuffle @indices @X2 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 a => BinaryShuffleT indices X2 (Min a) where
  binaryShuffle = coerce (binaryShuffle @indices @X2 @a)
  {-# INLINE binaryShuffle #-}
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
instance UnaryShuffleT indices X2 a => UnaryShuffleT indices X2 (Max a) where
  unaryShuffle = coerce (unaryShuffle @indices @X2 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 a => BinaryShuffleT indices X2 (Max a) where
  binaryShuffle = coerce (binaryShuffle @indices @X2 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X2 (IEEEMin a) = MkIEEEMinX2 (X2 a)
instance PackX2 X2 a => PackX2 X2 (IEEEMin a) where
  mkX2 = coerce (mkX2 @X2 @a)
  unpackX2 = coerce (unpackX2 @X2 @a)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 a => Broadcast X2 (IEEEMin a) where
  broadcast = coerce (broadcast @X2 @a)
  {-# INLINE broadcast #-}
instance SelectableF X2 a => SelectableF X2 (IEEEMin a) where
  selectF = coerce (selectF @X2 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X2 a => UnaryShuffleT indices X2 (IEEEMin a) where
  unaryShuffle = coerce (unaryShuffle @indices @X2 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 a => BinaryShuffleT indices X2 (IEEEMin a) where
  binaryShuffle = coerce (binaryShuffle @indices @X2 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X2 (IEEEMax a) = MkIEEEMaxX2 (X2 a)
instance PackX2 X2 a => PackX2 X2 (IEEEMax a) where
  mkX2 = coerce (mkX2 @X2 @a)
  unpackX2 = coerce (unpackX2 @X2 @a)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 a => Broadcast X2 (IEEEMax a) where
  broadcast = coerce (broadcast @X2 @a)
  {-# INLINE broadcast #-}
instance SelectableF X2 a => SelectableF X2 (IEEEMax a) where
  selectF = coerce (selectF @X2 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X2 a => UnaryShuffleT indices X2 (IEEEMax a) where
  unaryShuffle = coerce (unaryShuffle @indices @X2 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 a => BinaryShuffleT indices X2 (IEEEMax a) where
  binaryShuffle = coerce (binaryShuffle @indices @X2 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X2 (MinimumNumber a) = MkMinimumNumberX2 (X2 a)
instance PackX2 X2 a => PackX2 X2 (MinimumNumber a) where
  mkX2 = coerce (mkX2 @X2 @a)
  unpackX2 = coerce (unpackX2 @X2 @a)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 a => Broadcast X2 (MinimumNumber a) where
  broadcast = coerce (broadcast @X2 @a)
  {-# INLINE broadcast #-}
instance SelectableF X2 a => SelectableF X2 (MinimumNumber a) where
  selectF = coerce (selectF @X2 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X2 a => UnaryShuffleT indices X2 (MinimumNumber a) where
  unaryShuffle = coerce (unaryShuffle @indices @X2 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 a => BinaryShuffleT indices X2 (MinimumNumber a) where
  binaryShuffle = coerce (binaryShuffle @indices @X2 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X2 (MaximumNumber a) = MkMaximumNumberX2 (X2 a)
instance PackX2 X2 a => PackX2 X2 (MaximumNumber a) where
  mkX2 = coerce (mkX2 @X2 @a)
  unpackX2 = coerce (unpackX2 @X2 @a)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 a => Broadcast X2 (MaximumNumber a) where
  broadcast = coerce (broadcast @X2 @a)
  {-# INLINE broadcast #-}
instance SelectableF X2 a => SelectableF X2 (MaximumNumber a) where
  selectF = coerce (selectF @X2 @a)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X2 a => UnaryShuffleT indices X2 (MaximumNumber a) where
  unaryShuffle = coerce (unaryShuffle @indices @X2 @a)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 a => BinaryShuffleT indices X2 (MaximumNumber a) where
  binaryShuffle = coerce (binaryShuffle @indices @X2 @a)
  {-# INLINE binaryShuffle #-}
newtype instance X2 All = MkAllX2 (X2 Bool)
instance PackX2 X2 Bool => PackX2 X2 All where
  mkX2 = coerce (mkX2 @X2 @Bool)
  unpackX2 = coerce (unpackX2 @X2 @Bool)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Bool => Broadcast X2 All where
  broadcast = coerce (broadcast @X2 @Bool)
  {-# INLINE broadcast #-}
instance SelectableF X2 Bool => SelectableF X2 All where
  selectF = coerce (selectF @X2 @Bool)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X2 Bool => UnaryShuffleT indices X2 All where
  unaryShuffle = coerce (unaryShuffle @indices @X2 @Bool)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 Bool => BinaryShuffleT indices X2 All where
  binaryShuffle = coerce (binaryShuffle @indices @X2 @Bool)
  {-# INLINE binaryShuffle #-}
newtype instance X2 Any = MkAnyX2 (X2 Bool)
instance PackX2 X2 Bool => PackX2 X2 Any where
  mkX2 = coerce (mkX2 @X2 @Bool)
  unpackX2 = coerce (unpackX2 @X2 @Bool)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Bool => Broadcast X2 Any where
  broadcast = coerce (broadcast @X2 @Bool)
  {-# INLINE broadcast #-}
instance SelectableF X2 Bool => SelectableF X2 Any where
  selectF = coerce (selectF @X2 @Bool)
  {-# INLINE selectF #-}
instance UnaryShuffleT indices X2 Bool => UnaryShuffleT indices X2 Any where
  unaryShuffle = coerce (unaryShuffle @indices @X2 @Bool)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 Bool => BinaryShuffleT indices X2 Any where
  binaryShuffle = coerce (binaryShuffle @indices @X2 @Bool)
  {-# INLINE binaryShuffle #-}
instance (Num a, NumF X2 a) => Semigroup (X2 (Sum a)) where
  (<>) = coerce ((+) @(X2 a))
  {-# INLINE (<>) #-}
instance (Num a, NumF X2 a) => Monoid (X2 (Sum a)) where
  mempty = MkSumX2 (fromIntegerF 0)
  {-# INLINE mempty #-}
instance (Num a, NumF X2 a) => Semigroup (X2 (Product a)) where
  (<>) = coerce ((*) @(X2 a))
  {-# INLINE (<>) #-}
instance (Num a, NumF X2 a) => Monoid (X2 (Product a)) where
  mempty = MkProductX2 (fromIntegerF 1)
  {-# INLINE mempty #-}
instance (MinMax (X2 a), MinMaxIsCompatibleWithOrd a) => Semigroup (X2 (Min a)) where
  (<>) = coerce (min @(X2 a))
  {-# INLINE (<>) #-}
instance (Bounded a, MinMax (X2 a), MinMaxIsCompatibleWithOrd a, Broadcast X2 a) => Monoid (X2 (Min a)) where
  mempty = MkMinX2 (broadcast maxBound)
  {-# INLINE mempty #-}
instance (MinMax (X2 a), MinMaxIsCompatibleWithOrd a) => Semigroup (X2 (Max a)) where
  (<>) = coerce (max @(X2 a))
  {-# INLINE (<>) #-}
instance (Bounded a, MinMax (X2 a), MinMaxIsCompatibleWithOrd a, Broadcast X2 a) => Monoid (X2 (Max a)) where
  mempty = MkMaxX2 (broadcast minBound)
  {-# INLINE mempty #-}
instance MinMax (X2 a) => Semigroup (X2 (IEEEMin a)) where
  (<>) = coerce (min @(X2 a))
  {-# INLINE (<>) #-}
instance MinMax (X2 a) => Semigroup (X2 (IEEEMax a)) where
  (<>) = coerce (max @(X2 a))
  {-# INLINE (<>) #-}
instance MinMax (X2 a) => Semigroup (X2 (MinimumNumber a)) where
  (<>) = coerce (minimumNumber @(X2 a))
  {-# INLINE (<>) #-}
instance MinMax (X2 a) => Semigroup (X2 (MaximumNumber a)) where
  (<>) = coerce (maximumNumber @(X2 a))
  {-# INLINE (<>) #-}
instance Semigroup (X2 All) where
  (<>) = coerce ((B..&.) @(X2 Bool))
  {-# INLINE (<>) #-}
instance Monoid (X2 All) where
  mempty = MkAllX2 (broadcast True)
  {-# INLINE mempty #-}
instance Semigroup (X2 Any) where
  (<>) = coerce ((B..|.) @(X2 Bool))
  {-# INLINE (<>) #-}
instance Monoid (X2 Any) where
  mempty = MkAnyX2 (broadcast False)
  {-# INLINE mempty #-}
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
instance UnaryShuffleT indices X2 a => UnaryShuffleT indices X2 (Complex a) where
  unaryShuffle (MkComplexX2 x y) = MkComplexX2 (unaryShuffle @indices x) (unaryShuffle @indices y)
  {-# INLINE unaryShuffle #-}
instance BinaryShuffleT indices X2 a => BinaryShuffleT indices X2 (Complex a) where
  binaryShuffle (MkComplexX2 x y) (MkComplexX2 u v) = MkComplexX2 (binaryShuffle @indices x u) (binaryShuffle @indices y v)
  {-# INLINE binaryShuffle #-}
data instance X2 (Maybe a) = MkMaybeX2 !(X2 Bool) !(X2 a)
instance (Default a, PackX2 X2 a) => PackX2 X2 (Maybe a) where
  mkX2 x0 x1 = MkMaybeX2 (mkX2 (isJust x0) (isJust x1)) (mkX2 (fromMaybe def x0) (fromMaybe def x1))
  unpackX2 (MkMaybeX2 x y) = case unpackX2 x of (x0, x1) -> case unpackX2 y of (y0, y1) -> (if x0 then Just y0 else Nothing, if x1 then Just y1 else Nothing)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance (Default a, Broadcast X2 a) => Broadcast X2 (Maybe a) where
  broadcast (Just x) = MkMaybeX2 (broadcast True) (broadcast x)
  broadcast Nothing = MkMaybeX2 (broadcast False) (broadcast def)
  {-# INLINE broadcast #-}
instance SelectableF X2 a => SelectableF X2 (Maybe a) where
  selectF !cond (MkMaybeX2 x y) (MkMaybeX2 x' y') = MkMaybeX2 (selectF cond x x') (selectF cond y y')
  {-# INLINE selectF #-}
instance (UnaryShuffleT indices X2 Bool, UnaryShuffleT indices X2 a) => UnaryShuffleT indices X2 (Maybe a) where
  unaryShuffle (MkMaybeX2 x y) = MkMaybeX2 (unaryShuffle @indices x) (unaryShuffle @indices y)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X2 Bool, BinaryShuffleT indices X2 a) => BinaryShuffleT indices X2 (Maybe a) where
  binaryShuffle (MkMaybeX2 x y) (MkMaybeX2 u v) = MkMaybeX2 (binaryShuffle @indices x u) (binaryShuffle @indices y v)
  {-# INLINE binaryShuffle #-}
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
instance (i0 < 2, i1 < 2) => UnaryShuffleT '[i0, i1] X2 () where
  unaryShuffle _ = MkUnitX2
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4) => BinaryShuffleT '[i0, i1] X2 () where
  binaryShuffle _ _ = MkUnitX2
  {-# INLINE binaryShuffle #-}
instance Semigroup (X2()) where
  _ <> _ = MkUnitX2
  {-# INLINE (<>) #-}
instance Monoid (X2()) where
  mempty = MkUnitX2
  {-# INLINE mempty #-}
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
instance (UnaryShuffleT indices X2 a0, UnaryShuffleT indices X2 a1) => UnaryShuffleT indices X2 (a0, a1) where
  unaryShuffle (MkTuple2X2 x0 x1) = MkTuple2X2 (unaryShuffle @indices x0) (unaryShuffle @indices x1)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X2 a0, BinaryShuffleT indices X2 a1) => BinaryShuffleT indices X2 (a0, a1) where
  binaryShuffle (MkTuple2X2 x0 x1) (MkTuple2X2 y0 y1) = MkTuple2X2 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1)
  {-# INLINE binaryShuffle #-}
instance (Semigroup (X2 a0), Semigroup (X2 a1)) => Semigroup (X2 (a0, a1)) where
  MkTuple2X2 x0 x1 <> MkTuple2X2 y0 y1 = MkTuple2X2 (x0 <> y0) (x1 <> y1)
  {-# INLINE (<>) #-}
instance (Monoid (X2 a0), Monoid (X2 a1)) => Monoid (X2 (a0, a1)) where
  mempty = MkTuple2X2 mempty mempty
  {-# INLINE mempty #-}
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
instance (UnaryShuffleT indices X2 a0, UnaryShuffleT indices X2 a1, UnaryShuffleT indices X2 a2) => UnaryShuffleT indices X2 (a0, a1, a2) where
  unaryShuffle (MkTuple3X2 x0 x1 x2) = MkTuple3X2 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X2 a0, BinaryShuffleT indices X2 a1, BinaryShuffleT indices X2 a2) => BinaryShuffleT indices X2 (a0, a1, a2) where
  binaryShuffle (MkTuple3X2 x0 x1 x2) (MkTuple3X2 y0 y1 y2) = MkTuple3X2 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2)
  {-# INLINE binaryShuffle #-}
instance (Semigroup (X2 a0), Semigroup (X2 a1), Semigroup (X2 a2)) => Semigroup (X2 (a0, a1, a2)) where
  MkTuple3X2 x0 x1 x2 <> MkTuple3X2 y0 y1 y2 = MkTuple3X2 (x0 <> y0) (x1 <> y1) (x2 <> y2)
  {-# INLINE (<>) #-}
instance (Monoid (X2 a0), Monoid (X2 a1), Monoid (X2 a2)) => Monoid (X2 (a0, a1, a2)) where
  mempty = MkTuple3X2 mempty mempty mempty
  {-# INLINE mempty #-}
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
instance (UnaryShuffleT indices X2 a0, UnaryShuffleT indices X2 a1, UnaryShuffleT indices X2 a2, UnaryShuffleT indices X2 a3) => UnaryShuffleT indices X2 (a0, a1, a2, a3) where
  unaryShuffle (MkTuple4X2 x0 x1 x2 x3) = MkTuple4X2 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X2 a0, BinaryShuffleT indices X2 a1, BinaryShuffleT indices X2 a2, BinaryShuffleT indices X2 a3) => BinaryShuffleT indices X2 (a0, a1, a2, a3) where
  binaryShuffle (MkTuple4X2 x0 x1 x2 x3) (MkTuple4X2 y0 y1 y2 y3) = MkTuple4X2 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3)
  {-# INLINE binaryShuffle #-}
instance (Semigroup (X2 a0), Semigroup (X2 a1), Semigroup (X2 a2), Semigroup (X2 a3)) => Semigroup (X2 (a0, a1, a2, a3)) where
  MkTuple4X2 x0 x1 x2 x3 <> MkTuple4X2 y0 y1 y2 y3 = MkTuple4X2 (x0 <> y0) (x1 <> y1) (x2 <> y2) (x3 <> y3)
  {-# INLINE (<>) #-}
instance (Monoid (X2 a0), Monoid (X2 a1), Monoid (X2 a2), Monoid (X2 a3)) => Monoid (X2 (a0, a1, a2, a3)) where
  mempty = MkTuple4X2 mempty mempty mempty mempty
  {-# INLINE mempty #-}
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
instance (UnaryShuffleT indices X2 a0, UnaryShuffleT indices X2 a1, UnaryShuffleT indices X2 a2, UnaryShuffleT indices X2 a3, UnaryShuffleT indices X2 a4) => UnaryShuffleT indices X2 (a0, a1, a2, a3, a4) where
  unaryShuffle (MkTuple5X2 x0 x1 x2 x3 x4) = MkTuple5X2 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X2 a0, BinaryShuffleT indices X2 a1, BinaryShuffleT indices X2 a2, BinaryShuffleT indices X2 a3, BinaryShuffleT indices X2 a4) => BinaryShuffleT indices X2 (a0, a1, a2, a3, a4) where
  binaryShuffle (MkTuple5X2 x0 x1 x2 x3 x4) (MkTuple5X2 y0 y1 y2 y3 y4) = MkTuple5X2 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4)
  {-# INLINE binaryShuffle #-}
instance (Semigroup (X2 a0), Semigroup (X2 a1), Semigroup (X2 a2), Semigroup (X2 a3), Semigroup (X2 a4)) => Semigroup (X2 (a0, a1, a2, a3, a4)) where
  MkTuple5X2 x0 x1 x2 x3 x4 <> MkTuple5X2 y0 y1 y2 y3 y4 = MkTuple5X2 (x0 <> y0) (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4)
  {-# INLINE (<>) #-}
instance (Monoid (X2 a0), Monoid (X2 a1), Monoid (X2 a2), Monoid (X2 a3), Monoid (X2 a4)) => Monoid (X2 (a0, a1, a2, a3, a4)) where
  mempty = MkTuple5X2 mempty mempty mempty mempty mempty
  {-# INLINE mempty #-}
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
instance (UnaryShuffleT indices X2 a0, UnaryShuffleT indices X2 a1, UnaryShuffleT indices X2 a2, UnaryShuffleT indices X2 a3, UnaryShuffleT indices X2 a4, UnaryShuffleT indices X2 a5) => UnaryShuffleT indices X2 (a0, a1, a2, a3, a4, a5) where
  unaryShuffle (MkTuple6X2 x0 x1 x2 x3 x4 x5) = MkTuple6X2 (unaryShuffle @indices x0) (unaryShuffle @indices x1) (unaryShuffle @indices x2) (unaryShuffle @indices x3) (unaryShuffle @indices x4) (unaryShuffle @indices x5)
  {-# INLINE unaryShuffle #-}
instance (BinaryShuffleT indices X2 a0, BinaryShuffleT indices X2 a1, BinaryShuffleT indices X2 a2, BinaryShuffleT indices X2 a3, BinaryShuffleT indices X2 a4, BinaryShuffleT indices X2 a5) => BinaryShuffleT indices X2 (a0, a1, a2, a3, a4, a5) where
  binaryShuffle (MkTuple6X2 x0 x1 x2 x3 x4 x5) (MkTuple6X2 y0 y1 y2 y3 y4 y5) = MkTuple6X2 (binaryShuffle @indices x0 y0) (binaryShuffle @indices x1 y1) (binaryShuffle @indices x2 y2) (binaryShuffle @indices x3 y3) (binaryShuffle @indices x4 y4) (binaryShuffle @indices x5 y5)
  {-# INLINE binaryShuffle #-}
instance (Semigroup (X2 a0), Semigroup (X2 a1), Semigroup (X2 a2), Semigroup (X2 a3), Semigroup (X2 a4), Semigroup (X2 a5)) => Semigroup (X2 (a0, a1, a2, a3, a4, a5)) where
  MkTuple6X2 x0 x1 x2 x3 x4 x5 <> MkTuple6X2 y0 y1 y2 y3 y4 y5 = MkTuple6X2 (x0 <> y0) (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4) (x5 <> y5)
  {-# INLINE (<>) #-}
instance (Monoid (X2 a0), Monoid (X2 a1), Monoid (X2 a2), Monoid (X2 a3), Monoid (X2 a4), Monoid (X2 a5)) => Monoid (X2 (a0, a1, a2, a3, a4, a5)) where
  mempty = MkTuple6X2 mempty mempty mempty mempty mempty mempty
  {-# INLINE mempty #-}
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
  mkIEEEMin = coerce
  getIEEEMin' = coerce
  mkIEEEMax = coerce
  getIEEEMax' = coerce
  mkMinimumNumber = coerce
  getMinimumNumber' = coerce
  mkMaximumNumber = coerce
  getMaximumNumber' = coerce
  mkAll = coerce
  getAll' = coerce
  mkAny = coerce
  getAny' = coerce
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
  {-# INLINE mkIEEEMin #-}
  {-# INLINE getIEEEMin' #-}
  {-# INLINE mkIEEEMax #-}
  {-# INLINE getIEEEMax' #-}
  {-# INLINE mkMinimumNumber #-}
  {-# INLINE getMinimumNumber' #-}
  {-# INLINE mkMaximumNumber #-}
  {-# INLINE getMaximumNumber' #-}
  {-# INLINE mkAll #-}
  {-# INLINE getAll' #-}
  {-# INLINE mkAny #-}
  {-# INLINE getAny' #-}
  {-# INLINE mkComplex #-}
  {-# INLINE deconstructComplex #-}
instance (Broadcast X2 a, SelectableF X2 a) => LiftMaybe X2 a where
  mkMaybe = MkMaybeX2
  mkJust = MkMaybeX2 (broadcast True)
  isJust' (MkMaybeX2 x _) = x
  fromJust' (MkMaybeX2 _ y) = y
  fromMaybe' x (MkMaybeX2 y z) = selectF y z x
  maybe' d f (MkMaybeX2 x y) = select x (f y) d
  fmapMaybe f (MkMaybeX2 x y) = MkMaybeX2 x (f y)
  lift2Maybe f (MkMaybeX2 x y) (MkMaybeX2 u v) = MkMaybeX2 (x B..&. u) (f y v)
  bindMaybe (MkMaybeX2 x y) f = case f y of MkMaybeX2 u v -> MkMaybeX2 (x B..&. u) v
  joinMaybe (MkMaybeX2 x (MkMaybeX2 y z)) = MkMaybeX2 (x B..&. y) z
  {-# INLINE mkMaybe #-}
  {-# INLINE mkJust #-}
  {-# INLINE isJust' #-}
  {-# INLINE fromJust' #-}
  {-# INLINE fromMaybe' #-}
  {-# INLINE maybe' #-}
  {-# INLINE fmapMaybe #-}
  {-# INLINE lift2Maybe #-}
  {-# INLINE bindMaybe #-}
  {-# INLINE joinMaybe #-}
deriving via WrappedMulti X2 a instance SelectableF X2 a => Selectable (X2 a)
deriving via WrappedMulti X2 a instance NumF X2 a => Num (X2 a)
deriving via WrappedMulti X2 a instance FractionalF X2 a => Fractional (X2 a)
deriving via WrappedMulti X2 a instance FloatingF X2 a => Floating (X2 a)
