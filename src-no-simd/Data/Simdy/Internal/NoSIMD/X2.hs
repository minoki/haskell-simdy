-- This file was created by script/Gen.hs. Do not edit by hand!
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)
{-# LANGUAGE ExtendedLiterals #-}
#endif
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Simdy.Internal.NoSIMD.X2 where
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
import qualified GHC.Exts
import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..), intToInt8#, intToInt16#, intToInt32#, intToInt64#, wordToWord8#, wordToWord16#, wordToWord32#, wordToWord64#)
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
data instance X2 Float = MkFloatX2WithElems !Float !Float
instance PackX2 X2 Float where
  mkX2 = MkFloatX2WithElems
  unpackX2 (MkFloatX2WithElems x0 x1) = (x0, x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Float where
  broadcast !x = MkFloatX2WithElems x x
  {-# INLINE broadcast #-}
instance SelectableF X2 Float where
  selectF (MkBoolX2 !cond) (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = MkFloatX2WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1)
instance (i0 < 2, i1 < 2, Pick Float i0, Pick Float i1) => UnaryShuffle [i0, i1] X2 Float where
  unaryShuffle (MkFloatX2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkFloatX2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Float i0, Pick Float i1) => BinaryShuffle [i0, i1] X2 Float where
  binaryShuffle (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkFloatX2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Float where
  eqF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = mkX2 (x0 == y0) (x1 == y1)
  {-# INLINE eqF #-}
instance OrderedF X2 Float where
  ltF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = mkX2 (x0 < y0) (x1 < y1)
  leF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = mkX2 (x0 <= y0) (x1 <= y1)
  gtF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = mkX2 (x0 > y0) (x1 > y1)
  geF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = mkX2 (x0 >= y0) (x1 >= y1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Float where
  minF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = MkFloatX2WithElems (min x0 y0) (min x1 y1)
  maxF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = MkFloatX2WithElems (max x0 y0) (max x1 y1)
  minimumNumberF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = MkFloatX2WithElems (minimumNumber x0 y0) (minimumNumber x1 y1)
  maximumNumberF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = MkFloatX2WithElems (maximumNumber x0 y0) (maximumNumber x1 y1)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX2Float :: X2 Float -> X2 Float
negateX2Float (MkFloatX2WithElems x0 x1) = MkFloatX2WithElems (- x0) (- x1)
#if defined(USE_FMA)
{-# INLINE [0] negateX2Float #-}
#else
{-# INLINE negateX2Float #-}
#endif
instance NumF X2 Float where
  plusF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = MkFloatX2WithElems (x0 + y0) (x1 + y1)
  minusF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = MkFloatX2WithElems (x0 - y0) (x1 - y1)
  timesF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = MkFloatX2WithElems (x0 * y0) (x1 * y1)
  negateF = negateX2Float
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance FractionalF X2 Float where
  divideF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) = MkFloatX2WithElems (x0 / y0) (x1 / y1)
  recipF (MkFloatX2WithElems x0 x1) = MkFloatX2WithElems (recip x0) (recip x1)
  {-# INLINE divideF #-}
  {-# INLINE recipF #-}
instance FloatingF X2 Float where
  sqrtF (MkFloatX2WithElems x0 x1) = MkFloatX2WithElems (sqrt x0) (sqrt x1)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X2 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX2WithElems x0 x1) (MkFloatX2WithElems y0 y1) (MkFloatX2WithElems z0 z1) = MkFloatX2WithElems (fusedMultiplyAdd x0 y0 z0) (fusedMultiplyAdd x1 y1 z1)
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
  enumFromZero = MkFloatX2WithElems 0 1
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Float where
  indexByteArraySIMD# ba i = MkFloatX2WithElems (F# (GHC.Exts.indexFloatArray# ba i)) (F# (GHC.Exts.indexFloatArray# ba (i +# 1#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readFloatArray# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readFloatArray# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkFloatX2WithElems (F# x0) (F# x1) #)
  writeByteArraySIMD# mba i (MkFloatX2WithElems (F# x0) (F# x1)) s0 = case GHC.Exts.writeFloatArray# mba i x0 s0 of s1 -> GHC.Exts.writeFloatArray# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readFloatOffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkFloatX2WithElems (F# x0) (F# x1) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX2WithElems (F# x0) (F# x1)) = IO (\s0 -> case GHC.Exts.writeFloatOffAddr# addr i x0 s0 of s1 -> (# GHC.Exts.writeFloatOffAddr# addr (i +# 1#) x1 s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X2 Double = MkDoubleX2WithElems !Double !Double
instance PackX2 X2 Double where
  mkX2 = MkDoubleX2WithElems
  unpackX2 (MkDoubleX2WithElems x0 x1) = (x0, x1)
  {-# INLINE mkX2 #-}
  {-# INLINE unpackX2 #-}
instance Broadcast X2 Double where
  broadcast !x = MkDoubleX2WithElems x x
  {-# INLINE broadcast #-}
instance SelectableF X2 Double where
  selectF (MkBoolX2 !cond) (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = MkDoubleX2WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1)
instance (i0 < 2, i1 < 2, Pick Double i0, Pick Double i1) => UnaryShuffle [i0, i1] X2 Double where
  unaryShuffle (MkDoubleX2WithElems x0 x1) = let { sources = \case { 0 -> x0; _ -> x1 } } in MkDoubleX2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 4, i1 < 4, Pick Double i0, Pick Double i1) => BinaryShuffle [i0, i1] X2 Double where
  binaryShuffle (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkDoubleX2WithElems (pick @_ @i0 sources) (pick @_ @i1 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X2 Double where
  eqF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = mkX2 (x0 == y0) (x1 == y1)
  {-# INLINE eqF #-}
instance OrderedF X2 Double where
  ltF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = mkX2 (x0 < y0) (x1 < y1)
  leF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = mkX2 (x0 <= y0) (x1 <= y1)
  gtF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = mkX2 (x0 > y0) (x1 > y1)
  geF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = mkX2 (x0 >= y0) (x1 >= y1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X2 Double where
  minF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = MkDoubleX2WithElems (min x0 y0) (min x1 y1)
  maxF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = MkDoubleX2WithElems (max x0 y0) (max x1 y1)
  minimumNumberF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = MkDoubleX2WithElems (minimumNumber x0 y0) (minimumNumber x1 y1)
  maximumNumberF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = MkDoubleX2WithElems (maximumNumber x0 y0) (maximumNumber x1 y1)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX2Double :: X2 Double -> X2 Double
negateX2Double (MkDoubleX2WithElems x0 x1) = MkDoubleX2WithElems (- x0) (- x1)
#if defined(USE_FMA)
{-# INLINE [0] negateX2Double #-}
#else
{-# INLINE negateX2Double #-}
#endif
instance NumF X2 Double where
  plusF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = MkDoubleX2WithElems (x0 + y0) (x1 + y1)
  minusF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = MkDoubleX2WithElems (x0 - y0) (x1 - y1)
  timesF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = MkDoubleX2WithElems (x0 * y0) (x1 * y1)
  negateF = negateX2Double
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance FractionalF X2 Double where
  divideF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) = MkDoubleX2WithElems (x0 / y0) (x1 / y1)
  recipF (MkDoubleX2WithElems x0 x1) = MkDoubleX2WithElems (recip x0) (recip x1)
  {-# INLINE divideF #-}
  {-# INLINE recipF #-}
instance FloatingF X2 Double where
  sqrtF (MkDoubleX2WithElems x0 x1) = MkDoubleX2WithElems (sqrt x0) (sqrt x1)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X2 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX2WithElems x0 x1) (MkDoubleX2WithElems y0 y1) (MkDoubleX2WithElems z0 z1) = MkDoubleX2WithElems (fusedMultiplyAdd x0 y0 z0) (fusedMultiplyAdd x1 y1 z1)
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
  enumFromZero = MkDoubleX2WithElems 0 1
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X2 Double where
  indexByteArraySIMD# ba i = MkDoubleX2WithElems (D# (GHC.Exts.indexDoubleArray# ba i)) (D# (GHC.Exts.indexDoubleArray# ba (i +# 1#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readDoubleArray# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readDoubleArray# mba (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkDoubleX2WithElems (D# x0) (D# x1) #)
  writeByteArraySIMD# mba i (MkDoubleX2WithElems (D# x0) (D# x1)) s0 = case GHC.Exts.writeDoubleArray# mba i x0 s0 of s1 -> GHC.Exts.writeDoubleArray# mba (i +# 1#) x1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X2 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readDoubleOffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> (# s2, MkDoubleX2WithElems (D# x0) (D# x1) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX2WithElems (D# x0) (D# x1)) = IO (\s0 -> case GHC.Exts.writeDoubleOffAddr# addr i x0 s0 of s1 -> (# GHC.Exts.writeDoubleOffAddr# addr (i +# 1#) x1 s1, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
instance ImplementationDescription X2 where
  implementationDescription _ = "X2;maxBits=0"
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
