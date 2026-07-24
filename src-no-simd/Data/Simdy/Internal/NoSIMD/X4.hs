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
module Data.Simdy.Internal.NoSIMD.X4 where
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
data instance X4 Float = MkFloatX4WithElems !Float !Float !Float !Float
instance PackX4 X4 Float where
  mkX4 = MkFloatX4WithElems
  unpackX4 (MkFloatX4WithElems x0 x1 x2 x3) = (x0, x1, x2, x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Float where
  broadcast !x = MkFloatX4WithElems x x x x
  {-# INLINE broadcast #-}
instance SelectableF X4 Float where
  selectF (MkBoolX4 !cond) (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = MkFloatX4WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3)
instance (i0 < 4, i1 < 4, i2 < 4, i3 < 4, Pick Float i0, Pick Float i1, Pick Float i2, Pick Float i3) => UnaryShuffle [i0, i1, i2, i3] X4 Float where
  unaryShuffle (MkFloatX4WithElems x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkFloatX4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, Pick Float i0, Pick Float i1, Pick Float i2, Pick Float i3) => BinaryShuffle [i0, i1, i2, i3] X4 Float where
  binaryShuffle (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkFloatX4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Float where
  eqF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = mkX4 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3)
  {-# INLINE eqF #-}
instance OrderedF X4 Float where
  ltF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = mkX4 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3)
  leF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = mkX4 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3)
  gtF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = mkX4 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3)
  geF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = mkX4 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Float where
  minF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = MkFloatX4WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3)
  maxF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = MkFloatX4WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3)
  minimumNumberF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = MkFloatX4WithElems (minimumNumber x0 y0) (minimumNumber x1 y1) (minimumNumber x2 y2) (minimumNumber x3 y3)
  maximumNumberF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = MkFloatX4WithElems (maximumNumber x0 y0) (maximumNumber x1 y1) (maximumNumber x2 y2) (maximumNumber x3 y3)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX4Float :: X4 Float -> X4 Float
negateX4Float (MkFloatX4WithElems x0 x1 x2 x3) = MkFloatX4WithElems (- x0) (- x1) (- x2) (- x3)
#if defined(USE_FMA)
{-# INLINE [0] negateX4Float #-}
#else
{-# INLINE negateX4Float #-}
#endif
instance NumF X4 Float where
  plusF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = MkFloatX4WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
  minusF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = MkFloatX4WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3)
  timesF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = MkFloatX4WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
  negateF = negateX4Float
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance FractionalF X4 Float where
  divideF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) = MkFloatX4WithElems (x0 / y0) (x1 / y1) (x2 / y2) (x3 / y3)
  recipF (MkFloatX4WithElems x0 x1 x2 x3) = MkFloatX4WithElems (recip x0) (recip x1) (recip x2) (recip x3)
  {-# INLINE divideF #-}
  {-# INLINE recipF #-}
instance FloatingF X4 Float where
  sqrtF (MkFloatX4WithElems x0 x1 x2 x3) = MkFloatX4WithElems (sqrt x0) (sqrt x1) (sqrt x2) (sqrt x3)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X4 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX4WithElems x0 x1 x2 x3) (MkFloatX4WithElems y0 y1 y2 y3) (MkFloatX4WithElems z0 z1 z2 z3) = MkFloatX4WithElems (fusedMultiplyAdd x0 y0 z0) (fusedMultiplyAdd x1 y1 z1) (fusedMultiplyAdd x2 y2 z2) (fusedMultiplyAdd x3 y3 z3)
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
  enumFromZero = MkFloatX4WithElems 0 1 2 3
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Float where
  indexByteArraySIMD# ba i = MkFloatX4WithElems (F# (GHC.Exts.indexFloatArray# ba i)) (F# (GHC.Exts.indexFloatArray# ba (i +# 1#))) (F# (GHC.Exts.indexFloatArray# ba (i +# 2#))) (F# (GHC.Exts.indexFloatArray# ba (i +# 3#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readFloatArray# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readFloatArray# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readFloatArray# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readFloatArray# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkFloatX4WithElems (F# x0) (F# x1) (F# x2) (F# x3) #)
  writeByteArraySIMD# mba i (MkFloatX4WithElems (F# x0) (F# x1) (F# x2) (F# x3)) s0 = case GHC.Exts.writeFloatArray# mba i x0 s0 of s1 -> case GHC.Exts.writeFloatArray# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeFloatArray# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeFloatArray# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readFloatOffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkFloatX4WithElems (F# x0) (F# x1) (F# x2) (F# x3) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX4WithElems (F# x0) (F# x1) (F# x2) (F# x3)) = IO (\s0 -> case GHC.Exts.writeFloatOffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeFloatOffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeFloatOffAddr# addr (i +# 2#) x2 s2 of s3 -> (# GHC.Exts.writeFloatOffAddr# addr (i +# 3#) x3 s3, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Double = MkDoubleX4WithElems !Double !Double !Double !Double
instance PackX4 X4 Double where
  mkX4 = MkDoubleX4WithElems
  unpackX4 (MkDoubleX4WithElems x0 x1 x2 x3) = (x0, x1, x2, x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Double where
  broadcast !x = MkDoubleX4WithElems x x x x
  {-# INLINE broadcast #-}
instance SelectableF X4 Double where
  selectF (MkBoolX4 !cond) (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = MkDoubleX4WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3)
instance (i0 < 4, i1 < 4, i2 < 4, i3 < 4, Pick Double i0, Pick Double i1, Pick Double i2, Pick Double i3) => UnaryShuffle [i0, i1, i2, i3] X4 Double where
  unaryShuffle (MkDoubleX4WithElems x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkDoubleX4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, Pick Double i0, Pick Double i1, Pick Double i2, Pick Double i3) => BinaryShuffle [i0, i1, i2, i3] X4 Double where
  binaryShuffle (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkDoubleX4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Double where
  eqF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = mkX4 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3)
  {-# INLINE eqF #-}
instance OrderedF X4 Double where
  ltF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = mkX4 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3)
  leF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = mkX4 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3)
  gtF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = mkX4 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3)
  geF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = mkX4 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Double where
  minF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = MkDoubleX4WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3)
  maxF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = MkDoubleX4WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3)
  minimumNumberF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = MkDoubleX4WithElems (minimumNumber x0 y0) (minimumNumber x1 y1) (minimumNumber x2 y2) (minimumNumber x3 y3)
  maximumNumberF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = MkDoubleX4WithElems (maximumNumber x0 y0) (maximumNumber x1 y1) (maximumNumber x2 y2) (maximumNumber x3 y3)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX4Double :: X4 Double -> X4 Double
negateX4Double (MkDoubleX4WithElems x0 x1 x2 x3) = MkDoubleX4WithElems (- x0) (- x1) (- x2) (- x3)
#if defined(USE_FMA)
{-# INLINE [0] negateX4Double #-}
#else
{-# INLINE negateX4Double #-}
#endif
instance NumF X4 Double where
  plusF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = MkDoubleX4WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
  minusF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = MkDoubleX4WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3)
  timesF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = MkDoubleX4WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
  negateF = negateX4Double
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance FractionalF X4 Double where
  divideF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) = MkDoubleX4WithElems (x0 / y0) (x1 / y1) (x2 / y2) (x3 / y3)
  recipF (MkDoubleX4WithElems x0 x1 x2 x3) = MkDoubleX4WithElems (recip x0) (recip x1) (recip x2) (recip x3)
  {-# INLINE divideF #-}
  {-# INLINE recipF #-}
instance FloatingF X4 Double where
  sqrtF (MkDoubleX4WithElems x0 x1 x2 x3) = MkDoubleX4WithElems (sqrt x0) (sqrt x1) (sqrt x2) (sqrt x3)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X4 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX4WithElems x0 x1 x2 x3) (MkDoubleX4WithElems y0 y1 y2 y3) (MkDoubleX4WithElems z0 z1 z2 z3) = MkDoubleX4WithElems (fusedMultiplyAdd x0 y0 z0) (fusedMultiplyAdd x1 y1 z1) (fusedMultiplyAdd x2 y2 z2) (fusedMultiplyAdd x3 y3 z3)
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
  enumFromZero = MkDoubleX4WithElems 0 1 2 3
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Double where
  indexByteArraySIMD# ba i = MkDoubleX4WithElems (D# (GHC.Exts.indexDoubleArray# ba i)) (D# (GHC.Exts.indexDoubleArray# ba (i +# 1#))) (D# (GHC.Exts.indexDoubleArray# ba (i +# 2#))) (D# (GHC.Exts.indexDoubleArray# ba (i +# 3#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readDoubleArray# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readDoubleArray# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readDoubleArray# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readDoubleArray# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkDoubleX4WithElems (D# x0) (D# x1) (D# x2) (D# x3) #)
  writeByteArraySIMD# mba i (MkDoubleX4WithElems (D# x0) (D# x1) (D# x2) (D# x3)) s0 = case GHC.Exts.writeDoubleArray# mba i x0 s0 of s1 -> case GHC.Exts.writeDoubleArray# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeDoubleArray# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeDoubleArray# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readDoubleOffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkDoubleX4WithElems (D# x0) (D# x1) (D# x2) (D# x3) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX4WithElems (D# x0) (D# x1) (D# x2) (D# x3)) = IO (\s0 -> case GHC.Exts.writeDoubleOffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeDoubleOffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeDoubleOffAddr# addr (i +# 2#) x2 s2 of s3 -> (# GHC.Exts.writeDoubleOffAddr# addr (i +# 3#) x3 s3, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
instance ImplementationDescription X4 where
  implementationDescription _ = "X4;maxBits=0"
data instance X4 Int8 = MkInt8X4WithElems !Int8 !Int8 !Int8 !Int8
instance PackX4 X4 Int8 where
  mkX4 = MkInt8X4WithElems
  unpackX4 (MkInt8X4WithElems x0 x1 x2 x3) = (x0, x1, x2, x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Int8 where
  broadcast !x = MkInt8X4WithElems x x x x
  {-# INLINE broadcast #-}
instance SelectableF X4 Int8 where
  selectF (MkBoolX4 !cond) (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = MkInt8X4WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3)
instance (i0 < 4, i1 < 4, i2 < 4, i3 < 4, Pick Int8 i0, Pick Int8 i1, Pick Int8 i2, Pick Int8 i3) => UnaryShuffle [i0, i1, i2, i3] X4 Int8 where
  unaryShuffle (MkInt8X4WithElems x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt8X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, Pick Int8 i0, Pick Int8 i1, Pick Int8 i2, Pick Int8 i3) => BinaryShuffle [i0, i1, i2, i3] X4 Int8 where
  binaryShuffle (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt8X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Int8 where
  eqF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = mkX4 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3)
  {-# INLINE eqF #-}
instance OrderedF X4 Int8 where
  ltF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = mkX4 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3)
  leF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = mkX4 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3)
  gtF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = mkX4 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3)
  geF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = mkX4 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Int8 where
  minF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = MkInt8X4WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3)
  maxF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = MkInt8X4WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Int8 where
  plusF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = MkInt8X4WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
  minusF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = MkInt8X4WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3)
  timesF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = MkInt8X4WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
  negateF (MkInt8X4WithElems x0 x1 x2 x3) = MkInt8X4WithElems (- x0) (- x1) (- x2) (- x3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X4 Int8 where
  andF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = MkInt8X4WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3)
  orF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = MkInt8X4WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3)
  xorF (MkInt8X4WithElems x0 x1 x2 x3) (MkInt8X4WithElems y0 y1 y2 y3) = MkInt8X4WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3)
  complementF (MkInt8X4WithElems x0 x1 x2 x3) = MkInt8X4WithElems (complement x0) (complement x1) (complement x2) (complement x3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Int8 where
  shiftLF (MkInt8X4WithElems x0 x1 x2 x3) !i = MkInt8X4WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i)
  unsafeShiftLF (MkInt8X4WithElems x0 x1 x2 x3) !i = MkInt8X4WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i)
  shiftRF (MkInt8X4WithElems x0 x1 x2 x3) !i = MkInt8X4WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i)
  unsafeShiftRF (MkInt8X4WithElems x0 x1 x2 x3) !i = MkInt8X4WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X4 Int8 where
  enumFromZero = MkInt8X4WithElems 0 1 2 3
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Int8 where
  indexByteArraySIMD# ba i = MkInt8X4WithElems (I8# (GHC.Exts.indexInt8Array# ba i)) (I8# (GHC.Exts.indexInt8Array# ba (i +# 1#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 2#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 3#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt8X4WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) #)
  writeByteArraySIMD# mba i (MkInt8X4WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3)) s0 = case GHC.Exts.writeInt8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeInt8Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt8X4WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X4WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3)) = IO (\s0 -> case GHC.Exts.writeInt8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 2#) x2 s2 of s3 -> (# GHC.Exts.writeInt8OffAddr# addr (i +# 3#) x3 s3, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Int16 = MkInt16X4WithElems !Int16 !Int16 !Int16 !Int16
instance PackX4 X4 Int16 where
  mkX4 = MkInt16X4WithElems
  unpackX4 (MkInt16X4WithElems x0 x1 x2 x3) = (x0, x1, x2, x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Int16 where
  broadcast !x = MkInt16X4WithElems x x x x
  {-# INLINE broadcast #-}
instance SelectableF X4 Int16 where
  selectF (MkBoolX4 !cond) (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = MkInt16X4WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3)
instance (i0 < 4, i1 < 4, i2 < 4, i3 < 4, Pick Int16 i0, Pick Int16 i1, Pick Int16 i2, Pick Int16 i3) => UnaryShuffle [i0, i1, i2, i3] X4 Int16 where
  unaryShuffle (MkInt16X4WithElems x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt16X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, Pick Int16 i0, Pick Int16 i1, Pick Int16 i2, Pick Int16 i3) => BinaryShuffle [i0, i1, i2, i3] X4 Int16 where
  binaryShuffle (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt16X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Int16 where
  eqF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = mkX4 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3)
  {-# INLINE eqF #-}
instance OrderedF X4 Int16 where
  ltF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = mkX4 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3)
  leF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = mkX4 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3)
  gtF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = mkX4 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3)
  geF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = mkX4 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Int16 where
  minF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = MkInt16X4WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3)
  maxF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = MkInt16X4WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Int16 where
  plusF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = MkInt16X4WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
  minusF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = MkInt16X4WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3)
  timesF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = MkInt16X4WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
  negateF (MkInt16X4WithElems x0 x1 x2 x3) = MkInt16X4WithElems (- x0) (- x1) (- x2) (- x3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X4 Int16 where
  andF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = MkInt16X4WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3)
  orF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = MkInt16X4WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3)
  xorF (MkInt16X4WithElems x0 x1 x2 x3) (MkInt16X4WithElems y0 y1 y2 y3) = MkInt16X4WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3)
  complementF (MkInt16X4WithElems x0 x1 x2 x3) = MkInt16X4WithElems (complement x0) (complement x1) (complement x2) (complement x3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Int16 where
  shiftLF (MkInt16X4WithElems x0 x1 x2 x3) !i = MkInt16X4WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i)
  unsafeShiftLF (MkInt16X4WithElems x0 x1 x2 x3) !i = MkInt16X4WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i)
  shiftRF (MkInt16X4WithElems x0 x1 x2 x3) !i = MkInt16X4WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i)
  unsafeShiftRF (MkInt16X4WithElems x0 x1 x2 x3) !i = MkInt16X4WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X4 Int16 where
  enumFromZero = MkInt16X4WithElems 0 1 2 3
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Int16 where
  indexByteArraySIMD# ba i = MkInt16X4WithElems (I16# (GHC.Exts.indexInt16Array# ba i)) (I16# (GHC.Exts.indexInt16Array# ba (i +# 1#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 2#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 3#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt16Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt16Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt16X4WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) #)
  writeByteArraySIMD# mba i (MkInt16X4WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3)) s0 = case GHC.Exts.writeInt16Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt16Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt16Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeInt16Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt16X4WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X4WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3)) = IO (\s0 -> case GHC.Exts.writeInt16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 2#) x2 s2 of s3 -> (# GHC.Exts.writeInt16OffAddr# addr (i +# 3#) x3 s3, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Int32 = MkInt32X4WithElems !Int32 !Int32 !Int32 !Int32
instance PackX4 X4 Int32 where
  mkX4 = MkInt32X4WithElems
  unpackX4 (MkInt32X4WithElems x0 x1 x2 x3) = (x0, x1, x2, x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Int32 where
  broadcast !x = MkInt32X4WithElems x x x x
  {-# INLINE broadcast #-}
instance SelectableF X4 Int32 where
  selectF (MkBoolX4 !cond) (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = MkInt32X4WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3)
instance (i0 < 4, i1 < 4, i2 < 4, i3 < 4, Pick Int32 i0, Pick Int32 i1, Pick Int32 i2, Pick Int32 i3) => UnaryShuffle [i0, i1, i2, i3] X4 Int32 where
  unaryShuffle (MkInt32X4WithElems x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt32X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, Pick Int32 i0, Pick Int32 i1, Pick Int32 i2, Pick Int32 i3) => BinaryShuffle [i0, i1, i2, i3] X4 Int32 where
  binaryShuffle (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt32X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Int32 where
  eqF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = mkX4 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3)
  {-# INLINE eqF #-}
instance OrderedF X4 Int32 where
  ltF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = mkX4 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3)
  leF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = mkX4 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3)
  gtF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = mkX4 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3)
  geF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = mkX4 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Int32 where
  minF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = MkInt32X4WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3)
  maxF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = MkInt32X4WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Int32 where
  plusF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = MkInt32X4WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
  minusF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = MkInt32X4WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3)
  timesF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = MkInt32X4WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
  negateF (MkInt32X4WithElems x0 x1 x2 x3) = MkInt32X4WithElems (- x0) (- x1) (- x2) (- x3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X4 Int32 where
  andF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = MkInt32X4WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3)
  orF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = MkInt32X4WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3)
  xorF (MkInt32X4WithElems x0 x1 x2 x3) (MkInt32X4WithElems y0 y1 y2 y3) = MkInt32X4WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3)
  complementF (MkInt32X4WithElems x0 x1 x2 x3) = MkInt32X4WithElems (complement x0) (complement x1) (complement x2) (complement x3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Int32 where
  shiftLF (MkInt32X4WithElems x0 x1 x2 x3) !i = MkInt32X4WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i)
  unsafeShiftLF (MkInt32X4WithElems x0 x1 x2 x3) !i = MkInt32X4WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i)
  shiftRF (MkInt32X4WithElems x0 x1 x2 x3) !i = MkInt32X4WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i)
  unsafeShiftRF (MkInt32X4WithElems x0 x1 x2 x3) !i = MkInt32X4WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X4 Int32 where
  enumFromZero = MkInt32X4WithElems 0 1 2 3
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Int32 where
  indexByteArraySIMD# ba i = MkInt32X4WithElems (I32# (GHC.Exts.indexInt32Array# ba i)) (I32# (GHC.Exts.indexInt32Array# ba (i +# 1#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 2#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 3#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt32Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt32Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt32X4WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) #)
  writeByteArraySIMD# mba i (MkInt32X4WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3)) s0 = case GHC.Exts.writeInt32Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt32Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt32Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeInt32Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt32X4WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X4WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3)) = IO (\s0 -> case GHC.Exts.writeInt32OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 2#) x2 s2 of s3 -> (# GHC.Exts.writeInt32OffAddr# addr (i +# 3#) x3 s3, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Int64 = MkInt64X4WithElems !Int64 !Int64 !Int64 !Int64
instance PackX4 X4 Int64 where
  mkX4 = MkInt64X4WithElems
  unpackX4 (MkInt64X4WithElems x0 x1 x2 x3) = (x0, x1, x2, x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Int64 where
  broadcast !x = MkInt64X4WithElems x x x x
  {-# INLINE broadcast #-}
instance SelectableF X4 Int64 where
  selectF (MkBoolX4 !cond) (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = MkInt64X4WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3)
instance (i0 < 4, i1 < 4, i2 < 4, i3 < 4, Pick Int64 i0, Pick Int64 i1, Pick Int64 i2, Pick Int64 i3) => UnaryShuffle [i0, i1, i2, i3] X4 Int64 where
  unaryShuffle (MkInt64X4WithElems x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkInt64X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, Pick Int64 i0, Pick Int64 i1, Pick Int64 i2, Pick Int64 i3) => BinaryShuffle [i0, i1, i2, i3] X4 Int64 where
  binaryShuffle (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt64X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Int64 where
  eqF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = mkX4 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3)
  {-# INLINE eqF #-}
instance OrderedF X4 Int64 where
  ltF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = mkX4 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3)
  leF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = mkX4 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3)
  gtF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = mkX4 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3)
  geF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = mkX4 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Int64 where
  minF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = MkInt64X4WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3)
  maxF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = MkInt64X4WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Int64 where
  plusF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = MkInt64X4WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
  minusF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = MkInt64X4WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3)
  timesF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = MkInt64X4WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
  negateF (MkInt64X4WithElems x0 x1 x2 x3) = MkInt64X4WithElems (- x0) (- x1) (- x2) (- x3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X4 Int64 where
  andF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = MkInt64X4WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3)
  orF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = MkInt64X4WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3)
  xorF (MkInt64X4WithElems x0 x1 x2 x3) (MkInt64X4WithElems y0 y1 y2 y3) = MkInt64X4WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3)
  complementF (MkInt64X4WithElems x0 x1 x2 x3) = MkInt64X4WithElems (complement x0) (complement x1) (complement x2) (complement x3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Int64 where
  shiftLF (MkInt64X4WithElems x0 x1 x2 x3) !i = MkInt64X4WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i)
  unsafeShiftLF (MkInt64X4WithElems x0 x1 x2 x3) !i = MkInt64X4WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i)
  shiftRF (MkInt64X4WithElems x0 x1 x2 x3) !i = MkInt64X4WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i)
  unsafeShiftRF (MkInt64X4WithElems x0 x1 x2 x3) !i = MkInt64X4WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X4 Int64 where
  enumFromZero = MkInt64X4WithElems 0 1 2 3
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Int64 where
  indexByteArraySIMD# ba i = MkInt64X4WithElems (I64# (GHC.Exts.indexInt64Array# ba i)) (I64# (GHC.Exts.indexInt64Array# ba (i +# 1#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 2#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 3#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt64Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt64Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt64Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt64Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt64X4WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) #)
  writeByteArraySIMD# mba i (MkInt64X4WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3)) s0 = case GHC.Exts.writeInt64Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt64Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt64Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeInt64Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt64OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkInt64X4WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X4WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3)) = IO (\s0 -> case GHC.Exts.writeInt64OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 2#) x2 s2 of s3 -> (# GHC.Exts.writeInt64OffAddr# addr (i +# 3#) x3 s3, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Word8 = MkWord8X4WithElems !Word8 !Word8 !Word8 !Word8
instance PackX4 X4 Word8 where
  mkX4 = MkWord8X4WithElems
  unpackX4 (MkWord8X4WithElems x0 x1 x2 x3) = (x0, x1, x2, x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Word8 where
  broadcast !x = MkWord8X4WithElems x x x x
  {-# INLINE broadcast #-}
instance SelectableF X4 Word8 where
  selectF (MkBoolX4 !cond) (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = MkWord8X4WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3)
instance (i0 < 4, i1 < 4, i2 < 4, i3 < 4, Pick Word8 i0, Pick Word8 i1, Pick Word8 i2, Pick Word8 i3) => UnaryShuffle [i0, i1, i2, i3] X4 Word8 where
  unaryShuffle (MkWord8X4WithElems x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord8X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, Pick Word8 i0, Pick Word8 i1, Pick Word8 i2, Pick Word8 i3) => BinaryShuffle [i0, i1, i2, i3] X4 Word8 where
  binaryShuffle (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord8X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Word8 where
  eqF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = mkX4 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3)
  {-# INLINE eqF #-}
instance OrderedF X4 Word8 where
  ltF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = mkX4 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3)
  leF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = mkX4 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3)
  gtF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = mkX4 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3)
  geF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = mkX4 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Word8 where
  minF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = MkWord8X4WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3)
  maxF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = MkWord8X4WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Word8 where
  plusF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = MkWord8X4WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
  minusF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = MkWord8X4WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3)
  timesF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = MkWord8X4WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
  negateF (MkWord8X4WithElems x0 x1 x2 x3) = MkWord8X4WithElems (- x0) (- x1) (- x2) (- x3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X4 Word8 where
  andF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = MkWord8X4WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3)
  orF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = MkWord8X4WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3)
  xorF (MkWord8X4WithElems x0 x1 x2 x3) (MkWord8X4WithElems y0 y1 y2 y3) = MkWord8X4WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3)
  complementF (MkWord8X4WithElems x0 x1 x2 x3) = MkWord8X4WithElems (complement x0) (complement x1) (complement x2) (complement x3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Word8 where
  shiftLF (MkWord8X4WithElems x0 x1 x2 x3) !i = MkWord8X4WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i)
  unsafeShiftLF (MkWord8X4WithElems x0 x1 x2 x3) !i = MkWord8X4WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i)
  shiftRF (MkWord8X4WithElems x0 x1 x2 x3) !i = MkWord8X4WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i)
  unsafeShiftRF (MkWord8X4WithElems x0 x1 x2 x3) !i = MkWord8X4WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X4 Word8 where
  enumFromZero = MkWord8X4WithElems 0 1 2 3
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Word8 where
  indexByteArraySIMD# ba i = MkWord8X4WithElems (W8# (GHC.Exts.indexWord8Array# ba i)) (W8# (GHC.Exts.indexWord8Array# ba (i +# 1#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 2#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 3#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord8X4WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) #)
  writeByteArraySIMD# mba i (MkWord8X4WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3)) s0 = case GHC.Exts.writeWord8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeWord8Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord8X4WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X4WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3)) = IO (\s0 -> case GHC.Exts.writeWord8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 2#) x2 s2 of s3 -> (# GHC.Exts.writeWord8OffAddr# addr (i +# 3#) x3 s3, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Word16 = MkWord16X4WithElems !Word16 !Word16 !Word16 !Word16
instance PackX4 X4 Word16 where
  mkX4 = MkWord16X4WithElems
  unpackX4 (MkWord16X4WithElems x0 x1 x2 x3) = (x0, x1, x2, x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Word16 where
  broadcast !x = MkWord16X4WithElems x x x x
  {-# INLINE broadcast #-}
instance SelectableF X4 Word16 where
  selectF (MkBoolX4 !cond) (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = MkWord16X4WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3)
instance (i0 < 4, i1 < 4, i2 < 4, i3 < 4, Pick Word16 i0, Pick Word16 i1, Pick Word16 i2, Pick Word16 i3) => UnaryShuffle [i0, i1, i2, i3] X4 Word16 where
  unaryShuffle (MkWord16X4WithElems x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord16X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, Pick Word16 i0, Pick Word16 i1, Pick Word16 i2, Pick Word16 i3) => BinaryShuffle [i0, i1, i2, i3] X4 Word16 where
  binaryShuffle (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord16X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Word16 where
  eqF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = mkX4 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3)
  {-# INLINE eqF #-}
instance OrderedF X4 Word16 where
  ltF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = mkX4 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3)
  leF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = mkX4 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3)
  gtF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = mkX4 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3)
  geF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = mkX4 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Word16 where
  minF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = MkWord16X4WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3)
  maxF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = MkWord16X4WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Word16 where
  plusF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = MkWord16X4WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
  minusF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = MkWord16X4WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3)
  timesF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = MkWord16X4WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
  negateF (MkWord16X4WithElems x0 x1 x2 x3) = MkWord16X4WithElems (- x0) (- x1) (- x2) (- x3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X4 Word16 where
  andF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = MkWord16X4WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3)
  orF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = MkWord16X4WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3)
  xorF (MkWord16X4WithElems x0 x1 x2 x3) (MkWord16X4WithElems y0 y1 y2 y3) = MkWord16X4WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3)
  complementF (MkWord16X4WithElems x0 x1 x2 x3) = MkWord16X4WithElems (complement x0) (complement x1) (complement x2) (complement x3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Word16 where
  shiftLF (MkWord16X4WithElems x0 x1 x2 x3) !i = MkWord16X4WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i)
  unsafeShiftLF (MkWord16X4WithElems x0 x1 x2 x3) !i = MkWord16X4WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i)
  shiftRF (MkWord16X4WithElems x0 x1 x2 x3) !i = MkWord16X4WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i)
  unsafeShiftRF (MkWord16X4WithElems x0 x1 x2 x3) !i = MkWord16X4WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X4 Word16 where
  enumFromZero = MkWord16X4WithElems 0 1 2 3
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Word16 where
  indexByteArraySIMD# ba i = MkWord16X4WithElems (W16# (GHC.Exts.indexWord16Array# ba i)) (W16# (GHC.Exts.indexWord16Array# ba (i +# 1#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 2#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 3#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord16Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord16Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord16X4WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) #)
  writeByteArraySIMD# mba i (MkWord16X4WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3)) s0 = case GHC.Exts.writeWord16Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord16Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord16Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeWord16Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord16X4WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X4WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3)) = IO (\s0 -> case GHC.Exts.writeWord16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 2#) x2 s2 of s3 -> (# GHC.Exts.writeWord16OffAddr# addr (i +# 3#) x3 s3, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Word32 = MkWord32X4WithElems !Word32 !Word32 !Word32 !Word32
instance PackX4 X4 Word32 where
  mkX4 = MkWord32X4WithElems
  unpackX4 (MkWord32X4WithElems x0 x1 x2 x3) = (x0, x1, x2, x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Word32 where
  broadcast !x = MkWord32X4WithElems x x x x
  {-# INLINE broadcast #-}
instance SelectableF X4 Word32 where
  selectF (MkBoolX4 !cond) (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = MkWord32X4WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3)
instance (i0 < 4, i1 < 4, i2 < 4, i3 < 4, Pick Word32 i0, Pick Word32 i1, Pick Word32 i2, Pick Word32 i3) => UnaryShuffle [i0, i1, i2, i3] X4 Word32 where
  unaryShuffle (MkWord32X4WithElems x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord32X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, Pick Word32 i0, Pick Word32 i1, Pick Word32 i2, Pick Word32 i3) => BinaryShuffle [i0, i1, i2, i3] X4 Word32 where
  binaryShuffle (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord32X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Word32 where
  eqF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = mkX4 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3)
  {-# INLINE eqF #-}
instance OrderedF X4 Word32 where
  ltF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = mkX4 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3)
  leF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = mkX4 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3)
  gtF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = mkX4 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3)
  geF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = mkX4 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Word32 where
  minF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = MkWord32X4WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3)
  maxF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = MkWord32X4WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Word32 where
  plusF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = MkWord32X4WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
  minusF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = MkWord32X4WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3)
  timesF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = MkWord32X4WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
  negateF (MkWord32X4WithElems x0 x1 x2 x3) = MkWord32X4WithElems (- x0) (- x1) (- x2) (- x3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X4 Word32 where
  andF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = MkWord32X4WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3)
  orF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = MkWord32X4WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3)
  xorF (MkWord32X4WithElems x0 x1 x2 x3) (MkWord32X4WithElems y0 y1 y2 y3) = MkWord32X4WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3)
  complementF (MkWord32X4WithElems x0 x1 x2 x3) = MkWord32X4WithElems (complement x0) (complement x1) (complement x2) (complement x3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Word32 where
  shiftLF (MkWord32X4WithElems x0 x1 x2 x3) !i = MkWord32X4WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i)
  unsafeShiftLF (MkWord32X4WithElems x0 x1 x2 x3) !i = MkWord32X4WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i)
  shiftRF (MkWord32X4WithElems x0 x1 x2 x3) !i = MkWord32X4WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i)
  unsafeShiftRF (MkWord32X4WithElems x0 x1 x2 x3) !i = MkWord32X4WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X4 Word32 where
  enumFromZero = MkWord32X4WithElems 0 1 2 3
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Word32 where
  indexByteArraySIMD# ba i = MkWord32X4WithElems (W32# (GHC.Exts.indexWord32Array# ba i)) (W32# (GHC.Exts.indexWord32Array# ba (i +# 1#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 2#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 3#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord32Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord32Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord32X4WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) #)
  writeByteArraySIMD# mba i (MkWord32X4WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3)) s0 = case GHC.Exts.writeWord32Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord32Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord32Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeWord32Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord32X4WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X4WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3)) = IO (\s0 -> case GHC.Exts.writeWord32OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 2#) x2 s2 of s3 -> (# GHC.Exts.writeWord32OffAddr# addr (i +# 3#) x3 s3, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X4 Word64 = MkWord64X4WithElems !Word64 !Word64 !Word64 !Word64
instance PackX4 X4 Word64 where
  mkX4 = MkWord64X4WithElems
  unpackX4 (MkWord64X4WithElems x0 x1 x2 x3) = (x0, x1, x2, x3)
  {-# INLINE mkX4 #-}
  {-# INLINE unpackX4 #-}
instance Broadcast X4 Word64 where
  broadcast !x = MkWord64X4WithElems x x x x
  {-# INLINE broadcast #-}
instance SelectableF X4 Word64 where
  selectF (MkBoolX4 !cond) (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = MkWord64X4WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3)
instance (i0 < 4, i1 < 4, i2 < 4, i3 < 4, Pick Word64 i0, Pick Word64 i1, Pick Word64 i2, Pick Word64 i3) => UnaryShuffle [i0, i1, i2, i3] X4 Word64 where
  unaryShuffle (MkWord64X4WithElems x0 x1 x2 x3) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; _ -> x3 } } in MkWord64X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, Pick Word64 i0, Pick Word64 i1, Pick Word64 i2, Pick Word64 i3) => BinaryShuffle [i0, i1, i2, i3] X4 Word64 where
  binaryShuffle (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord64X4WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X4 Word64 where
  eqF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = mkX4 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3)
  {-# INLINE eqF #-}
instance OrderedF X4 Word64 where
  ltF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = mkX4 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3)
  leF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = mkX4 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3)
  gtF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = mkX4 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3)
  geF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = mkX4 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X4 Word64 where
  minF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = MkWord64X4WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3)
  maxF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = MkWord64X4WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X4 Word64 where
  plusF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = MkWord64X4WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3)
  minusF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = MkWord64X4WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3)
  timesF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = MkWord64X4WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3)
  negateF (MkWord64X4WithElems x0 x1 x2 x3) = MkWord64X4WithElems (- x0) (- x1) (- x2) (- x3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X4 Word64 where
  andF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = MkWord64X4WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3)
  orF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = MkWord64X4WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3)
  xorF (MkWord64X4WithElems x0 x1 x2 x3) (MkWord64X4WithElems y0 y1 y2 y3) = MkWord64X4WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3)
  complementF (MkWord64X4WithElems x0 x1 x2 x3) = MkWord64X4WithElems (complement x0) (complement x1) (complement x2) (complement x3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X4 Word64 where
  shiftLF (MkWord64X4WithElems x0 x1 x2 x3) !i = MkWord64X4WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i)
  unsafeShiftLF (MkWord64X4WithElems x0 x1 x2 x3) !i = MkWord64X4WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i)
  shiftRF (MkWord64X4WithElems x0 x1 x2 x3) !i = MkWord64X4WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i)
  unsafeShiftRF (MkWord64X4WithElems x0 x1 x2 x3) !i = MkWord64X4WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X4 Word64 where
  enumFromZero = MkWord64X4WithElems 0 1 2 3
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X4 Word64 where
  indexByteArraySIMD# ba i = MkWord64X4WithElems (W64# (GHC.Exts.indexWord64Array# ba i)) (W64# (GHC.Exts.indexWord64Array# ba (i +# 1#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 2#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 3#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord64Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord64Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord64Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord64Array# mba (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord64X4WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) #)
  writeByteArraySIMD# mba i (MkWord64X4WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3)) s0 = case GHC.Exts.writeWord64Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord64Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord64Array# mba (i +# 2#) x2 s2 of s3 -> GHC.Exts.writeWord64Array# mba (i +# 3#) x3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X4 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord64OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> (# s4, MkWord64X4WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X4WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3)) = IO (\s0 -> case GHC.Exts.writeWord64OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 2#) x2 s2 of s3 -> (# GHC.Exts.writeWord64OffAddr# addr (i +# 3#) x3 s3, () #))
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
