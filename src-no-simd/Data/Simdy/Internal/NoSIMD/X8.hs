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
module Data.Simdy.Internal.NoSIMD.X8 where
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
data instance X8 Float = MkFloatX8WithElems !Float !Float !Float !Float !Float !Float !Float !Float
instance PackX8 X8 Float where
  mkX8 = MkFloatX8WithElems
  unpackX8 (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Float where
  broadcast !x = MkFloatX8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Float where
  selectF (MkBoolX8 !cond) (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkFloatX8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Float i0, Pick Float i1, Pick Float i2, Pick Float i3, Pick Float i4, Pick Float i5, Pick Float i6, Pick Float i7) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Float where
  unaryShuffle (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkFloatX8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Float i0, Pick Float i1, Pick Float i2, Pick Float i3, Pick Float i4, Pick Float i5, Pick Float i6, Pick Float i7) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Float where
  binaryShuffle (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkFloatX8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Float where
  eqF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Float where
  ltF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Float where
  minF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkFloatX8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkFloatX8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkFloatX8WithElems (minimumNumber x0 y0) (minimumNumber x1 y1) (minimumNumber x2 y2) (minimumNumber x3 y3) (minimumNumber x4 y4) (minimumNumber x5 y5) (minimumNumber x6 y6) (minimumNumber x7 y7)
  maximumNumberF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkFloatX8WithElems (maximumNumber x0 y0) (maximumNumber x1 y1) (maximumNumber x2 y2) (maximumNumber x3 y3) (maximumNumber x4 y4) (maximumNumber x5 y5) (maximumNumber x6 y6) (maximumNumber x7 y7)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX8Float :: X8 Float -> X8 Float
negateX8Float (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkFloatX8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
#if defined(USE_FMA)
{-# INLINE [0] negateX8Float #-}
#else
{-# INLINE negateX8Float #-}
#endif
instance NumF X8 Float where
  plusF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkFloatX8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkFloatX8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkFloatX8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF = negateX8Float
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance FractionalF X8 Float where
  divideF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkFloatX8WithElems (x0 / y0) (x1 / y1) (x2 / y2) (x3 / y3) (x4 / y4) (x5 / y5) (x6 / y6) (x7 / y7)
  recipF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkFloatX8WithElems (recip x0) (recip x1) (recip x2) (recip x3) (recip x4) (recip x5) (recip x6) (recip x7)
  {-# INLINE divideF #-}
  {-# INLINE recipF #-}
instance FloatingF X8 Float where
  sqrtF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkFloatX8WithElems (sqrt x0) (sqrt x1) (sqrt x2) (sqrt x3) (sqrt x4) (sqrt x5) (sqrt x6) (sqrt x7)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X8 Float where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkFloatX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkFloatX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) (MkFloatX8WithElems z0 z1 z2 z3 z4 z5 z6 z7) = MkFloatX8WithElems (fusedMultiplyAdd x0 y0 z0) (fusedMultiplyAdd x1 y1 z1) (fusedMultiplyAdd x2 y2 z2) (fusedMultiplyAdd x3 y3 z3) (fusedMultiplyAdd x4 y4 z4) (fusedMultiplyAdd x5 y5 z5) (fusedMultiplyAdd x6 y6 z6) (fusedMultiplyAdd x7 y7 z7)
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
  enumFromZero = MkFloatX8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Float where
  indexByteArraySIMD# ba i = MkFloatX8WithElems (F# (GHC.Exts.indexFloatArray# ba i)) (F# (GHC.Exts.indexFloatArray# ba (i +# 1#))) (F# (GHC.Exts.indexFloatArray# ba (i +# 2#))) (F# (GHC.Exts.indexFloatArray# ba (i +# 3#))) (F# (GHC.Exts.indexFloatArray# ba (i +# 4#))) (F# (GHC.Exts.indexFloatArray# ba (i +# 5#))) (F# (GHC.Exts.indexFloatArray# ba (i +# 6#))) (F# (GHC.Exts.indexFloatArray# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readFloatArray# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readFloatArray# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readFloatArray# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readFloatArray# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readFloatArray# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readFloatArray# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readFloatArray# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readFloatArray# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkFloatX8WithElems (F# x0) (F# x1) (F# x2) (F# x3) (F# x4) (F# x5) (F# x6) (F# x7) #)
  writeByteArraySIMD# mba i (MkFloatX8WithElems (F# x0) (F# x1) (F# x2) (F# x3) (F# x4) (F# x5) (F# x6) (F# x7)) s0 = case GHC.Exts.writeFloatArray# mba i x0 s0 of s1 -> case GHC.Exts.writeFloatArray# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeFloatArray# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeFloatArray# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeFloatArray# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeFloatArray# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeFloatArray# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeFloatArray# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Float where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readFloatOffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readFloatOffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkFloatX8WithElems (F# x0) (F# x1) (F# x2) (F# x3) (F# x4) (F# x5) (F# x6) (F# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkFloatX8WithElems (F# x0) (F# x1) (F# x2) (F# x3) (F# x4) (F# x5) (F# x6) (F# x7)) = IO (\s0 -> case GHC.Exts.writeFloatOffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeFloatOffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeFloatOffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeFloatOffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeFloatOffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeFloatOffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeFloatOffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeFloatOffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Double = MkDoubleX8WithElems !Double !Double !Double !Double !Double !Double !Double !Double
instance PackX8 X8 Double where
  mkX8 = MkDoubleX8WithElems
  unpackX8 (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Double where
  broadcast !x = MkDoubleX8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Double where
  selectF (MkBoolX8 !cond) (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Double i0, Pick Double i1, Pick Double i2, Pick Double i3, Pick Double i4, Pick Double i5, Pick Double i6, Pick Double i7) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Double where
  unaryShuffle (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkDoubleX8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Double i0, Pick Double i1, Pick Double i2, Pick Double i3, Pick Double i4, Pick Double i5, Pick Double i6, Pick Double i7) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Double where
  binaryShuffle (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkDoubleX8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Double where
  eqF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Double where
  ltF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Double where
  minF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX8WithElems (minimumNumber x0 y0) (minimumNumber x1 y1) (minimumNumber x2 y2) (minimumNumber x3 y3) (minimumNumber x4 y4) (minimumNumber x5 y5) (minimumNumber x6 y6) (minimumNumber x7 y7)
  maximumNumberF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX8WithElems (maximumNumber x0 y0) (maximumNumber x1 y1) (maximumNumber x2 y2) (maximumNumber x3 y3) (maximumNumber x4 y4) (maximumNumber x5 y5) (maximumNumber x6 y6) (maximumNumber x7 y7)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
negateX8Double :: X8 Double -> X8 Double
negateX8Double (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkDoubleX8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
#if defined(USE_FMA)
{-# INLINE [0] negateX8Double #-}
#else
{-# INLINE negateX8Double #-}
#endif
instance NumF X8 Double where
  plusF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF = negateX8Double
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance FractionalF X8 Double where
  divideF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkDoubleX8WithElems (x0 / y0) (x1 / y1) (x2 / y2) (x3 / y3) (x4 / y4) (x5 / y5) (x6 / y6) (x7 / y7)
  recipF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkDoubleX8WithElems (recip x0) (recip x1) (recip x2) (recip x3) (recip x4) (recip x5) (recip x6) (recip x7)
  {-# INLINE divideF #-}
  {-# INLINE recipF #-}
instance FloatingF X8 Double where
  sqrtF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkDoubleX8WithElems (sqrt x0) (sqrt x1) (sqrt x2) (sqrt x3) (sqrt x4) (sqrt x5) (sqrt x6) (sqrt x7)
  {-# INLINE sqrtF #-}
instance HasFMA => FusedMultiplyAddF X8 Double where
#if defined(USE_FMA)
  fusedMultiplyAddF (MkDoubleX8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkDoubleX8WithElems y0 y1 y2 y3 y4 y5 y6 y7) (MkDoubleX8WithElems z0 z1 z2 z3 z4 z5 z6 z7) = MkDoubleX8WithElems (fusedMultiplyAdd x0 y0 z0) (fusedMultiplyAdd x1 y1 z1) (fusedMultiplyAdd x2 y2 z2) (fusedMultiplyAdd x3 y3 z3) (fusedMultiplyAdd x4 y4 z4) (fusedMultiplyAdd x5 y5 z5) (fusedMultiplyAdd x6 y6 z6) (fusedMultiplyAdd x7 y7 z7)
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
  enumFromZero = MkDoubleX8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Double where
  indexByteArraySIMD# ba i = MkDoubleX8WithElems (D# (GHC.Exts.indexDoubleArray# ba i)) (D# (GHC.Exts.indexDoubleArray# ba (i +# 1#))) (D# (GHC.Exts.indexDoubleArray# ba (i +# 2#))) (D# (GHC.Exts.indexDoubleArray# ba (i +# 3#))) (D# (GHC.Exts.indexDoubleArray# ba (i +# 4#))) (D# (GHC.Exts.indexDoubleArray# ba (i +# 5#))) (D# (GHC.Exts.indexDoubleArray# ba (i +# 6#))) (D# (GHC.Exts.indexDoubleArray# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readDoubleArray# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readDoubleArray# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readDoubleArray# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readDoubleArray# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readDoubleArray# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readDoubleArray# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readDoubleArray# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readDoubleArray# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkDoubleX8WithElems (D# x0) (D# x1) (D# x2) (D# x3) (D# x4) (D# x5) (D# x6) (D# x7) #)
  writeByteArraySIMD# mba i (MkDoubleX8WithElems (D# x0) (D# x1) (D# x2) (D# x3) (D# x4) (D# x5) (D# x6) (D# x7)) s0 = case GHC.Exts.writeDoubleArray# mba i x0 s0 of s1 -> case GHC.Exts.writeDoubleArray# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeDoubleArray# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeDoubleArray# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeDoubleArray# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeDoubleArray# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeDoubleArray# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeDoubleArray# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Double where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readDoubleOffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readDoubleOffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkDoubleX8WithElems (D# x0) (D# x1) (D# x2) (D# x3) (D# x4) (D# x5) (D# x6) (D# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkDoubleX8WithElems (D# x0) (D# x1) (D# x2) (D# x3) (D# x4) (D# x5) (D# x6) (D# x7)) = IO (\s0 -> case GHC.Exts.writeDoubleOffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeDoubleOffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeDoubleOffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeDoubleOffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeDoubleOffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeDoubleOffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeDoubleOffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeDoubleOffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
instance ImplementationDescription X8 where
  implementationDescription _ = "X8;maxBits=0"
data instance X8 Int8 = MkInt8X8WithElems !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8 !Int8
instance PackX8 X8 Int8 where
  mkX8 = MkInt8X8WithElems
  unpackX8 (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int8 where
  broadcast !x = MkInt8X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Int8 where
  selectF (MkBoolX8 !cond) (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Int8 i0, Pick Int8 i1, Pick Int8 i2, Pick Int8 i3, Pick Int8 i4, Pick Int8 i5, Pick Int8 i6, Pick Int8 i7) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int8 where
  unaryShuffle (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Int8 i0, Pick Int8 i1, Pick Int8 i2, Pick Int8 i3, Pick Int8 i4, Pick Int8 i5, Pick Int8 i6, Pick Int8 i7) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int8 where
  binaryShuffle (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int8 where
  eqF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Int8 where
  ltF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int8 where
  minF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int8 where
  plusF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt8X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Int8 where
  andF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt8X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt8X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int8 where
  shiftLF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt8X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt8X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt8X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkInt8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt8X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Int8 where
  enumFromZero = MkInt8X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int8 where
  indexByteArraySIMD# ba i = MkInt8X8WithElems (I8# (GHC.Exts.indexInt8Array# ba i)) (I8# (GHC.Exts.indexInt8Array# ba (i +# 1#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 2#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 3#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 4#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 5#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 6#))) (I8# (GHC.Exts.indexInt8Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt8Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt8Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt8Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt8Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt8X8WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) #)
  writeByteArraySIMD# mba i (MkInt8X8WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7)) s0 = case GHC.Exts.writeInt8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt8Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt8Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt8Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeInt8Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt8OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt8X8WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt8X8WithElems (I8# x0) (I8# x1) (I8# x2) (I8# x3) (I8# x4) (I8# x5) (I8# x6) (I8# x7)) = IO (\s0 -> case GHC.Exts.writeInt8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt8OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeInt8OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int16 = MkInt16X8WithElems !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16 !Int16
instance PackX8 X8 Int16 where
  mkX8 = MkInt16X8WithElems
  unpackX8 (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int16 where
  broadcast !x = MkInt16X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Int16 where
  selectF (MkBoolX8 !cond) (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Int16 i0, Pick Int16 i1, Pick Int16 i2, Pick Int16 i3, Pick Int16 i4, Pick Int16 i5, Pick Int16 i6, Pick Int16 i7) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int16 where
  unaryShuffle (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Int16 i0, Pick Int16 i1, Pick Int16 i2, Pick Int16 i3, Pick Int16 i4, Pick Int16 i5, Pick Int16 i6, Pick Int16 i7) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int16 where
  binaryShuffle (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int16 where
  eqF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Int16 where
  ltF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int16 where
  minF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int16 where
  plusF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt16X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Int16 where
  andF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt16X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt16X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int16 where
  shiftLF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt16X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt16X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt16X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkInt16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt16X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Int16 where
  enumFromZero = MkInt16X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int16 where
  indexByteArraySIMD# ba i = MkInt16X8WithElems (I16# (GHC.Exts.indexInt16Array# ba i)) (I16# (GHC.Exts.indexInt16Array# ba (i +# 1#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 2#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 3#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 4#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 5#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 6#))) (I16# (GHC.Exts.indexInt16Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt16Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt16Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt16Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt16Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt16Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt16Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt16X8WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) #)
  writeByteArraySIMD# mba i (MkInt16X8WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7)) s0 = case GHC.Exts.writeInt16Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt16Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt16Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt16Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt16Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt16Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt16Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeInt16Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt16OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt16X8WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt16X8WithElems (I16# x0) (I16# x1) (I16# x2) (I16# x3) (I16# x4) (I16# x5) (I16# x6) (I16# x7)) = IO (\s0 -> case GHC.Exts.writeInt16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt16OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeInt16OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int32 = MkInt32X8WithElems !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32 !Int32
instance PackX8 X8 Int32 where
  mkX8 = MkInt32X8WithElems
  unpackX8 (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int32 where
  broadcast !x = MkInt32X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Int32 where
  selectF (MkBoolX8 !cond) (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Int32 i0, Pick Int32 i1, Pick Int32 i2, Pick Int32 i3, Pick Int32 i4, Pick Int32 i5, Pick Int32 i6, Pick Int32 i7) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int32 where
  unaryShuffle (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Int32 i0, Pick Int32 i1, Pick Int32 i2, Pick Int32 i3, Pick Int32 i4, Pick Int32 i5, Pick Int32 i6, Pick Int32 i7) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int32 where
  binaryShuffle (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int32 where
  eqF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Int32 where
  ltF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int32 where
  minF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int32 where
  plusF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt32X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Int32 where
  andF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt32X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt32X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int32 where
  shiftLF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt32X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt32X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt32X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkInt32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt32X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Int32 where
  enumFromZero = MkInt32X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int32 where
  indexByteArraySIMD# ba i = MkInt32X8WithElems (I32# (GHC.Exts.indexInt32Array# ba i)) (I32# (GHC.Exts.indexInt32Array# ba (i +# 1#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 2#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 3#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 4#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 5#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 6#))) (I32# (GHC.Exts.indexInt32Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt32Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt32Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt32Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt32Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt32Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt32Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt32X8WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) #)
  writeByteArraySIMD# mba i (MkInt32X8WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7)) s0 = case GHC.Exts.writeInt32Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt32Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt32Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt32Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt32Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt32Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt32Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeInt32Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt32OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt32X8WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt32X8WithElems (I32# x0) (I32# x1) (I32# x2) (I32# x3) (I32# x4) (I32# x5) (I32# x6) (I32# x7)) = IO (\s0 -> case GHC.Exts.writeInt32OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt32OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeInt32OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Int64 = MkInt64X8WithElems !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64 !Int64
instance PackX8 X8 Int64 where
  mkX8 = MkInt64X8WithElems
  unpackX8 (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Int64 where
  broadcast !x = MkInt64X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Int64 where
  selectF (MkBoolX8 !cond) (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Int64 i0, Pick Int64 i1, Pick Int64 i2, Pick Int64 i3, Pick Int64 i4, Pick Int64 i5, Pick Int64 i6, Pick Int64 i7) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int64 where
  unaryShuffle (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkInt64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Int64 i0, Pick Int64 i1, Pick Int64 i2, Pick Int64 i3, Pick Int64 i4, Pick Int64 i5, Pick Int64 i6, Pick Int64 i7) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Int64 where
  binaryShuffle (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkInt64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Int64 where
  eqF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Int64 where
  ltF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Int64 where
  minF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Int64 where
  plusF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt64X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Int64 where
  andF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkInt64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkInt64X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkInt64X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Int64 where
  shiftLF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt64X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt64X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt64X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkInt64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkInt64X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Int64 where
  enumFromZero = MkInt64X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Int64 where
  indexByteArraySIMD# ba i = MkInt64X8WithElems (I64# (GHC.Exts.indexInt64Array# ba i)) (I64# (GHC.Exts.indexInt64Array# ba (i +# 1#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 2#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 3#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 4#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 5#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 6#))) (I64# (GHC.Exts.indexInt64Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readInt64Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readInt64Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt64Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt64Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt64Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt64Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt64Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt64Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt64X8WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) #)
  writeByteArraySIMD# mba i (MkInt64X8WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7)) s0 = case GHC.Exts.writeInt64Array# mba i x0 s0 of s1 -> case GHC.Exts.writeInt64Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt64Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt64Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt64Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt64Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt64Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeInt64Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Int64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readInt64OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readInt64OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkInt64X8WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkInt64X8WithElems (I64# x0) (I64# x1) (I64# x2) (I64# x3) (I64# x4) (I64# x5) (I64# x6) (I64# x7)) = IO (\s0 -> case GHC.Exts.writeInt64OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeInt64OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeInt64OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word8 = MkWord8X8WithElems !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8 !Word8
instance PackX8 X8 Word8 where
  mkX8 = MkWord8X8WithElems
  unpackX8 (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word8 where
  broadcast !x = MkWord8X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Word8 where
  selectF (MkBoolX8 !cond) (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Word8 i0, Pick Word8 i1, Pick Word8 i2, Pick Word8 i3, Pick Word8 i4, Pick Word8 i5, Pick Word8 i6, Pick Word8 i7) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word8 where
  unaryShuffle (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Word8 i0, Pick Word8 i1, Pick Word8 i2, Pick Word8 i3, Pick Word8 i4, Pick Word8 i5, Pick Word8 i6, Pick Word8 i7) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word8 where
  binaryShuffle (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord8X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word8 where
  eqF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Word8 where
  ltF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word8 where
  minF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word8 where
  plusF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord8X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Word8 where
  andF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord8X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord8X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord8X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word8 where
  shiftLF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord8X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord8X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord8X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkWord8X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord8X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Word8 where
  enumFromZero = MkWord8X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word8 where
  indexByteArraySIMD# ba i = MkWord8X8WithElems (W8# (GHC.Exts.indexWord8Array# ba i)) (W8# (GHC.Exts.indexWord8Array# ba (i +# 1#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 2#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 3#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 4#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 5#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 6#))) (W8# (GHC.Exts.indexWord8Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord8Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord8Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord8Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord8Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord8Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord8X8WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) #)
  writeByteArraySIMD# mba i (MkWord8X8WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7)) s0 = case GHC.Exts.writeWord8Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord8Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord8Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord8Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord8Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeWord8Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word8 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord8OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord8OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord8X8WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord8X8WithElems (W8# x0) (W8# x1) (W8# x2) (W8# x3) (W8# x4) (W8# x5) (W8# x6) (W8# x7)) = IO (\s0 -> case GHC.Exts.writeWord8OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord8OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeWord8OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word16 = MkWord16X8WithElems !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16 !Word16
instance PackX8 X8 Word16 where
  mkX8 = MkWord16X8WithElems
  unpackX8 (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word16 where
  broadcast !x = MkWord16X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Word16 where
  selectF (MkBoolX8 !cond) (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Word16 i0, Pick Word16 i1, Pick Word16 i2, Pick Word16 i3, Pick Word16 i4, Pick Word16 i5, Pick Word16 i6, Pick Word16 i7) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word16 where
  unaryShuffle (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Word16 i0, Pick Word16 i1, Pick Word16 i2, Pick Word16 i3, Pick Word16 i4, Pick Word16 i5, Pick Word16 i6, Pick Word16 i7) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word16 where
  binaryShuffle (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord16X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word16 where
  eqF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Word16 where
  ltF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word16 where
  minF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word16 where
  plusF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord16X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Word16 where
  andF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord16X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord16X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord16X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word16 where
  shiftLF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord16X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord16X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord16X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkWord16X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord16X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Word16 where
  enumFromZero = MkWord16X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word16 where
  indexByteArraySIMD# ba i = MkWord16X8WithElems (W16# (GHC.Exts.indexWord16Array# ba i)) (W16# (GHC.Exts.indexWord16Array# ba (i +# 1#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 2#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 3#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 4#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 5#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 6#))) (W16# (GHC.Exts.indexWord16Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord16Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord16Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord16Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord16Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord16Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord16Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord16Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord16X8WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) #)
  writeByteArraySIMD# mba i (MkWord16X8WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7)) s0 = case GHC.Exts.writeWord16Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord16Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord16Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord16Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord16Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord16Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord16Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeWord16Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word16 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord16OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord16OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord16X8WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord16X8WithElems (W16# x0) (W16# x1) (W16# x2) (W16# x3) (W16# x4) (W16# x5) (W16# x6) (W16# x7)) = IO (\s0 -> case GHC.Exts.writeWord16OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord16OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeWord16OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word32 = MkWord32X8WithElems !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32
instance PackX8 X8 Word32 where
  mkX8 = MkWord32X8WithElems
  unpackX8 (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word32 where
  broadcast !x = MkWord32X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Word32 where
  selectF (MkBoolX8 !cond) (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Word32 i0, Pick Word32 i1, Pick Word32 i2, Pick Word32 i3, Pick Word32 i4, Pick Word32 i5, Pick Word32 i6, Pick Word32 i7) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word32 where
  unaryShuffle (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Word32 i0, Pick Word32 i1, Pick Word32 i2, Pick Word32 i3, Pick Word32 i4, Pick Word32 i5, Pick Word32 i6, Pick Word32 i7) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word32 where
  binaryShuffle (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord32X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word32 where
  eqF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Word32 where
  ltF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word32 where
  minF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word32 where
  plusF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord32X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Word32 where
  andF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord32X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord32X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord32X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word32 where
  shiftLF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord32X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord32X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord32X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkWord32X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord32X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Word32 where
  enumFromZero = MkWord32X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word32 where
  indexByteArraySIMD# ba i = MkWord32X8WithElems (W32# (GHC.Exts.indexWord32Array# ba i)) (W32# (GHC.Exts.indexWord32Array# ba (i +# 1#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 2#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 3#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 4#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 5#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 6#))) (W32# (GHC.Exts.indexWord32Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord32Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord32Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord32Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord32Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord32Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord32Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord32Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord32X8WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) #)
  writeByteArraySIMD# mba i (MkWord32X8WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7)) s0 = case GHC.Exts.writeWord32Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord32Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord32Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord32Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord32Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord32Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord32Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeWord32Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word32 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord32OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord32OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord32X8WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord32X8WithElems (W32# x0) (W32# x1) (W32# x2) (W32# x3) (W32# x4) (W32# x5) (W32# x6) (W32# x7)) = IO (\s0 -> case GHC.Exts.writeWord32OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord32OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeWord32OffAddr# addr (i +# 7#) x7 s7, () #))
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
data instance X8 Word64 = MkWord64X8WithElems !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64
instance PackX8 X8 Word64 where
  mkX8 = MkWord64X8WithElems
  unpackX8 (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = (x0, x1, x2, x3, x4, x5, x6, x7)
  {-# INLINE mkX8 #-}
  {-# INLINE unpackX8 #-}
instance Broadcast X8 Word64 where
  broadcast !x = MkWord64X8WithElems x x x x x x x x
  {-# INLINE broadcast #-}
instance SelectableF X8 Word64 where
  selectF (MkBoolX8 !cond) (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (if testBit cond 0 then x0 else y0) (if testBit cond 1 then x1 else y1) (if testBit cond 2 then x2 else y2) (if testBit cond 3 then x3 else y3) (if testBit cond 4 then x4 else y4) (if testBit cond 5 then x5 else y5) (if testBit cond 6 then x6 else y6) (if testBit cond 7 then x7 else y7)
instance (i0 < 8, i1 < 8, i2 < 8, i3 < 8, i4 < 8, i5 < 8, i6 < 8, i7 < 8, Pick Word64 i0, Pick Word64 i1, Pick Word64 i2, Pick Word64 i3, Pick Word64 i4, Pick Word64 i5, Pick Word64 i6, Pick Word64 i7) => UnaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word64 where
  unaryShuffle (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; _ -> x7 } } in MkWord64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE unaryShuffle #-}
instance (i0 < 16, i1 < 16, i2 < 16, i3 < 16, i4 < 16, i5 < 16, i6 < 16, i7 < 16, Pick Word64 i0, Pick Word64 i1, Pick Word64 i2, Pick Word64 i3, Pick Word64 i4, Pick Word64 i5, Pick Word64 i6, Pick Word64 i7) => BinaryShuffle [i0, i1, i2, i3, i4, i5, i6, i7] X8 Word64 where
  binaryShuffle (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems x8 x9 x10 x11 x12 x13 x14 x15) = let { sources = \case { 0 -> x0; 1 -> x1; 2 -> x2; 3 -> x3; 4 -> x4; 5 -> x5; 6 -> x6; 7 -> x7; 8 -> x8; 9 -> x9; 10 -> x10; 11 -> x11; 12 -> x12; 13 -> x13; 14 -> x14; _ -> x15 } } in MkWord64X8WithElems (pick @_ @i0 sources) (pick @_ @i1 sources) (pick @_ @i2 sources) (pick @_ @i3 sources) (pick @_ @i4 sources) (pick @_ @i5 sources) (pick @_ @i6 sources) (pick @_ @i7 sources)
  {-# INLINE binaryShuffle #-}
instance EquatableF X8 Word64 where
  eqF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 == y0) (x1 == y1) (x2 == y2) (x3 == y3) (x4 == y4) (x5 == y5) (x6 == y6) (x7 == y7)
  {-# INLINE eqF #-}
instance OrderedF X8 Word64 where
  ltF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 < y0) (x1 < y1) (x2 < y2) (x3 < y3) (x4 < y4) (x5 < y5) (x6 < y6) (x7 < y7)
  leF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 <= y0) (x1 <= y1) (x2 <= y2) (x3 <= y3) (x4 <= y4) (x5 <= y5) (x6 <= y6) (x7 <= y7)
  gtF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 > y0) (x1 > y1) (x2 > y2) (x3 > y3) (x4 > y4) (x5 > y5) (x6 > y6) (x7 > y7)
  geF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = mkX8 (x0 >= y0) (x1 >= y1) (x2 >= y2) (x3 >= y3) (x4 >= y4) (x5 >= y5) (x6 >= y6) (x7 >= y7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 Word64 where
  minF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (min x0 y0) (min x1 y1) (min x2 y2) (min x3 y3) (min x4 y4) (min x5 y5) (min x6 y6) (min x7 y7)
  maxF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (max x0 y0) (max x1 y1) (max x2 y2) (max x3 y3) (max x4 y4) (max x5 y5) (max x6 y6) (max x7 y7)
  minimumNumberF = minF
  maximumNumberF = maxF
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance NumF X8 Word64 where
  plusF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (x0 + y0) (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4) (x5 + y5) (x6 + y6) (x7 + y7)
  minusF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (x0 - y0) (x1 - y1) (x2 - y2) (x3 - y3) (x4 - y4) (x5 - y5) (x6 - y6) (x7 - y7)
  timesF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (x0 * y0) (x1 * y1) (x2 * y2) (x3 * y3) (x4 * y4) (x5 * y5) (x6 * y6) (x7 * y7)
  negateF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord64X8WithElems (- x0) (- x1) (- x2) (- x3) (- x4) (- x5) (- x6) (- x7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance BooleanF X8 Word64 where
  andF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (x0 .&. y0) (x1 .&. y1) (x2 .&. y2) (x3 .&. y3) (x4 .&. y4) (x5 .&. y5) (x6 .&. y6) (x7 .&. y7)
  orF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (x0 .|. y0) (x1 .|. y1) (x2 .|. y2) (x3 .|. y3) (x4 .|. y4) (x5 .|. y5) (x6 .|. y6) (x7 .|. y7)
  xorF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) (MkWord64X8WithElems y0 y1 y2 y3 y4 y5 y6 y7) = MkWord64X8WithElems (xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
  complementF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) = MkWord64X8WithElems (complement x0) (complement x1) (complement x2) (complement x3) (complement x4) (complement x5) (complement x6) (complement x7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 Word64 where
  shiftLF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord64X8WithElems (shiftL x0 i) (shiftL x1 i) (shiftL x2 i) (shiftL x3 i) (shiftL x4 i) (shiftL x5 i) (shiftL x6 i) (shiftL x7 i)
  unsafeShiftLF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord64X8WithElems (unsafeShiftL x0 i) (unsafeShiftL x1 i) (unsafeShiftL x2 i) (unsafeShiftL x3 i) (unsafeShiftL x4 i) (unsafeShiftL x5 i) (unsafeShiftL x6 i) (unsafeShiftL x7 i)
  shiftRF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord64X8WithElems (shiftR x0 i) (shiftR x1 i) (shiftR x2 i) (shiftR x3 i) (shiftR x4 i) (shiftR x5 i) (shiftR x6 i) (shiftR x7 i)
  unsafeShiftRF (MkWord64X8WithElems x0 x1 x2 x3 x4 x5 x6 x7) !i = MkWord64X8WithElems (unsafeShiftR x0 i) (unsafeShiftR x1 i) (unsafeShiftR x2 i) (unsafeShiftR x3 i) (unsafeShiftR x4 i) (unsafeShiftR x5 i) (unsafeShiftR x6 i) (unsafeShiftR x7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance EnumFromZero_ X8 Word64 where
  enumFromZero = MkWord64X8WithElems 0 1 2 3 4 5 6 7
  -- {-# INLINE enumFromZero #-}
instance MultiPrim X8 Word64 where
  indexByteArraySIMD# ba i = MkWord64X8WithElems (W64# (GHC.Exts.indexWord64Array# ba i)) (W64# (GHC.Exts.indexWord64Array# ba (i +# 1#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 2#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 3#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 4#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 5#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 6#))) (W64# (GHC.Exts.indexWord64Array# ba (i +# 7#)))
  readByteArraySIMD# mba i s0 = case GHC.Exts.readWord64Array# mba i s0 of (# s1, x0 #) -> case GHC.Exts.readWord64Array# mba (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord64Array# mba (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord64Array# mba (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord64Array# mba (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord64Array# mba (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord64Array# mba (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord64Array# mba (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord64X8WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) #)
  writeByteArraySIMD# mba i (MkWord64X8WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7)) s0 = case GHC.Exts.writeWord64Array# mba i x0 s0 of s1 -> case GHC.Exts.writeWord64Array# mba (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord64Array# mba (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord64Array# mba (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord64Array# mba (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord64Array# mba (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord64Array# mba (i +# 6#) x6 s6 of s7 -> GHC.Exts.writeWord64Array# mba (i +# 7#) x7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance MultiStorable X8 Word64 where
  peekElemOffSIMD (Ptr addr) (I# i) = IO (\s0 -> case GHC.Exts.readWord64OffAddr# addr i s0 of (# s1, x0 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 1#) s1 of (# s2, x1 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 2#) s2 of (# s3, x2 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 3#) s3 of (# s4, x3 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 4#) s4 of (# s5, x4 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 5#) s5 of (# s6, x5 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 6#) s6 of (# s7, x6 #) -> case GHC.Exts.readWord64OffAddr# addr (i +# 7#) s7 of (# s8, x7 #) -> (# s8, MkWord64X8WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7) #))
  pokeElemOffSIMD (Ptr addr) (I# i) (MkWord64X8WithElems (W64# x0) (W64# x1) (W64# x2) (W64# x3) (W64# x4) (W64# x5) (W64# x6) (W64# x7)) = IO (\s0 -> case GHC.Exts.writeWord64OffAddr# addr i x0 s0 of s1 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 1#) x1 s1 of s2 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 2#) x2 s2 of s3 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 3#) x3 s3 of s4 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 4#) x4 s4 of s5 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 5#) x5 s5 of s6 -> case GHC.Exts.writeWord64OffAddr# addr (i +# 6#) x6 s6 of s7 -> (# GHC.Exts.writeWord64OffAddr# addr (i +# 7#) x7 s7, () #))
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
