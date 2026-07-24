-- This file was created by script/Gen.hs. Do not edit by hand!
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Simdy.Internal.NoSIMD.X16 where
import           Data.Bits
import           Data.Coerce (coerce)
import           Data.Complex
import           Data.Monoid
import           Data.Primitive (Prim)
import           Data.Semigroup
import           Data.Simdy.Internal.Bits (Boolean, BitShift)
import           Data.Simdy.Internal.Class
import           Data.Simdy.Internal.NoSIMD.X8
import           Foreign.Storable (Storable)
import qualified GHC.Exts
import           GHC.Exts (Ptr (..), Float (..), Double (..), coerce, (+#), IsList (..))
import           GHC.Int
import           GHC.IO
import           GHC.Word
import           Prelude hiding (not, (&&), (||), (==), (<), (<=), (>), (>=), min, max)
-- | @'X16' a@ is a fixed-length vector of length 16.
--
-- Conceptually, @data 'X16' a = MkX16 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a@.
--
-- You can access the elements by 'mkX16', 'packX16' and 'unpackX16'.
data X16 a = MkX16WithX8 !(X8 a) !(X8 a)
instance ImplementationDescription X16 where
  implementationDescription _ = "X16;maxBits=0"
instance KnownSIMDLength X16 where
  type SIMDLength X16 = 16
  simdLength = 16
  {-# INLINE simdLength #-}
type instance Mask (X16 a) = X16 Bool
instance MaskIsLiftedBool X16 a
deriving via WrappedMulti X16 a instance EquatableF X8 a => Equatable (X16 a)
deriving via WrappedMulti X16 a instance OrderedF X8 a => Ordered (X16 a)
deriving via WrappedMulti X16 a instance SelectableF X8 a => Selectable (X16 a)
deriving via WrappedMulti X16 a instance (Num a, NumF X8 a, Broadcast X8 a, PackX8 X8 a) => Num (X16 a)
deriving via WrappedMulti X16 a instance (Fractional a, FractionalF X8 a, Broadcast X8 a, PackX8 X8 a) => Fractional (X16 a)
deriving via WrappedMulti X16 a instance (Floating a, FloatingF X8 a, Broadcast X8 a, PackX8 X8 a) => Floating (X16 a)
deriving via WrappedMulti X16 a instance BooleanF X16 a => Boolean (X16 a)
deriving via WrappedMulti X16 a instance BitShiftF X16 a => BitShift (X16 a)
deriving via WrappedMulti X16 a instance MinMaxF X16 a => MinMax (X16 a)
deriving via WrappedMulti X16 a instance (Num a, FusedMultiplyAddF X8 a, Broadcast X8 a, PackX8 X8 a) => FusedMultiplyAdd (X16 a)
instance PackX16 X16 a => IsList (X16 a) where
  type Item (X16 a) = a
  toList = toListX16
  fromList = fromListX16
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX8 X8 a => PackX16 X16 a where
  mkX16 !x0 !x1 !x2 !x3 !x4 !x5 !x6 !x7 !x8 !x9 !x10 !x11 !x12 !x13 !x14 !x15 = MkX16WithX8 (mkX8 x0 x1 x2 x3 x4 x5 x6 x7) (mkX8 x8 x9 x10 x11 x12 x13 x14 x15)
  unpackX16 (MkX16WithX8 u0 u1) = case unpackX8 u0 of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 u1 of (x8, x9, x10, x11, x12, x13, x14, x15) -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15)
  {-# INLINE mkX16 #-}
  {-# INLINE unpackX16 #-}
instance (PackX8 X8 a, PackX8 X8 b) => LiftSIMD X16 a b where
  liftSIMD f !v = case unpackX16 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> mkX16 (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9) (f x10) (f x11) (f x12) (f x13) (f x14) (f x15)
  {-# INLINE liftSIMD #-}
instance (PackX16 X16 a, PackX16 X16 b, PackX16 X16 c) => LiftSIMD2 X16 a b c where
  liftSIMD2 f !u !v = case unpackX16 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15) -> case unpackX16 v of (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15) -> mkX16 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4) (f x5 y5) (f x6 y6) (f x7 y7) (f x8 y8) (f x9 y9) (f x10 y10) (f x11 y11) (f x12 y12) (f x13 y13) (f x14 y14) (f x15 y15)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X8 => LiftConstructor X16 where
  mkTuple2 (MkX16WithX8 u0_0 u0_1) (MkX16WithX8 u1_0 u1_1) = MkX16WithX8 (mkTuple2 u0_0 u1_0) (mkTuple2 u0_1 u1_1)
  mkTuple3 (MkX16WithX8 u0_0 u0_1) (MkX16WithX8 u1_0 u1_1) (MkX16WithX8 u2_0 u2_1) = MkX16WithX8 (mkTuple3 u0_0 u1_0 u2_0) (mkTuple3 u0_1 u1_1 u2_1)
  mkTuple4 (MkX16WithX8 u0_0 u0_1) (MkX16WithX8 u1_0 u1_1) (MkX16WithX8 u2_0 u2_1) (MkX16WithX8 u3_0 u3_1) = MkX16WithX8 (mkTuple4 u0_0 u1_0 u2_0 u3_0) (mkTuple4 u0_1 u1_1 u2_1 u3_1)
  mkTuple5 (MkX16WithX8 u0_0 u0_1) (MkX16WithX8 u1_0 u1_1) (MkX16WithX8 u2_0 u2_1) (MkX16WithX8 u3_0 u3_1) (MkX16WithX8 u4_0 u4_1) = MkX16WithX8 (mkTuple5 u0_0 u1_0 u2_0 u3_0 u4_0) (mkTuple5 u0_1 u1_1 u2_1 u3_1 u4_1)
  mkTuple6 (MkX16WithX8 u0_0 u0_1) (MkX16WithX8 u1_0 u1_1) (MkX16WithX8 u2_0 u2_1) (MkX16WithX8 u3_0 u3_1) (MkX16WithX8 u4_0 u4_1) (MkX16WithX8 u5_0 u5_1) = MkX16WithX8 (mkTuple6 u0_0 u1_0 u2_0 u3_0 u4_0 u5_0) (mkTuple6 u0_1 u1_1 u2_1 u3_1 u4_1 u5_1)
  deconstructTuple2 (MkX16WithX8 u0 u1) = case deconstructTuple2 u0 of (u0_0, u0_1) -> case deconstructTuple2 u1 of (u1_0, u1_1) -> (MkX16WithX8 u0_0 u1_0, MkX16WithX8 u0_1 u1_1)
  deconstructTuple3 (MkX16WithX8 u0 u1) = case deconstructTuple3 u0 of (u0_0, u0_1, u0_2) -> case deconstructTuple3 u1 of (u1_0, u1_1, u1_2) -> (MkX16WithX8 u0_0 u1_0, MkX16WithX8 u0_1 u1_1, MkX16WithX8 u0_2 u1_2)
  deconstructTuple4 (MkX16WithX8 u0 u1) = case deconstructTuple4 u0 of (u0_0, u0_1, u0_2, u0_3) -> case deconstructTuple4 u1 of (u1_0, u1_1, u1_2, u1_3) -> (MkX16WithX8 u0_0 u1_0, MkX16WithX8 u0_1 u1_1, MkX16WithX8 u0_2 u1_2, MkX16WithX8 u0_3 u1_3)
  deconstructTuple5 (MkX16WithX8 u0 u1) = case deconstructTuple5 u0 of (u0_0, u0_1, u0_2, u0_3, u0_4) -> case deconstructTuple5 u1 of (u1_0, u1_1, u1_2, u1_3, u1_4) -> (MkX16WithX8 u0_0 u1_0, MkX16WithX8 u0_1 u1_1, MkX16WithX8 u0_2 u1_2, MkX16WithX8 u0_3 u1_3, MkX16WithX8 u0_4 u1_4)
  deconstructTuple6 (MkX16WithX8 u0 u1) = case deconstructTuple6 u0 of (u0_0, u0_1, u0_2, u0_3, u0_4, u0_5) -> case deconstructTuple6 u1 of (u1_0, u1_1, u1_2, u1_3, u1_4, u1_5) -> (MkX16WithX8 u0_0 u1_0, MkX16WithX8 u0_1 u1_1, MkX16WithX8 u0_2 u1_2, MkX16WithX8 u0_3 u1_3, MkX16WithX8 u0_4 u1_4, MkX16WithX8 u0_5 u1_5)
  mkSum (MkX16WithX8 u0 u1) = MkX16WithX8 (mkSum u0) (mkSum u1)
  getSum' (MkX16WithX8 u0 u1) = MkX16WithX8 (getSum' u0) (getSum' u1)
  mkProduct (MkX16WithX8 u0 u1) = MkX16WithX8 (mkProduct u0) (mkProduct u1)
  getProduct' (MkX16WithX8 u0 u1) = MkX16WithX8 (getProduct' u0) (getProduct' u1)
  mkMin (MkX16WithX8 u0 u1) = MkX16WithX8 (mkMin u0) (mkMin u1)
  getMin' (MkX16WithX8 u0 u1) = MkX16WithX8 (getMin' u0) (getMin' u1)
  mkMax (MkX16WithX8 u0 u1) = MkX16WithX8 (mkMax u0) (mkMax u1)
  getMax' (MkX16WithX8 u0 u1) = MkX16WithX8 (getMax' u0) (getMax' u1)
  mkComplex (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (mkComplex u0 v0) (mkComplex u1 v1)
  deconstructComplex (MkX16WithX8 u0 u1) = case deconstructComplex u0 of (v0, w0) -> case deconstructComplex u1 of (v1, w1) -> (MkX16WithX8 v0 v1, MkX16WithX8 w0 w1)
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
instance Broadcast X8 a => Broadcast X16 a where
  broadcast !x = let !v = broadcast x in MkX16WithX8 v v
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X16 a where
  selectF (MkX16WithX8 cond0 cond1) (MkX16WithX8 x0 x1) (MkX16WithX8 y0 y1) = MkX16WithX8 (selectF cond0 x0 y0) (selectF cond1 x1 y1)
  {-# INLINE selectF #-}
instance EquatableF X8 a => EquatableF X16 a where
  eqF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (eqF u0 v0) (eqF u1 v1)
  {-# INLINE eqF #-}
instance OrderedF X8 a => OrderedF X16 a where
  ltF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (ltF u0 v0) (ltF u1 v1)
  leF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (leF u0 v0) (leF u1 v1)
  gtF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (gtF u0 v0) (gtF u1 v1)
  geF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (geF u0 v0) (geF u1 v1)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 a => MinMaxF X16 a where
  minF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (minF u0 v0) (minF u1 v1)
  maxF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (maxF u0 v0) (maxF u1 v1)
  minimumNumberF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (minimumNumberF u0 v0) (minimumNumberF u1 v1)
  maximumNumberF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (maximumNumberF u0 v0) (maximumNumberF u1 v1)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance (Num a, NumF X8 a, Broadcast X8 a, PackX8 X8 a) => NumF X16 a where
  plusF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (plusF u0 v0) (plusF u1 v1)
  minusF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (minusF u0 v0) (minusF u1 v1)
  timesF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (timesF u0 v0) (timesF u1 v1)
  negateF (MkX16WithX8 u0 u1) = MkX16WithX8 (negateF u0) (negateF u1)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance (Fractional a, FractionalF X8 a, Broadcast X8 a, PackX8 X8 a) => FractionalF X16 a where
  divideF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (divideF u0 v0) (divideF u1 v1)
  recipF (MkX16WithX8 u0 u1) = MkX16WithX8 (recipF u0) (recipF u1)
  {-# INLINE divideF #-}
  {-# INLINE recipF #-}
instance (Floating a, FloatingF X8 a, Broadcast X8 a, PackX8 X8 a) => FloatingF X16 a where
  sqrtF (MkX16WithX8 u0 u1) = MkX16WithX8 (sqrtF u0) (sqrtF u1)
  {-# INLINE sqrtF #-}
instance BooleanF X8 a => BooleanF X16 a where
  andF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (andF u0 v0) (andF u1 v1)
  orF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (orF u0 v0) (orF u1 v1)
  xorF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) = MkX16WithX8 (xorF u0 v0) (xorF u1 v1)
  complementF (MkX16WithX8 u0 u1) = MkX16WithX8 (complementF u0) (complementF u1)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 a => BitShiftF X16 a where
  shiftLF (MkX16WithX8 u0 u1) !i = MkX16WithX8 (shiftLF u0 i) (shiftLF u1 i)
  unsafeShiftLF (MkX16WithX8 u0 u1) !i = MkX16WithX8 (unsafeShiftLF u0 i) (unsafeShiftLF u1 i)
  shiftRF (MkX16WithX8 u0 u1) !i = MkX16WithX8 (shiftRF u0 i) (shiftRF u1 i)
  unsafeShiftRF (MkX16WithX8 u0 u1) !i = MkX16WithX8 (unsafeShiftRF u0 i) (unsafeShiftRF u1 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance (Num a, FusedMultiplyAddF X8 a, Broadcast X8 a, PackX8 X8 a) => FusedMultiplyAddF X16 a where
  fusedMultiplyAddF (MkX16WithX8 u0 u1) (MkX16WithX8 v0 v1) (MkX16WithX8 w0 w1) = MkX16WithX8 (fusedMultiplyAddF u0 v0 w0) (fusedMultiplyAddF u1 v1 w1)
  {-# INLINE fusedMultiplyAddF #-}
instance (Num a, PackX8 X8 a) => EnumFromZero_ X16 a where
  enumFromZero = mkX16 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
  {-# INLINE enumFromZero #-}
instance (Prim a, MultiPrim X8 a) => MultiPrim X16 a where
  indexByteArraySIMD# ba i = MkX16WithX8 (indexByteArraySIMD# ba i) (indexByteArraySIMD# ba (i +# 8#))
  readByteArraySIMD# mba i s0 = case readByteArraySIMD# mba i s0 of (# s1, u0 #) -> case readByteArraySIMD# mba (i +# 8#) s1 of (# s2, u1 #) -> (# s2, MkX16WithX8 u0 u1 #)
  writeByteArraySIMD# mba i (MkX16WithX8 u0 u1) s0 = case writeByteArraySIMD# mba i u0 s0 of s1 -> writeByteArraySIMD# mba (i +# 1#) u1 s1
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance (Storable a, MultiStorable X8 a) => MultiStorable X16 a where
  peekElemOffSIMD !ptr !i = MkX16WithX8 <$> peekElemOffSIMD ptr i <*> peekElemOffSIMD ptr (i + 1)
  pokeElemOffSIMD !ptr !i (MkX16WithX8 u0 u1) = pokeElemOffSIMD ptr i u0 >> pokeElemOffSIMD ptr (i + 1) u1
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
