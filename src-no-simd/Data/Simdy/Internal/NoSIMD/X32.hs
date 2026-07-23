-- This file was created by script/Gen.hs. Do not edit by hand!
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
#if MIN_VERSION_GLASGOW_HASKELL(9, 8, 1, 0)
{-# LANGUAGE ExtendedLiterals #-}
#endif
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Simdy.Internal.NoSIMD.X32 where
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
-- | @'X32' a@ is a fixed-length vector of length 32.
--
-- Conceptually, @data 'X32' a = MkX32 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a@.
--
-- You can access the elements by 'mkX32', 'packX32' and 'unpackX32'.
data X32 a = MkX32WithX8 !(X8 a) !(X8 a) !(X8 a) !(X8 a)
instance ImplementationDescription X32 where
  implementationDescription _ = "X32;maxBits=0"
instance KnownSIMDLength X32 where
  type SIMDLength X32 = 32
  simdLength = 32
  {-# INLINE simdLength #-}
type instance Mask (X32 a) = X32 Bool
instance MaskIsLiftedBool X32 a
deriving via WrappedMulti X32 a instance EquatableF X8 a => Equatable (X32 a)
deriving via WrappedMulti X32 a instance OrderedF X8 a => Ordered (X32 a)
deriving via WrappedMulti X32 a instance SelectableF X8 a => Selectable (X32 a)
deriving via WrappedMulti X32 a instance (Num a, NumF X8 a, Broadcast X8 a, PackX8 X8 a) => Num (X32 a)
deriving via WrappedMulti X32 a instance (Fractional a, FractionalF X8 a, Broadcast X8 a, PackX8 X8 a) => Fractional (X32 a)
deriving via WrappedMulti X32 a instance (Floating a, FloatingF X8 a, Broadcast X8 a, PackX8 X8 a) => Floating (X32 a)
deriving via WrappedMulti X32 a instance BooleanF X32 a => Boolean (X32 a)
deriving via WrappedMulti X32 a instance BitShiftF X32 a => BitShift (X32 a)
deriving via WrappedMulti X32 a instance MinMaxF X32 a => MinMax (X32 a)
deriving via WrappedMulti X32 a instance (Num a, FusedMultiplyAddF X8 a, Broadcast X8 a, PackX8 X8 a) => FusedMultiplyAdd (X32 a)
instance PackX32 X32 a => IsList (X32 a) where
  type Item (X32 a) = a
  toList = toListX32
  fromList = fromListX32
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX8 X8 a => PackX32 X32 a where
  mkX32 !x0 !x1 !x2 !x3 !x4 !x5 !x6 !x7 !x8 !x9 !x10 !x11 !x12 !x13 !x14 !x15 !x16 !x17 !x18 !x19 !x20 !x21 !x22 !x23 !x24 !x25 !x26 !x27 !x28 !x29 !x30 !x31 = MkX32WithX8 (mkX8 x0 x1 x2 x3 x4 x5 x6 x7) (mkX8 x8 x9 x10 x11 x12 x13 x14 x15) (mkX8 x16 x17 x18 x19 x20 x21 x22 x23) (mkX8 x24 x25 x26 x27 x28 x29 x30 x31)
  unpackX32 (MkX32WithX8 u0 u1 u2 u3) = case unpackX8 u0 of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 u1 of (x8, x9, x10, x11, x12, x13, x14, x15) -> case unpackX8 u2 of (x16, x17, x18, x19, x20, x21, x22, x23) -> case unpackX8 u3 of (x24, x25, x26, x27, x28, x29, x30, x31) -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31)
  {-# INLINE mkX32 #-}
  {-# INLINE unpackX32 #-}
instance (PackX8 X8 a, PackX8 X8 b) => LiftSIMD X32 a b where
  liftSIMD f !v = case unpackX32 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> mkX32 (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9) (f x10) (f x11) (f x12) (f x13) (f x14) (f x15) (f x16) (f x17) (f x18) (f x19) (f x20) (f x21) (f x22) (f x23) (f x24) (f x25) (f x26) (f x27) (f x28) (f x29) (f x30) (f x31)
  {-# INLINE liftSIMD #-}
instance (PackX32 X32 a, PackX32 X32 b, PackX32 X32 c) => LiftSIMD2 X32 a b c where
  liftSIMD2 f !u !v = case unpackX32 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31) -> case unpackX32 v of (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29, y30, y31) -> mkX32 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4) (f x5 y5) (f x6 y6) (f x7 y7) (f x8 y8) (f x9 y9) (f x10 y10) (f x11 y11) (f x12 y12) (f x13 y13) (f x14 y14) (f x15 y15) (f x16 y16) (f x17 y17) (f x18 y18) (f x19 y19) (f x20 y20) (f x21 y21) (f x22 y22) (f x23 y23) (f x24 y24) (f x25 y25) (f x26 y26) (f x27 y27) (f x28 y28) (f x29 y29) (f x30 y30) (f x31 y31)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X8 => LiftConstructor X32 where
  mkTuple2 (MkX32WithX8 u0_0 u0_1 u0_2 u0_3) (MkX32WithX8 u1_0 u1_1 u1_2 u1_3) = MkX32WithX8 (mkTuple2 u0_0 u1_0) (mkTuple2 u0_1 u1_1) (mkTuple2 u0_2 u1_2) (mkTuple2 u0_3 u1_3)
  mkTuple3 (MkX32WithX8 u0_0 u0_1 u0_2 u0_3) (MkX32WithX8 u1_0 u1_1 u1_2 u1_3) (MkX32WithX8 u2_0 u2_1 u2_2 u2_3) = MkX32WithX8 (mkTuple3 u0_0 u1_0 u2_0) (mkTuple3 u0_1 u1_1 u2_1) (mkTuple3 u0_2 u1_2 u2_2) (mkTuple3 u0_3 u1_3 u2_3)
  mkTuple4 (MkX32WithX8 u0_0 u0_1 u0_2 u0_3) (MkX32WithX8 u1_0 u1_1 u1_2 u1_3) (MkX32WithX8 u2_0 u2_1 u2_2 u2_3) (MkX32WithX8 u3_0 u3_1 u3_2 u3_3) = MkX32WithX8 (mkTuple4 u0_0 u1_0 u2_0 u3_0) (mkTuple4 u0_1 u1_1 u2_1 u3_1) (mkTuple4 u0_2 u1_2 u2_2 u3_2) (mkTuple4 u0_3 u1_3 u2_3 u3_3)
  mkTuple5 (MkX32WithX8 u0_0 u0_1 u0_2 u0_3) (MkX32WithX8 u1_0 u1_1 u1_2 u1_3) (MkX32WithX8 u2_0 u2_1 u2_2 u2_3) (MkX32WithX8 u3_0 u3_1 u3_2 u3_3) (MkX32WithX8 u4_0 u4_1 u4_2 u4_3) = MkX32WithX8 (mkTuple5 u0_0 u1_0 u2_0 u3_0 u4_0) (mkTuple5 u0_1 u1_1 u2_1 u3_1 u4_1) (mkTuple5 u0_2 u1_2 u2_2 u3_2 u4_2) (mkTuple5 u0_3 u1_3 u2_3 u3_3 u4_3)
  mkTuple6 (MkX32WithX8 u0_0 u0_1 u0_2 u0_3) (MkX32WithX8 u1_0 u1_1 u1_2 u1_3) (MkX32WithX8 u2_0 u2_1 u2_2 u2_3) (MkX32WithX8 u3_0 u3_1 u3_2 u3_3) (MkX32WithX8 u4_0 u4_1 u4_2 u4_3) (MkX32WithX8 u5_0 u5_1 u5_2 u5_3) = MkX32WithX8 (mkTuple6 u0_0 u1_0 u2_0 u3_0 u4_0 u5_0) (mkTuple6 u0_1 u1_1 u2_1 u3_1 u4_1 u5_1) (mkTuple6 u0_2 u1_2 u2_2 u3_2 u4_2 u5_2) (mkTuple6 u0_3 u1_3 u2_3 u3_3 u4_3 u5_3)
  deconstructTuple2 (MkX32WithX8 u0 u1 u2 u3) = case deconstructTuple2 u0 of (u0_0, u0_1) -> case deconstructTuple2 u1 of (u1_0, u1_1) -> case deconstructTuple2 u2 of (u2_0, u2_1) -> case deconstructTuple2 u3 of (u3_0, u3_1) -> (MkX32WithX8 u0_0 u1_0 u2_0 u3_0, MkX32WithX8 u0_1 u1_1 u2_1 u3_1)
  deconstructTuple3 (MkX32WithX8 u0 u1 u2 u3) = case deconstructTuple3 u0 of (u0_0, u0_1, u0_2) -> case deconstructTuple3 u1 of (u1_0, u1_1, u1_2) -> case deconstructTuple3 u2 of (u2_0, u2_1, u2_2) -> case deconstructTuple3 u3 of (u3_0, u3_1, u3_2) -> (MkX32WithX8 u0_0 u1_0 u2_0 u3_0, MkX32WithX8 u0_1 u1_1 u2_1 u3_1, MkX32WithX8 u0_2 u1_2 u2_2 u3_2)
  deconstructTuple4 (MkX32WithX8 u0 u1 u2 u3) = case deconstructTuple4 u0 of (u0_0, u0_1, u0_2, u0_3) -> case deconstructTuple4 u1 of (u1_0, u1_1, u1_2, u1_3) -> case deconstructTuple4 u2 of (u2_0, u2_1, u2_2, u2_3) -> case deconstructTuple4 u3 of (u3_0, u3_1, u3_2, u3_3) -> (MkX32WithX8 u0_0 u1_0 u2_0 u3_0, MkX32WithX8 u0_1 u1_1 u2_1 u3_1, MkX32WithX8 u0_2 u1_2 u2_2 u3_2, MkX32WithX8 u0_3 u1_3 u2_3 u3_3)
  deconstructTuple5 (MkX32WithX8 u0 u1 u2 u3) = case deconstructTuple5 u0 of (u0_0, u0_1, u0_2, u0_3, u0_4) -> case deconstructTuple5 u1 of (u1_0, u1_1, u1_2, u1_3, u1_4) -> case deconstructTuple5 u2 of (u2_0, u2_1, u2_2, u2_3, u2_4) -> case deconstructTuple5 u3 of (u3_0, u3_1, u3_2, u3_3, u3_4) -> (MkX32WithX8 u0_0 u1_0 u2_0 u3_0, MkX32WithX8 u0_1 u1_1 u2_1 u3_1, MkX32WithX8 u0_2 u1_2 u2_2 u3_2, MkX32WithX8 u0_3 u1_3 u2_3 u3_3, MkX32WithX8 u0_4 u1_4 u2_4 u3_4)
  deconstructTuple6 (MkX32WithX8 u0 u1 u2 u3) = case deconstructTuple6 u0 of (u0_0, u0_1, u0_2, u0_3, u0_4, u0_5) -> case deconstructTuple6 u1 of (u1_0, u1_1, u1_2, u1_3, u1_4, u1_5) -> case deconstructTuple6 u2 of (u2_0, u2_1, u2_2, u2_3, u2_4, u2_5) -> case deconstructTuple6 u3 of (u3_0, u3_1, u3_2, u3_3, u3_4, u3_5) -> (MkX32WithX8 u0_0 u1_0 u2_0 u3_0, MkX32WithX8 u0_1 u1_1 u2_1 u3_1, MkX32WithX8 u0_2 u1_2 u2_2 u3_2, MkX32WithX8 u0_3 u1_3 u2_3 u3_3, MkX32WithX8 u0_4 u1_4 u2_4 u3_4, MkX32WithX8 u0_5 u1_5 u2_5 u3_5)
  mkSum (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (mkSum u0) (mkSum u1) (mkSum u2) (mkSum u3)
  getSum' (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (getSum' u0) (getSum' u1) (getSum' u2) (getSum' u3)
  mkProduct (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (mkProduct u0) (mkProduct u1) (mkProduct u2) (mkProduct u3)
  getProduct' (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (getProduct' u0) (getProduct' u1) (getProduct' u2) (getProduct' u3)
  mkMin (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (mkMin u0) (mkMin u1) (mkMin u2) (mkMin u3)
  getMin' (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (getMin' u0) (getMin' u1) (getMin' u2) (getMin' u3)
  mkMax (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (mkMax u0) (mkMax u1) (mkMax u2) (mkMax u3)
  getMax' (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (getMax' u0) (getMax' u1) (getMax' u2) (getMax' u3)
  mkComplex (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (mkComplex u0 v0) (mkComplex u1 v1) (mkComplex u2 v2) (mkComplex u3 v3)
  deconstructComplex (MkX32WithX8 u0 u1 u2 u3) = case deconstructComplex u0 of (v0, w0) -> case deconstructComplex u1 of (v1, w1) -> case deconstructComplex u2 of (v2, w2) -> case deconstructComplex u3 of (v3, w3) -> (MkX32WithX8 v0 v1 v2 v3, MkX32WithX8 w0 w1 w2 w3)
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
instance Broadcast X8 a => Broadcast X32 a where
  broadcast !x = let !v = broadcast x in MkX32WithX8 v v v v
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X32 a where
  selectF (MkX32WithX8 cond0 cond1 cond2 cond3) (MkX32WithX8 x0 x1 x2 x3) (MkX32WithX8 y0 y1 y2 y3) = MkX32WithX8 (selectF cond0 x0 y0) (selectF cond1 x1 y1) (selectF cond2 x2 y2) (selectF cond3 x3 y3)
  {-# INLINE selectF #-}
instance EquatableF X8 a => EquatableF X32 a where
  eqF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (eqF u0 v0) (eqF u1 v1) (eqF u2 v2) (eqF u3 v3)
  {-# INLINE eqF #-}
instance OrderedF X8 a => OrderedF X32 a where
  ltF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (ltF u0 v0) (ltF u1 v1) (ltF u2 v2) (ltF u3 v3)
  leF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (leF u0 v0) (leF u1 v1) (leF u2 v2) (leF u3 v3)
  gtF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (gtF u0 v0) (gtF u1 v1) (gtF u2 v2) (gtF u3 v3)
  geF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (geF u0 v0) (geF u1 v1) (geF u2 v2) (geF u3 v3)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 a => MinMaxF X32 a where
  minF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (minF u0 v0) (minF u1 v1) (minF u2 v2) (minF u3 v3)
  maxF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (maxF u0 v0) (maxF u1 v1) (maxF u2 v2) (maxF u3 v3)
  minimumNumberF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (minimumNumberF u0 v0) (minimumNumberF u1 v1) (minimumNumberF u2 v2) (minimumNumberF u3 v3)
  maximumNumberF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (maximumNumberF u0 v0) (maximumNumberF u1 v1) (maximumNumberF u2 v2) (maximumNumberF u3 v3)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance (Num a, NumF X8 a, Broadcast X8 a, PackX8 X8 a) => NumF X32 a where
  plusF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (plusF u0 v0) (plusF u1 v1) (plusF u2 v2) (plusF u3 v3)
  minusF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (minusF u0 v0) (minusF u1 v1) (minusF u2 v2) (minusF u3 v3)
  timesF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (timesF u0 v0) (timesF u1 v1) (timesF u2 v2) (timesF u3 v3)
  negateF (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (negateF u0) (negateF u1) (negateF u2) (negateF u3)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance (Fractional a, FractionalF X8 a, Broadcast X8 a, PackX8 X8 a) => FractionalF X32 a where
  divideF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (divideF u0 v0) (divideF u1 v1) (divideF u2 v2) (divideF u3 v3)
  recipF (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (recipF u0) (recipF u1) (recipF u2) (recipF u3)
  {-# INLINE divideF #-}
  {-# INLINE recipF #-}
instance (Floating a, FloatingF X8 a, Broadcast X8 a, PackX8 X8 a) => FloatingF X32 a where
  sqrtF (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (sqrtF u0) (sqrtF u1) (sqrtF u2) (sqrtF u3)
  {-# INLINE sqrtF #-}
instance BooleanF X8 a => BooleanF X32 a where
  andF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (andF u0 v0) (andF u1 v1) (andF u2 v2) (andF u3 v3)
  orF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (orF u0 v0) (orF u1 v1) (orF u2 v2) (orF u3 v3)
  xorF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) = MkX32WithX8 (xorF u0 v0) (xorF u1 v1) (xorF u2 v2) (xorF u3 v3)
  complementF (MkX32WithX8 u0 u1 u2 u3) = MkX32WithX8 (complementF u0) (complementF u1) (complementF u2) (complementF u3)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 a => BitShiftF X32 a where
  shiftLF (MkX32WithX8 u0 u1 u2 u3) !i = MkX32WithX8 (shiftLF u0 i) (shiftLF u1 i) (shiftLF u2 i) (shiftLF u3 i)
  unsafeShiftLF (MkX32WithX8 u0 u1 u2 u3) !i = MkX32WithX8 (unsafeShiftLF u0 i) (unsafeShiftLF u1 i) (unsafeShiftLF u2 i) (unsafeShiftLF u3 i)
  shiftRF (MkX32WithX8 u0 u1 u2 u3) !i = MkX32WithX8 (shiftRF u0 i) (shiftRF u1 i) (shiftRF u2 i) (shiftRF u3 i)
  unsafeShiftRF (MkX32WithX8 u0 u1 u2 u3) !i = MkX32WithX8 (unsafeShiftRF u0 i) (unsafeShiftRF u1 i) (unsafeShiftRF u2 i) (unsafeShiftRF u3 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance (Num a, FusedMultiplyAddF X8 a, Broadcast X8 a, PackX8 X8 a) => FusedMultiplyAddF X32 a where
  fusedMultiplyAddF (MkX32WithX8 u0 u1 u2 u3) (MkX32WithX8 v0 v1 v2 v3) (MkX32WithX8 w0 w1 w2 w3) = MkX32WithX8 (fusedMultiplyAddF u0 v0 w0) (fusedMultiplyAddF u1 v1 w1) (fusedMultiplyAddF u2 v2 w2) (fusedMultiplyAddF u3 v3 w3)
  {-# INLINE fusedMultiplyAddF #-}
instance (Num a, PackX8 X8 a) => EnumFromZero_ X32 a where
  enumFromZero = mkX32 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
  {-# INLINE enumFromZero #-}
instance (Prim a, MultiPrim X8 a) => MultiPrim X32 a where
  indexByteArraySIMD# ba i = MkX32WithX8 (indexByteArraySIMD# ba i) (indexByteArraySIMD# ba (i +# 8#)) (indexByteArraySIMD# ba (i +# 16#)) (indexByteArraySIMD# ba (i +# 24#))
  readByteArraySIMD# mba i s0 = case readByteArraySIMD# mba i s0 of (# s1, u0 #) -> case readByteArraySIMD# mba (i +# 8#) s1 of (# s2, u1 #) -> case readByteArraySIMD# mba (i +# 16#) s2 of (# s3, u2 #) -> case readByteArraySIMD# mba (i +# 24#) s3 of (# s4, u3 #) -> (# s4, MkX32WithX8 u0 u1 u2 u3 #)
  writeByteArraySIMD# mba i (MkX32WithX8 u0 u1 u2 u3) s0 = case writeByteArraySIMD# mba i u0 s0 of s1 -> case writeByteArraySIMD# mba (i +# 1#) u1 s1 of s2 -> case writeByteArraySIMD# mba (i +# 2#) u2 s2 of s3 -> writeByteArraySIMD# mba (i +# 3#) u3 s3
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance (Storable a, MultiStorable X8 a) => MultiStorable X32 a where
  peekElemOffSIMD !ptr !i = MkX32WithX8 <$> peekElemOffSIMD ptr i <*> peekElemOffSIMD ptr (i + 1) <*> peekElemOffSIMD ptr (i + 2) <*> peekElemOffSIMD ptr (i + 3)
  pokeElemOffSIMD !ptr !i (MkX32WithX8 u0 u1 u2 u3) = pokeElemOffSIMD ptr i u0 >> pokeElemOffSIMD ptr (i + 1) u1 >> pokeElemOffSIMD ptr (i + 2) u2 >> pokeElemOffSIMD ptr (i + 3) u3
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
