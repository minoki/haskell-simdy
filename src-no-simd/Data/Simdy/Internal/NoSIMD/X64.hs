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
module Data.Simdy.Internal.NoSIMD.X64 where
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
-- | @'X64' a@ is a fixed-length vector of length 64.
--
-- Conceptually, @data 'X64' a = MkX64 !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a !a@.
--
-- You can access the elements by 'mkX64', 'packX64' and 'unpackX64'.
data X64 a = MkX64WithX8 !(X8 a) !(X8 a) !(X8 a) !(X8 a) !(X8 a) !(X8 a) !(X8 a) !(X8 a)
instance ImplementationDescription X64 where
  implementationDescription _ = "X64;maxBits=0"
instance KnownSIMDLength X64 where
  type SIMDLength X64 = 64
  simdLength = 64
  {-# INLINE simdLength #-}
type instance Mask (X64 a) = X64 Bool
instance MaskIsLiftedBool X64 a
deriving via WrappedMulti X64 a instance EquatableF X8 a => Equatable (X64 a)
deriving via WrappedMulti X64 a instance OrderedF X8 a => Ordered (X64 a)
deriving via WrappedMulti X64 a instance SelectableF X8 a => Selectable (X64 a)
deriving via WrappedMulti X64 a instance (Num a, NumF X8 a, Broadcast X8 a, PackX8 X8 a) => Num (X64 a)
deriving via WrappedMulti X64 a instance (Fractional a, FractionalF X8 a, Broadcast X8 a, PackX8 X8 a) => Fractional (X64 a)
deriving via WrappedMulti X64 a instance (Floating a, FloatingF X8 a, Broadcast X8 a, PackX8 X8 a) => Floating (X64 a)
deriving via WrappedMulti X64 a instance BooleanF X64 a => Boolean (X64 a)
deriving via WrappedMulti X64 a instance BitShiftF X64 a => BitShift (X64 a)
deriving via WrappedMulti X64 a instance MinMaxF X64 a => MinMax (X64 a)
deriving via WrappedMulti X64 a instance (Num a, FusedMultiplyAddF X8 a, Broadcast X8 a, PackX8 X8 a) => FusedMultiplyAdd (X64 a)
instance PackX64 X64 a => IsList (X64 a) where
  type Item (X64 a) = a
  toList = toListX64
  fromList = fromListX64
  {-# INLINE toList #-}
  {-# INLINE fromList #-}
instance PackX8 X8 a => PackX64 X64 a where
  mkX64 !x0 !x1 !x2 !x3 !x4 !x5 !x6 !x7 !x8 !x9 !x10 !x11 !x12 !x13 !x14 !x15 !x16 !x17 !x18 !x19 !x20 !x21 !x22 !x23 !x24 !x25 !x26 !x27 !x28 !x29 !x30 !x31 !x32 !x33 !x34 !x35 !x36 !x37 !x38 !x39 !x40 !x41 !x42 !x43 !x44 !x45 !x46 !x47 !x48 !x49 !x50 !x51 !x52 !x53 !x54 !x55 !x56 !x57 !x58 !x59 !x60 !x61 !x62 !x63 = MkX64WithX8 (mkX8 x0 x1 x2 x3 x4 x5 x6 x7) (mkX8 x8 x9 x10 x11 x12 x13 x14 x15) (mkX8 x16 x17 x18 x19 x20 x21 x22 x23) (mkX8 x24 x25 x26 x27 x28 x29 x30 x31) (mkX8 x32 x33 x34 x35 x36 x37 x38 x39) (mkX8 x40 x41 x42 x43 x44 x45 x46 x47) (mkX8 x48 x49 x50 x51 x52 x53 x54 x55) (mkX8 x56 x57 x58 x59 x60 x61 x62 x63)
  unpackX64 (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = case unpackX8 u0 of (x0, x1, x2, x3, x4, x5, x6, x7) -> case unpackX8 u1 of (x8, x9, x10, x11, x12, x13, x14, x15) -> case unpackX8 u2 of (x16, x17, x18, x19, x20, x21, x22, x23) -> case unpackX8 u3 of (x24, x25, x26, x27, x28, x29, x30, x31) -> case unpackX8 u4 of (x32, x33, x34, x35, x36, x37, x38, x39) -> case unpackX8 u5 of (x40, x41, x42, x43, x44, x45, x46, x47) -> case unpackX8 u6 of (x48, x49, x50, x51, x52, x53, x54, x55) -> case unpackX8 u7 of (x56, x57, x58, x59, x60, x61, x62, x63) -> (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63)
  {-# INLINE mkX64 #-}
  {-# INLINE unpackX64 #-}
instance (PackX8 X8 a, PackX8 X8 b) => LiftSIMD X64 a b where
  liftSIMD f !v = case unpackX64 v of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) -> mkX64 (f x0) (f x1) (f x2) (f x3) (f x4) (f x5) (f x6) (f x7) (f x8) (f x9) (f x10) (f x11) (f x12) (f x13) (f x14) (f x15) (f x16) (f x17) (f x18) (f x19) (f x20) (f x21) (f x22) (f x23) (f x24) (f x25) (f x26) (f x27) (f x28) (f x29) (f x30) (f x31) (f x32) (f x33) (f x34) (f x35) (f x36) (f x37) (f x38) (f x39) (f x40) (f x41) (f x42) (f x43) (f x44) (f x45) (f x46) (f x47) (f x48) (f x49) (f x50) (f x51) (f x52) (f x53) (f x54) (f x55) (f x56) (f x57) (f x58) (f x59) (f x60) (f x61) (f x62) (f x63)
  {-# INLINE liftSIMD #-}
instance (PackX64 X64 a, PackX64 X64 b, PackX64 X64 c) => LiftSIMD2 X64 a b c where
  liftSIMD2 f !u !v = case unpackX64 u of (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63) -> case unpackX64 v of (y0, y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22, y23, y24, y25, y26, y27, y28, y29, y30, y31, y32, y33, y34, y35, y36, y37, y38, y39, y40, y41, y42, y43, y44, y45, y46, y47, y48, y49, y50, y51, y52, y53, y54, y55, y56, y57, y58, y59, y60, y61, y62, y63) -> mkX64 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3) (f x4 y4) (f x5 y5) (f x6 y6) (f x7 y7) (f x8 y8) (f x9 y9) (f x10 y10) (f x11 y11) (f x12 y12) (f x13 y13) (f x14 y14) (f x15 y15) (f x16 y16) (f x17 y17) (f x18 y18) (f x19 y19) (f x20 y20) (f x21 y21) (f x22 y22) (f x23 y23) (f x24 y24) (f x25 y25) (f x26 y26) (f x27 y27) (f x28 y28) (f x29 y29) (f x30 y30) (f x31 y31) (f x32 y32) (f x33 y33) (f x34 y34) (f x35 y35) (f x36 y36) (f x37 y37) (f x38 y38) (f x39 y39) (f x40 y40) (f x41 y41) (f x42 y42) (f x43 y43) (f x44 y44) (f x45 y45) (f x46 y46) (f x47 y47) (f x48 y48) (f x49 y49) (f x50 y50) (f x51 y51) (f x52 y52) (f x53 y53) (f x54 y54) (f x55 y55) (f x56 y56) (f x57 y57) (f x58 y58) (f x59 y59) (f x60 y60) (f x61 y61) (f x62 y62) (f x63 y63)
  {-# INLINE liftSIMD2 #-}
instance LiftConstructor X8 => LiftConstructor X64 where
  mkTuple2 (MkX64WithX8 u0_0 u0_1 u0_2 u0_3 u0_4 u0_5 u0_6 u0_7) (MkX64WithX8 u1_0 u1_1 u1_2 u1_3 u1_4 u1_5 u1_6 u1_7) = MkX64WithX8 (mkTuple2 u0_0 u1_0) (mkTuple2 u0_1 u1_1) (mkTuple2 u0_2 u1_2) (mkTuple2 u0_3 u1_3) (mkTuple2 u0_4 u1_4) (mkTuple2 u0_5 u1_5) (mkTuple2 u0_6 u1_6) (mkTuple2 u0_7 u1_7)
  mkTuple3 (MkX64WithX8 u0_0 u0_1 u0_2 u0_3 u0_4 u0_5 u0_6 u0_7) (MkX64WithX8 u1_0 u1_1 u1_2 u1_3 u1_4 u1_5 u1_6 u1_7) (MkX64WithX8 u2_0 u2_1 u2_2 u2_3 u2_4 u2_5 u2_6 u2_7) = MkX64WithX8 (mkTuple3 u0_0 u1_0 u2_0) (mkTuple3 u0_1 u1_1 u2_1) (mkTuple3 u0_2 u1_2 u2_2) (mkTuple3 u0_3 u1_3 u2_3) (mkTuple3 u0_4 u1_4 u2_4) (mkTuple3 u0_5 u1_5 u2_5) (mkTuple3 u0_6 u1_6 u2_6) (mkTuple3 u0_7 u1_7 u2_7)
  mkTuple4 (MkX64WithX8 u0_0 u0_1 u0_2 u0_3 u0_4 u0_5 u0_6 u0_7) (MkX64WithX8 u1_0 u1_1 u1_2 u1_3 u1_4 u1_5 u1_6 u1_7) (MkX64WithX8 u2_0 u2_1 u2_2 u2_3 u2_4 u2_5 u2_6 u2_7) (MkX64WithX8 u3_0 u3_1 u3_2 u3_3 u3_4 u3_5 u3_6 u3_7) = MkX64WithX8 (mkTuple4 u0_0 u1_0 u2_0 u3_0) (mkTuple4 u0_1 u1_1 u2_1 u3_1) (mkTuple4 u0_2 u1_2 u2_2 u3_2) (mkTuple4 u0_3 u1_3 u2_3 u3_3) (mkTuple4 u0_4 u1_4 u2_4 u3_4) (mkTuple4 u0_5 u1_5 u2_5 u3_5) (mkTuple4 u0_6 u1_6 u2_6 u3_6) (mkTuple4 u0_7 u1_7 u2_7 u3_7)
  mkTuple5 (MkX64WithX8 u0_0 u0_1 u0_2 u0_3 u0_4 u0_5 u0_6 u0_7) (MkX64WithX8 u1_0 u1_1 u1_2 u1_3 u1_4 u1_5 u1_6 u1_7) (MkX64WithX8 u2_0 u2_1 u2_2 u2_3 u2_4 u2_5 u2_6 u2_7) (MkX64WithX8 u3_0 u3_1 u3_2 u3_3 u3_4 u3_5 u3_6 u3_7) (MkX64WithX8 u4_0 u4_1 u4_2 u4_3 u4_4 u4_5 u4_6 u4_7) = MkX64WithX8 (mkTuple5 u0_0 u1_0 u2_0 u3_0 u4_0) (mkTuple5 u0_1 u1_1 u2_1 u3_1 u4_1) (mkTuple5 u0_2 u1_2 u2_2 u3_2 u4_2) (mkTuple5 u0_3 u1_3 u2_3 u3_3 u4_3) (mkTuple5 u0_4 u1_4 u2_4 u3_4 u4_4) (mkTuple5 u0_5 u1_5 u2_5 u3_5 u4_5) (mkTuple5 u0_6 u1_6 u2_6 u3_6 u4_6) (mkTuple5 u0_7 u1_7 u2_7 u3_7 u4_7)
  mkTuple6 (MkX64WithX8 u0_0 u0_1 u0_2 u0_3 u0_4 u0_5 u0_6 u0_7) (MkX64WithX8 u1_0 u1_1 u1_2 u1_3 u1_4 u1_5 u1_6 u1_7) (MkX64WithX8 u2_0 u2_1 u2_2 u2_3 u2_4 u2_5 u2_6 u2_7) (MkX64WithX8 u3_0 u3_1 u3_2 u3_3 u3_4 u3_5 u3_6 u3_7) (MkX64WithX8 u4_0 u4_1 u4_2 u4_3 u4_4 u4_5 u4_6 u4_7) (MkX64WithX8 u5_0 u5_1 u5_2 u5_3 u5_4 u5_5 u5_6 u5_7) = MkX64WithX8 (mkTuple6 u0_0 u1_0 u2_0 u3_0 u4_0 u5_0) (mkTuple6 u0_1 u1_1 u2_1 u3_1 u4_1 u5_1) (mkTuple6 u0_2 u1_2 u2_2 u3_2 u4_2 u5_2) (mkTuple6 u0_3 u1_3 u2_3 u3_3 u4_3 u5_3) (mkTuple6 u0_4 u1_4 u2_4 u3_4 u4_4 u5_4) (mkTuple6 u0_5 u1_5 u2_5 u3_5 u4_5 u5_5) (mkTuple6 u0_6 u1_6 u2_6 u3_6 u4_6 u5_6) (mkTuple6 u0_7 u1_7 u2_7 u3_7 u4_7 u5_7)
  deconstructTuple2 (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = case deconstructTuple2 u0 of (u0_0, u0_1) -> case deconstructTuple2 u1 of (u1_0, u1_1) -> case deconstructTuple2 u2 of (u2_0, u2_1) -> case deconstructTuple2 u3 of (u3_0, u3_1) -> case deconstructTuple2 u4 of (u4_0, u4_1) -> case deconstructTuple2 u5 of (u5_0, u5_1) -> case deconstructTuple2 u6 of (u6_0, u6_1) -> case deconstructTuple2 u7 of (u7_0, u7_1) -> (MkX64WithX8 u0_0 u1_0 u2_0 u3_0 u4_0 u5_0 u6_0 u7_0, MkX64WithX8 u0_1 u1_1 u2_1 u3_1 u4_1 u5_1 u6_1 u7_1)
  deconstructTuple3 (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = case deconstructTuple3 u0 of (u0_0, u0_1, u0_2) -> case deconstructTuple3 u1 of (u1_0, u1_1, u1_2) -> case deconstructTuple3 u2 of (u2_0, u2_1, u2_2) -> case deconstructTuple3 u3 of (u3_0, u3_1, u3_2) -> case deconstructTuple3 u4 of (u4_0, u4_1, u4_2) -> case deconstructTuple3 u5 of (u5_0, u5_1, u5_2) -> case deconstructTuple3 u6 of (u6_0, u6_1, u6_2) -> case deconstructTuple3 u7 of (u7_0, u7_1, u7_2) -> (MkX64WithX8 u0_0 u1_0 u2_0 u3_0 u4_0 u5_0 u6_0 u7_0, MkX64WithX8 u0_1 u1_1 u2_1 u3_1 u4_1 u5_1 u6_1 u7_1, MkX64WithX8 u0_2 u1_2 u2_2 u3_2 u4_2 u5_2 u6_2 u7_2)
  deconstructTuple4 (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = case deconstructTuple4 u0 of (u0_0, u0_1, u0_2, u0_3) -> case deconstructTuple4 u1 of (u1_0, u1_1, u1_2, u1_3) -> case deconstructTuple4 u2 of (u2_0, u2_1, u2_2, u2_3) -> case deconstructTuple4 u3 of (u3_0, u3_1, u3_2, u3_3) -> case deconstructTuple4 u4 of (u4_0, u4_1, u4_2, u4_3) -> case deconstructTuple4 u5 of (u5_0, u5_1, u5_2, u5_3) -> case deconstructTuple4 u6 of (u6_0, u6_1, u6_2, u6_3) -> case deconstructTuple4 u7 of (u7_0, u7_1, u7_2, u7_3) -> (MkX64WithX8 u0_0 u1_0 u2_0 u3_0 u4_0 u5_0 u6_0 u7_0, MkX64WithX8 u0_1 u1_1 u2_1 u3_1 u4_1 u5_1 u6_1 u7_1, MkX64WithX8 u0_2 u1_2 u2_2 u3_2 u4_2 u5_2 u6_2 u7_2, MkX64WithX8 u0_3 u1_3 u2_3 u3_3 u4_3 u5_3 u6_3 u7_3)
  deconstructTuple5 (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = case deconstructTuple5 u0 of (u0_0, u0_1, u0_2, u0_3, u0_4) -> case deconstructTuple5 u1 of (u1_0, u1_1, u1_2, u1_3, u1_4) -> case deconstructTuple5 u2 of (u2_0, u2_1, u2_2, u2_3, u2_4) -> case deconstructTuple5 u3 of (u3_0, u3_1, u3_2, u3_3, u3_4) -> case deconstructTuple5 u4 of (u4_0, u4_1, u4_2, u4_3, u4_4) -> case deconstructTuple5 u5 of (u5_0, u5_1, u5_2, u5_3, u5_4) -> case deconstructTuple5 u6 of (u6_0, u6_1, u6_2, u6_3, u6_4) -> case deconstructTuple5 u7 of (u7_0, u7_1, u7_2, u7_3, u7_4) -> (MkX64WithX8 u0_0 u1_0 u2_0 u3_0 u4_0 u5_0 u6_0 u7_0, MkX64WithX8 u0_1 u1_1 u2_1 u3_1 u4_1 u5_1 u6_1 u7_1, MkX64WithX8 u0_2 u1_2 u2_2 u3_2 u4_2 u5_2 u6_2 u7_2, MkX64WithX8 u0_3 u1_3 u2_3 u3_3 u4_3 u5_3 u6_3 u7_3, MkX64WithX8 u0_4 u1_4 u2_4 u3_4 u4_4 u5_4 u6_4 u7_4)
  deconstructTuple6 (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = case deconstructTuple6 u0 of (u0_0, u0_1, u0_2, u0_3, u0_4, u0_5) -> case deconstructTuple6 u1 of (u1_0, u1_1, u1_2, u1_3, u1_4, u1_5) -> case deconstructTuple6 u2 of (u2_0, u2_1, u2_2, u2_3, u2_4, u2_5) -> case deconstructTuple6 u3 of (u3_0, u3_1, u3_2, u3_3, u3_4, u3_5) -> case deconstructTuple6 u4 of (u4_0, u4_1, u4_2, u4_3, u4_4, u4_5) -> case deconstructTuple6 u5 of (u5_0, u5_1, u5_2, u5_3, u5_4, u5_5) -> case deconstructTuple6 u6 of (u6_0, u6_1, u6_2, u6_3, u6_4, u6_5) -> case deconstructTuple6 u7 of (u7_0, u7_1, u7_2, u7_3, u7_4, u7_5) -> (MkX64WithX8 u0_0 u1_0 u2_0 u3_0 u4_0 u5_0 u6_0 u7_0, MkX64WithX8 u0_1 u1_1 u2_1 u3_1 u4_1 u5_1 u6_1 u7_1, MkX64WithX8 u0_2 u1_2 u2_2 u3_2 u4_2 u5_2 u6_2 u7_2, MkX64WithX8 u0_3 u1_3 u2_3 u3_3 u4_3 u5_3 u6_3 u7_3, MkX64WithX8 u0_4 u1_4 u2_4 u3_4 u4_4 u5_4 u6_4 u7_4, MkX64WithX8 u0_5 u1_5 u2_5 u3_5 u4_5 u5_5 u6_5 u7_5)
  mkSum (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (mkSum u0) (mkSum u1) (mkSum u2) (mkSum u3) (mkSum u4) (mkSum u5) (mkSum u6) (mkSum u7)
  getSum' (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (getSum' u0) (getSum' u1) (getSum' u2) (getSum' u3) (getSum' u4) (getSum' u5) (getSum' u6) (getSum' u7)
  mkProduct (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (mkProduct u0) (mkProduct u1) (mkProduct u2) (mkProduct u3) (mkProduct u4) (mkProduct u5) (mkProduct u6) (mkProduct u7)
  getProduct' (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (getProduct' u0) (getProduct' u1) (getProduct' u2) (getProduct' u3) (getProduct' u4) (getProduct' u5) (getProduct' u6) (getProduct' u7)
  mkMin (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (mkMin u0) (mkMin u1) (mkMin u2) (mkMin u3) (mkMin u4) (mkMin u5) (mkMin u6) (mkMin u7)
  getMin' (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (getMin' u0) (getMin' u1) (getMin' u2) (getMin' u3) (getMin' u4) (getMin' u5) (getMin' u6) (getMin' u7)
  mkMax (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (mkMax u0) (mkMax u1) (mkMax u2) (mkMax u3) (mkMax u4) (mkMax u5) (mkMax u6) (mkMax u7)
  getMax' (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (getMax' u0) (getMax' u1) (getMax' u2) (getMax' u3) (getMax' u4) (getMax' u5) (getMax' u6) (getMax' u7)
  mkComplex (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (mkComplex u0 v0) (mkComplex u1 v1) (mkComplex u2 v2) (mkComplex u3 v3) (mkComplex u4 v4) (mkComplex u5 v5) (mkComplex u6 v6) (mkComplex u7 v7)
  deconstructComplex (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = case deconstructComplex u0 of (v0, w0) -> case deconstructComplex u1 of (v1, w1) -> case deconstructComplex u2 of (v2, w2) -> case deconstructComplex u3 of (v3, w3) -> case deconstructComplex u4 of (v4, w4) -> case deconstructComplex u5 of (v5, w5) -> case deconstructComplex u6 of (v6, w6) -> case deconstructComplex u7 of (v7, w7) -> (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7, MkX64WithX8 w0 w1 w2 w3 w4 w5 w6 w7)
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
instance Broadcast X8 a => Broadcast X64 a where
  broadcast !x = let !v = broadcast x in MkX64WithX8 v v v v v v v v
  {-# INLINE broadcast #-}
instance SelectableF X8 a => SelectableF X64 a where
  selectF (MkX64WithX8 cond0 cond1 cond2 cond3 cond4 cond5 cond6 cond7) (MkX64WithX8 x0 x1 x2 x3 x4 x5 x6 x7) (MkX64WithX8 y0 y1 y2 y3 y4 y5 y6 y7) = MkX64WithX8 (selectF cond0 x0 y0) (selectF cond1 x1 y1) (selectF cond2 x2 y2) (selectF cond3 x3 y3) (selectF cond4 x4 y4) (selectF cond5 x5 y5) (selectF cond6 x6 y6) (selectF cond7 x7 y7)
  {-# INLINE selectF #-}
instance EquatableF X8 a => EquatableF X64 a where
  eqF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (eqF u0 v0) (eqF u1 v1) (eqF u2 v2) (eqF u3 v3) (eqF u4 v4) (eqF u5 v5) (eqF u6 v6) (eqF u7 v7)
  {-# INLINE eqF #-}
instance OrderedF X8 a => OrderedF X64 a where
  ltF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (ltF u0 v0) (ltF u1 v1) (ltF u2 v2) (ltF u3 v3) (ltF u4 v4) (ltF u5 v5) (ltF u6 v6) (ltF u7 v7)
  leF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (leF u0 v0) (leF u1 v1) (leF u2 v2) (leF u3 v3) (leF u4 v4) (leF u5 v5) (leF u6 v6) (leF u7 v7)
  gtF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (gtF u0 v0) (gtF u1 v1) (gtF u2 v2) (gtF u3 v3) (gtF u4 v4) (gtF u5 v5) (gtF u6 v6) (gtF u7 v7)
  geF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (geF u0 v0) (geF u1 v1) (geF u2 v2) (geF u3 v3) (geF u4 v4) (geF u5 v5) (geF u6 v6) (geF u7 v7)
  {-# INLINE ltF #-}
  {-# INLINE leF #-}
  {-# INLINE gtF #-}
  {-# INLINE geF #-}
instance MinMaxF X8 a => MinMaxF X64 a where
  minF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (minF u0 v0) (minF u1 v1) (minF u2 v2) (minF u3 v3) (minF u4 v4) (minF u5 v5) (minF u6 v6) (minF u7 v7)
  maxF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (maxF u0 v0) (maxF u1 v1) (maxF u2 v2) (maxF u3 v3) (maxF u4 v4) (maxF u5 v5) (maxF u6 v6) (maxF u7 v7)
  minimumNumberF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (minimumNumberF u0 v0) (minimumNumberF u1 v1) (minimumNumberF u2 v2) (minimumNumberF u3 v3) (minimumNumberF u4 v4) (minimumNumberF u5 v5) (minimumNumberF u6 v6) (minimumNumberF u7 v7)
  maximumNumberF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (maximumNumberF u0 v0) (maximumNumberF u1 v1) (maximumNumberF u2 v2) (maximumNumberF u3 v3) (maximumNumberF u4 v4) (maximumNumberF u5 v5) (maximumNumberF u6 v6) (maximumNumberF u7 v7)
  {-# INLINE minF #-}
  {-# INLINE maxF #-}
  {-# INLINE minimumNumberF #-}
  {-# INLINE maximumNumberF #-}
instance (Num a, NumF X8 a, Broadcast X8 a, PackX8 X8 a) => NumF X64 a where
  plusF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (plusF u0 v0) (plusF u1 v1) (plusF u2 v2) (plusF u3 v3) (plusF u4 v4) (plusF u5 v5) (plusF u6 v6) (plusF u7 v7)
  minusF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (minusF u0 v0) (minusF u1 v1) (minusF u2 v2) (minusF u3 v3) (minusF u4 v4) (minusF u5 v5) (minusF u6 v6) (minusF u7 v7)
  timesF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (timesF u0 v0) (timesF u1 v1) (timesF u2 v2) (timesF u3 v3) (timesF u4 v4) (timesF u5 v5) (timesF u6 v6) (timesF u7 v7)
  negateF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (negateF u0) (negateF u1) (negateF u2) (negateF u3) (negateF u4) (negateF u5) (negateF u6) (negateF u7)
  {-# INLINE plusF #-}
  {-# INLINE minusF #-}
  {-# INLINE timesF #-}
  {-# INLINE negateF #-}
instance (Fractional a, FractionalF X8 a, Broadcast X8 a, PackX8 X8 a) => FractionalF X64 a where
  divideF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (divideF u0 v0) (divideF u1 v1) (divideF u2 v2) (divideF u3 v3) (divideF u4 v4) (divideF u5 v5) (divideF u6 v6) (divideF u7 v7)
  recipF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (recipF u0) (recipF u1) (recipF u2) (recipF u3) (recipF u4) (recipF u5) (recipF u6) (recipF u7)
  {-# INLINE divideF #-}
  {-# INLINE recipF #-}
instance (Floating a, FloatingF X8 a, Broadcast X8 a, PackX8 X8 a) => FloatingF X64 a where
  sqrtF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (sqrtF u0) (sqrtF u1) (sqrtF u2) (sqrtF u3) (sqrtF u4) (sqrtF u5) (sqrtF u6) (sqrtF u7)
  {-# INLINE sqrtF #-}
instance BooleanF X8 a => BooleanF X64 a where
  andF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (andF u0 v0) (andF u1 v1) (andF u2 v2) (andF u3 v3) (andF u4 v4) (andF u5 v5) (andF u6 v6) (andF u7 v7)
  orF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (orF u0 v0) (orF u1 v1) (orF u2 v2) (orF u3 v3) (orF u4 v4) (orF u5 v5) (orF u6 v6) (orF u7 v7)
  xorF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) = MkX64WithX8 (xorF u0 v0) (xorF u1 v1) (xorF u2 v2) (xorF u3 v3) (xorF u4 v4) (xorF u5 v5) (xorF u6 v6) (xorF u7 v7)
  complementF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = MkX64WithX8 (complementF u0) (complementF u1) (complementF u2) (complementF u3) (complementF u4) (complementF u5) (complementF u6) (complementF u7)
  {-# INLINE andF #-}
  {-# INLINE orF #-}
  {-# INLINE xorF #-}
  {-# INLINE complementF #-}
instance BitShiftF X8 a => BitShiftF X64 a where
  shiftLF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) !i = MkX64WithX8 (shiftLF u0 i) (shiftLF u1 i) (shiftLF u2 i) (shiftLF u3 i) (shiftLF u4 i) (shiftLF u5 i) (shiftLF u6 i) (shiftLF u7 i)
  unsafeShiftLF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) !i = MkX64WithX8 (unsafeShiftLF u0 i) (unsafeShiftLF u1 i) (unsafeShiftLF u2 i) (unsafeShiftLF u3 i) (unsafeShiftLF u4 i) (unsafeShiftLF u5 i) (unsafeShiftLF u6 i) (unsafeShiftLF u7 i)
  shiftRF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) !i = MkX64WithX8 (shiftRF u0 i) (shiftRF u1 i) (shiftRF u2 i) (shiftRF u3 i) (shiftRF u4 i) (shiftRF u5 i) (shiftRF u6 i) (shiftRF u7 i)
  unsafeShiftRF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) !i = MkX64WithX8 (unsafeShiftRF u0 i) (unsafeShiftRF u1 i) (unsafeShiftRF u2 i) (unsafeShiftRF u3 i) (unsafeShiftRF u4 i) (unsafeShiftRF u5 i) (unsafeShiftRF u6 i) (unsafeShiftRF u7 i)
  {-# INLINE shiftLF #-}
  {-# INLINE unsafeShiftLF #-}
  {-# INLINE shiftRF #-}
  {-# INLINE unsafeShiftRF #-}
instance (Num a, FusedMultiplyAddF X8 a, Broadcast X8 a, PackX8 X8 a) => FusedMultiplyAddF X64 a where
  fusedMultiplyAddF (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) (MkX64WithX8 v0 v1 v2 v3 v4 v5 v6 v7) (MkX64WithX8 w0 w1 w2 w3 w4 w5 w6 w7) = MkX64WithX8 (fusedMultiplyAddF u0 v0 w0) (fusedMultiplyAddF u1 v1 w1) (fusedMultiplyAddF u2 v2 w2) (fusedMultiplyAddF u3 v3 w3) (fusedMultiplyAddF u4 v4 w4) (fusedMultiplyAddF u5 v5 w5) (fusedMultiplyAddF u6 v6 w6) (fusedMultiplyAddF u7 v7 w7)
  {-# INLINE fusedMultiplyAddF #-}
instance (Num a, PackX8 X8 a) => EnumFromZero_ X64 a where
  enumFromZero = mkX64 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
  {-# INLINE enumFromZero #-}
instance (Prim a, MultiPrim X8 a) => MultiPrim X64 a where
  indexByteArraySIMD# ba i = MkX64WithX8 (indexByteArraySIMD# ba i) (indexByteArraySIMD# ba (i +# 8#)) (indexByteArraySIMD# ba (i +# 16#)) (indexByteArraySIMD# ba (i +# 24#)) (indexByteArraySIMD# ba (i +# 32#)) (indexByteArraySIMD# ba (i +# 40#)) (indexByteArraySIMD# ba (i +# 48#)) (indexByteArraySIMD# ba (i +# 56#))
  readByteArraySIMD# mba i s0 = case readByteArraySIMD# mba i s0 of (# s1, u0 #) -> case readByteArraySIMD# mba (i +# 8#) s1 of (# s2, u1 #) -> case readByteArraySIMD# mba (i +# 16#) s2 of (# s3, u2 #) -> case readByteArraySIMD# mba (i +# 24#) s3 of (# s4, u3 #) -> case readByteArraySIMD# mba (i +# 32#) s4 of (# s5, u4 #) -> case readByteArraySIMD# mba (i +# 40#) s5 of (# s6, u5 #) -> case readByteArraySIMD# mba (i +# 48#) s6 of (# s7, u6 #) -> case readByteArraySIMD# mba (i +# 56#) s7 of (# s8, u7 #) -> (# s8, MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7 #)
  writeByteArraySIMD# mba i (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) s0 = case writeByteArraySIMD# mba i u0 s0 of s1 -> case writeByteArraySIMD# mba (i +# 1#) u1 s1 of s2 -> case writeByteArraySIMD# mba (i +# 2#) u2 s2 of s3 -> case writeByteArraySIMD# mba (i +# 3#) u3 s3 of s4 -> case writeByteArraySIMD# mba (i +# 4#) u4 s4 of s5 -> case writeByteArraySIMD# mba (i +# 5#) u5 s5 of s6 -> case writeByteArraySIMD# mba (i +# 6#) u6 s6 of s7 -> writeByteArraySIMD# mba (i +# 7#) u7 s7
  {-# INLINE indexByteArraySIMD# #-}
  {-# INLINE readByteArraySIMD# #-}
  {-# INLINE writeByteArraySIMD# #-}
instance (Storable a, MultiStorable X8 a) => MultiStorable X64 a where
  peekElemOffSIMD !ptr !i = MkX64WithX8 <$> peekElemOffSIMD ptr i <*> peekElemOffSIMD ptr (i + 1) <*> peekElemOffSIMD ptr (i + 2) <*> peekElemOffSIMD ptr (i + 3) <*> peekElemOffSIMD ptr (i + 4) <*> peekElemOffSIMD ptr (i + 5) <*> peekElemOffSIMD ptr (i + 6) <*> peekElemOffSIMD ptr (i + 7)
  pokeElemOffSIMD !ptr !i (MkX64WithX8 u0 u1 u2 u3 u4 u5 u6 u7) = pokeElemOffSIMD ptr i u0 >> pokeElemOffSIMD ptr (i + 1) u1 >> pokeElemOffSIMD ptr (i + 2) u2 >> pokeElemOffSIMD ptr (i + 3) u3 >> pokeElemOffSIMD ptr (i + 4) u4 >> pokeElemOffSIMD ptr (i + 5) u5 >> pokeElemOffSIMD ptr (i + 6) u6 >> pokeElemOffSIMD ptr (i + 7) u7
  {-# INLINE peekElemOffSIMD #-}
  {-# INLINE pokeElemOffSIMD #-}
