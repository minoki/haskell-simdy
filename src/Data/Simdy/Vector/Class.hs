{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Type classes connecting SIMD operations with the @vector@ library.
--
-- 'SIMDMVector' and 'SIMDVector' extend 'Data.Vector.Generic.Mutable.MVector'
-- and 'Data.Vector.Generic.Vector' with multi-element (SIMD-width) read\/write operations.
-- 'MultiUnbox' bundles these for 'Data.Vector.Unboxed.Unbox' vectors.
module Data.Simdy.Vector.Class where
import           Control.Applicative (liftA3)
import           Control.Monad (liftM4, liftM5)
import           Control.Monad.Primitive (unsafePrimToST)
import           Data.Array.Byte (ByteArray (..), MutableByteArray (..))
import           Data.Complex (Complex)
import           Data.Int (Int16, Int32, Int64, Int8)
import           Data.Monoid (Product, Sum)
import           Data.Semigroup (Max, Min)
import           Data.Simdy.Internal.Class
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Base as VUB
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word (Word16, Word32, Word64, Word8)
import           Foreign.ForeignPtr (withForeignPtr)
import           GHC.Int (Int (I#))
import           GHC.ST (ST (..))
import           System.IO.Unsafe (unsafeDupablePerformIO)

-- | Mutable vectors that support SIMD-width read and write.
class (VGM.MVector v a, KnownSIMDLength f) => SIMDMVector v f a where
  -- | Read a SIMD vector starting at the given index. The index is in elements, not vectors.
  unsafeReadMulti :: v s a -> Int -> ST s (f a)
  -- | Write a SIMD vector starting at the given index.
  unsafeWriteMulti :: v s a -> Int -> f a -> ST s ()

-- | Immutable vectors that support SIMD-width indexing.
class (VG.Vector v a, SIMDMVector (VG.Mutable v) f a) => SIMDVector v f a where
  -- | Index a SIMD vector starting at the given element offset.
  unsafeIndexMulti :: v a -> Int -> f a

-- | Convenience class bundling 'SIMDVector' and 'SIMDMVector' for 'Data.Vector.Unboxed.Unbox' types.
class (SIMDVector VU.Vector f a, SIMDMVector VUM.MVector f a, VU.Unbox a) => MultiUnbox f a

--
-- Primitive Vectors
--

instance MultiPrim f a => SIMDMVector VPM.MVector f a where
  unsafeReadMulti (VPM.MVector offset _length (MutableByteArray mba#)) i = case offset + i of
    I# i# -> ST (readByteArraySIMD# mba# i#)
  unsafeWriteMulti (VPM.MVector offset _length (MutableByteArray mba#)) i !v = case offset + i of
    I# i# -> ST (\s -> (# writeByteArraySIMD# mba# i# v s, () #))
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f a => SIMDVector VP.Vector f a where
  unsafeIndexMulti (VP.Vector offset _length (ByteArray ba#)) i = case offset + i of
    I# i# -> indexByteArraySIMD# ba# i#
  {-# INLINE unsafeIndexMulti #-}

--
-- Unboxed Vectors
--

instance MultiPrim f Float => SIMDMVector VUM.MVector f Float where
  unsafeReadMulti (VUB.MV_Float v) = unsafeReadMulti v
  unsafeWriteMulti (VUB.MV_Float v) = unsafeWriteMulti v
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f Float => SIMDVector VU.Vector f Float where
  unsafeIndexMulti (VUB.V_Float v) = unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance MultiPrim f Double => SIMDMVector VUM.MVector f Double where
  unsafeReadMulti (VUB.MV_Double v) = unsafeReadMulti v
  unsafeWriteMulti (VUB.MV_Double v) = unsafeWriteMulti v
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f Double => SIMDVector VU.Vector f Double where
  unsafeIndexMulti (VUB.V_Double v) = unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance MultiPrim f Int8 => SIMDMVector VUM.MVector f Int8 where
  unsafeReadMulti (VUB.MV_Int8 v) = unsafeReadMulti v
  unsafeWriteMulti (VUB.MV_Int8 v) = unsafeWriteMulti v
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f Int8 => SIMDVector VU.Vector f Int8 where
  unsafeIndexMulti (VUB.V_Int8 v) = unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance MultiPrim f Int16 => SIMDMVector VUM.MVector f Int16 where
  unsafeReadMulti (VUB.MV_Int16 v) = unsafeReadMulti v
  unsafeWriteMulti (VUB.MV_Int16 v) = unsafeWriteMulti v
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f Int16 => SIMDVector VU.Vector f Int16 where
  unsafeIndexMulti (VUB.V_Int16 v) = unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance MultiPrim f Int32 => SIMDMVector VUM.MVector f Int32 where
  unsafeReadMulti (VUB.MV_Int32 v) = unsafeReadMulti v
  unsafeWriteMulti (VUB.MV_Int32 v) = unsafeWriteMulti v
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f Int32 => SIMDVector VU.Vector f Int32 where
  unsafeIndexMulti (VUB.V_Int32 v) = unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance MultiPrim f Int64 => SIMDMVector VUM.MVector f Int64 where
  unsafeReadMulti (VUB.MV_Int64 v) = unsafeReadMulti v
  unsafeWriteMulti (VUB.MV_Int64 v) = unsafeWriteMulti v
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f Int64 => SIMDVector VU.Vector f Int64 where
  unsafeIndexMulti (VUB.V_Int64 v) = unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance MultiPrim f Word8 => SIMDMVector VUM.MVector f Word8 where
  unsafeReadMulti (VUB.MV_Word8 v) = unsafeReadMulti v
  unsafeWriteMulti (VUB.MV_Word8 v) = unsafeWriteMulti v
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f Word8 => SIMDVector VU.Vector f Word8 where
  unsafeIndexMulti (VUB.V_Word8 v) = unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance MultiPrim f Word16 => SIMDMVector VUM.MVector f Word16 where
  unsafeReadMulti (VUB.MV_Word16 v) = unsafeReadMulti v
  unsafeWriteMulti (VUB.MV_Word16 v) = unsafeWriteMulti v
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f Word16 => SIMDVector VU.Vector f Word16 where
  unsafeIndexMulti (VUB.V_Word16 v) = unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance MultiPrim f Word32 => SIMDMVector VUM.MVector f Word32 where
  unsafeReadMulti (VUB.MV_Word32 v) = unsafeReadMulti v
  unsafeWriteMulti (VUB.MV_Word32 v) = unsafeWriteMulti v
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f Word32 => SIMDVector VU.Vector f Word32 where
  unsafeIndexMulti (VUB.V_Word32 v) = unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance MultiPrim f Word64 => SIMDMVector VUM.MVector f Word64 where
  unsafeReadMulti (VUB.MV_Word64 v) = unsafeReadMulti v
  unsafeWriteMulti (VUB.MV_Word64 v) = unsafeWriteMulti v
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance MultiPrim f Word64 => SIMDVector VU.Vector f Word64 where
  unsafeIndexMulti (VUB.V_Word64 v) = unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance (LiftConstructor f, SIMDMVector VUM.MVector f a, VU.Unbox a) => SIMDMVector VUM.MVector f (Sum a) where
  unsafeReadMulti (VUB.MV_Sum v) !i = mkSum <$> unsafeReadMulti v i
  unsafeWriteMulti (VUB.MV_Sum v) !i = unsafeWriteMulti v i . getSum'
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (LiftConstructor f, SIMDVector VU.Vector f a, VU.Unbox a) => SIMDVector VU.Vector f (Sum a) where
  unsafeIndexMulti (VUB.V_Sum v) = mkSum . unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance (LiftConstructor f, SIMDMVector VUM.MVector f a, VU.Unbox a) => SIMDMVector VUM.MVector f (Product a) where
  unsafeReadMulti (VUB.MV_Product v) !i = mkProduct <$> unsafeReadMulti v i
  unsafeWriteMulti (VUB.MV_Product v) !i = unsafeWriteMulti v i . getProduct'
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (LiftConstructor f, SIMDVector VU.Vector f a, VU.Unbox a) => SIMDVector VU.Vector f (Product a) where
  unsafeIndexMulti (VUB.V_Product v) = mkProduct . unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance (LiftConstructor f, SIMDMVector VUM.MVector f a, VU.Unbox a) => SIMDMVector VUM.MVector f (Min a) where
  unsafeReadMulti (VUB.MV_Min v) !i = mkMin <$> unsafeReadMulti v i
  unsafeWriteMulti (VUB.MV_Min v) !i = unsafeWriteMulti v i . getMin'
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (LiftConstructor f, SIMDVector VU.Vector f a, VU.Unbox a) => SIMDVector VU.Vector f (Min a) where
  unsafeIndexMulti (VUB.V_Min v) = mkMin . unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance (LiftConstructor f, SIMDMVector VUM.MVector f a, VU.Unbox a) => SIMDMVector VUM.MVector f (Max a) where
  unsafeReadMulti (VUB.MV_Max v) !i = mkMax <$> unsafeReadMulti v i
  unsafeWriteMulti (VUB.MV_Max v) !i = unsafeWriteMulti v i . getMax'
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (LiftConstructor f, SIMDVector VU.Vector f a, VU.Unbox a) => SIMDVector VU.Vector f (Max a) where
  unsafeIndexMulti (VUB.V_Max v) = mkMax . unsafeIndexMulti v
  {-# INLINE unsafeIndexMulti #-}

instance (KnownSIMDLength f, Broadcast f ()) => SIMDMVector VUM.MVector f () where
  unsafeReadMulti (VUB.MV_Unit _) _ = pure (broadcast ())
  unsafeWriteMulti (VUB.MV_Unit _) _ _ = pure ()
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (KnownSIMDLength f, Broadcast f ()) => SIMDVector VU.Vector f () where
  unsafeIndexMulti (VUB.V_Unit _) _ = broadcast ()
  {-# INLINE unsafeIndexMulti #-}

instance (SIMDMVector VUM.MVector f a, SIMDMVector VUM.MVector f b, LiftConstructor f, VU.Unbox a, VU.Unbox b) => SIMDMVector VUM.MVector f (a, b) where
  unsafeReadMulti (VUB.MV_2 _ va vb) !i = liftA2 mkTuple2 (unsafeReadMulti va i) (unsafeReadMulti vb i)
  unsafeWriteMulti (VUB.MV_2 _ va vb) !i t = case deconstructTuple2 t of
    (a, b) -> unsafeWriteMulti va i a >> unsafeWriteMulti vb i b
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (SIMDVector VU.Vector f a, SIMDVector VU.Vector f b, LiftConstructor f, VU.Unbox a, VU.Unbox b) => SIMDVector VU.Vector f (a, b) where
  unsafeIndexMulti (VUB.V_2 _ va vb) !i = mkTuple2 (unsafeIndexMulti va i) (unsafeIndexMulti vb i)
  {-# INLINE unsafeIndexMulti #-}

instance (SIMDMVector VUM.MVector f a, SIMDMVector VUM.MVector f b, SIMDMVector VUM.MVector f c, LiftConstructor f, VU.Unbox a, VU.Unbox b, VU.Unbox c) => SIMDMVector VUM.MVector f (a, b, c) where
  unsafeReadMulti (VUB.MV_3 _ va vb vc) !i = liftA3 mkTuple3 (unsafeReadMulti va i) (unsafeReadMulti vb i) (unsafeReadMulti vc i)
  unsafeWriteMulti (VUB.MV_3 _ va vb vc) !i t = case deconstructTuple3 t of
    (a, b, c) -> unsafeWriteMulti va i a >> unsafeWriteMulti vb i b >> unsafeWriteMulti vc i c
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (SIMDVector VU.Vector f a, SIMDVector VU.Vector f b, SIMDVector VU.Vector f c, LiftConstructor f, VU.Unbox a, VU.Unbox b, VU.Unbox c) => SIMDVector VU.Vector f (a, b, c) where
  unsafeIndexMulti (VUB.V_3 _ va vb vc) !i = mkTuple3 (unsafeIndexMulti va i) (unsafeIndexMulti vb i) (unsafeIndexMulti vc i)
  {-# INLINE unsafeIndexMulti #-}

instance (SIMDMVector VUM.MVector f a, SIMDMVector VUM.MVector f b, SIMDMVector VUM.MVector f c, SIMDMVector VUM.MVector f d, LiftConstructor f, VU.Unbox a, VU.Unbox b, VU.Unbox c, VU.Unbox d) => SIMDMVector VUM.MVector f (a, b, c, d) where
  unsafeReadMulti (VUB.MV_4 _ va vb vc vd) !i = liftM4 mkTuple4 (unsafeReadMulti va i) (unsafeReadMulti vb i) (unsafeReadMulti vc i) (unsafeReadMulti vd i)
  unsafeWriteMulti (VUB.MV_4 _ va vb vc vd) !i t = case deconstructTuple4 t of
    (a, b, c, d) -> unsafeWriteMulti va i a >> unsafeWriteMulti vb i b >> unsafeWriteMulti vc i c >> unsafeWriteMulti vd i d
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (SIMDVector VU.Vector f a, SIMDVector VU.Vector f b, SIMDVector VU.Vector f c, SIMDVector VU.Vector f d, LiftConstructor f, VU.Unbox a, VU.Unbox b, VU.Unbox c, VU.Unbox d) => SIMDVector VU.Vector f (a, b, c, d) where
  unsafeIndexMulti (VUB.V_4 _ va vb vc vd) !i = mkTuple4 (unsafeIndexMulti va i) (unsafeIndexMulti vb i) (unsafeIndexMulti vc i) (unsafeIndexMulti vd i)
  {-# INLINE unsafeIndexMulti #-}

instance (SIMDMVector VUM.MVector f a, SIMDMVector VUM.MVector f b, SIMDMVector VUM.MVector f c, SIMDMVector VUM.MVector f d, SIMDMVector VUM.MVector f e, LiftConstructor f, VU.Unbox a, VU.Unbox b, VU.Unbox c, VU.Unbox d, VU.Unbox e) => SIMDMVector VUM.MVector f (a, b, c, d, e) where
  unsafeReadMulti (VUB.MV_5 _ va vb vc vd ve) !i = liftM5 mkTuple5 (unsafeReadMulti va i) (unsafeReadMulti vb i) (unsafeReadMulti vc i) (unsafeReadMulti vd i) (unsafeReadMulti ve i)
  unsafeWriteMulti (VUB.MV_5 _ va vb vc vd ve) !i t = case deconstructTuple5 t of
    (a, b, c, d, e) -> unsafeWriteMulti va i a >> unsafeWriteMulti vb i b >> unsafeWriteMulti vc i c >> unsafeWriteMulti vd i d >> unsafeWriteMulti ve i e
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (SIMDVector VU.Vector f a, SIMDVector VU.Vector f b, SIMDVector VU.Vector f c, SIMDVector VU.Vector f d, SIMDVector VU.Vector f e, LiftConstructor f, VU.Unbox a, VU.Unbox b, VU.Unbox c, VU.Unbox d, VU.Unbox e) => SIMDVector VU.Vector f (a, b, c, d, e) where
  unsafeIndexMulti (VUB.V_5 _ va vb vc vd ve) !i = mkTuple5 (unsafeIndexMulti va i) (unsafeIndexMulti vb i) (unsafeIndexMulti vc i) (unsafeIndexMulti vd i) (unsafeIndexMulti ve i)
  {-# INLINE unsafeIndexMulti #-}

instance (SIMDMVector VUM.MVector v a, SIMDMVector VUM.MVector v b, SIMDMVector VUM.MVector v c, SIMDMVector VUM.MVector v d, SIMDMVector VUM.MVector v e, SIMDMVector VUM.MVector v f, LiftConstructor v, VU.Unbox a, VU.Unbox b, VU.Unbox c, VU.Unbox d, VU.Unbox e, VU.Unbox f) => SIMDMVector VUM.MVector v (a, b, c, d, e, f) where
  unsafeReadMulti (VUB.MV_6 _ va vb vc vd ve vf) !i = do
    !a <- unsafeReadMulti va i
    !b <- unsafeReadMulti vb i
    !c <- unsafeReadMulti vc i
    !d <- unsafeReadMulti vd i
    !e <- unsafeReadMulti ve i
    !f <- unsafeReadMulti vf i
    pure (mkTuple6 a b c d e f)
  unsafeWriteMulti (VUB.MV_6 _ va vb vc vd ve vf) !i t = case deconstructTuple6 t of
    (a, b, c, d, e, f) -> do
      unsafeWriteMulti va i a
      unsafeWriteMulti vb i b
      unsafeWriteMulti vc i c
      unsafeWriteMulti vd i d
      unsafeWriteMulti ve i e
      unsafeWriteMulti vf i f
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (SIMDVector VU.Vector v a, SIMDVector VU.Vector v b, SIMDVector VU.Vector v c, SIMDVector VU.Vector v d, SIMDVector VU.Vector v e, SIMDVector VU.Vector v f, LiftConstructor v, VU.Unbox a, VU.Unbox b, VU.Unbox c, VU.Unbox d, VU.Unbox e, VU.Unbox f) => SIMDVector VU.Vector v (a, b, c, d, e, f) where
  unsafeIndexMulti (VUB.V_6 _ va vb vc vd ve vf) !i = mkTuple6 (unsafeIndexMulti va i) (unsafeIndexMulti vb i) (unsafeIndexMulti vc i) (unsafeIndexMulti vd i) (unsafeIndexMulti ve i) (unsafeIndexMulti vf i)
  {-# INLINE unsafeIndexMulti #-}

instance (SIMDMVector VUM.MVector f a, LiftConstructor f, VU.Unbox a) => SIMDMVector VUM.MVector f (Complex a) where
  unsafeReadMulti (VUB.MV_Complex vt) !i = do
    t <- unsafeReadMulti vt i
    let (!x,!y) = deconstructTuple2 t
    pure (MkComplex x y)
  unsafeWriteMulti (VUB.MV_Complex vt) !i z = unsafeWriteMulti vt i (uncurry mkTuple2 (deconstructComplex z))
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (SIMDVector VU.Vector f a, LiftConstructor f, VU.Unbox a) => SIMDVector VU.Vector f (Complex a) where
  unsafeIndexMulti (VUB.V_Complex vt) !i = uncurry mkComplex $ deconstructTuple2 $ unsafeIndexMulti vt i
  {-# INLINE unsafeIndexMulti #-}

instance MultiPrim f Float => MultiUnbox f Float
instance MultiPrim f Double => MultiUnbox f Double
instance MultiPrim f Int8 => MultiUnbox f Int8
instance MultiPrim f Int16 => MultiUnbox f Int16
instance MultiPrim f Int32 => MultiUnbox f Int32
instance MultiPrim f Int64 => MultiUnbox f Int64
instance MultiPrim f Word8 => MultiUnbox f Word8
instance MultiPrim f Word16 => MultiUnbox f Word16
instance MultiPrim f Word32 => MultiUnbox f Word32
instance MultiPrim f Word64 => MultiUnbox f Word64
instance (LiftConstructor f, MultiUnbox f a) => MultiUnbox f (Sum a)
instance (LiftConstructor f, MultiUnbox f a) => MultiUnbox f (Product a)
instance (LiftConstructor f, MultiUnbox f a) => MultiUnbox f (Min a)
instance (LiftConstructor f, MultiUnbox f a) => MultiUnbox f (Max a)
instance (LiftConstructor f, MultiUnbox f a) => MultiUnbox f (Complex a)
instance (KnownSIMDLength f, Broadcast f ()) => MultiUnbox f ()
instance (LiftConstructor f, MultiUnbox f a, MultiUnbox f b) => MultiUnbox f (a, b)
instance (LiftConstructor f, MultiUnbox f a, MultiUnbox f b, MultiUnbox f c) => MultiUnbox f (a, b, c)
instance (LiftConstructor f, MultiUnbox f a, MultiUnbox f b, MultiUnbox f c, MultiUnbox f d) => MultiUnbox f (a, b, c, d)
instance (LiftConstructor f, MultiUnbox f a, MultiUnbox f b, MultiUnbox f c, MultiUnbox f d, MultiUnbox f e) => MultiUnbox f (a, b, c, d, e)
instance (LiftConstructor x, MultiUnbox x a, MultiUnbox x b, MultiUnbox x c, MultiUnbox x d, MultiUnbox x e, MultiUnbox x f) => MultiUnbox x (a, b, c, d, e, f)

--
-- Storable Vectors
--

instance (VSM.Storable a, MultiStorable f a) => SIMDMVector VSM.MVector f a where
  unsafeReadMulti (VSM.MVector _ fp) !i = unsafePrimToST (withForeignPtr fp (\ptr -> peekElemOffSIMD ptr i))
  unsafeWriteMulti (VSM.MVector _ fp) !i !v = unsafePrimToST (withForeignPtr fp (\ptr -> pokeElemOffSIMD ptr i v))
  {-# INLINE unsafeReadMulti #-}
  {-# INLINE unsafeWriteMulti #-}

instance (VSM.Storable a, MultiStorable f a) => SIMDVector VS.Vector f a where
  -- Data.Vector.Storable uses Control.Monad.Primitive.unsafeInlineIO. Should we copy it?
  unsafeIndexMulti !v i = unsafeDupablePerformIO (VS.unsafeWith v (\ptr -> peekElemOffSIMD ptr i))
  {-# INLINE unsafeIndexMulti #-}
