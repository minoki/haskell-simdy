{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonoLocalBinds #-}
module MatMul where
import           Control.DeepSeq
import           Control.Monad
import           Data.Kind (Type)
import           Data.Proxy
import           Data.Simdy
import           Data.Simdy.FMA
import qualified Data.Simdy.Vector.Class as SV
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           GHC.TypeNats

type Mat :: Natural -> Natural -> Type -> Type
newtype Mat m n a = MkMat (VU.Vector a)
  deriving (Eq, Show)
  deriving newtype NFData
-- mat (i,j) = v ! (i * n + j)

matMulNaive :: forall l m n a. (KnownNat l, KnownNat m, KnownNat n, VU.Unbox a, SIMDNum a) => Mat l m a -> Mat m n a -> Mat l n a
matMulNaive (MkMat !a) (MkMat !b) = MkMat $ VU.create $ do
  let l, m, n :: Int
      !l = fromIntegral (natVal (Proxy @l))
      !m = fromIntegral (natVal (Proxy @m))
      !n = fromIntegral (natVal (Proxy @n))
  !result <- VUM.unsafeNew (l * n)
  forM_ [0..l-1] $ \ !i ->
    forM_ [0..n-1] $ \ !k ->
      VUM.unsafeWrite result (i * n + k) $! sum [a `VU.unsafeIndex` (i * m + j) * b `VU.unsafeIndex` (j * n + k) | j <- [0..m-1]]
  pure result
{-# SPECIALIZE matMulNaive :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulNaive :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Mat l m Double -> Mat m n Double -> Mat l n Double #-}

matMulSIMD :: forall x l m n a. (SIMD x, KnownNat l, KnownNat m, KnownNat n, SV.MultiUnbox x a, SIMDNum a) => Proxy x -> Mat l m a -> Mat m n a -> Mat l n a
matMulSIMD _ (MkMat !a) (MkMat !b) = MkMat $ VU.create $ do
  let l, m, n, simdLen :: Int
      !l = fromIntegral (natVal (Proxy @l))
      !m = fromIntegral (natVal (Proxy @m))
      !n = fromIntegral (natVal (Proxy @n))
      !simdLen = simdLength @x
  !result <- VUM.replicate (l * n) 0
  forM_ [0..l-1] $ \ !i -> do
    forM_ [0..m-1] $ \ !j -> do
      let !a_ij = a `VU.unsafeIndex` (i * m + j)
          a_ij_v :: x a
          !a_ij_v = broadcast a_ij
      let loopVector !k
            | k + simdLen > n = loopScalar k
            | otherwise = do
              !acc <- SV.unsafeReadMulti result (i * n + k)
              SV.unsafeWriteMulti result (i * n + k) $! acc + a_ij_v * SV.unsafeIndexMulti b (j * n + k)
              loopVector (k + simdLen)
          loopScalar !k
            | k >= n = pure ()
            | otherwise = do
              !acc <- VUM.unsafeRead result (i * n + k)
              VUM.unsafeWrite result (i * n + k) $! acc + a_ij * VU.unsafeIndex b (j * n + k)
              loopScalar (k + 1)
      loopVector 0
  pure result
{-# SPECIALIZE matMulSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X4 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X8 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X16 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X32 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X4 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X8 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X16 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X32 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}

matMulFMA :: forall x l m n a. (HasFMA, SIMD x, KnownNat l, KnownNat m, KnownNat n, SV.MultiUnbox x a, SIMDFMA a) => Proxy x -> Mat l m a -> Mat m n a -> Mat l n a
matMulFMA _ (MkMat !a) (MkMat !b) = MkMat $ VU.create $ do
  let l, m, n, simdLen :: Int
      !l = fromIntegral (natVal (Proxy @l))
      !m = fromIntegral (natVal (Proxy @m))
      !n = fromIntegral (natVal (Proxy @n))
      !simdLen = simdLength @x
  !result <- VUM.replicate (l * n) 0
  forM_ [0..l-1] $ \ !i -> do
    forM_ [0..m-1] $ \ !j -> do
      let !a_ij = a `VU.unsafeIndex` (i * m + j)
          a_ij_v :: x a
          !a_ij_v = broadcast a_ij
      let loopVector !k
            | k + simdLen > n = loopScalar k
            | otherwise = do
              !acc <- SV.unsafeReadMulti result (i * n + k)
              SV.unsafeWriteMulti result (i * n + k) $! fusedMultiplyAdd a_ij_v (SV.unsafeIndexMulti b (j * n + k)) acc
              loopVector (k + simdLen)
          loopScalar !k
            | k >= n = pure ()
            | otherwise = do
              !acc <- VUM.unsafeRead result (i * n + k)
              VUM.unsafeWrite result (i * n + k) $! fusedMultiplyAdd a_ij (VU.unsafeIndex b (j * n + k)) acc
              loopScalar (k + 1)
      loopVector 0
  pure result
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X4 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X8 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X16 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X32 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X4 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X8 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X16 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X32 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
