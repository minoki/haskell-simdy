{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
module MatMul where
import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable (foldlM)
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

matMulTranspose :: forall l m n a. (KnownNat l, KnownNat m, KnownNat n, VU.Unbox a, SIMDNum a) => Mat l m a -> Mat m n a -> Mat l n a
matMulTranspose (MkMat !a) (MkMat !b) = MkMat $ VU.create $ do
  let l, m, n :: Int
      !l = fromIntegral (natVal (Proxy @l))
      !m = fromIntegral (natVal (Proxy @m))
      !n = fromIntegral (natVal (Proxy @n))
      !bT = VU.create $ do
        bT' <- VUM.unsafeNew (n * m)
        forM_ [0..m-1] $ \ !j ->
          forM_ [0..n-1] $ \ !k -> do
            VUM.unsafeWrite bT' (k * m + j) $ b `VU.unsafeIndex` (j * n + k)
        pure bT'
  !result <- VUM.unsafeNew (l * n)
  forM_ [0..l-1] $ \ !i ->
    forM_ [0..n-1] $ \ !k ->
      VUM.unsafeWrite result (i * n + k) $! sum [a `VU.unsafeIndex` (i * m + j) * bT `VU.unsafeIndex` (k * m + j) | j <- [0..m-1]]
  pure result
{-# SPECIALIZE matMulTranspose :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulTranspose :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Mat l m Double -> Mat m n Double -> Mat l n Double #-}

data Chunk = Block !Int
           | Elem !Int
           deriving (Eq, Show)

forBlockM_ :: Monad m => Int -> Int -> (Chunk -> m ()) -> m ()
forBlockM_ !n !b f = loop 0
  where
    loop i | i + b > n = loopElem i
           | otherwise = f (Block i) >> loop (i + b)
    loopElem i | i >= n = pure ()
               | otherwise = f (Elem i) >> loopElem (i + 1)

matMulBlock :: forall l m n a. (KnownNat l, KnownNat m, KnownNat n, VU.Unbox a, SIMDNum a) => Int -> Int -> Int -> Mat l m a -> Mat m n a -> Mat l n a
matMulBlock !lB !mB !nB (MkMat !a) (MkMat !b) = MkMat $ VU.create $ do
  let l, m, n :: Int
      !l = fromIntegral (natVal (Proxy @l))
      !m = fromIntegral (natVal (Proxy @m))
      !n = fromIntegral (natVal (Proxy @n))
  !result <- VUM.replicate (l * n) 0
  !aBlock <- VUM.unsafeNew (lB * mB)
  !bBlock <- VUM.unsafeNew (mB * nB)
  forBlockM_ l lB $ \case
    Block i ->
      forBlockM_ m mB $ \case
        Block j -> do
          -- for i <= i' < i + lB and j <= j' < mB,
          -- aBlock ! (i' * mB + j') = a ! ((i + i') * m + j + j')
          forM_ [0..lB-1] $ \ !i' ->
            VU.copy (VUM.slice (i' * mB) mB aBlock) (VU.slice ((i + i') * m + j) mB a)
          forBlockM_ n nB $ \case
            Block k -> do
              -- for j <= j' < mB and k <= k' < nB,
              -- bBlock ! (j' * nB + k') = b ! ((j + j') * n + k + k')
              forM_ [0..mB-1] $ \ !j' ->
                VU.copy (VUM.slice (j' * nB) nB bBlock) (VU.slice ((j + j') * n + k) nB b)
              forM_ [0..lB-1] $ \ !i' ->
                forM_ [0..nB-1] $ \ !k' -> do
                  let !ik = (i + i') * n + k + k'
                  !c_ik <- VUM.unsafeRead result ik
                  let go !acc !j' = do
                        !a_ij <- aBlock `VUM.unsafeRead` (i' * mB + j')
                        !b_jk <- bBlock `VUM.unsafeRead` (j' * nB + k')
                        pure $! acc + a_ij * b_jk
                  !c_ik' <- foldlM go c_ik [0..mB-1]
                  VUM.unsafeWrite result ik c_ik'
            Elem k ->
              forM_ [0..lB-1] $ \ !i' -> do
                let !ik = (i + i') * n + k
                !c_ik <- VUM.unsafeRead result ik
                let go !acc !j' = do
                      !a_ij <- aBlock `VUM.unsafeRead` (i' * mB + j')
                      let !b_jk = b `VU.unsafeIndex` ((j + j') * n + k)
                      pure $! acc + a_ij * b_jk
                !c_ik' <- foldlM go c_ik [0..mB-1]
                VUM.unsafeWrite result ik c_ik'
        Elem j ->
          forM_ [0..lB-1] $ \ !i' -> do
            forM_ [0..n-1] $ \ !k ->
              flip (VUM.unsafeModify result) ((i + i') * n + k) $ \x -> x + a `VU.unsafeIndex` ((i + i') * m + j) * b `VU.unsafeIndex` (j * n + k)
    Elem i ->
      forM_ [0..n-1] $ \ !k ->
        VUM.unsafeWrite result (i * n + k) $! sum [a `VU.unsafeIndex` (i * m + j) * b `VU.unsafeIndex` (j * n + k) | j <- [0..m-1]]
  pure result
{-# SPECIALIZE matMulBlock :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Int -> Int -> Int -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulBlock :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Int -> Int -> Int -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}

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
{-# INLINABLE matMulFMA #-}
{-
The SPECIALIZE pragma doesn't work with HasFMA constraint.
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X4 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X8 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X16 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X32 -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X4 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X8 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X16 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulFMA :: forall l m n. (HasFMA, KnownNat l, KnownNat m, KnownNat n) => Proxy X32 -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
-}
