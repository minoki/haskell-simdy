{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -fplugin=Data.Simdy.Shuffle.Plugin #-}
module MatMul where
import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable (foldlM)
import           Data.Kind (Type)
import           Data.Proxy
import           Data.Simdy
import           Data.Simdy.FMA
import           Data.Simdy.Horizontal
import           Data.Simdy.Shuffle
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

matMulSIMD_4_3 :: forall x l m n a. (SIMD x, KnownNat l, KnownNat m, KnownNat n, SV.MultiUnbox x a, SIMDNum a) => Proxy x -> Mat l m a -> Mat m n a -> Mat l n a
matMulSIMD_4_3 _ (MkMat !a) (MkMat !b) = MkMat $ VU.create $ do
  let l, m, n, simdLen :: Int
      !l = fromIntegral (natVal (Proxy @l))
      !m = fromIntegral (natVal (Proxy @m))
      !n = fromIntegral (natVal (Proxy @n))
      !simdLen = simdLength @x
  !result <- VUM.replicate (l * n) 0
  forBlockM_ l 4 $ \case
    Block i ->
      forBlockM_ m 3 $ \case
        Block j -> do
          let !a_i0j0 = a `VU.unsafeIndex` (i * m + j)
              !a_i1j0 = a `VU.unsafeIndex` ((i + 1) * m + j)
              !a_i2j0 = a `VU.unsafeIndex` ((i + 2) * m + j)
              !a_i3j0 = a `VU.unsafeIndex` ((i + 3) * m + j)
              !a_i0j1 = a `VU.unsafeIndex` (i * m + j + 1)
              !a_i1j1 = a `VU.unsafeIndex` ((i + 1) * m + j + 1)
              !a_i2j1 = a `VU.unsafeIndex` ((i + 2) * m + j + 1)
              !a_i3j1 = a `VU.unsafeIndex` ((i + 3) * m + j + 1)
              !a_i0j2 = a `VU.unsafeIndex` (i * m + j + 2)
              !a_i1j2 = a `VU.unsafeIndex` ((i + 1) * m + j + 2)
              !a_i2j2 = a `VU.unsafeIndex` ((i + 2) * m + j + 2)
              !a_i3j2 = a `VU.unsafeIndex` ((i + 3) * m + j + 2)
          let loopVector !k
                | k + simdLen > n = loopScalar k
                | otherwise = do
                  !acc0 <- SV.unsafeReadMulti result (i * n + k)
                  !acc1 <- SV.unsafeReadMulti result ((i + 1) * n + k)
                  !acc2 <- SV.unsafeReadMulti result ((i + 2) * n + k)
                  !acc3 <- SV.unsafeReadMulti result ((i + 3) * n + k)
                  let !b_j0k = SV.unsafeIndexMulti b (j * n + k) :: x a
                      !b_j1k = SV.unsafeIndexMulti b ((j + 1) * n + k) :: x a
                      !b_j2k = SV.unsafeIndexMulti b ((j + 2) * n + k) :: x a
                  SV.unsafeWriteMulti result (i * n + k) $! acc0 + broadcast a_i0j0 * b_j0k + broadcast a_i0j1 * b_j1k + broadcast a_i0j2 * b_j2k
                  SV.unsafeWriteMulti result ((i + 1) * n + k) $! acc1 + broadcast a_i1j0 * b_j0k + broadcast a_i1j1 * b_j1k + broadcast a_i1j2 * b_j2k
                  SV.unsafeWriteMulti result ((i + 2) * n + k) $! acc2 + broadcast a_i2j0 * b_j0k + broadcast a_i2j1 * b_j1k + broadcast a_i2j2 * b_j2k
                  SV.unsafeWriteMulti result ((i + 3) * n + k) $! acc3 + broadcast a_i3j0 * b_j0k + broadcast a_i3j1 * b_j1k + broadcast a_i3j2 * b_j2k
                  loopVector (k + simdLen)
              loopScalar !k
                | k >= n = pure ()
                | otherwise = do
                  !acc0 <- VUM.unsafeRead result (i * n + k)
                  !acc1 <- VUM.unsafeRead result ((i + 1) * n + k)
                  !acc2 <- VUM.unsafeRead result ((i + 2) * n + k)
                  !acc3 <- VUM.unsafeRead result ((i + 3) * n + k)
                  let !b_j0k = VU.unsafeIndex b (j * n + k)
                      !b_j1k = VU.unsafeIndex b ((j + 1) * n + k)
                      !b_j2k = VU.unsafeIndex b ((j + 2) * n + k)
                  VUM.unsafeWrite result (i * n + k) $! acc0 + a_i0j0 * b_j0k + a_i0j1 * b_j1k + a_i0j2 * b_j2k
                  VUM.unsafeWrite result ((i + 1) * n + k) $! acc1 + a_i1j0 * b_j0k + a_i1j1 * b_j1k + a_i1j2 * b_j2k
                  VUM.unsafeWrite result ((i + 2) * n + k) $! acc2 + a_i2j0 * b_j0k + a_i2j1 * b_j1k + a_i2j2 * b_j2k
                  VUM.unsafeWrite result ((i + 3) * n + k) $! acc3 + a_i3j0 * b_j0k + a_i3j1 * b_j1k + a_i3j2 * b_j2k
                  loopScalar (k + 1)
          loopVector 0
        Elem j -> do
          let !a_i0j = a `VU.unsafeIndex` (i * m + j)
              !a_i1j = a `VU.unsafeIndex` ((i + 1) * m + j)
              !a_i2j = a `VU.unsafeIndex` ((i + 2) * m + j)
              !a_i3j = a `VU.unsafeIndex` ((i + 3) * m + j)
          let loopVector !k
                | k + simdLen > n = loopScalar k
                | otherwise = do
                  !acc0 <- SV.unsafeReadMulti result (i * n + k)
                  !acc1 <- SV.unsafeReadMulti result ((i + 1) * n + k)
                  !acc2 <- SV.unsafeReadMulti result ((i + 2) * n + k)
                  !acc3 <- SV.unsafeReadMulti result ((i + 3) * n + k)
                  let !b_jk = SV.unsafeIndexMulti b (j * n + k) :: x a
                  SV.unsafeWriteMulti result (i * n + k) $! acc0 + broadcast a_i0j * b_jk
                  SV.unsafeWriteMulti result ((i + 1) * n + k) $! acc1 + broadcast a_i1j * b_jk
                  SV.unsafeWriteMulti result ((i + 2) * n + k) $! acc2 + broadcast a_i2j * b_jk
                  SV.unsafeWriteMulti result ((i + 3) * n + k) $! acc3 + broadcast a_i3j * b_jk
                  loopVector (k + simdLen)
              loopScalar !k
                | k >= n = pure ()
                | otherwise = do
                  !acc0 <- VUM.unsafeRead result (i * n + k)
                  !acc1 <- VUM.unsafeRead result ((i + 1) * n + k)
                  !acc2 <- VUM.unsafeRead result ((i + 2) * n + k)
                  !acc3 <- VUM.unsafeRead result ((i + 3) * n + k)
                  let !b_jk = VU.unsafeIndex b (j * n + k)
                  VUM.unsafeWrite result (i * n + k) $! acc0 + a_i0j * b_jk
                  VUM.unsafeWrite result ((i + 1) * n + k) $! acc1 + a_i1j * b_jk
                  VUM.unsafeWrite result ((i + 2) * n + k) $! acc2 + a_i2j * b_jk
                  VUM.unsafeWrite result ((i + 3) * n + k) $! acc3 + a_i3j * b_jk
                  loopScalar (k + 1)
          loopVector 0
    Elem i ->
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
{-# INLINABLE matMulSIMD_4_3 #-}

matMulBlockSIMD :: forall x l m n a. (SIMD x, KnownNat l, KnownNat m, KnownNat n, SV.MultiUnbox x a, SIMDNum a) => Proxy x -> Int -> Int -> Int -> Mat l m a -> Mat m n a -> Mat l n a
matMulBlockSIMD _ !lB !mB !nB (MkMat !a) (MkMat !b) = MkMat $ VU.create $ do
  let l, m, n, simdLen :: Int
      !l = fromIntegral (natVal (Proxy @l))
      !m = fromIntegral (natVal (Proxy @m))
      !n = fromIntegral (natVal (Proxy @n))
      !simdLen = simdLength @x
  !result <- VUM.replicate (l * n) 0
  !aBlock <- VUM.unsafeNew (lB * mB)
  !bBlockT <- VUM.unsafeNew (nB * mB)
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
              -- bBlockT ! (k' * mB + j') = b ! ((j + j') * n + k + k')
              forM_ [0..mB-1] $ \ !j' ->
                forM_ [0..nB-1] $ \ !k' -> do
                  let !b_jk = b `VU.unsafeIndex` ((j + j') * n + k + k')
                  VUM.unsafeWrite bBlockT (k' * mB + j') b_jk
              forM_ [0..lB-1] $ \ !i' ->
                forM_ [0..nB-1] $ \ !k' -> do
                  let !ik = (i + i') * n + k + k'
                  !c_ik <- VUM.unsafeRead result ik
                  let goVector !acc !j'
                        | j' + simdLen <= mB = do
                          !a_ij <- aBlock `SV.unsafeReadMulti` (i' * mB + j')
                          !b_jk <- bBlockT `SV.unsafeReadMulti` (k' * mB + j')
                          goVector (acc + a_ij * b_jk :: x a) (j' + simdLen)
                        | otherwise = goScalar (horizontalSum acc) j'
                      goScalar !acc !j'
                        | j' < mB = do
                          !a_ij <- aBlock `VUM.unsafeRead` (i' * mB + j')
                          !b_jk <- bBlockT `VUM.unsafeRead` (k' * mB + j')
                          goScalar (acc + a_ij * b_jk) (j' + 1)
                        | otherwise = pure acc
                  !c_ik' <- goVector 0 0
                  VUM.unsafeWrite result ik (c_ik + c_ik')
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
{-# SPECIALIZE matMulBlockSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X4 -> Int -> Int -> Int -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulBlockSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X8 -> Int -> Int -> Int -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulBlockSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X16 -> Int -> Int -> Int -> Mat l m Float -> Mat m n Float -> Mat l n Float #-}
{-# SPECIALIZE matMulBlockSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X4 -> Int -> Int -> Int -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulBlockSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X8 -> Int -> Int -> Int -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}
{-# SPECIALIZE matMulBlockSIMD :: forall l m n. (KnownNat l, KnownNat m, KnownNat n) => Proxy X16 -> Int -> Int -> Int -> Mat l m Double -> Mat m n Double -> Mat l n Double #-}

matMulBlockSIMDX8 :: forall l m n a. (SIMD X8, KnownNat l, KnownNat m, KnownNat n, SV.MultiUnbox X8 a, SIMDNum a, a ~ Float) => Int -> Int -> Int -> Mat l m a -> Mat m n a -> Mat l n a
matMulBlockSIMDX8 !lB !mB !nB (MkMat !a) (MkMat !b) = MkMat $ VU.create $ do
  let l, m, n, simdLen :: Int
      !l = fromIntegral (natVal (Proxy @l))
      !m = fromIntegral (natVal (Proxy @m))
      !n = fromIntegral (natVal (Proxy @n))
      !simdLen = 8
  !result <- VUM.replicate (l * n) 0
  !aBlock <- VUM.unsafeNew (lB * mB)
  !bBlockT <- VUM.unsafeNew (nB * mB)
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
              -- bBlockT ! (k' * mB + j') = b ! ((j + j') * n + k + k')
              {-
              forM_ [0..mB-1] $ \ !j' ->
                forM_ [0..nB-1] $ \ !k' -> do
                  let !b_jk = b `VU.unsafeIndex` ((j + j') * n + k + k')
                  VUM.unsafeWrite bBlockT (k' * mB + j') b_jk
              -}
              forBlockM_ mB 4 $ \case
                Block j' ->
                  forBlockM_ nB 4 $ \case
                    Block k' -> do
                      let !b_j0k = b `SV.unsafeIndexMulti` ((j + j') * n + k + k')
                          !b_j1k = b `SV.unsafeIndexMulti` ((j + j' + 1) * n + k + k')
                          !b_j2k = b `SV.unsafeIndexMulti` ((j + j' + 2) * n + k + k')
                          !b_j3k = b `SV.unsafeIndexMulti` ((j + j' + 3) * n + k + k')
                          d0 = binaryShuffleWithX4 (\(u00, u01, u02, u03) (u10, u11, u12, u13) -> (u00, u10, u01, u11)) b_j0k b_j1k
                          d1 = binaryShuffleWithX4 (\(u00, u01, u02, u03) (u10, u11, u12, u13) -> (u02, u12, u03, u13)) b_j0k b_j1k
                          d2 = binaryShuffleWithX4 (\(u20, u21, u22, u23) (u30, u31, u32, u33) -> (u20, u30, u21, u31)) b_j2k b_j3k
                          d3 = binaryShuffleWithX4 (\(u20, u21, u22, u23) (u30, u31, u32, u33) -> (u22, u32, u23, u33)) b_j2k b_j3k
                      SV.unsafeWriteMulti bBlockT (k' * mB + j')       (binaryShuffleWithX4 (\(u00, u10, u01, u11) (u20, u30, u21, u31) -> (u00, u10, u20, u30)) d0 d2)
                      SV.unsafeWriteMulti bBlockT ((k' + 1) * mB + j') (binaryShuffleWithX4 (\(u00, u10, u01, u11) (u20, u30, u21, u31) -> (u01, u11, u21, u31)) d0 d2)
                      SV.unsafeWriteMulti bBlockT ((k' + 2) * mB + j') (binaryShuffleWithX4 (\(u02, u12, u03, u13) (u22, u32, u23, u33) -> (u02, u12, u22, u32)) d1 d3)
                      SV.unsafeWriteMulti bBlockT ((k' + 3) * mB + j') (binaryShuffleWithX4 (\(u02, u12, u03, u13) (u22, u32, u23, u33) -> (u03, u13, u23, u33)) d1 d3)
                    Elem k' -> do
                      forM_ [j'..j'+4-1] $ \ !j'' -> do
                        let !b_jk = b `VU.unsafeIndex` ((j + j'') * n + k + k')
                        VUM.unsafeWrite bBlockT (k' * mB + j'') b_jk
                Elem j' ->
                  forM_ [0..nB-1] $ \ !k' -> do
                    let !b_jk = b `VU.unsafeIndex` ((j + j') * n + k + k')
                    VUM.unsafeWrite bBlockT (k' * mB + j') b_jk
              {-
              forBlockM_ mB simdLen $ \case
                Block j' ->
                  forBlockM_ nB simdLen $ \case
                    Block k' -> do
                      let !b_j0k = b `SV.unsafeIndexMulti` ((j + j') * n + k + k')
                          !b_j1k = b `SV.unsafeIndexMulti` ((j + j' + 1) * n + k + k')
                          !b_j2k = b `SV.unsafeIndexMulti` ((j + j' + 2) * n + k + k')
                          !b_j3k = b `SV.unsafeIndexMulti` ((j + j' + 3) * n + k + k')
                          !b_j4k = b `SV.unsafeIndexMulti` ((j + j' + 4) * n + k + k')
                          !b_j5k = b `SV.unsafeIndexMulti` ((j + j' + 5) * n + k + k')
                          !b_j6k = b `SV.unsafeIndexMulti` ((j + j' + 6) * n + k + k')
                          !b_j7k = b `SV.unsafeIndexMulti` ((j + j' + 7) * n + k + k')
                          d0 = binaryShuffleWithX8 (\(u00, u01, u02, u03, u04, u05, u06, u07) (u10, u11, u12, u13, u14, u15, u16, u17) -> (u00, u10, u01, u11, u02, u12, u03, u13)) b_j0k b_j1k
                          d1 = binaryShuffleWithX8 (\(u00, u01, u02, u03, u04, u05, u06, u07) (u10, u11, u12, u13, u14, u15, u16, u17) -> (u04, u14, u05, u15, u06, u16, u07, u17)) b_j0k b_j1k
                          d2 = binaryShuffleWithX8 (\(u20, u21, u22, u23, u24, u25, u26, u27) (u30, u31, u32, u33, u34, u35, u36, u37) -> (u20, u30, u21, u31, u22, u32, u23, u33)) b_j2k b_j3k
                          d3 = binaryShuffleWithX8 (\(u20, u21, u22, u23, u24, u25, u26, u27) (u30, u31, u32, u33, u34, u35, u36, u37) -> (u24, u34, u25, u35, u26, u36, u27, u37)) b_j2k b_j3k
                          e0 = binaryShuffleWithX8 (\(u00, u10, u01, u11, u02, u12, u03, u13) (u20, u30, u21, u31, u22, u32, u23, u33) -> (u00, u10, u20, u30, u01, u11, u21, u31)) d0 d2
                          e1 = binaryShuffleWithX8 (\(u00, u10, u01, u11, u02, u12, u03, u13) (u20, u30, u21, u31, u22, u32, u23, u33) -> (u02, u12, u22, u32, u03, u13, u23, u33)) d0 d2
                          e2 = binaryShuffleWithX8 (\(u04, u14, u05, u15, u06, u16, u07, u17) (u24, u34, u25, u35, u26, u36, u27, u37) -> (u04, u14, u24, u34, u05, u15, u25, u35)) d1 d3
                          e3 = binaryShuffleWithX8 (\(u04, u14, u05, u15, u06, u16, u07, u17) (u24, u34, u25, u35, u26, u36, u27, u37) -> (u06, u16, u26, u36, u07, u17, u27, u37)) d1 d3
                          d4 = binaryShuffleWithX8 (\(u40, u41, u42, u43, u44, u45, u46, u47) (u50, u51, u52, u53, u54, u55, u56, u57) -> (u40, u50, u41, u51, u42, u52, u43, u53)) b_j4k b_j5k
                          d5 = binaryShuffleWithX8 (\(u40, u41, u42, u43, u44, u45, u46, u47) (u50, u51, u52, u53, u54, u55, u56, u57) -> (u44, u54, u45, u55, u46, u56, u47, u57)) b_j4k b_j5k
                          d6 = binaryShuffleWithX8 (\(u60, u61, u62, u63, u64, u65, u66, u67) (u70, u71, u72, u73, u74, u75, u76, u77) -> (u60, u70, u61, u71, u62, u72, u63, u73)) b_j6k b_j7k
                          d7 = binaryShuffleWithX8 (\(u60, u61, u62, u63, u64, u65, u66, u67) (u70, u71, u72, u73, u74, u75, u76, u77) -> (u64, u74, u65, u75, u66, u76, u67, u77)) b_j6k b_j7k
                          e4 = binaryShuffleWithX8 (\(u40, u50, u41, u51, u42, u52, u43, u53) (u60, u70, u61, u71, u62, u72, u63, u73) -> (u40, u50, u60, u70, u41, u51, u61, u71)) d4 d6
                          e5 = binaryShuffleWithX8 (\(u40, u50, u41, u51, u42, u52, u43, u53) (u60, u70, u61, u71, u62, u72, u63, u73) -> (u42, u52, u62, u72, u43, u53, u63, u73)) d4 d6
                          e6 = binaryShuffleWithX8 (\(u44, u54, u45, u55, u46, u56, u47, u57) (u64, u74, u65, u75, u66, u76, u67, u77) -> (u44, u54, u64, u74, u45, u55, u65, u75)) d5 d7
                          e7 = binaryShuffleWithX8 (\(u44, u54, u45, u55, u46, u56, u47, u57) (u64, u74, u65, u75, u66, u76, u67, u77) -> (u46, u56, u66, u76, u47, u57, u67, u77)) d5 d7
                      SV.unsafeWriteMulti bBlockT (k' * mB + j')       (binaryShuffleWithX8 (\(u00, u10, u20, u30, u01, u11, u21, u31) (u40, u50, u60, u70, u41, u51, u61, u71) -> (u00, u10, u20, u30, u40, u50, u60, u70)) e0 e4)
                      SV.unsafeWriteMulti bBlockT ((k' + 1) * mB + j') (binaryShuffleWithX8 (\(u00, u10, u20, u30, u01, u11, u21, u31) (u40, u50, u60, u70, u41, u51, u61, u71) -> (u01, u11, u21, u31, u41, u51, u61, u71)) e0 e4)
                      SV.unsafeWriteMulti bBlockT ((k' + 2) * mB + j') (binaryShuffleWithX8 (\(u02, u12, u22, u32, u03, u13, u23, u33) (u42, u52, u62, u72, u43, u53, u63, u73) -> (u02, u12, u22, u32, u42, u52, u62, u72)) e1 e5)
                      SV.unsafeWriteMulti bBlockT ((k' + 3) * mB + j') (binaryShuffleWithX8 (\(u02, u12, u22, u32, u03, u13, u23, u33) (u42, u52, u62, u72, u43, u53, u63, u73) -> (u03, u13, u23, u33, u43, u53, u63, u73)) e1 e5)
                      SV.unsafeWriteMulti bBlockT ((k' + 4) * mB + j') (binaryShuffleWithX8 (\(u04, u14, u24, u34, u05, u15, u25, u35) (u44, u54, u64, u74, u45, u55, u65, u75) -> (u04, u14, u24, u34, u44, u54, u64, u74)) e2 e6)
                      SV.unsafeWriteMulti bBlockT ((k' + 5) * mB + j') (binaryShuffleWithX8 (\(u04, u14, u24, u34, u05, u15, u25, u35) (u44, u54, u64, u74, u45, u55, u65, u75) -> (u05, u15, u25, u35, u45, u55, u65, u75)) e2 e6)
                      SV.unsafeWriteMulti bBlockT ((k' + 6) * mB + j') (binaryShuffleWithX8 (\(u06, u16, u26, u36, u07, u17, u27, u37) (u46, u56, u66, u76, u47, u57, u67, u77) -> (u06, u16, u26, u36, u46, u56, u66, u76)) e3 e7)
                      SV.unsafeWriteMulti bBlockT ((k' + 7) * mB + j') (binaryShuffleWithX8 (\(u06, u16, u26, u36, u07, u17, u27, u37) (u46, u56, u66, u76, u47, u57, u67, u77) -> (u07, u17, u27, u37, u47, u57, u67, u77)) e3 e7)
                    Elem k' -> do
                      forM_ [j'..j'+simdLen-1] $ \ !j'' -> do
                        let !b_jk = b `VU.unsafeIndex` ((j + j'') * n + k + k')
                        VUM.unsafeWrite bBlockT (k' * mB + j'') b_jk
                Elem j' ->
                  forM_ [0..nB-1] $ \ !k' -> do
                    let !b_jk = b `VU.unsafeIndex` ((j + j') * n + k + k')
                    VUM.unsafeWrite bBlockT (k' * mB + j') b_jk
              -}
              forM_ [0..lB-1] $ \ !i' ->
                forM_ [0..nB-1] $ \ !k' -> do
                  let !ik = (i + i') * n + k + k'
                  !c_ik <- VUM.unsafeRead result ik
                  let goVector !acc !j'
                        | j' + simdLen <= mB = do
                          !a_ij <- aBlock `SV.unsafeReadMulti` (i' * mB + j')
                          !b_jk <- bBlockT `SV.unsafeReadMulti` (k' * mB + j')
                          goVector (acc + a_ij * b_jk :: X8 a) (j' + simdLen)
                        | otherwise = goScalar (horizontalSum acc) j'
                      goScalar !acc !j'
                        | j' < mB = do
                          !a_ij <- aBlock `VUM.unsafeRead` (i' * mB + j')
                          !b_jk <- bBlockT `VUM.unsafeRead` (k' * mB + j')
                          goScalar (acc + a_ij * b_jk) (j' + 1)
                        | otherwise = pure acc
                  !c_ik' <- goVector 0 0
                  VUM.unsafeWrite result ik (c_ik + c_ik')
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
{-# INLINABLE matMulBlockSIMDX8 #-}

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
