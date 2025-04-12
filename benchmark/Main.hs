import           Data.Simdy (X4, X8, X16, X32)
import qualified Data.Simdy.Vector.Generic as V.SIMD
import qualified Data.Vector.Unboxed as VU
import           Test.Tasty.Bench

dotProdVU :: VU.Vector Float -> VU.Vector Float -> Float
dotProdVU a b = VU.foldl' (+) 0 (VU.zipWith (*) a b)

dotProdX4 :: VU.Vector Float -> VU.Vector Float -> Float
dotProdX4 a b = V.SIMD.fold' @X4 (+) 0 (V.SIMD.zipWith @X4 (*) (*) a b)

dotProdX8 :: VU.Vector Float -> VU.Vector Float -> Float
dotProdX8 a b = V.SIMD.fold' @X8 (+) 0 (V.SIMD.zipWith @X8 (*) (*) a b)

dotProdX16 :: VU.Vector Float -> VU.Vector Float -> Float
dotProdX16 a b = V.SIMD.fold' @X16 (+) 0 (V.SIMD.zipWith @X16 (*) (*) a b)

dotProdX32 :: VU.Vector Float -> VU.Vector Float -> Float
dotProdX32 a b = V.SIMD.fold' @X32 (+) 0 (V.SIMD.zipWith @X32 (*) (*) a b)

dotProdVU_D :: VU.Vector Double -> VU.Vector Double -> Double
dotProdVU_D a b = VU.foldl' (+) 0 (VU.zipWith (*) a b)

dotProdX4_D :: VU.Vector Double -> VU.Vector Double -> Double
dotProdX4_D a b = V.SIMD.fold' @X4 (+) 0 (V.SIMD.zipWith @X4 (*) (*) a b)

dotProdX8_D :: VU.Vector Double -> VU.Vector Double -> Double
dotProdX8_D a b = V.SIMD.fold' @X8 (+) 0 (V.SIMD.zipWith @X8 (*) (*) a b)

dotProdX16_D :: VU.Vector Double -> VU.Vector Double -> Double
dotProdX16_D a b = V.SIMD.fold' @X16 (+) 0 (V.SIMD.zipWith @X16 (*) (*) a b)

dotProdX32_D :: VU.Vector Double -> VU.Vector Double -> Double
dotProdX32_D a b = V.SIMD.fold' @X32 (+) 0 (V.SIMD.zipWith @X32 (*) (*) a b)

main :: IO ()
main = defaultMain
  [ bgroup "dotProd (Float)"
    [ bench "baseline" $ nf (uncurry dotProdVU) (vecA, vecB)
    , bench "X4" $ nf (uncurry dotProdX4) (vecA, vecB)
    , bench "X8" $ nf (uncurry dotProdX8) (vecA, vecB)
    , bench "X16" $ nf (uncurry dotProdX16) (vecA, vecB)
    , bench "X32" $ nf (uncurry dotProdX32) (vecA, vecB)
    ]
  , bgroup "dotProd (Double)"
    [ bench "baseline" $ nf (uncurry dotProdVU_D) (vecA_D, vecB_D)
    , bench "X4" $ nf (uncurry dotProdX4_D) (vecA_D, vecB_D)
    , bench "X8" $ nf (uncurry dotProdX8_D) (vecA_D, vecB_D)
    , bench "X16" $ nf (uncurry dotProdX16_D) (vecA_D, vecB_D)
    , bench "X32" $ nf (uncurry dotProdX32_D) (vecA_D, vecB_D)
    ]
  ]
  where
    vecA, vecB :: VU.Vector Float
    vecA = VU.fromList [0..10000]
    vecB = VU.fromList [10000,9999..0]
    vecA_D, vecB_D :: VU.Vector Double
    vecA_D = VU.fromList [0..10000]
    vecB_D = VU.fromList [10000,9999..0]
