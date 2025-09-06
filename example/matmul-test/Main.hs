import           Data.Proxy
import           Data.Simdy
import qualified Data.Vector.Unboxed as VU
import           GHC.TypeNats
import           MatMul
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

genMatrix :: (KnownNat m, KnownNat n, VU.Unbox a, Num a) => Proxy m -> Proxy n -> Gen (Mat m n a)
genMatrix pm pn = MkMat <$> VU.replicateM (fromIntegral (natVal pm) * fromIntegral (natVal pn)) (fromIntegral <$> chooseInt (-100, 100))

elemTypeProxy :: f a -> Proxy a -> f a
elemTypeProxy x _ = x

shrinkNat :: Natural -> [Natural]
shrinkNat n | n <= 1 = []
            | otherwise = [n - 1]

properties = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "matMulNaive == matMulSIMD X4 Float" $
      QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \l -> case someNatVal l of
        SomeNat pl ->
          QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \m -> case someNatVal m of
            SomeNat pm ->
              QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \n -> case someNatVal n of
                SomeNat pn ->
                  QC.forAll (genMatrix pl pm) $ \a ->
                    QC.forAll (genMatrix pm pn) $ \b ->
                      matMulNaive a b QC.=== (matMulSIMD (Proxy @X4) a b `elemTypeProxy` Proxy @Float)
  ]
