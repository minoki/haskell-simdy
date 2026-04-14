{-# LANGUAGE MonoLocalBinds #-}
import           Data.Proxy
import           Data.Simdy
import           Data.Simdy.FMA
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
            | otherwise = [n `quot` 2, n - 1]

properties :: TestTree
properties = testGroup "(checked by QuickCheck)" $
  [ QC.testProperty "matMulNaive == matMulTranspose X4 Float" $
      QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \l -> case someNatVal l of
        SomeNat pl ->
          QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \m -> case someNatVal m of
            SomeNat pm ->
              QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \n -> case someNatVal n of
                SomeNat pn ->
                  QC.forAll (genMatrix pl pm) $ \a ->
                    QC.forAll (genMatrix pm pn) $ \b ->
                      matMulNaive a b QC.=== (matMulTranspose a b `elemTypeProxy` Proxy @Float)
  , QC.testProperty "matMulNaive == matMulBlock X4 Float" $
      QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \l -> case someNatVal l of
        SomeNat pl ->
          QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \m -> case someNatVal m of
            SomeNat pm ->
              QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \n -> case someNatVal n of
                SomeNat pn ->
                  QC.forAll (genMatrix pl pm) $ \a ->
                    QC.forAll (genMatrix pm pn) $ \b ->
                      matMulNaive a b QC.=== (matMulBlock 8 8 8 a b `elemTypeProxy` Proxy @Float)
  , QC.testProperty "matMulNaive == matMulSIMD X4 Float" $
      QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \l -> case someNatVal l of
        SomeNat pl ->
          QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \m -> case someNatVal m of
            SomeNat pm ->
              QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \n -> case someNatVal n of
                SomeNat pn ->
                  QC.forAll (genMatrix pl pm) $ \a ->
                    QC.forAll (genMatrix pm pn) $ \b ->
                      matMulNaive a b QC.=== (matMulSIMD (Proxy @X4) a b `elemTypeProxy` Proxy @Float)
  , QC.testProperty "matMulNaive == matMulSIMD_4_3 X4 Float" $
      QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \l -> case someNatVal l of
        SomeNat pl ->
          QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \m -> case someNatVal m of
            SomeNat pm ->
              QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \n -> case someNatVal n of
                SomeNat pn ->
                  QC.forAll (genMatrix pl pm) $ \a ->
                    QC.forAll (genMatrix pm pn) $ \b ->
                      matMulNaive a b QC.=== (matMulSIMD_4_3 (Proxy @X4) a b `elemTypeProxy` Proxy @Float)
  , QC.testProperty "matMulNaive == matMulBlockSIMD X4 Float" $
      QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \l -> case someNatVal l of
        SomeNat pl ->
          QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \m -> case someNatVal m of
            SomeNat pm ->
              QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \n -> case someNatVal n of
                SomeNat pn ->
                  QC.forAll (genMatrix pl pm) $ \a ->
                    QC.forAll (genMatrix pm pn) $ \b ->
                      matMulNaive a b QC.=== (matMulBlockSIMD (Proxy @X4) 32 32 32 a b `elemTypeProxy` Proxy @Float)
  , QC.testProperty "matMulNaive == matMulBlockSIMDX8 Float" $
      QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \l -> case someNatVal l of
        SomeNat pl ->
          QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \m -> case someNatVal m of
            SomeNat pm ->
              QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \n -> case someNatVal n of
                SomeNat pn ->
                  QC.forAll (genMatrix pl pm) $ \a ->
                    QC.forAll (genMatrix pm pn) $ \b ->
                      matMulNaive a b QC.=== (matMulBlockSIMDX8 32 32 32 a b `elemTypeProxy` Proxy @Float)
  ] ++ case isFMAAvailable of
    Just MkFMAWitness ->
      [ QC.testProperty "matMulNaive == matMulFMA X4 Float" $
          QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \l -> case someNatVal l of
            SomeNat pl ->
              QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \m -> case someNatVal m of
                SomeNat pm ->
                  QC.forAllShrink (chooseEnum (1, 50)) shrinkNat $ \n -> case someNatVal n of
                    SomeNat pn ->
                      QC.forAll (genMatrix pl pm) $ \a ->
                        QC.forAll (genMatrix pm pn) $ \b ->
                          matMulNaive a b QC.=== (matMulFMA (Proxy @X4) a b `elemTypeProxy` Proxy @Float)
      ]
    Nothing -> []
