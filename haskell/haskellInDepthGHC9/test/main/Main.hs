import Chapter14.StreamPlaySpec qualified
import Chapter14.CsvSpec qualified
import Chapter12_GenericsSQL_Spec qualified
import Chapter12_Template_Spec qualified
import Chapter2_1 qualified
import Chapter5_2_2_Spec qualified
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Utils

main :: IO ()
main = do
  runTest $ do
    Chapter2_1.test
  testSpec "main" allSpecs >>= defaultMain
  where
    allSpecs = describe "main" $ do
      Chapter5_2_2_Spec.specs
      Chapter12_GenericsSQL_Spec.specs
      Chapter12_Template_Spec.specs
      Chapter14.StreamPlaySpec.spec
      Chapter14.CsvSpec.spec
