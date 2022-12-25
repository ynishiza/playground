import Chapter12_GenericsSQL qualified
import Chapter12_Template qualified
import Chapter2_1 qualified
import Chapter5_2_2 qualified
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
      Chapter5_2_2.specs
      Chapter12_GenericsSQL.specs
      Chapter12_Template.specs
