import Test.Tasty
import Test.Tasty.Hspec
import qualified Chapter12_Template

main :: IO ()
main = do
  testSpec "main" allSpecs >>= defaultMain
  where
    allSpecs = describe "main" $ do
      Chapter12_Template.specs
