import qualified MessagingSpec
import qualified TemplateSpec
import Test.Tasty
import Test.Hspec
import Test.Tasty.Hspec

main :: IO ()
main = testSpec "main" allSpecs >>= defaultMain
  where 
    allSpecs = describe "all" $ do
      MessagingSpec.specs
      TemplateSpec.spec
