import ApiSpec qualified
import ServantSpec qualified
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = testSpec "RESTAPI" allSpecs >>= defaultMain
  where
    allSpecs = describe "" $ do
      ApiSpec.spec
      ServantSpec.spec
