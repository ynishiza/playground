import qualified Properties
import qualified Spec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

main :: IO ()
main = do
  specs <- testSpec "Hspec" Spec.specs
  defaultMain $
    testGroup
      "iplookup"
      [ specs,
        fromGroup Properties.group
      ]
