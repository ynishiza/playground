import Properties qualified
import Spec qualified
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

main :: IO ()
main = do
  specs <- testSpecs Spec.spec
  defaultMain $
    testGroup "read" $
      specs
        <> [ fromGroup Properties.properties,
             fromGroup Properties.readPEquivalence
           ]
