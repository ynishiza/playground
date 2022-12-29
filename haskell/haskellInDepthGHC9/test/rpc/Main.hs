import qualified Spec
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = testSpec "main" Spec.specs >>= defaultMain
