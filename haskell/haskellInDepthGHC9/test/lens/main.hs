import Spec qualified
import SpecTemplate qualified
import Test.Hspec

main :: IO ()
main = hspec $ describe "main" $ Spec.spec >> SpecTemplate.spec
