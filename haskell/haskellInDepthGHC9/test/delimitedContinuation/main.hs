import Test.Hspec
import qualified Free.Spec
import qualified Spec

main :: IO ()
main = hspec $ describe "" $ do 
  Free.Spec.spec
  Spec.spec
