import Test.Hspec
import ExpressionsSpec qualified
import CanonicalVectorsSpec qualified

main :: IO ()
main = hspec $ describe "main" $ do 
  ExpressionsSpec.spec
  CanonicalVectorsSpec.spec
