import Test.Hspec
import TypeLevel.ExpressionsSpec qualified
import TypeLevel.CanonicalVectorsSpec qualified

main :: IO ()
main = hspec $ describe "main" $ do 
  TypeLevel.ExpressionsSpec.spec
  TypeLevel.CanonicalVectorsSpec.spec
