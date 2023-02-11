import SimpleStreamSpec qualified
import SimpleCStreamSpec qualified
import Test.Hspec

main :: IO ()
main = hspec $ describe "main" $ do 
  SimpleStreamSpec.spec
  SimpleCStreamSpec.spec
  SimpleCStreamSpec.preludeSpec
