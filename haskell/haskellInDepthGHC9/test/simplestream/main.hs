import SimpleStreamSpec qualified
import Control.Exception
import Control.Monad
import SimpleCStreamSpec qualified
import SimpleCStreamProperties qualified
import Test.Hspec
import Hedgehog

main :: IO ()
main = do
  hspec $ describe "main" $ do 
    SimpleStreamSpec.spec
    SimpleCStreamSpec.spec
    SimpleCStreamSpec.preludeSpec

  res <- checkSequential SimpleCStreamProperties.properties
  unless res $ throw $ userError "Failed property test"
