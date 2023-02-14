import Control.Exception
import Control.Monad
import Hedgehog
import SimpleCStreamProperties qualified
import SimpleCStreamSpec qualified
import SimpleStreamSpec qualified
import Test.Hspec

main :: IO ()
main = do
  hspec $ describe "main" $ do
    SimpleStreamSpec.spec
    SimpleCStreamSpec.spec
    SimpleCStreamSpec.preludeSpec

  res <- checkSequential SimpleCStreamProperties.properties
  unless res $ throw $ userError "Failed property test"
