import DoorSpec qualified
import DoorProperties qualified
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Hedgehog

main :: IO ()
main = sequence [
    testSpec "dependent types" allSpecs,
    pure (fromGroup DoorProperties.group)
                ] >>= defaultMain . testGroup "all"
  where
    allSpecs =
      describe "" $
        DoorSpec.spec
