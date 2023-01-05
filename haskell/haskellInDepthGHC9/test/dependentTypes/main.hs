import DoorProperties qualified
import DoorSpec qualified
import ElevatorSpec qualified
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.Hspec

main :: IO ()
main =
  sequence
    [ testSpec "dependent types" allSpecs,
      pure (fromGroup DoorProperties.group)
    ]
    >>= defaultMain . testGroup "all"
  where
    allSpecs :: SpecWith ()
    allSpecs =
      describe "hspec" $ do
        DoorSpec.spec
        ElevatorSpec.spec
