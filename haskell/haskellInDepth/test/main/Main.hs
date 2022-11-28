import qualified Chapter2_1
import qualified Chapter5_2_2
import Utils
import Test.Tasty
import Test.Tasty.Hspec

main :: IO ()
main = do
  runTest $ do
    Chapter2_1.test
    -- Chapter5_2_2.testShuntingYard
  testSpec "main" Chapter5_2_2.specs >>= defaultMain 

