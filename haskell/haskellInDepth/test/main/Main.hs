import qualified Chapter12_GenericsSQL
import qualified Chapter2_1
import qualified Chapter5_2_2
import Test.Tasty
import Test.Tasty.Hspec
import Utils

main :: IO ()
main = do
  runTest $ do
    Chapter2_1.test
  -- Chapter5_2_2.testShuntingYard
  testSpec "main" allSpecs >>= defaultMain
  where
    allSpecs = describe "main" $ do
      Chapter5_2_2.specs
      Chapter12_GenericsSQL.specs
