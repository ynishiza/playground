import qualified Chapter2_1
import qualified Chapter5_2_2
import Radar
import Utils

main :: IO ()
main = do
  runTest $ do
    Chapter2_1.test
    Chapter5_2_2.testShuntingYard
