import qualified Door
import qualified Problems
main :: IO ()
main = 
  putStrLn "===Door.hs===" >> Door.test >>
  putStrLn "===Problems.hs===" >> Problems.test

