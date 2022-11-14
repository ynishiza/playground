{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module  Chapter2Test (
  test
  ) where

-- import Debug.Trace(trace)
import GHC.Generics
import System.Random
import System.Random.Stateful 
import Control.Monad
import Data.Foldable
import Fmt
import Radar
import Utils

test :: IO ()
test = do
  dirPairs <- (getRandomDirectionsSequence @Direction) 1000 2
  let 
    toPair [d1,d2] = (d1,d2) 
    toPair _ = undefined
   in testRotateOrientDuality $ toPair <$> dirPairs

  dirSequences <- (getRandomDirectionsSequence @Direction) 100 10
  testRotateOrientDualityMany dirSequences

  v::Int <- getStdRandom uniform

  putStrLn ""

deriving instance Generic Direction

instance UniformRange Direction where
  uniformRM (low, high) g = do
    v <- uniformRM (fromEnum low::Int, fromEnum high) g
    let 
      y = mod v (getEnumLength North)
    pure $ toEnum y

instance Uniform Direction where
  uniformM = uniformRM (minBound, maxBound) 

getRandomDirections :: Uniform a => Int -> IO [a]
getRandomDirections n = replicateM n (getStdRandom uniform)

getRandomDirectionsSequence :: Uniform a => Int -> Int -> IO [[a]]
getRandomDirectionsSequence n k = replicateM n (getRandomDirections k)

testRotateOrientDualityMany :: [[Direction]] -> IO()
testRotateOrientDualityMany dirSeqs = do
  printBanner "testRotateOrientDualityMany start"
  traverse_ testOne dirSeqs
  fmtLn $ "Number of pairs="+|length dirSeqs|+"\n"
    +|"sample:"+|| take 10 dirSeqs ||+"..."
  printBanner "testRotateOrientDualityMany done"
    where 
      testOne :: [Direction] -> IO()
      testOne dirSeq = do
        let 
          rots = orientMany dirSeq
          dirSeq2 = rotateManyInSteps (head dirSeq) rots
        assertIsEqualSilent dirSeq dirSeq2
        return ()

testRotateOrientDuality :: [(Direction, Direction)] -> IO ()
testRotateOrientDuality dirPairs = do
  printBanner "testRotateOrientDuality start"
  traverse_ testOne dirPairs
  fmtLn $ "Number of pairs="+|length dirPairs|+"\n"
    +|"sample:"+|| take 10 dirPairs ||+"..."
  printBanner "testRotateOrientDuality done"
    where 
      testOne :: (Direction, Direction) -> IO()
      testOne (d1, d2) = do
        let 
          rot = orient d1 d2
          dirRes = rotate rot d1
        assertIsEqualSilent d2 dirRes

