{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-} 
{-# OPTIONS_GHC -Wno-orphans #-}

module Chapter2_1
  ( test,
  )
where

-- import Debug.Trace(trace)

import qualified Data.List.NonEmpty as L
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Exts
import Control.Monad
import Data.Foldable
import Fmt
import GHC.Generics
import Radar
import System.Random
import System.Random.Stateful
import Utils

test :: TestState
test =
  createChapterTest
    "2"
    "Radar"
    ( do
        dirPairs <- (getRandomDirectionsSequence @Direction) 1000 2
        let toPair [d1, d2] = (d1, d2)
            toPair _ = error "undefined"
         in testRotateOrientDuality $ toPair <$> dirPairs

        dirSequences <- (getRandomDirectionsSequence @Direction) 100 10
        testRotateOrientDualityMany dirSequences

        putStrLn ""
    )

deriving instance Generic Direction

instance UniformRange Direction where
  uniformRM (low, high) g = do
    v <- uniformRM (fromEnum low :: Int, fromEnum high) g
    let y = mod v (getEnumLength North)
    pure $ toEnum y

instance Uniform Direction where
  uniformM = uniformRM (minBound, maxBound)

getRandomDirections :: Uniform a => Int -> IO [a]
getRandomDirections n = replicateM n (getStdRandom uniform)

getRandomDirectionsSequence :: Uniform a => Int -> Int -> IO (NonEmpty [a])
getRandomDirectionsSequence n k = fromList <$> replicateM n (getRandomDirections k)

testRotateOrientDualityMany :: NonEmpty [Direction] -> IO ()
testRotateOrientDualityMany dirSeqs = do
  printBanner "testRotateOrientDualityMany start"
  traverse_ testOne dirSeqs
  fmtLn $
    "Number of pairs=" +| L.length dirSeqs |+ "\n"
      +| "sample:" +|| L.take 10 dirSeqs ||+ "..."
  printBanner "testRotateOrientDualityMany done"
  where
    testOne :: [Direction] -> IO ()
    testOne dirSeq@(h:_) = do
      let rots = orientMany dirSeq
          dirSeq2 = rotateManyInSteps h rots
      assertIsEqualSilent dirSeq dirSeq2
      return ()
    testOne _ = error "empty"

testRotateOrientDuality :: NonEmpty (Direction, Direction) -> IO ()
testRotateOrientDuality dirPairs = do
  printBanner "testRotateOrientDuality start"
  traverse_ testOne dirPairs
  fmtLn $
    "Number of pairs=" +| L.length dirPairs |+ "\n"
      +| "sample:" +|| L.take 10 dirPairs ||+ "..."
  printBanner "testRotateOrientDuality done"
  where
    testOne :: (Direction, Direction) -> IO ()
    testOne (d1, d2) = do
      let rot = orient d1 d2
          dirRes = rotate rot d1
      assertIsEqualSilent d2 dirRes
