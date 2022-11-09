{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter2
  ( rotate,
    orient,
    Direction (..),
    Turn (..),
    run,
  )
where

import Radar
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Fmt

run :: IO ()
run = do
  testTurn
  print $ orientMany [North, North, South, West, South]
  print $ rotateManyInSteps North [TLeft, TLeft, TRight, TAround, TNone]
  print $ rotateMany North [TLeft, TLeft, TRight, TAround, TNone]
  pure ()

testTurn :: IO ()
testTurn = do
  _ <-
    let allPairs = (,) <$> allTurns <*> allTurns
        test (turn1, turn2) = do
          putStrLn $ fmt $ turn1 ||+ " <> " +|| turn2 ||+ " = " +|| turn1 <> turn2 ||+ ""
     in traverse test allPairs

  _ <-
    let allPairs = (,) <$> allDirections <*> allDirections
        test (dir1, dir2) = do
          putStrLn $ fmt $ "orient " +| dir1 ||+ " " +|| dir2 ||+ " = " +|| turn ||+ ""
          putStrLn $ fmt $ "rotate " +| turn ||+ " " +|| dir1 ||+ " = " +|| d3 ||+ ""
          putStrLn $ fmt $ "inverse? " +| d3 == dir2 |+ ""
          unless (d3 == dir2) $ error $ fmt $ "not inverse" +|| d3 ||+"=="+||dir2||+ ""
            where
              turn = orient dir1 dir2
              d3 = rotate turn dir1
     in traverse test allPairs

  return ()
