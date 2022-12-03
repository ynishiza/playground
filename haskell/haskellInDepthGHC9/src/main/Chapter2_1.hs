{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chapter2_1
  ( rotateFromFile,
    orientFromFile,
    run,
  )
where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt
import Fmt.Internal.Core
import Radar
import System.Environment
import Utils

run :: TestState
run =
  createChapterTest
    "2.1"
    "Radar"
    ( do
        args <- getArgs
        case args of
          ["-o", filename] -> orientFromFile filename
          ["-r", filename, dir] -> rotateFromFile (read dir) filename
          ["-t"] -> do
            testTurn
          _ -> error "-o FILE | -r FILE DIR"
        pure ()
    )

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
          unless (d3 == dir2) $ error $ fmt $ "not inverse" +|| d3 ||+ "==" +|| dir2 ||+ ""
          where
            turn = orient dir1 dir2
            d3 = rotate turn dir1
     in traverse test allPairs

  return ()

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir path = do
  text <- TIO.readFile path
  let rotations = (read @Turn) . T.unpack <$> T.lines text
      directions = rotateManyInSteps dir rotations
  fmt $ getRotations rotations directions
  pure ()

orientFromFile :: FilePath -> IO ()
orientFromFile path = do
  text <- TIO.readFile path
  let directions = (read @Direction) . T.unpack <$> T.lines text
      rotations = orientMany directions
  fmt $ getRotations rotations directions
  pure ()

formatList :: Buildable a => Int -> [a] -> Builder
formatList padding = foldr (+|) "" . (padBothF padding ' ' <$>)

getRotations :: FromBuilder b => [Turn] -> [Direction] -> b
getRotations rotations directions =
  " " +| formatList 3 rotations +| "\n"
    +| formatList 3 directions
    +| "\n"
