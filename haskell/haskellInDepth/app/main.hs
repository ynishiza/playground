{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt
import qualified Chapter1
import qualified Chapter2

main :: IO ()
main = do
  let 
    chapterList :: [(T.Text, Int)]
    chapterList = zip [
              "Chapter1",
              "Chapter2"
            ] [1..]

  putStrLn $ fmt $ 
    "Choose chapter to run\n"+|
    blockListF' "" (\(name, i) -> i|+":"+|name|+"") chapterList

  res <- TIO.getLine
  case read (T.unpack res) of
    1 -> Chapter1.run
    2 -> Chapter2.run
    _ -> error $ "Unknown chapter"+|res|+""
