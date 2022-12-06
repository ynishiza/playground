{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BenchParseIP (benchParseIP, benchParseIPRangeDBMany) where

import Criterion.Main
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fmt
import IPParse
import System.FilePath
import System.PosixCompat.Files

benchParseIP :: Benchmark
benchParseIP =
  bench "parseIP" (whnf parseIP "192.168.0.1")

dataBase :: FilePath
dataBase = "./benchmark/data/iplookup"

fileNames :: [FilePath]
fileNames = ["1.iprs", "2.iprs", "3.iprs"]

filePaths :: [FilePath]
filePaths = (dataBase</>) <$> fileNames

benchParseIPRangeDBMany :: IO Benchmark
benchParseIPRangeDBMany = bgroup "parseIPRangeDB" <$> traverse f filePaths
  where f p = testSingle . (p,) <$> getFileStatus p

readDBFile :: FilePath -> IO T.Text
readDBFile = T.readFile

testSingle :: (FilePath, FileStatus) -> Benchmark
testSingle (p, s) = bench ("file: " +| p |+ " size:" +|| fileSize s ||+"B") $
  nfIO $ do
    Right v <- parseIPRangeDB <$> readDBFile p
    return v
