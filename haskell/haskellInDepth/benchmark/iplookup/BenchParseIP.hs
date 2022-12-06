{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module BenchParseIP
  ( benchParseIP,
    benchFileRead,
    benchParseIPRangeDBMany,
    benchParseIPRangeDBManyBad,
  )
where

import Criterion.Main
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fmt
import IPParse
import System.FilePath
import System.PosixCompat.Files
import Utils

benchParseIP :: Benchmark
benchParseIP =
  bench "parseIP" (whnf parseIP "192.168.0.1")

baseDir :: FilePath
baseDir = "./benchmark/data/iplookup"

type FileInfo = (T.Text, FilePath, FileStatus)

fileInfo :: IO [FileInfo]
fileInfo =
  traverse
    f
    [ ("small", "1.iprs"),
      ("medium", "2.iprs"),
      ("large", "3.iprs")
    ]
  where
    f (l, n) =
      let p = baseDir </> n
       in (l,p,) <$> getFileStatus p

getFileInfoLabel :: FileInfo -> String
getFileInfoLabel (label, p, s) = label |+ " file=" +| p |+ " size=" +|| fileSize s ||+ "B"

benchFileRead :: IO Benchmark
benchFileRead = bgroup "fileRead" . (f <$>) <$> fileInfo
  where
    f g@(_, p, _) = bench (getFileInfoLabel g) $ nfIO (readDBFile p)

benchParseIPRangeDBMany :: IO Benchmark
benchParseIPRangeDBMany = bgroup "parseIPRangeDB" . (f <$>) <$> fileInfo
  where
    f g@(_abel, p, _) = env (readDBFile p) $ bench (getFileInfoLabel g) . nf readRange

benchParseIPRangeDBManyBad :: IO Benchmark
benchParseIPRangeDBManyBad = bgroup "BAD parseIPRangeDB including file read" . (f <$>) <$> fileInfo
  where
    f g@(_abel, p, _) = bench (getFileInfoLabel g) $ nfIO $ readRange <$> readDBFile p

readDBFile :: FilePath -> IO T.Text
readDBFile = T.readFile

readRange :: T.Text -> IPRangeDB
readRange = fromRight shouldNeverHappen . parseIPRangeDB
