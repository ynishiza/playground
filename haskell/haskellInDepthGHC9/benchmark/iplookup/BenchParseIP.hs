{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BenchParseIP
  ( benchParseIP,
    benchFileRead,
    benchParseIPRangeDBMany,
    benchParseIPRangeDBManyBad,
    benchGroup,
  )
where

import Control.DeepSeq
import Criterion.Main
import qualified Data
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fmt
import IPParse
import System.PosixCompat.Files
import Utils

benchGroup :: IO Benchmark
benchGroup = do
  m <-
    sequence
      [ benchFileRead,
        benchParseIPRangeDBMany,
        benchParseIPRangeDBManyBad
      ]
  return $
    bgroup
      "BenchParseIP"
      $ benchParseIP : m

benchParseIP :: Benchmark
benchParseIP =
  bench "parseIP" (whnf parseIP "192.168.0.1")

type FileInfo = (T.Text, FilePath, FileStatus)

instance NFData FileStatus where
  rnf f = seq f ()

fileInfo :: IO [FileInfo]
fileInfo = traverse f Data.iPRangeDBFiles
  where
    f (l, p) = (l,p,) <$> getFileStatus p

getFileInfoLabel :: FileInfo -> String
getFileInfoLabel (label, p, s) = label |+ " file=" +| p |+ " size=" +|| fileSize s ||+ "B"

benchFileRead :: IO Benchmark
benchFileRead = do
  info <- fileInfo
  return $ bgroup "fileRead" $ f <$> info
  where
    f ~g@(_, p, _) = bench (getFileInfoLabel g) $ nfIO (readDBFile p)

benchParseIPRangeDBMany :: IO Benchmark
benchParseIPRangeDBMany = do
  info <- fileInfo
  return $ bgroup "parseIPRangeDB" $ f <$> info
  where
    f ~g@(_abel, p, _) = env (readDBFile p) $ bench (getFileInfoLabel g) . nf readRange

benchParseIPRangeDBManyBad :: IO Benchmark
benchParseIPRangeDBManyBad = do
  info <- fileInfo
  return $ bgroup "BAD parseIPRangeDB including file read" $ f <$> info
  where
    f g@(_, p, _) = bench (getFileInfoLabel g) $ nfIO (readRange <$> readDBFile p)

readDBFile :: FilePath -> IO T.Text
readDBFile = T.readFile

readRange :: T.Text -> IPRangeDB
readRange = fromRight shouldNeverHappen . parseIPRangeDB
