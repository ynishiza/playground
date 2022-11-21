{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import Control.Arrow
import AppTypes
import AppWRST
import DirTree
import DiskUsage
import DuUtils
import FileCount
import Fmt
import TextShow
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory.Extra
import System.FilePath

main :: IO ()
main = do
  cwd <- getCurrentDirectory
  conf <- normalizeAppConfigWithBase cwd <$> execParser (info (helper <*> parser) parserInfo) 
  env <- initAppEnv conf
  print env

  (_, usage, _) <- runAll diskUsage env 0
  (_, files, _) <- runAll fileCount env 0
  (_, tree, _) <- runAll dirTree env 0

  let
    normaliseData = (first (normalizeEntryPath cwd) <$>)
  fmtLn $ normaliseData usage ||+ "\n"
  fmtLn $ normaliseData files ||+ "\n"

  let 
    report :: T.Text
    report = buildEntries "Directory:" treeEntryBuilder (normaliseData tree) |+ "\n" 
      +|buildEntries "Files:" tabEntryBuilder (normaliseData files)|+"\n"
      +|buildEntries "Usage:" tabEntryBuilder (normaliseData usage)|+"\n"

  TIO.putStrLn report
  return ()

parserInfo :: InfoMod a
parserInfo = fullDesc <> progDesc "Directory usage info"

parser :: Parser AppConfig
parser =
  AppConfig
    <$> strArgument (metavar "DIRECTORY" <> help "Base path")
    <*> option auto (long "depth" <> short 'd' <> value 0 <> metavar "DEPTH" <> help "Display an entry for all directories DEPTH directories deep")
    <*> optional (strOption (long "extension" <> short 'e' <> metavar "EXT" <> help "Filter files by extension"))
    <*> flag False True (short 'L' <> help "Follow symlinks (Off by default)")

buildEntries :: Builder -> (a -> Builder) -> [a] -> Builder
buildEntries title builder entries = unlinesF $ title : (builder <$> entries)

treeEntryBuilder :: Integral a => (FilePath, a) -> Builder
treeEntryBuilder (fpath, depth) = indent|+""+|fromString (takeBaseName fpath)|+"/"
  where
    indent = replicate (fromIntegral $ 2 * depth) ' '

tabEntryBuilder :: TextShow a => (FilePath, a) -> Builder
tabEntryBuilder (fpath, x) = showb x|+"\t\t"+|fpath|+"/"
