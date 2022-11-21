{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import AppTypes
import AppWRST
import Control.Arrow
import Data.List (sortOn)
import DirTree
import DiskUsage
import FileCount
import Fmt
import System.FilePath.Posix
import Utils

main :: IO ()
main = do
  testTestDir

testTestDir :: IO ()
testTestDir = do
  def <- defaultAppConfig
  env <-
    initAppEnv $
      def
        { basePath = basePath def </> joinPath ["test", "du", "data"],
          maxDepth = 3,
          extension = Just ".txt",
          followSymLinks = True
        }

  (_, usage, _) <- runAll diskUsage env 0
  (_, files, _) <- runAll fileCount env 0
  (_, tree, _) <- runAll dirTree env 0

  assertIsEqual
    (normalizeOutput (basePath def) tree)
    [ ("test/du/data", 0),
      ("test/du/data/a", 1),
      ("test/du/data/a/b", 2),
      ("test/du/data/a/bb", 2),
      ("test/du/data/a/bc", 2),
      ("test/du/data/a/b/bb", 3),
      ("test/du/data/a/b/c", 3),
      ("test/du/data/a/bb/c", 3),
      ("test/du/data/a/bc/c", 3)
    ]
  assertIsEqual
    (normalizeOutput (basePath def) files)
    [ ("test/du/data", 0),
      ("test/du/data/a/b/c", 0),
      ("test/du/data/a/bc", 0),
      ("test/du/data/a", 1),
      ("test/du/data/a/bb/c", 1),
      ("test/du/data/a/bc/c", 1),
      ("test/du/data/a/b", 2),
      ("test/du/data/a/b/bb", 2),
      ("test/du/data/a/bb", 2)
    ]
  assertIsEqual
    (normalizeOutput (basePath def) usage)
    [ ("test/du/data/a/bb/c", 101),
      ("test/du/data/a/bc/c", 101),
      ("test/du/data/a/bc", 101),
      ("test/du/data/a/b/bb", 303),
      ("test/du/data/a/b/c", 303),
      ("test/du/data/a/bb", 303),
      ("test/du/data/a/b", 808),
      ("test/du/data/a", 1313),
      ("test/du/data", 1313)
    ]

  fmtLn "done"
  where
    normalizeOutput base info = sortOn snd $ first (makeRelative base) <$> info
