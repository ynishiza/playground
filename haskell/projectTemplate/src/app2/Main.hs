{-# LANGUAGE OverloadedStrings #-} 

module Main (main) where

import System.Directory
import qualified Lib2
import qualified B.Lib
import Fmt

main :: IO ()
main = do
  fmtLn "App2"
  Lib2.run
  B.Lib.run
  getCurrentDirectory >>= (fmtLn . ("CWD="+|) . build)
