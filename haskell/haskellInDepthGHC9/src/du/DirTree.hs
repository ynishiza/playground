{-# LANGUAGE OverloadedStrings #-}

module DirTree
  ( dirTree,
  )
where

import AppTypes
import AppWRST
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import DuUtils
import System.PosixCompat.Files

dirTree :: DuApp (FilePath, Int) s ()
dirTree = do
  AppEnv {..} <- ask
  when (isDirectory status && depth <= maxDepth config) $
    tell [(path, depth)] >> traverseDirectoriesWith dirTree
