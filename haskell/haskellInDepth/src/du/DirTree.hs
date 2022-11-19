{-# LANGUAGE OverloadedStrings #-} 
module DirTree (TreeEntry, dirTree, treeEntryBuilder) where

import AppTypes
import AppWRST
import DuUtils
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import System.PosixCompat.Files
import Fmt
import Data.String

type TreeEntry = (FilePath, Int)

dirTree :: DuApp TreeEntry s ()
dirTree = do
  AppEnv {..} <- ask
  when (isDirectory status && depth <= maxDepth config) $ 
    tell [(path, depth)] >> traverseDirectoriesWith dirTree

treeEntryBuilder :: TreeEntry -> Builder
treeEntryBuilder (fpath, depth) = fromString indent <> fromString fpath
  where indent = replicate (2*depth) ' '

