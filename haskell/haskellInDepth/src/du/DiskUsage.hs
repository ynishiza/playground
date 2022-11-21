{-# LANGUAGE FlexibleContexts #-}

module DiskUsage (diskUsage) where

import AppTypes
import AppWRST
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import DuUtils
import System.Posix.Types
import System.PosixCompat.Files

diskUsage :: DuApp (FilePath, FileOffset) FileOffset ()
diskUsage = do
  AppEnv {..} <- ask
  case () of
    ()
      | isDirectory status -> do
        before <- get
        traverseContentWith (const (return True)) diskUsage
        after <- get
        when (depth <= maxDepth config) $ tell [(path, after - before)]
      | isIncludedFile config True path -> modify (+ fileSize status)
      | otherwise -> pure ()

