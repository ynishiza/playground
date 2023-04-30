module FileCount (fileCount) where

import AppTypes
import AppWRST
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import DuUtils
import System.Directory.Extra
import System.PosixCompat.Files

fileCount :: DuApp (FilePath, Int) s ()
fileCount = do
  AppEnv {..} <- ask
  when (isDirectory status && depth <= maxDepth config) $ do
    l <- liftIO $ listContents path
    tell [(path, length $ filter (isIncludedFile config True) l)]
    traverseDirectoriesWith fileCount
