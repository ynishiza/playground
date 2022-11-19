module FileCount (fileCount) where

import AppTypes
import AppWRST
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Control.Monad.Writer.Class
import DuUtils
import System.Directory.Extra
import System.FilePath.Posix
import System.PosixCompat.Files

type FileCountEntry = (FilePath, Int)

fileCount :: DuApp FileCountEntry s ()
fileCount = do
  AppEnv {..} <- ask
  when (isDirectory status && depth <= maxDepth config) $ do
    l <- liftIO $ listContents path
    tell [(path, length $ filter (predicate config) l)]
    traverseDirectoriesWith fileCount
  where
    predicate config p = maybe True (`isExtensionOf` p) (extension config)
