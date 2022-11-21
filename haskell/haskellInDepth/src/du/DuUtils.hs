module DuUtils
  ( getCurrentContentEnvs,
    traverseContentWith,
    traverseDirectoriesWith,
    normalizeEntryPath,
  )
where

import AppTypes
import AppWRST
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Foldable
import System.FilePath
import System.PosixCompat.Files

getCurrentContentEnvs :: DuApp l s [AppEnv]
getCurrentContentEnvs =
  ask
    >>= liftIO . getContentEnvs

traverseContentWith :: (AppEnv -> IO Bool) -> DuApp l s () -> DuApp l s ()
traverseContentWith f app =
  getCurrentContentEnvs
    >>= liftIO . filterM f
    >>= traverse_ (flip local app . const)

traverseDirectoriesWith :: DuApp l s () -> DuApp l s ()
traverseDirectoriesWith = traverseContentWith (return . isDirectory . status)

normalizeEntryPath :: FilePath -> FilePath -> FilePath
normalizeEntryPath = makeRelative
