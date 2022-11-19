module DuUtils
  ( 
    getCurrentContentEnvs,
    traverseContentWith,
    traverseDirectoriesWith,
  )
where

import AppTypes
import AppWRST
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Foldable
import System.PosixCompat.Files

getCurrentContentEnvs :: DuApp l s [AppEnv]
getCurrentContentEnvs = ask >>= liftIO . getContentEnvs

traverseContentWith :: DuApp l s () -> (AppEnv -> IO Bool) -> DuApp l s ()
traverseContentWith app f = getCurrentContentEnvs >>= liftIO . filterM f >>= traverse_ (flip local app . const)

traverseDirectoriesWith :: DuApp l s () -> DuApp l s ()
traverseDirectoriesWith = flip traverseContentWith (return . isDirectory . status)
