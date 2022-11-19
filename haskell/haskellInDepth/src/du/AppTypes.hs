{-# OPTIONS_GHC -Wno-orphans #-}
module AppTypes
  ( AppConfig (..),
    AppEnv (..),
    initAppEnv,
    getFileStatusFromConfig,
    getContentEnvs,
    defaultAppConfig,
  )
where

import System.Directory.Extra
import System.FilePath.Posix
import System.PosixCompat.Files
import Fmt

data AppConfig = AppConfig
  { basePath :: !FilePath,
    maxDepth :: !Int,
    extension :: !(Maybe String),
    followSymLinks :: !Bool
  } deriving (Show)

data AppEnv = AppEnv
  { config :: !AppConfig,
    path :: !FilePath,
    depth :: !Int,
    status :: !FileStatus
  } deriving (Show)

defaultAppConfig :: IO AppConfig
defaultAppConfig = do
  dir <- getCurrentDirectory
  return $ AppConfig dir 1 Nothing True

instance Buildable AppConfig where
  build = build . show 

instance Show FileStatus where
  show status = show $ isDirectory status

instance Buildable AppEnv where
  build = build . show 

getFileStatusFromConfig :: AppConfig -> FilePath -> IO FileStatus
getFileStatusFromConfig config = if followSymLinks config then getFileStatus else getSymbolicLinkStatus

initAppEnv :: AppConfig -> IO AppEnv
initAppEnv c@AppConfig {..} = AppEnv c basePath 0 <$> getFileStatusFromConfig c basePath

getContentEnvs :: AppEnv -> IO [AppEnv]
getContentEnvs env@AppEnv {..} = do
  listContents path >>= traverse createEnv
  where
    createEnv name = getFileStatusFromConfig config p >>= \s -> return $ env { path = p, status = s, depth = depth + 1 }
      where p = path </> name
