{-# OPTIONS_GHC -Wno-orphans #-}

module AppTypes
  ( AppConfig (..),
    AppEnv (..),
    initAppEnv,
    initAppEnvWith,
    getFileStatusFromConfig,
    getContentEnvs,
    defaultAppConfig,
    isIncludedFile,
    normalizeAppConfigWithBase,
  )
where

import Fmt
import System.Directory.Extra
import System.FilePath
import System.PosixCompat.Files

data AppConfig = AppConfig
  { basePath :: !FilePath,
    maxDepth :: !Int,
    extension :: !(Maybe String),
    followSymLinks :: !Bool
  }
  deriving (Show)

data AppEnv = AppEnv
  { config :: !AppConfig,
    path :: !FilePath,
    depth :: !Int,
    status :: !FileStatus
  }
  deriving (Show)

defaultAppConfig :: IO AppConfig
defaultAppConfig = flip normalizeAppConfigWithBase (AppConfig "." 1 Nothing True) <$> getCurrentDirectory

instance Buildable AppConfig where
  build = build . show

instance Show FileStatus where
  show status = show $ isDirectory status

instance Buildable AppEnv where
  build = build . show

getFileStatusFromConfig :: AppConfig -> FilePath -> IO FileStatus
getFileStatusFromConfig config = if followSymLinks config then getFileStatus else getSymbolicLinkStatus

normalizeAppConfigWithBase :: FilePath -> AppConfig -> AppConfig
normalizeAppConfigWithBase base config@AppConfig {..} =
  if isAbsolute basePath then config else config {basePath = normalise (base </> basePath)}

initAppEnv :: AppConfig -> IO AppEnv
initAppEnv c@AppConfig {..} = initAppEnvWith c basePath

initAppEnvWith :: AppConfig -> FilePath -> IO AppEnv
initAppEnvWith c p = AppEnv c p 0 <$> getFileStatusFromConfig c p

getContentEnvs :: AppEnv -> IO [AppEnv]
getContentEnvs env@AppEnv {..} = listContents path >>= traverse createEnv
  where
    createEnv name = do
      s <- getFileStatusFromConfig config p
      return $ env {path = p, status = s, depth = depth + 1}
      where
        p = path </> name

isIncludedFile :: AppConfig -> Bool -> FilePath -> Bool
isIncludedFile config regFile p = regFile && maybe True (`isExtensionOf` p) (extension config)
