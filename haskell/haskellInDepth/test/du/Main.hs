{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import AppTypes
import AppWRST
import Data.List (find, nub, sort)
import Fmt
import System.FilePath.Posix
import System.PosixCompat.Files
import DirTree
import Utils

main :: IO ()
main = do
  testDefaultConfig

  testDirtree
  pure ()

getDefaultEnv :: IO AppEnv
getDefaultEnv = defaultAppConfig >>= initAppEnv

testDefaultConfig :: IO ()
testDefaultConfig = do
  defEnv <- getDefaultEnv
  let AppConfig {..} = config defEnv
  assertIsEqual basePath $ path defEnv
  assertIsEqual (takeFileName $ path defEnv) "haskellInDepth"
  assertIsEqual (isDirectory $ status defEnv) True

  envs <- getContentEnvs defEnv
  fmtLn $ listF envs
  testEnvByName (filterEnvByName "src") envs $ \e -> do
    assertIsEqual True $ isDirectory $ status e
    print e
  testEnvByName (filterEnvByName "test") envs $ \e -> do
    assertIsEqual True $ isDirectory $ status e
    print e
  testEnvByName (filterEnvByName "package.yaml") envs $ \e -> do
    print e
    assertIsEqual False $ isDirectory $ status e

  return ()
    where 
      filterEnvByName name env = takeFileName (path env) == name
      testEnvByName :: (AppEnv -> Bool) -> [AppEnv] -> (AppEnv -> IO ()) -> IO ()
      testEnvByName f envs test = do
            let x = find f envs
            maybe (error "") test x


testDirtree :: IO ()
testDirtree = do
  testDirtreeDepth 0
  testDirtreeDepth 1
  testDirtreeDepth 2
  return ()

testDirtreeDepth :: Int -> IO ()
testDirtreeDepth d = do
  config@AppConfig {..} <- defaultAppConfig
  env <- initAppEnv $ config { maxDepth = d }
  result <- runAll dirTree env () >>= traverse (testEach basePath) . v2

  print result
  assertIsEqual (sort $ nub $ v3 <$> result) [0..d]
  return ()
    where
      v2 (_,x,_) = x
      v3 (_,_,x) = x
      testEach basePath (p, depth) = do
        let 
          relative = makeRelative basePath p
          split = filter (/=".") $ splitDirectories relative
        assertIsEqualSilent depth $ length split
        return (p, relative, depth)

