{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import App
import Control.Exception
-- import qualified Control.Monad.Catch as MC
import Data.Aeson
import Data.Maybe
import Fmt
import Options.Applicative
import ProcessRequest
import STExcept
import Types

main :: IO ()
main = do
  p@Params {..} <- execParser parserInfo
  config <-
    decodeFileStrict @WebAPIAuth configFilePath
      >>= maybe (throwIO (ConfigError $ "Failed to parse config at " +| configFilePath |+ "")) return
  processMain p config
    `Control.Exception.catch` ( \(e :: SomeException) -> do
                                  putStrLn $ "config:" +| config |+ ""
                                  throwIO e
                              )

processMain :: Params -> WebAPIAuth -> IO ()
processMain Params {..} config = do
  case () of
    ()
      | isInteractive && isJust inputFilePath -> throwIO $ ParamError "Cannot specify interactive and file together."
      | not isInteractive && isNothing inputFilePath -> throwIO $ ParamError "At least one of interactive or file must be specified."
      | isInteractive -> runApp ProcessRequest.processInteractive logLevel config
      | Just f <- inputFilePath -> runApp (ProcessRequest.processFile f) logLevel config
      | otherwise -> throwIO $ ParamError "Unknown"
  return ()

data Params = Params
  { inputFilePath :: !(Maybe String),
    isInteractive :: !Bool,
    configFilePath :: !String,
    logLevel :: !LogLevel
  }

parserInfo :: ParserInfo Params
parserInfo = info (helper <*> parser) (fullDesc <> progDesc "Reports sunrise/sunset times for the specified location")

parser :: Parser Params
parser =
  Params
    <$> optional (strOption (long "file" <> short 'f' <> metavar "FILENAME" <> help "Input file"))
    <*> switch (long "interactive" <> short 'i' <> help "Interactive mode")
    <*> strOption (long "config" <> short 'c' <> metavar "CONFIG" <> value "./src/data/suntimes/config.json" <> help "Configuration file")
    <*> (toDebug <$> switch (long "verbose" <> short 'v' <> help "Debug mode"))
  where
    toDebug True = LevelDebug
    toDebug _ = LevelInfo
