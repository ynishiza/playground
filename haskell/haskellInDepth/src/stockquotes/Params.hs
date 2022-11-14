{-# LANGUAGE OverloadedStrings #-}

module Params
  ( parseParams,
    Params (filename, company, includeChart, htmlFilename, silent, Params),
  )
where

import qualified Data.Text as T
import Options.Applicative

data Params = Params
  { filename :: !FilePath,
    company :: !(Maybe T.Text),
    includeChart :: !Bool,
    htmlFilename :: !(Maybe FilePath),
    silent :: !Bool
  }
  deriving (Show, Eq)

parser :: Parser Params
parser =
  Params
    <$> strArgument (metavar "FILE" <> help "CSV file")
    <*> optional (option str (metavar "NAME" <> long "company" <> help "Company name"))
    <*> switch (short 'c' <> long "chart" <> help "Include chart")
    <*> optional (option str (metavar "FILE" <> short 'H' <> long "html" <> help "Include html"))
    <*> switch (short 's' <> long "silent" <> help "")

parseParams :: IO Params
parseParams = execParser parserInfo
  where
    parserInfo =
      info
        (parser <**> helper)
        (fullDesc <> progDesc "")
