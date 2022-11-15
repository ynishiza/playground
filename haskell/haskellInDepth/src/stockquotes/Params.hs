{-# LANGUAGE OverloadedStrings #-}

module Params
  ( parseParams,
    Params (dataFilepath, company, includeChart, htmlFilepath, silent, Params),
  )
where

import qualified Data.Text as T
import Options.Applicative

data Params = Params
  { dataFilepath :: !FilePath,
    company :: !(Maybe T.Text),
    includeChart :: !Bool,
    htmlFilepath :: !(Maybe FilePath),
    silent :: !Bool
  }
  deriving (Show, Eq)

parser :: Parser Params
parser =
  Params
    <$> strArgument (metavar "FILE" <> help "CSV file")
    <*> optional (option str (metavar "NAME" <> long "name" <> short 'n' <> help "Company name"))
    <*> switch (short 'c' <> long "chart" <> help "Include chart")
    <*> optional (option str (metavar "FILE" <> short 'H' <> long "html" <> help "Include html"))
    <*> switch (short 's' <> long "silent" <> help "")

parseParams :: IO Params
parseParams = execParser parserInfo
  where
    parserInfo =
      info
        (parser <**> helper)
        (fullDesc <> progDesc "Compute stock quote stats")
