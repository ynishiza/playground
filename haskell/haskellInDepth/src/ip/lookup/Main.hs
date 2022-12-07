{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Foldable
import Data.List.Extra
import qualified Data.Text as T
import Fmt
import IPLookup
import IPParse
import Options.Applicative

main :: IO ()
main = do
  Params {..} <- execParser parserInfo
  (Right db) <- parseIPRangeDB . T.pack <$> readFile dbPath
  case ips of
    Right x -> run db x
    Left e -> error $ e ||+ ""

run :: IPRangeDB -> [IP] -> IO ()
run db = traverse_ f
  where
    f ip = fmtLn $ ip ||+ ":" +| lookupIP db ip |+ ""

data Params = Params
  { dbPath :: !FilePath,
    ips :: !(Either String [IP])
  }
  deriving (Show)

parserInfo :: ParserInfo Params
parserInfo = info (helper <*> parser) (fullDesc <> progDesc "")

parser :: Parser Params
parser =
  Params
    <$> strOption (long "db" <> short 'd')
    <*> (tryParseIP <$> strOption (long "ips" <> short 'i'))

tryParseIP :: String -> Either String [IP]
tryParseIP = traverse p . split (== ',')
  where
    p t = case parseIP (T.pack $ trim t) of
      Just x -> Right x
      Nothing -> Left $ "Failed to parse IP:" +| t |+ ""
