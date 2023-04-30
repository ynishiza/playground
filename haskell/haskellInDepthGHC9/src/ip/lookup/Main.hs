{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use camelCase" #-}

module Main (main) where

import Data.Foldable
import Data.List.Extra
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import Fmt
import IPLookup
import IPLookupFast
import IPParse
import Options.Applicative

main :: IO ()
main = do
  Params {..} <- execParser parserInfo
  (Right db@(IPRangeDB rs)) <- parseIPRangeDB <$> T.readFile dbPath

  -- Note: slow version with String instead of Text
  -- (Right db@(IPRangeDB rs)) <- parseIPRangeDBString <$> readFile dbPath
  let res
        | (Just s) <- ipOption = case tryParseIP s of
          Right x -> runManualLookupMode db x
          Left e -> error $ "Failed to process database:" +| e |+ ""
        | (Just n) <- sizeOption = runGenerateModeWithFastLookup db n
        -- NOTE:
        --  | (Just n) <- sizeOption = runGenerateModeWithFastLookup db n

        -- Note: inefficient version
        --  | (Just n) <- sizeOption = runGenerateMode_highMemory db n
        | sortOption = putStr $ unlines $ show <$> sort rs
        | otherwise = error "Must specify at least one of -n|-i|-s"
   in res

runManualLookupMode :: IPRangeDB -> [IP] -> IO ()
runManualLookupMode db = traverse_ f
  where
    f ip = fmtLn $ ip ||+ ":" +| lookupIP db ip |+ ""

runGenerateModeWithFastLookup :: IPRangeDB -> Int -> IO ()
runGenerateModeWithFastLookup db n = do
  let t = length $ filter (== True) $ lookupIPFast (toFastRangeDB db) <$> generateIPs n
  fmtLn $ True |+ ":" +| t |+ " " +| False |+ " :" +| n - t |+ ""

runGenerateModeOptimized :: IPRangeDB -> Int -> IO ()
runGenerateModeOptimized db n = do
  let t = length $ filter (== True) $ lookupIP db <$> generateIPs n
  fmtLn $ True |+ ":" +| t |+ " " +| False |+ " :" +| n - t |+ ""

runGenerateMode_highMemory :: IPRangeDB -> Int -> IO ()
runGenerateMode_highMemory db n = do
  -- NOTE: slow because whole list needs to be loaded
  let (ts, fs) = partition (== True) $ lookupIP db <$> generateIPs n
  fmtLn $ True |+ ":" +| length ts |+ " " +| False |+ " :" +| length fs |+ ""

generateIPs :: Int -> [IP]
generateIPs n
  | n > 0 =
    let wds :: [Word32]
        wds = enumFromThenTo 0 step maxBound
     in IP <$> wds
  | otherwise = error "Bad n"
  where
    step = maxBound @Word32 `div` fromIntegral n

data Params = Params
  { dbPath :: FilePath,
    ipOption :: (Maybe String),
    sizeOption :: Maybe Int,
    sortOption :: Bool
  }
  deriving (Show)

parserInfo :: ParserInfo Params
parserInfo = info (helper <*> parser) (fullDesc <> progDesc "")

parser :: Parser Params
parser =
  Params
    <$> strOption (long "db" <> short 'd' <> metavar "IP database path")
    <*> optional (strOption (long "ip" <> short 'i' <> metavar "IPLIST"))
    <*> optional (option auto (short 'n' <> metavar "N"))
    <*> switch (long "sort" <> short 's')

tryParseIP :: String -> Either String [IP]
tryParseIP = traverse p . split (== ',')
  where
    p t = case parseIP (T.pack $ trim t) of
      Just x -> Right x
      Nothing -> Left $ "Failed to parse IP:" +| t |+ ""
