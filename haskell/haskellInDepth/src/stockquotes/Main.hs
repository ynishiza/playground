{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
    processParams,
    readCsvFile,
    test,
  )
where

import Text.Read (readMaybe, readEither)
import Data.Either
import Charts
import qualified Colonnade as CL
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Fmt hiding (format)
import HtmlReport
import Params
import QuoteData
import StatReport
import System.Environment
import System.FilePath.Posix
import Utils

defaultChartPath :: FilePath
defaultChartPath = "./mychart.png"

main :: IO ()
main = do
  params <- parseParams
  processParams params

processParams :: Params -> IO ()
processParams params = do
  let Params {..} = params
      writeLog = unless silent . fmtLn

  -- step: read stock data
  writeLog $ params ||+ ""
  quotes <- readCsvFile dataFilepath

  -- step: compute stats
  let stats :: [QuoteFieldStats Double]
      stats = computeQuoteStats quotes
      chartPath = maybe defaultChartPath (\p -> dropExtensions p ++ ".png") htmlFilepath
  writeLog $ quoteStatsToText stats |+ ""

  -- step: compute chart
  when includeChart $ do
    plotChartToFile quotes chartPath defaultPlotOptions
    writeLog $ "Chart saved at path=" +| chartPath |+ ""

  -- step: render HTML
  case htmlFilepath of
    Just htmlPath -> do
      let title = maybe "Report" (`T.append` "'s report") company
          html = getHtmlReportText title quotes [chartPath | includeChart]
      TIO.writeFile htmlPath html
      writeLog $ "HTML saved at path=" +| htmlPath |+ ""
    Nothing -> return ()

f :: String -> String -> Maybe Int
f s t = do
  x <- readMaybe s
  y <- readMaybe t
  return (x*y)
g :: String -> String -> Either String Int
g s t = do
  x <- h s
  y <- h t
  return (x*y)
    where h s = case readMaybe s of Just x -> return x; Nothing -> Left $ "Failed" ++ s

test :: IO ()
test = do
  args <- getArgs

  quotes <- readCsvFile (head args)
  let allStats :: [QuoteFieldStats Double]
      allStats = computeQuoteStats quotes
      plotOptions =
        PlotOptions
          { width = traceShowId "" 1280,
            height = 720,
            title = "",
            format = SVG
          }
      svgChart = "./mychart.svg"
      pngChart = "./mychart.png"

  fmtLn $ build $ quoteStatsToText allStats
  fmtLn $ build $ toTable $ V.take 10 quotes
  plotChartToFile quotes pngChart $ plotOptions {format = PNG}
  plotChartToFile quotes svgChart $ plotOptions {format = SVG}

readCsvFile :: FilePath -> IO (Vector QuoteData)
readCsvFile fname = do
  text <- BSL.readFile fname
  let parsed :: Either String (Csv.Header, Vector QuoteData)
      parsed = Csv.decodeByName text
   in case parsed of
        Left e -> error e
        Right v -> return $ snd v

quoteDataTableColumns :: CL.Colonnade CL.Headed QuoteData String
quoteDataTableColumns =
  mconcat
    [ CL.headed "Day" (show . day),
      CL.headed "Volume" (show . volume),
      CL.headed "Open" (show . open),
      CL.headed "High" (show . high),
      CL.headed "Low" (show . low),
      CL.headed "Close" (show . close)
    ]

toTable :: Vector QuoteData -> Text
toTable qd = T.pack $ CL.ascii quoteDataTableColumns qd
