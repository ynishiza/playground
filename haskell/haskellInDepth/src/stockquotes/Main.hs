{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
    readCsvFile,
    test,
  )
where


import qualified Colonnade as CL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO as TIO
import Data.Vector (Vector)
import qualified Data.Vector as V
import Fmt hiding (format)
import System.Environment
import Data.Maybe
import Control.Monad
import System.FilePath.Posix

import Charts
import Params
import QuoteData
import StatReport
import HtmlReport
import Utils

defaultChartPath :: FilePath
defaultChartPath = "./mychart.svg"

main :: IO ()
main = do
  params <- parseParams
  let
    Params {..} = params
    printIfNotSilent = unless silent . fmtLn

  fmtLn $ params ||+ ""
  quotes <- readCsvFile filename

  let
    stats :: [QuoteFieldStats Double]
    stats = computeQuoteStats quotes
    chartPath = maybe "./mychart.png" (\p -> dropExtensions p ++ ".png") htmlFilename

  printIfNotSilent $ quoteStatsToText stats|+""

  plotChartToFile quotes chartPath defaultPlotOptions
  printIfNotSilent $ "Chart saved at path="+|chartPath|+""

  case htmlFilename of
      Just htmlPath -> do
        let
          title = maybe "Report" (`T.append` "'s report") company
          html = getHtmlReportText title quotes [chartPath]
        TIO.writeFile htmlPath html
        printIfNotSilent $ "HTML saved at path="+|htmlPath|+""
      Nothing -> return ()

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

colQuoteData :: CL.Colonnade CL.Headed QuoteData String
colQuoteData =
  mconcat
    [ CL.headed "Day" (show . day),
      CL.headed "Volume" (show . volume),
      CL.headed "Open" (show . open),
      CL.headed "High" (show . high),
      CL.headed "Low" (show . low),
      CL.headed "Close" (show . close)
    ]

toTable :: Vector QuoteData -> Text
toTable qd = T.pack $ CL.ascii colQuoteData qd
