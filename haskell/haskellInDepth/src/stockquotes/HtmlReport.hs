{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module HtmlReport (getHtmlReportText, getHtmlReport) where

import qualified Colonnade as C
-- import qualified Data.ByteString as B

import Control.Monad
import Data.Foldable
import Data.String
import qualified Data.Text as T
import Fmt
import QuoteData
import StatReport
import Text.Blaze.Colonnade
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

class_DATATABLE :: T.Text

class_STATSTABLE = "stats"

class_STATSTABLE :: T.Text

class_DATATABLE = "data"

toHtmlText :: Buildable a => a -> H.Html
toHtmlText = H.text . pretty

quoteStatsTableColumns :: Real a => C.Colonnade C.Headed (QuoteFieldStats a) H.Html
quoteStatsTableColumns =
  mconcat
    [ C.headed "Quote Field" (toHtmlText . show . field),
      C.headed "Mean" (toHtmlText . mean),
      C.headed "Min" (toHtmlText . minValue),
      C.headed "Max" (toHtmlText . maxValue),
      C.headed "Days between Min/Max" (toHtmlText . daysBetweenMinMax)
    ]

quoteDataTableColumns :: C.Colonnade C.Headed QuoteData H.Html
quoteDataTableColumns =
  mconcat
    [ C.headed "Day" (toHtmlText . day),
      C.headed "Volume" (toHtmlText . volume),
      C.headed "Open" (toHtmlText . open),
      C.headed "High" (toHtmlText . high),
      C.headed "Low" (toHtmlText . low),
      C.headed "Close" (toHtmlText . close)
    ]

getHtmlReportText :: Foldable f => T.Text -> f QuoteData -> [FilePath] -> T.Text
getHtmlReportText reportTitle quotes images = T.pack $ renderHtml $ getHtmlReport reportTitle quotes images

getHtmlReport :: Foldable f => T.Text -> f QuoteData -> [FilePath] -> H.Html
getHtmlReport reportTitle quotes images = H.html $ do
  H.head $ do
    H.title $ fromString $ T.unpack reportTitle
    H.style $ H.toMarkup reportStyle

  H.body $ do
    unless (null images) $ do
      H.section $ do
        H.h1 "Charts"
        traverse_ ((H.img !) . HA.src . H.toValue) images

    H.section $ do
      H.h1 "Stats"
      H.span $ "Total: " <> fromString (show $ length quotes)
      encodeHtmlTable mempty quoteStatsTableColumns stats ! HA.class_ (H.toValue class_STATSTABLE)

    H.section $ do
      H.h1 "Data"
      encodeHtmlTable mempty quoteDataTableColumns quotes ! HA.class_ (H.toValue class_DATATABLE)
  where
    stats :: [QuoteFieldStats Double]
    stats = computeQuoteStats quotes
    reportStyle :: T.Text
    reportStyle =
      fmtLn $
        "body { font-size: 1.2rem; }\n"
          <> "table { border-collapse: collapse }\n"
          <> "td,th { border: 1px solid black; padding: 0.3rem; }"
          +| "."
          +| class_DATATABLE |+ " {}"
          +| "."
          +| class_STATSTABLE |+ " td:first-child { font-style: italic; }"
