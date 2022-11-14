{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module HtmlReport (getHtmlReportText, getHtmlReport) where

import Data.Foldable
import qualified Colonnade as C
-- import qualified Data.ByteString as B
import Fmt
import qualified Data.Text as T
import QuoteData
import StatReport
import Text.Blaze.Colonnade
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes (src)
import Text.Blaze.Html.Renderer.String
import Data.String

toText :: Buildable a => a -> H.Html
toText = H.text . pretty

colStats :: Real a => C.Colonnade C.Headed (QuoteFieldStats a) H.Html
colStats =
  mconcat
    [ C.headed "Quote Field" (H.i . H.string . show . field),
      C.headed "Mean" (toText . mean),
      C.headed "Min" (toText . minValue),
      C.headed "Max" (toText . maxValue),
      C.headed "Days between Min/Max" (toText . daysBetweenMinMax)
    ]

colQuoteData :: C.Colonnade C.Headed QuoteData H.Html
colQuoteData =
  mconcat
    [ C.headed "Day" (toText . day),
      C.headed "Volume" (toText . volume),
      C.headed "Open" (toText . open),
      C.headed "High" (toText . high),
      C.headed "Low" (toText . low),
      C.headed "Close" (toText . close)
    ]

getHtmlReportText :: Foldable f => T.Text -> f QuoteData -> [FilePath] -> T.Text
getHtmlReportText title quotes images = T.pack $ renderHtml $ getHtmlReport title quotes images

getHtmlReport :: Foldable f => T.Text -> f QuoteData -> [FilePath] -> H.Html
getHtmlReport title quotes images = H.html $ do
  H.head $ do
    H.title $ fromString $ T.unpack title
    H.style style

  H.body $ do
    H.section $ do
      H.h1 "Charts"
      traverse_ ((H.img H.!) . src . H.toValue) images

    H.section $ do
      H.h1 "Stats"
      H.span $ "Total: " <> fromString (show $ length quotes)
      encodeHtmlTable mempty colStats stats

    H.section $ do
      H.h1 "Data"
      encodeHtmlTable mempty colQuoteData quotes
  where
    stats :: [QuoteFieldStats Double]
    stats = computeQuoteStats quotes
    style = "body { font-size: 1.2rem; }\n"
        <> "table { border-collapse: collapse }\n"
        <> "td,th { border: 1px solid black; padding: 0.3rem; }"
