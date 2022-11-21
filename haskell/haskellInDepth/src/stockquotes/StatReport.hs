{-# LANGUAGE OverloadedStrings #-}

module StatReport
  ( QuoteFieldStats (field, mean, maxValue, minValue, daysBetweenMinMax),
    QuoteFieldStatValue,
    quoteStatsToText,
    computeQuoteStats,
  )
where

import qualified Colonnade as CL
import Data.Foldable
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Time as Time
import Fmt
import QuoteData

type QuoteFieldSelect a = QuoteData -> a

data QuoteFieldStatValue a = QuoteFieldStatValue
  { decimalPlaces :: !Int,
    value :: !a
  }
  deriving (Show, Read, Eq, Ord)

data QuoteFieldStats a = QuoteFieldStats
  { field :: !QuoteField,
    mean :: !(QuoteFieldStatValue a),
    minValue :: !(QuoteFieldStatValue a),
    maxValue :: !(QuoteFieldStatValue a),
    daysBetweenMinMax :: !Int
  }
  deriving (Show, Read, Eq, Ord)

quoteFieldAsFractional :: Fractional a => QuoteField -> QuoteFieldSelect a
quoteFieldAsFractional Open = realToFrac . open
quoteFieldAsFractional Close = realToFrac . close
quoteFieldAsFractional High = realToFrac . high
quoteFieldAsFractional Low = realToFrac . low
quoteFieldAsFractional Volume = realToFrac . volume
quoteFieldAsFractional Day = error ""

quoteFieldRealDecimalPlaces :: Int
quoteFieldRealDecimalPlaces = 2

quoteFieldDecimalPlaces :: QuoteField -> Int
quoteFieldDecimalPlaces Volume = 0
quoteFieldDecimalPlaces _ = quoteFieldRealDecimalPlaces

instance Real a => Buildable (QuoteFieldStatValue a) where
  build (QuoteFieldStatValue dec v) = fixedF dec v

instance Real a => Buildable (QuoteFieldStats a) where
  build (QuoteFieldStats qfield meanV minV maxV days) =
    "" +|| qfield ||+ ":"
      +| meanV |+ " (mean),"
      +| minV |+ " (min),"
      +| maxV |+ " (max),"
      +| days |+ " (days)"

computeMean :: (Fractional a, Foldable t) => t QuoteData -> QuoteFieldSelect a -> a
computeMean quotes select = foldr f 0 quotes / realToFrac (length quotes)
  where
    f x accum = select x + accum

computeMinMaxStates :: (Ord a, Foldable t) => t QuoteData -> QuoteFieldSelect a -> (a, a, Int)
computeMinMaxStates quotes select = (select minEntry, select maxEntry, diff)
  where
    minEntry = minimumBy (comparing select) quotes
    maxEntry = maximumBy (comparing select) quotes
    diff = fromInteger $ abs $ Time.diffDays (day maxEntry) (day minEntry)

computeQuoteStats :: (Foldable t, Ord a, Fractional a) => t QuoteData -> [QuoteFieldStats a]
computeQuoteStats quotes = computeQuoteStatsForField quotes <$> [Volume, Open, Close, High, Low]

computeQuoteStatsForField :: (Foldable t, Ord a, Fractional a) => t QuoteData -> QuoteField -> QuoteFieldStats a
computeQuoteStatsForField quotes qfield =
  QuoteFieldStats
    { field = qfield,
      mean = QuoteFieldStatValue quoteFieldRealDecimalPlaces $ computeMean quotes select,
      minValue = QuoteFieldStatValue qdecimals minV,
      maxValue = QuoteFieldStatValue qdecimals maxV,
      daysBetweenMinMax = daysDiff
    }
  where
    qdecimals = quoteFieldDecimalPlaces qfield
    select = quoteFieldAsFractional qfield
    (minV, maxV, daysDiff) = computeMinMaxStates quotes select

colStatData :: Real a => CL.Colonnade CL.Headed (QuoteFieldStats a) String
colStatData =
  CL.headed "Quote Field" (show . field)
    <> CL.headed "Mean" (pretty . mean)
    <> CL.headed "Min" (pretty . minValue)
    <> CL.headed "Max" (pretty . maxValue)
    <> CL.headed "Days between Min/Max" (pretty . daysBetweenMinMax)

quoteStatsToText :: Real a => [QuoteFieldStats a] -> T.Text
quoteStatsToText st = T.pack $ CL.ascii colStatData st
