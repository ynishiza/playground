{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
module QuoteData (
  QuoteData(..),
  QuoteField(..),
  barDelim,
  ) where

import Data.ByteString.Char8 (unpack)
import Data.Char (ord)
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.Csv (FromField, parseField, FromNamedRecord, FromRecord, DecodeOptions(..))
import GHC.Generics(Generic)
import Fmt

data QuoteData = QuoteData { 
  day :: !Day,
  volume :: !Int,
  close :: !Double,
  open :: !Double,
  high :: !Double,
  low :: !Double
                           } deriving (Eq, Ord, Show, Read, Generic)

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack 

instance  Buildable QuoteData where
  build = build.show

deriving instance FromRecord QuoteData
deriving instance FromNamedRecord QuoteData

data QuoteField = Day | Volume | Open | Close | High | Low deriving (Eq, Ord, Show, Read, Generic)

barDelim :: DecodeOptions
barDelim = DecodeOptions $ fromIntegral (ord '|')
                           

