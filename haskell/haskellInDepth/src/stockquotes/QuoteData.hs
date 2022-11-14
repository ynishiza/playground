{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module QuoteData
  ( QuoteData (..),
    QuoteField (..),
    barDelim,
  )
where

import Data.ByteString.Char8 (unpack)
import Data.Char (ord)
import Data.Csv (DecodeOptions (..), FromField, FromNamedRecord, FromRecord, parseField)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Fmt
import GHC.Generics (Generic)

data QuoteData = QuoteData
  { day :: !Day,
    volume :: !Int,
    close :: !Double,
    open :: !Double,
    high :: !Double,
    low :: !Double
  }
  deriving (Eq, Ord, Show, Read, Generic)

data QuoteField = Day | Volume | Open | Close | High | Low deriving (Eq, Ord, Show, Read, Generic)

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . unpack

instance Buildable QuoteData where
  build = build . show

deriving instance FromRecord QuoteData

deriving instance FromNamedRecord QuoteData


barDelim :: DecodeOptions
barDelim = DecodeOptions $ fromIntegral (ord '|')
