{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

{- ORMOLU_DISABLE -}
module CovidData (
  CountryData(..),
  iso_code,
  continent,
  name,
  current_total_cases,
  current_total_deaths,
  days,
  stat,

  DayInfo(..),
  cases,
  deaths,

  DayCases(..),
  total_cases,
  new_cases,

  DayDeaths(..),
  total_deaths,
  new_deaths,
  
  CountryStat(..),
  population,
  population_density,
  withDaysAndTotals,
  ) where
{- ORMOLU_ENABLE -}

import Control.DeepSeq
import Control.Lens
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Text (Text)
import Data.Time (Day)
import GHC.Generics (Generic)
import TextShow

data CountryData where
  CountryData ::
    { _iso_code :: ByteString,
      _continent :: Text,
      _name :: Text,
      _current_total_cases :: Int,
      _current_total_deaths :: Int,
      _days :: [(Day, DayInfo)],
      _stat :: CountryStat
    } ->
    CountryData
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data DayInfo where
  DayInfo ::
    { _cases :: DayCases,
      _deaths :: DayDeaths
    } ->
    DayInfo
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data DayCases where
  DayCases ::
    { _total_cases :: Int,
      _new_cases :: Int
    } ->
    DayCases
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data DayDeaths where
  DayDeaths ::
    { _total_deaths :: Int,
      _new_deaths :: Int
    } ->
    DayDeaths
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

data CountryStat where
  CountryStat ::
    { _population :: Int,
      _population_density :: Maybe Double
    } ->
    CountryStat
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

makeLenses ''CountryData
makeLenses ''DayInfo
makeLenses ''DayCases
makeLenses ''DayDeaths
makeLenses ''CountryStat

instance Semigroup CountryData where
  c1 <> c2 =
    if _iso_code c1 == _iso_code c2
      then withDaysAndTotals c1 (_days c2)
      else error $ "country mismatch:" <> show c1 <> " <> " <> show c2

instance TextShow CountryData where
  showb x =
    ""
      <> (x & view (iso_code . to (fromString . B.unpack)))
      <> " "
      <> (x & view (name . to fromText))
      <> " "
      <> (x & view (current_total_cases . to showb))
      <> " "
      <> (x & view (current_total_deaths . to showb))

withDaysAndTotals :: CountryData -> [(Day, DayInfo)] -> CountryData
withDaysAndTotals countryData dayInfo =
  countryData'
    & set current_total_cases maxCases
    & set current_total_deaths maxDeaths
  where
    countryData' = countryData & over days (++ dayInfo)
    maxCases = maximum1Of (days . traverse . _2 . cases . total_cases) countryData'
    maxDeaths = maximum1Of (days . traverse . _2 . deaths . total_deaths) countryData'
