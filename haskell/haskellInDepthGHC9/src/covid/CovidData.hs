{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
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
  accumulatedStatsFor,
  ) where
{- ORMOLU_ENABLE -}

import Control.Lens
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (Day)
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
  deriving (Show, Eq)

data DayInfo where
  DayInfo ::
    { _cases :: DayCases,
      _deaths :: DayDeaths
    } ->
    DayInfo
  deriving (Show, Eq)

data DayCases where
  DayCases ::
    { _total_cases :: Int,
      _new_cases :: Int
    } ->
    DayCases
  deriving (Show, Eq)

data DayDeaths where
  DayDeaths ::
    { _total_deaths :: Int,
      _new_deaths :: Int
    } ->
    DayDeaths
  deriving (Show, Eq)

data CountryStat where
  CountryStat ::
    { _population :: Int,
      _population_density :: Maybe Double
    } ->
    CountryStat
  deriving (Show, Eq)

data AccumulatedStats where
  AccumulatedStats ::
    { _accumulated_population :: Int,
      _accumulated_cases :: Int,
      _accumulated_deaths :: Int
    } ->
    AccumulatedStats
  deriving (Show, Eq)

makeLenses ''CountryData
makeLenses ''DayInfo
makeLenses ''DayCases
makeLenses ''DayDeaths
makeLenses ''CountryStat

instance TextShow CountryData where
  showb x =
    fromText ""
      <> (x & view (iso_code . to showb))
      <> " "
      <> (x & view (name . to showb))
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
    maxCases = maximum1Of (days . folded . _2 . cases . total_cases) countryData'
    maxDeaths = maximum1Of (days . folded . _2 . deaths . total_deaths) countryData'

accumulatedStatsFor :: CountryData -> AccumulatedStats
accumulatedStatsFor countryData =
  AccumulatedStats
    (countryData & view (stat . population))
    (countryData & view current_total_cases)
    (countryData & view current_total_deaths)
