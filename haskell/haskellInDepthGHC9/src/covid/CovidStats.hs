{-# LANGUAGE GADTs #-}
module CovidStats (
  AccumulatedStats(..),
  ContinentStats,
  forCountry,
  byContinent,
  worldStats,
  ) where

import CovidData
import Control.Lens
import Data.Map (Map)
import Data.Text (Text)
import Data.Map qualified as M

type ContinentStats = Map Text AccumulatedStats

data AccumulatedStats where
  AccumulatedStats ::
    { _accumulated_population :: Int,
      _accumulated_cases :: Int,
      _accumulated_deaths :: Int
    } ->
    AccumulatedStats
  deriving (Show, Eq)

forCountry :: CountryData -> AccumulatedStats
forCountry countryData =
  AccumulatedStats
    (countryData & view (stat . population))
    (countryData & view current_total_cases)
    (countryData & view current_total_deaths)

byContinent :: ContinentStats -> CountryData -> ContinentStats
byContinent db countryData = M.insertWith (<>) (_continent countryData) (forCountry countryData) db

worldStats :: ContinentStats -> AccumulatedStats
worldStats = M.foldl (<>) mempty

instance Semigroup AccumulatedStats where
  (AccumulatedStats a b c) <> (AccumulatedStats a' b' c') = AccumulatedStats (a + a') (b + b') (c + c')

instance Monoid AccumulatedStats where
  mempty = AccumulatedStats 0 0 0
