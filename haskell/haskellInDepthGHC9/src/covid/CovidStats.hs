{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module CovidStats
  ( AccumulatedStats (..),
    ContinentStats,
    forCountry,
    byContinent,
    worldStats,
    initContinentStats,
  )
where

import Control.DeepSeq
import Control.Lens
import CovidData
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import TextShow

type ContinentStats = Map Text AccumulatedStats

data AccumulatedStats where
  AccumulatedStats ::
    { _accumulated_population :: Int,
      _accumulated_cases :: Int,
      _accumulated_deaths :: Int
    } ->
    AccumulatedStats
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

makeLenses ''AccumulatedStats

instance TextShow AccumulatedStats where
  showb s =
    ""
      <> showb (s & view accumulated_population)
      <> " "
      <> showb (s & view accumulated_cases)
      <> " "
      <> showb (s & view accumulated_deaths)

instance TextShow ContinentStats where
  showb =
    M.foldlWithKey
      (\bd key stats -> bd <> fromText key <> " " <> showb stats <> "\n")
      ""

forCountry :: CountryData -> AccumulatedStats
forCountry countryData =
  AccumulatedStats
    (countryData & view (stat . population))
    (countryData & view current_total_cases)
    (countryData & view current_total_deaths)

initContinentStats :: ContinentStats
initContinentStats = M.empty

byContinent :: ContinentStats -> CountryData -> ContinentStats
byContinent db countryData = M.insertWith (<>) (_continent countryData) (forCountry countryData) db

worldStats :: ContinentStats -> AccumulatedStats
worldStats = M.foldl (<>) mempty

instance Semigroup AccumulatedStats where
  (AccumulatedStats a b c) <> (AccumulatedStats a' b' c') = AccumulatedStats (a + a') (b + b') (c + c')

instance Monoid AccumulatedStats where
  mempty = AccumulatedStats 0 0 0
