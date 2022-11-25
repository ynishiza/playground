{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( When (..),
    Address,
    SunTimes (..),
    GeoCoords (..),
    WebAPIAuth (..),
    sunTimeToZonedTime,
    defaultWebAPIAuth,
  )
where

import Data.Aeson
import qualified Data.Text as T
import Data.Time
import Fmt
import GHC.Generics
import TextShow

type Address = T.Text

data When = Now | On !Day deriving (Show)

data GeoCoords = GeoCoords
  { lat :: !T.Text,
    lon :: !T.Text
  }
  deriving (Generic, Show, Eq, FromJSON)

data SunTimes d = SunTimes
  { sunrise :: !d,
    sunset :: !d
  }
  deriving (Generic, Show, Eq, FromJSON)

sunTimeToZonedTime :: TimeZone -> SunTimes UTCTime -> SunTimes ZonedTime
sunTimeToZonedTime tz SunTimes {..} =
  SunTimes
    { sunrise = utcToZonedTime tz sunrise,
      sunset = utcToZonedTime tz sunset
    }

instance Show d => TextShow (SunTimes d) where
  showb = build . show

data WebAPIAuth = WebAPIAuth
  { timezoneDBKey :: !T.Text,
    email :: !T.Text,
    agent :: !T.Text
  }
  deriving (Generic, Show, Eq, FromJSON)

defaultWebAPIAuth :: WebAPIAuth
defaultWebAPIAuth = WebAPIAuth "WK58Y5KBT5AX" "test@mail.com" "suntimes"

instance Buildable WebAPIAuth where
  build = build . show
