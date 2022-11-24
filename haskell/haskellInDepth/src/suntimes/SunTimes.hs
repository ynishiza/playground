{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module SunTimes
  ( getSuntimesUTC,
    getSuntimesLocal,
    getTimeZone,
    TimeZoneResponse (..),
  )
where

import App
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Text as T
import Data.Time hiding (getTimeZone)
import GHC.Generics
import Types

suntimeUri :: T.Text
suntimeUri = "api.sunrise-sunset.org"

timezoneDBUri :: T.Text
timezoneDBUri = "api.timezonedb.com"

newtype SuntimesFormat dt = SuntimesFormat {results :: SunTimes dt}
  deriving (Generic, Show, Eq, FromJSON)

getSuntimesUTC :: GeoCoords -> When -> SuntimesApp (SunTimes UTCTime)
getSuntimesUTC GeoCoords {..} w = do
  let uri = https suntimeUri /: "json"
      params =
        "lat" =: lat
          <> "lng" =: lon
          <> "formatted" =: (0 :: Int)
          <> ( case w of
                 Now -> mempty
                 On d -> "date" =: formatTime defaultTimeLocale "%Y-%m-%d" d
             )
  results . responseBody . snd <$> appGET @(JsonResponse (SuntimesFormat UTCTime)) uri params jsonResponse

getSuntimesLocal :: GeoCoords -> When -> SuntimesApp (SunTimes ZonedTime)
getSuntimesLocal coords w = do
  tz <- defaultToUtc $ getTimeZone coords
  res <- getSuntimesUTC coords w
  return $ sunTimeToZonedTime tz res
  where
    defaultToUtc v = catchAll v (const (return utc))

getTimeZone :: GeoCoords -> SuntimesApp TimeZone
getTimeZone GeoCoords {..} = do
  wauth <- ask
  let uri = https timezoneDBUri /: "v2.1" /: "get-time-zone"
      params =
        "key" =: timezoneDBKey wauth
          <> "lat" =: lat
          <> "lng" =: lon
          <> "by" =: ("position" :: T.Text)
          <> "format" =: ("json" :: T.Text)
          <> "fields" =: ("gmtOffset,abbreviation,dst" :: T.Text)
  timeZoneResponseToTimeZone . responseBody . snd <$> appGET @(JsonResponse TimeZoneResponse) uri params jsonResponse

data TimeZoneResponse = TimeZoneResponse
  { dst :: !T.Text,
    abbreviation :: !T.Text,
    gmtOffset :: !Int
  }
  deriving (Generic, Show, Eq, FromJSON)

timeZoneResponseToTimeZone :: TimeZoneResponse -> TimeZone
timeZoneResponseToTimeZone TimeZoneResponse {..} =
  (minutesToTimeZone $ gmtOffset `div` 60)
    { timeZoneName = T.unpack abbreviation,
      timeZoneSummerOnly = dst == "1"
    }
