module Main (main) where

import App
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime hiding (getTimeZone)
import GeoCoordsReq
import SunTimes
import Types
import Utils

defaultWauth :: WebAPIAuth
defaultWauth = defaultWebAPIAuth

loc0 :: Address
loc0 = "Kobe, Japan"

coords0 :: GeoCoords
coords0 = GeoCoords "34.6932379" "135.1943764"

timezoneLocal :: TimeZone
timezoneLocal =
  (minutesToTimeZone $gmtOffset tzInfo0 `div` 60)
    { timeZoneName = "JST",
      timeZoneSummerOnly = False
    }

suntimesDate0 :: Day
suntimesDate0 = fromOrdinalDate 2000 1

suntimes0UTC :: SunTimes UTCTime
suntimes0UTC =
  SunTimes
    { sunrise = posixSecondsToUTCTime 946677889, -- 1999-12-31 22:04:49 UTC
      sunset = posixSecondsToUTCTime 946713592 -- 2000-01-01 07:59:52 UTC
    }

suntimes0Local :: SunTimes ZonedTime
suntimes0Local =
  SunTimes
    { sunrise = utcToZonedTime timezoneLocal (sunrise suntimes0UTC), -- 2000-01-01 07:04:49
      sunset = utcToZonedTime timezoneLocal (sunset suntimes0UTC) -- 2000-01-01 16:59:52
    }

tzInfo0 :: TimeZoneResponse
tzInfo0 =
  TimeZoneResponse
    { dst = "0",
      abbreviation = "JST",
      gmtOffset = 32400
    }

main :: IO ()
main = do
  testGeoCoords
  testSuntimes

testSuntimes :: IO ()
testSuntimes = do
  sres <- runApp (getSuntimesUTC coords0 (On suntimesDate0)) defaultWauth
  assertIsEqual sres suntimes0UTC

  tres <- runApp (getTimeZone coords0) defaultWauth
  assertIsEqual tres timezoneLocal

  sres2 <- runApp (getSuntimesLocal coords0 (On suntimesDate0)) defaultWauth
  assertIsEqual (show sres2) (show suntimes0Local)
  pure ()

testGeoCoords :: IO ()
testGeoCoords = do
  res <- runApp (getGeoCoords loc0) defaultWauth
  assertIsEqual res coords0
  pure ()
