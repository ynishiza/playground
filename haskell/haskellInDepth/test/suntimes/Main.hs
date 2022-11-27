module Main (main, testRequestProcessor) where

import App
import Control.Concurrent
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime hiding (getTimeZone)
import GeoCoordsReq
import ProcessRequest
import STExcept
import SunTimes
import Types
import Utils

defaultWauth :: WebAPIAuth
defaultWauth = defaultWebAPIAuth

defaultLogLevel :: LogLevel
-- defaultLogLevel = LevelInfo
defaultLogLevel = LevelDebug

runWithDefaults :: SuntimesApp a -> IO a
runWithDefaults app = runApp app defaultLogLevel defaultWauth

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
  testRequestProcessor

testSuntimes :: IO ()
testSuntimes = printBannerWrap "Time lookup API" $ do
  sres <- runWithDefaults (getSuntimesUTC coords0 (On suntimesDate0))
  assertIsEqual sres suntimes0UTC

  tres <- runWithDefaults (getTimeZone coords0)
  assertIsEqual tres timezoneLocal

  sres2 <- runWithDefaults (getSuntimesLocal coords0 (On suntimesDate0))
  assertIsEqual (show sres2) (show suntimes0Local)
  pure ()

testGeoCoords :: IO ()
testGeoCoords = printBannerWrap "GeoCoords API" $ do
  res <- runWithDefaults (getGeoCoords loc0)
  assertIsEqual res coords0
  pure ()

testRequestProcessor :: IO ()
testRequestProcessor = printBannerWrap "ProcessRequest.processInput" $ do
  printBannerWrap "Success" $ do
    res1 <- runWithDefaults (ProcessRequest.processInput "2000-01-01@kobe")
    assertIsEqual (fst res1) coords0
    assertIsEqual (show $ snd res1) (show suntimes0Local)

    res2 <- runWithDefaults (ProcessRequest.processInput "kobe")
    assertIsEqual (fst res2) coords0
    pure ()

  printBannerWrap "Fail" $ do
    _ <-
      testError
        defaultWauth
        ""
        "Invalid address: Failed to process line=''\t Missing address"
    _ <-
      testError
        defaultWauth
        "2000-01-01@"
        "Invalid address: Failed to process line='2000-01-01@'\t Missing address"

    _ <-
      testError
        defaultWauth
        "2000@kobe"
        "Invalid date format: Failed to process line='2000@kobe'\t Invalid date = 2000"

    _ <-
      testError
        defaultWauth
        "aidalskfjaskdljfasdkfj"
        "Failed while determining coordinates :aidalskfjaskdljfasdkfj"

    _ <-
      testError
        (defaultWauth {timezoneDBKey = ""})
        "kobe"
        "Error while communicating with external services: Failed to access timezone DB. Is a valid API key set in your config?\nStatus {statusCode = 400, statusMessage = \"Bad Request\"}"
    _ <-
      testError
        (defaultWauth {email = "", agent = ""})
        "kobe"
        "Error while communicating with external services: Failed to lookup coordinates. Are email and agent set in your config?\nStatus {statusCode = 404, statusMessage = \"Not Found\"}"
    pure ()
  where
    testError config input message = do
      x <- runApp ((ProcessRequest.processInput input >> return "") `catch` handleError) defaultLogLevel config
      assertIsEqual x message
    handleError :: SuntimeException -> SuntimesApp T.Text
    handleError = return . T.pack . show
