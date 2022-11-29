module Main (main, specs) where

import App
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
import Test.Tasty
import Test.Tasty.Hspec
import Types

defaultWauth :: WebAPIAuth
defaultWauth = defaultWebAPIAuth

defaultLogLevel :: LogLevel
defaultLogLevel = LevelInfo

-- defaultLogLevel = LevelDebug

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
  testSpec "Suntimes" specs >>= defaultMain

specs :: SpecWith ()
specs = describe "Suntimes" $ do
  describe "GeoCoords API" $ do
    it "should get coordinates" $ do
      res <- runWithDefaults (getGeoCoords loc0)
      shouldBe res coords0

  describe "Time lookup API" $ do
    it "Should get the suntimes in UTC" $ do
      sres <- runWithDefaults (getSuntimesUTC coords0 (On suntimesDate0))
      sres `shouldBe` suntimes0UTC
    it "Should get the suntimes in local timezone" $ do
      tres <- runWithDefaults (getTimeZone coords0)
      shouldBe tres timezoneLocal

    it "should get the timezone" $ do
      sres2 <- runWithDefaults (getSuntimesLocal coords0 (On suntimesDate0))
      shouldBe (show sres2) (show suntimes0Local)

  describe "requestProcessor" $ do
    let testError config input message = do
          x <- runApp ((ProcessRequest.processInput input >> return "") `catch` handleError) defaultLogLevel config
          x `shouldBe` message
        handleError :: SuntimeException -> SuntimesApp T.Text
        handleError = return . T.pack . show

    it "determines the suntimes of a given location on a given date" $ do
      res1 <- runWithDefaults (ProcessRequest.processInput "2000-01-01@kobe")
      shouldBe (fst res1) coords0
      shouldBe (show $ snd res1) (show suntimes0Local)

    it "determines the suntimes of a given location with the current date" $ do
      res2 <- runWithDefaults (ProcessRequest.processInput "kobe")
      shouldBe (fst res2) coords0

    it "fails if the input is empty" $ do
      testError
        defaultWauth
        ""
        "Invalid address: Failed to process line=''\t Missing address"

    it "fails if only a date is provided" $ do
      testError
        defaultWauth
        "2000-01-01@"
        "Invalid address: Failed to process line='2000-01-01@'\t Missing address"

    it "fails if the date format is invalid" $ do
      testError
        defaultWauth
        "2000@kobe"
        "Invalid date format: Failed to process line='2000@kobe'\t Invalid date = 2000"
    it "fails if the location is not found" $ do
      testError
        defaultWauth
        "aidalskfjaskdljfasdkfj"
        "Failed while determining coordinates :aidalskfjaskdljfasdkfj"

    it "fails if the timezone API config is invalid" $ do
      testError
        (defaultWauth {timezoneDBKey = ""})
        "kobe"
        "Error while communicating with external services: Failed to access timezone DB. Is a valid API key set in your config?\nStatus {statusCode = 400, statusMessage = \"Bad Request\"}"

    it "fails if Timezone API config is invalid" $ do
      testError
        (defaultWauth {email = "", agent = ""})
        "kobe"
        "Error while communicating with external services: Failed to lookup coordinates. Are email and agent set in your config?\nStatus {statusCode = 404, statusMessage = \"Not Found\"}"
