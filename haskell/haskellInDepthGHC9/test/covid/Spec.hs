{-# LANGUAGE OverloadedStrings #-}

module Spec
  ( spec,
  )
where

import Control.Monad
import CovidData
import CsvParser
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as C
import Data.ByteString.Char8 qualified as B
import Data.Time.Calendar.OrdinalDate
import Test.Hspec

expectParseResult :: (Show a, Eq a) => (String, Parser a) -> a -> Expectation
expectParseResult (input, p) expected = C.parseOnly p (B.pack input) `shouldBe` Right expected

expectFailure :: (Show a) => (String, Parser a) -> (String -> Expectation) -> Expectation
expectFailure (input, p) fn = run $ C.parseOnly p (B.pack input)
  where
    run (Left e) = fn e
    run (Right v) = expectationFailure $ "Expected failure but succeeded with " <> show v

country1Line :: String
country1Line = "CAN,North America,Canada,2020-08-10,119451.0,245.0,8981.0,5.0,3164.922,6.491,237.957,0.132,43772.0,4447810.0,117.847,1.16,38516.0,1.021,104.46,0.01,people tested,67.13,37742157.0,4.037,41.4,16.984,10.797,44"

country1 :: CountryData
country1 =
  CountryData
    "CAN"
    "North America"
    "Canada"
    0
    0
    [ (fromOrdinalDate 2020 223, DayInfo (DayCases 119451 245) (DayDeaths 8981 5))
    ]
    (CountryStat 37742157 (Just 4.037))

spec :: Spec
spec = describe "COVID data" $ do
  dataSpec
  parserSpec

dataSpec :: Spec
dataSpec = describe "CovidData" $ do
  it "[withDaysAndTotals]" $ do
    withDaysAndTotals country1 [] `shouldBe` country1 {_current_total_cases = 119451, _current_total_deaths = 8981}
    withDaysAndTotals
      country1
      [ (fromOrdinalDate 2020 1, DayInfo (DayCases 200000 456) (DayDeaths 1111 2)),
        (fromOrdinalDate 2020 2, DayInfo (DayCases 999 456) (DayDeaths 828923 2))
      ]
      `shouldBe` country1
        { _current_total_cases = 200000,
          _current_total_deaths = 828923,
          _days =
            [ (fromOrdinalDate 2020 223, DayInfo (DayCases 119451 245) (DayDeaths 8981 5)),
              (fromOrdinalDate 2020 1, DayInfo (DayCases 200000 456) (DayDeaths 1111 2)),
              (fromOrdinalDate 2020 2, DayInfo (DayCases 999 456) (DayDeaths 828923 2))
            ]
        }

parserSpec :: Spec
parserSpec = describe "Parser" $ do
  describe "Parser" $ do
    it "parses fields" $ do
      ("hello,world,0,1,2", fields) `expectParseResult` ["hello", "world", "0", "1", "2"]
      ("a,b,,,c", fields) `expectParseResult` ["a", "b", "", "", "c"]

    it "parses country stats" $ do
      ("123,456.789", countryStat) `expectParseResult` CountryStat 123 (Just 456.789)
      ("123,", countryStat) `expectParseResult` CountryStat 123 Nothing
      (",", countryStat) `expectFailure` (`shouldStartWith` "CountryStat")

    it "parses day info" $ do
      ("123,1000,456,2000", dayInfo) `expectParseResult` DayInfo (DayCases 123 1000) (DayDeaths 456 2000)
      ("1,,,,", dayInfo) `expectFailure` (`shouldStartWith` "DayCases")
      ("1,2,,,", dayInfo) `expectFailure` (`shouldStartWith` "DayDeaths")

    it "parses date" $ do
      ("2023-01-02", dateField) `expectParseResult` fromOrdinalDate 2023 2
      ("2023-12-20", dateField) `expectParseResult` fromOrdinalDate 2023 354
      ("2023-", dateField) `expectFailure` (`shouldContain` "no parse of \"2023-\"")

    it "[countryData] parses country data" $ do
      ( country1Line,
        countryData
        )
        `expectParseResult` country1
      ( "AFG,Asia,Afghanistan,2019-12-31,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,,,,,,,,,,,38928341.0,54.422,18.6,2.581,1.337,1803.987,,597.029,9.59,,,37.746,0.5,64.83",
        countryData
        )
        `expectParseResult` CountryData
          "AFG"
          "Asia"
          "Afghanistan"
          0
          0
          [ (fromOrdinalDate 2019 365, DayInfo (DayCases 0 0) (DayDeaths 0 0))
          ]
          (CountryStat 38928341 (Just 54.422))

    it "[countryData] error if parsing fails" $ do
      ( ",North America,Canada,2020-08-10,119451.0,245.0,8981.0,5.0,3164.922,6.491,237.957,0.132,43772.0,4447810.0,117.847,1.16,38516.0,1.021,104.46,0.01,people tested,67.13,37742157.0,4.037,41.4,16.984,10.797,44",
        countryData
        )
        `expectFailure` (`shouldContain` "CountryData > iso")
      ( "Can,,Canada,2020-08-10,119451.0,245.0,8981.0,5.0,3164.922,6.491,237.957,0.132,43772.0,4447810.0,117.847,1.16,38516.0,1.021,104.46,0.01,people tested,67.13,37742157.0,4.037,41.4,16.984,10.797,44",
        countryData
        )
        `expectFailure` (`shouldContain` "CountryData > country")
      ( "Can,North America,Canada,2020-08-10,.0,245.0,8981.0,5.0,3164.922,6.491,237.957,0.132,43772.0,4447810.0,117.847,1.16,38516.0,1.021,104.46,0.01,people tested,67.13,37742157.0,4.037,41.4,16.984,10.797,44",
        countryData
        )
        `expectFailure` (`shouldContain` "CountryData > DayCases > Number")

    it "[maybeCountryData] parse country or skip" $ do
      let parseMany = replicateM 3 maybeCountryData
      (country1Line <> "\n" <> country1Line <> "\n" <> country1Line, parseMany)
        `expectParseResult` [Just country1, Just country1, Just country1]
      (country1Line <> "\n,,\n" <> country1Line, parseMany)
        `expectParseResult` [Just country1, Nothing, Just country1]
      (country1Line <> "\n" <> country1Line <> "\n,", parseMany)
        `expectParseResult` [Just country1, Just country1, Nothing]

    it "[maybeCountryData] may end with end or without new line" $ do
      let parseMany = replicateM 3 maybeCountryData
      (country1Line <> ",,\n,,\n", parseMany)
        `expectParseResult` [Just country1, Nothing, Nothing]
      (country1Line <> ",,\n,,\n", parseMany)
        `expectParseResult` [Just country1, Nothing, Nothing]
      (",,\n" <> country1Line <> ",,\n", parseMany)
        `expectParseResult` [Nothing, Just country1, Nothing]
