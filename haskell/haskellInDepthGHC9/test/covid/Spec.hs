{-# LANGUAGE OverloadedStrings #-}

module Spec
  ( spec,
  )
where

import App
import Control.Monad
import Control.Monad.Trans.Resource
import CovidData
import CovidStats
import CsvParser
import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Attoparsec.ByteString.Char8 qualified as C
import Data.ByteString.Char8 qualified as B
import Data.List (intercalate)
import Data.Map qualified as M
import Data.Time.Calendar.OrdinalDate
import Streaming.ByteString.Char8 qualified as SB
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
  streamSpec

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

streamSpec :: Spec
streamSpec = describe "stream" $ do
  let dat =
        intercalate
          "\n"
          [ -- CAN
            "CAN,North America,Canada,2020-05-01,53236.0,1649.0,3184.0,188.0,1410.518,43.691,84.362,4.981,43939.0,832222.0,22.05,1.164,26975.0,0.715,16.955,0.059000000000000004,people tested,72.69,37742157.0,4.037,41.4,16.984,10.797,44017.591,0.5,105.599,7.37,12.0,16.6,,2.5,82.43",
            "CAN,North America,Canada,2020-05-02,55061.0,1825.0,3391.0,207.0,1458.873,48.354,89.846,5.485,,,,,27614.0,0.732,17.283,0.057999999999999996,people tested,72.69,37742157.0,4.037,41.4,16.984,10.797,44017.591,0.5,105.599,7.37,12.0,16.6,,2.5,82.43",
            "CAN,North America,Canada,2020-05-03,56714.0,1653.0,3566.0,175.0,1502.67,43.797,94.483,4.637,,893490.0,23.674,,28832.0,0.764,17.746,0.055999999999999994,people tested,72.69,37742157.0,4.037,41.4,16.984,10.797,44017.591,0.5,105.599,7.37,12.0,16.6,,2.5,82.43",
            "CAN,North America,Canada,2020-05-04,59474.0,2760.0,3682.0,116.0,1575.798,73.128,97.557,3.073,25878.0,919368.0,24.359,0.686,28845.0,0.764,16.038,0.062,people tested,72.69,37742157.0,4.037,41.4,16.984,10.797,44017.591,0.5,105.599,7.37,12.0,16.6,,2.5,82.43",
            "CAN,North America,Canada,2020-05-05,60772.0,1298.0,3854.0,172.0,1610.189,34.391,102.114,4.557,21199.0,940567.0,24.921,0.562,28530.0,0.756,16.259,0.062,people tested,72.69,37742157.0,4.037,41.4,16.984,10.797,44017.591,0.5,105.599,7.37,12.0,16.6,,2.5,82.43",
            -- EGY
            "EGY,Africa,Egypt,2020-05-01,5537.0,269.0,392.0,12.0,54.107,2.629,3.831,0.117,,,,,,,,,,84.26,102334403.0,97.999,25.3,5.159,2.891,10550.206,1.3,525.432,17.31,0.2,50.1,89.827,1.6,71.99",
            "EGY,Africa,Egypt,2020-05-02,5895.0,358.0,406.0,14.0,57.605,3.498,3.967,0.137,,,,,,,,,,84.26,102334403.0,97.999,25.3,5.159,2.891,10550.206,1.3,525.432,17.31,0.2,50.1,89.827,1.6,71.99",
            "EGY,Africa,Egypt,2020-05-03,6193.0,298.0,415.0,9.0,60.517,2.912,4.055,0.088,,,,,,,,,,84.26,102334403.0,97.999,25.3,5.159,2.891,10550.206,1.3,525.432,17.31,0.2,50.1,89.827,1.6,71.99",
            "EGY,Africa,Egypt,2020-05-04,6465.0,272.0,429.0,14.0,63.175,2.658,4.192,0.137,,,,,,,,,,84.26,102334403.0,97.999,25.3,5.159,2.891,10550.206,1.3,525.432,17.31,0.2,50.1,89.827,1.6,71.99",
            "EGY,Africa,Egypt,2020-05-05,6813.0,348.0,436.0,7.0,66.576,3.401,4.261,0.068,,,,,,,,,,84.26,102334403.0,97.999,25.3,5.159,2.891,10550.206,1.3,525.432,17.31,0.2,50.1,89.827,1.6,71.99",
            -- FIN
            "FIN,Europe,Finland,2020-05-01,4995.0,89.0,211.0,5.0,901.508,16.063,38.082,0.902,3257.0,106438.0,19.21,0.588,3308.0,0.597,32.568000000000005,0.031,samples tested,57.41,5540718.0,18.136,42.8,21.228,13.264,40585.721,,153.507,5.76,18.3,22.6,,3.28,81.91",
            "FIN,Europe,Finland,2020-05-02,5051.0,56.0,218.0,7.0,911.615,10.107,39.345,1.263,1464.0,107902.0,19.474,0.264,3089.0,0.558,32.961999999999996,0.03,samples tested,57.41,5540718.0,18.136,42.8,21.228,13.264,40585.721,,153.507,5.76,18.3,22.6,,3.28,81.91",
            "FIN,Europe,Finland,2020-05-03,5179.0,128.0,220.0,2.0,934.716,23.102,39.706,0.361,1616.0,109518.0,19.766,0.292,2965.0,0.535,29.482,0.034,samples tested,57.41,5540718.0,18.136,42.8,21.228,13.264,40585.721,,153.507,5.76,18.3,22.6,,3.28,81.91",
            "FIN,Europe,Finland,2020-05-04,5254.0,75.0,230.0,10.0,948.253,13.536,41.511,1.805,1717.0,111235.0,20.076,0.31,2867.0,0.517,29.6,0.034,samples tested,57.41,5540718.0,18.136,42.8,21.228,13.264,40585.721,,153.507,5.76,18.3,22.6,,3.28,81.91",
            "FIN,Europe,Finland,2020-05-05,5327.0,73.0,240.0,10.0,961.428,13.175,43.316,1.805,3681.0,114916.0,20.74,0.664,2871.0,0.518,31.799,0.031,samples tested,57.41,5540718.0,18.136,42.8,21.228,13.264,40585.721,,153.507,5.76,18.3,22.6,,3.28,81.91",
            -- KOR
            "KOR,Asia,South Korea,2020-05-01,10774.0,9.0,248.0,1.0,210.146,0.176,4.837,0.02,,614384.0,11.983,,4923.0,0.096,522.136,0.002,people tested,43.52,51269183.0,527.967,43.4,13.914,8.622,35938.374,0.2,85.998,6.8,6.2,40.9,,12.27,83.03",
            "KOR,Asia,South Korea,2020-05-02,10780.0,6.0,250.0,2.0,210.263,0.117,4.876,0.039,4682.0,619066.0,12.075,0.091,4981.0,0.097,562.371,0.002,people tested,43.52,51269183.0,527.967,43.4,13.914,8.622,35938.374,0.2,85.998,6.8,6.2,40.9,,12.27,83.03",
            "KOR,Asia,South Korea,2020-05-03,10793.0,13.0,250.0,0.0,210.516,0.254,4.876,0.0,3319.0,622385.0,12.14,0.065,4843.0,0.094,521.554,0.002,people tested,43.52,51269183.0,527.967,43.4,13.914,8.622,35938.374,0.2,85.998,6.8,6.2,40.9,,12.27,83.03",
            "KOR,Asia,South Korea,2020-05-04,10801.0,8.0,252.0,2.0,210.672,0.156,4.915,0.039,3360.0,625745.0,12.205,0.066,4711.0,0.092,523.444,0.002,people tested,43.52,51269183.0,527.967,43.4,13.914,8.622,35938.374,0.2,85.998,6.8,6.2,40.9,,12.27,83.03",
            "KOR,Asia,South Korea,2020-05-05,10804.0,3.0,254.0,2.0,210.731,0.059,4.954,0.039,5634.0,631379.0,12.315,0.11,4581.0,0.089,616.673,0.002,people tested,43.52,51269183.0,527.967,43.4,13.914,8.622,35938.374,0.2,85.998,6.8,6.2,40.9,,12.27,83.03",
            -- JPN
            "JPN,Asia,Japan,2020-05-01,14281.0,193.0,432.0,17.0,112.914,1.526,3.416,0.134,,,,,8062.0,0.064,27.65,0.036000000000000004,tests performed,47.22,126476458.0,347.778,48.2,27.049,18.493,39002.223,,79.37,5.72,11.2,33.7,,13.05,84.63",
            "JPN,Asia,Japan,2020-05-02,14544.0,263.0,458.0,26.0,114.994,2.079,3.621,0.206,,307601.0,2.432,,8018.0,0.063,33.975,0.028999999999999998,tests performed,47.22,126476458.0,347.778,48.2,27.049,18.493,39002.223,,79.37,5.72,11.2,33.7,,13.05,84.63",
            "JPN,Asia,Japan,2020-05-03,14839.0,295.0,492.0,34.0,117.326,2.332,3.89,0.269,,,,,7825.0,0.062,33.056999999999995,0.03,tests performed,47.22,126476458.0,347.778,48.2,27.049,18.493,39002.223,,79.37,5.72,11.2,33.7,,13.05,84.63",
            "JPN,Asia,Japan,2020-05-04,15057.0,218.0,510.0,18.0,119.05,1.724,4.032,0.142,,,,,7631.0,0.06,31.948,0.031,tests performed,47.22,126476458.0,347.778,48.2,27.049,18.493,39002.223,,79.37,5.72,11.2,33.7,,13.05,84.63",
            "JPN,Asia,Japan,2020-05-05,15231.0,174.0,521.0,11.0,120.426,1.376,4.119,0.087,,,,,7437.0,0.059,31.456,0.032,tests performed,47.22,126476458.0,347.778,48.2,27.049,18.493,39002.223,,79.37,5.72,11.2,33.7,,13.05,84.63"
          ]

  it "parses a byte stream" $ do
    parsed <- parseByteStream $ SB.string dat
    parsed
      `shouldBe` M.fromList
        [ ("Africa", AccumulatedStats {_accumulated_population = 102334403, _accumulated_cases = 6813, _accumulated_deaths = 436}),
          ("Asia", AccumulatedStats {_accumulated_population = 177745641, _accumulated_cases = 26035, _accumulated_deaths = 775}),
          ("Europe", AccumulatedStats {_accumulated_population = 5540718, _accumulated_cases = 5327, _accumulated_deaths = 240}),
          ("North America", AccumulatedStats {_accumulated_population = 37742157, _accumulated_cases = 60772, _accumulated_deaths = 3854})
        ]

  it "parses csv" $ do
    parsed <- runResourceT $ parseZipppedCsv "./src/data/owid-covid-data.csv.gz"
    parsed
      `shouldBe` M.fromList
        [ ("Africa", AccumulatedStats {_accumulated_population = 1339423921, _accumulated_cases = 1085589, _accumulated_deaths = 24680}),
          ("Asia", AccumulatedStats {_accumulated_population = 4599891093, _accumulated_cases = 5399479, _accumulated_deaths = 115546}),
          ("Europe", AccumulatedStats {_accumulated_population = 748506210, _accumulated_cases = 3098486, _accumulated_deaths = 207749}),
          ("North America", AccumulatedStats {_accumulated_population = 591242473, _accumulated_cases = 6239663, _accumulated_deaths = 240036}),
          ("Oceania", AccumulatedStats {_accumulated_population = 40958320, _accumulated_cases = 24606, _accumulated_deaths = 394}),
          ("South America", AccumulatedStats {_accumulated_population = 430461090, _accumulated_cases = 5052258, _accumulated_deaths = 171093})
        ]

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
