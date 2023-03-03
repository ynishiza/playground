{-# LANGUAGE BangPatterns #-}
import App
import Control.Arrow ((>>>))
import Control.Monad.Trans.Resource
import CovidData
import CovidStats
import Criterion
import Criterion.Main
import CsvParser
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.Function
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Streaming.ByteString.Char8 qualified as SB
import Streaming.Zip qualified as S
import TextShow
import Control.Monad

csvPath :: FilePath
csvPath = "./src/data/owid-covid-data.csv.gz"

assertWorldStats :: AccumulatedStats -> IO ()
assertWorldStats s = unless (s == AccumulatedStats 7750483107 20900081 759498) $ throwM $ userError $ "Invalid world stats:" <> tshow s

tshow :: TextShow a => a -> String
tshow = toString . showb

main :: IO ()
main =
  defaultMain
    [ bgroup
        "In memory"
        [ bench "file read only" (nfIO readFileOnly),
          bench "parse CountryData" (nfIO (readFileOnly >>= runInMemoryParser)),
          env readFileOnly (bench "env - parse CountryData" . nfIO . runInMemoryParser)
        ],
      bgroup
        "Stream"
        [ bench "NF - parse CountryData" (nfIO runStreamParser),
          bench "WHNF - parse CountryData" (whnfIO runStreamParser)
        ]
    ]

runStreamParser :: IO ContinentStats
runStreamParser = do 
  !res <- runResourceT (parseZipppedCsv csvPath)
  putStrLn $ "world stats:" <> tshow (worldStats res)
  assertWorldStats $ worldStats res
  return res

readFileOnly :: IO ByteString
readFileOnly =
  runResourceT
    ( csvPath
        & SB.readFile
        & S.gunzip
        & SB.toStrict_
    )

runInMemoryParser :: ByteString -> IO ContinentStats
runInMemoryParser bs = 
  putStrLn ("world stats:" <> tshow (worldStats res)) 
  >> assertWorldStats (worldStats res)
  >> return res
  where !res = parseInMemory bs

parseInMemory :: ByteString -> ContinentStats
parseInMemory =
  P.parseOnly (P.many1 maybeCountryData)
    >>> fromRight (error "Failed to parse")
    >>> catMaybes
    >>> foldr (\c m -> M.insertWith (<>) (_name c) c m) M.empty
    >>> foldl byContinent initContinentStats

