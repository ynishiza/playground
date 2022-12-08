{-# LANGUAGE OverloadedStrings #-}

module BenchIPLookup
  ( benchGroup,
    benchIPLookup,
    benchIPLookupFast,
  )
where

import Control.DeepSeq
import Criterion
import qualified Data
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fmt
import IPLookup
import IPLookupFast
import IPParse
import Utils

benchGroup :: Benchmark
benchGroup =
  bgroup
    "BenchIPLookup"
    [ benchIPLookup
    ]

iptexts :: [T.Text]
iptexts =
  [ "0.0.0.1",
    "192.168.1.1",
    "17.0.32.2",
    "255.255.252.41",
    "255.255.252.42"
  ]

ipRangeDB :: IO IPRangeDB
ipRangeDB = fromRight shouldNeverHappen . parseIPRangeDB <$> T.readFile p
  where
    p = snd $ last Data.iPRangeDBFiles

benchIPLookup :: Benchmark
benchIPLookup = benchIPLookupWith "lookupIP" id lookupIP

benchIPLookupFast :: Benchmark
benchIPLookupFast = benchIPLookupWith "lookupIPFast" toFastRangeDB lookupIPFast

benchIPLookupWith :: String -> (IPRangeDB -> db) -> (db -> IP -> Bool) -> Benchmark
benchIPLookupWith name getDB lkup =
  bgroup
    name
    [ 
      env ipRangeDB $ \rawdb ->
        let db = getDB rawdb
         in bgroup "single" $ (\(t, ip) -> bench ("ip:" +| t |+ "") (nf (lkup db) ip)) <$> ipInfo,
      env
        ipRangeDB
        ( \rawdb ->
            let db = getDB rawdb
             in bench "multiple" $ nf (lkup db . snd <$>) ipInfo
        )
    ]
  where
    ipInfo =
      let x = (\t -> (t, fromJust (parseIP t))) <$> iptexts
       in deepseq x x
