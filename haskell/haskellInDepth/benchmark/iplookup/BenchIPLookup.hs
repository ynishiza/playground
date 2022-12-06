{-# LANGUAGE OverloadedStrings #-}

module BenchIPLookup
  ( benchGroup,
    benchIPLookup,
  )
where

import Criterion
import qualified Data
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fmt
import IPLookup
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
benchIPLookup =
  bgroup
    "IPLookup"
    [ env ipRangeDB $ \db -> bgroup "single" $ (\ip -> bench ("ip:" +| ip |+ "") (nf (lookupWithDB db) ip)) <$> iptexts,
      env ipRangeDB (\db -> bench "multiple" $ nf (lookupWithDB db <$>) iptexts)
    ]
  where
    lookupWithDB db = lookupIP db . fromJust . parseIP
