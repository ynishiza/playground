module Main (main) where

import qualified BenchBuildIP
import qualified BenchIPLookup
import qualified BenchParseIP
import Criterion.Main
import Criterion.Types

config :: Config
config = defaultConfig {reportFile = Just "bench.iplookup.html"}

main :: IO ()
main = do
  benchParse <- BenchParseIP.benchGroup
  defaultMainWith
    config
    [ BenchBuildIP.benchGroup,
      benchParse,
      BenchIPLookup.benchGroup
    ]
