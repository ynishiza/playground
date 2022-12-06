module Main (main) where

import qualified BenchBuildIP
import qualified BenchParseIP
import Criterion.Main
import Criterion.Types

main :: IO ()
main = do
  manyDB <- BenchParseIP.benchParseIPRangeDBMany
  defaultMainWith
    (defaultConfig {reportFile = Just "bench.iplookup.html"})
    [ BenchBuildIP.fixIP,
      BenchBuildIP.randomIPs,
      BenchParseIP.benchParseIP,
      manyDB
    ]
