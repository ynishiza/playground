module Main (main) where

import qualified BenchBuildIP
import qualified BenchParseIP
import Criterion.Main
import Criterion.Types

main :: IO ()
main =
  sequence
    [ BenchParseIP.benchFileRead,
      BenchParseIP.benchParseIPRangeDBMany,
      BenchParseIP.benchParseIPRangeDBManyBad
    ]
    >>= defaultMainWith
      c
      . ( [ BenchBuildIP.fixIP,
            BenchBuildIP.randomIPs,
            BenchParseIP.benchParseIP
          ]
            ++
        )
  where
    c = defaultConfig {reportFile = Just "bench.iplookup.html"}
