import Criterion.Main
import Criterion.Types
import Prime

pn :: Integer
pn = 16183

-- pn = 102982987

main :: IO ()
main =
  defaultMainWith
    (defaultConfig {reportFile = Just "bench.prime.html"})
    [ bench "isPrime (list)" (whnf isPrime pn),
      bench "isPrimeV2 (list)" (whnf isPrimeV2 pn),
      bench "isPrime" (whnf isPrime1 pn)
    ]
