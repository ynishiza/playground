module BenchBuildIP (fixIP, randomIPs, benchGroup) where

import Control.Monad
import Criterion
import IPParse
import System.Random

benchGroup :: Benchmark
benchGroup =
  bgroup
    "BenchBuildIP"
    [ fixIP,
      randomIPs
    ]

randomSeq :: IO ByteSeq
randomSeq = replicateM 4 (getStdRandom genWord8)

fixIP :: Benchmark
fixIP = createBenchGroup "buidIP: 17.0.32.3" test
  where
    test bd = whnf bd v
    v = [17, 0, 32, 3]

randomIPs :: Benchmark
randomIPs = createBenchGroup "build 10 random IP" test
  where
    test bd = nfIO $ do
      vs <- replicateM 10 randomSeq
      return $ bd <$> vs

createBenchGroup :: String -> ((ByteSeq -> IP) -> Benchmarkable) -> Benchmark
createBenchGroup name test =
  bgroup
    name
    [ bench "buildIP" (test buildIP),
      bench "buildIP_foldl" (test buildIP_foldl),
      bench "buildIP_foldr" (test buildIP_foldr),
      bench "buildIP_foldl_shl" (test buildIP_foldl_shl)
    ]
