module Gen
  ( 
    midIP,
    generateByte,
    generateByteSeq,
    generateIP,
    generateIPLinearFrom,
    generateIPLinear,
    generateIPRange,
    generateInvalidIPRange,
    generateIPRangeDBOf,
    generateIPRangeDB,
    module X,
  )
where

import Control.Monad
import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as G
import IPParse as X

generateByte :: Gen Word8
generateByte = G.word8 G.linearBounded

generateByteSeq :: Gen ByteSeq
generateByteSeq = G.list (G.singleton 4) generateByte

ipMidpoint :: IP -> IP -> IP
ipMidpoint (IP l) (IP u) = IP ((u - l) `div` 2)

midIP :: IP
midIP = ipMidpoint minBound maxBound

generateIP :: Gen IP
generateIP = generateIPLinearFrom midIP minBound maxBound

generateIPLinear:: IP -> IP -> Gen IP
generateIPLinear ipl ipu = generateIPLinearFrom (ipMidpoint ipl ipu) ipl ipu

generateIPLinearFrom :: IP -> IP -> IP -> Gen IP
generateIPLinearFrom (IP o) (IP l) (IP h) =
  guard (l <= h)
    >> IP <$> G.word32 (G.linearFrom o l h)

generateIPRange :: Gen IPRange
generateIPRange = do
  ip1 <- generateIP
  ip2 <- generateIPLinearFrom (ipModify ip1 (+1)) ip1 maxBound
  return $ IPRange ip1 ip2

generateInvalidIPRange :: Gen IPRange
generateInvalidIPRange = do
  ip1 <- G.filter (> minBound) generateIP
  let u = ipModify ip1 (subtract 1)
  ip2 <- generateIPLinearFrom u minBound u
  return $ IPRange ip1 ip2

generateIPRangeDB :: Gen IPRangeDB
generateIPRangeDB = generateIPRangeDBOf 50 10 100

generateIPRangeDBOf :: Int -> Int -> Int -> Gen IPRangeDB
generateIPRangeDBOf o l u = IPRangeDB <$> G.list (G.linearFrom o l u) generateIPRange

