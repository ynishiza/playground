module Gen
  ( generateByte,
    generateByteSeq,
    generateIP,
    generateIPBetween,
    generateIPRange,
    generateInvalidIPRange,
    generateIPRangeDBOf,
    generateIPRangeDB,
    module X,
  )
where

import Control.Monad
import Data.Function
import Data.Functor
import Hedgehog
import Hedgehog.Gen qualified as G hiding (constant)
import Hedgehog.Range qualified as G
import IPParse as X

generateByte :: Gen Word8
generateByte = G.word8 G.constantBounded

generateByteSeq :: Gen ByteSeq
generateByteSeq = G.list (G.singleton 4) generateByte

generateIPIn :: Integral a => Range a -> Gen IP
generateIPIn range = IP <$> G.word32 (fromIntegral <$> range)

generateIP :: Gen IP
generateIP =
  [0 .. 31]
    <&> (\x -> generateIPIn @Word32 (G.constant (2 ^ x - 1) (2 ^ (x + 1))))
    & G.choice

generateIPBetween :: IP -> IP -> Gen IP
generateIPBetween (IP l) (IP h) = do
  guard (l <= h)
  generateIPIn (G.constant l h)

generateIPRange :: Gen IPRange
generateIPRange = do
  rangeSize <- G.word32 (G.exponential 1 (maxBound - 1))
  ipLow <- generateIPIn (G.exponential 0 (maxBound - rangeSize))
  return $ IPRange ipLow (ipLow + IP rangeSize)

generateInvalidIPRange :: Gen IPRange
generateInvalidIPRange = do
  ipLow <- G.filter (> minBound) generateIP
  IPRange ipLow <$> generateIPIn (G.constant minBound (ipLow - 1))

generateIPRangeDB :: Gen IPRangeDB
generateIPRangeDB = generateIPRangeDBOf 50 10 100

generateIPRangeDBOf :: Int -> Int -> Int -> Gen IPRangeDB
generateIPRangeDBOf o l u = IPRangeDB <$> G.list (G.linearFrom o l u) generateIPRange
