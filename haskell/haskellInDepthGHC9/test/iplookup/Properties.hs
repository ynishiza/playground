{-# LANGUAGE TemplateHaskell #-}

module Properties (group) where

import Control.Lens
import GHC.Exts (fromString)
import Gen
import Hedgehog
import Hedgehog.Gen qualified as G
import Hedgehog.Internal.Property (CoverPercentage)
import IPLookup
import IPLookupFast

$(makeLensesFor [("groupProperties", "_groupProperties")] 'Group)

numTests :: TestLimit
numTests = 1000

-- numTests = 100

toLabelName :: Show a => a -> LabelName
toLabelName = fromString . show

group :: Group
group =
  $$discover
    & over (_groupProperties . each . _2) (withTests numTests)

-- Group
--   "IP properties"
--   [
prop_buildIPVariants :: Property
prop_buildIPVariants = property $ do
  ipSeq <- forAll generateByteSeq
  buildIP ipSeq === buildIP_foldl ipSeq
  buildIP ipSeq === buildIP_foldr ipSeq
  buildIP ipSeq === buildIP_foldl_shl ipSeq

prop_parseIP :: Property
prop_parseIP = property $ do
  ip <- forAll generateIP
  coversIPs "IP" ip
  tripping ip unparseIP parseIP

prop_parseIPRange :: Property
prop_parseIPRange = property $ do
  ipr@(IPRange ip1 ip2) <- forAll generateIPRange
  assert (ip1 <= ip2)
  coversIPRange ipr
  tripping ipr unparseIPRange parseIPRange

prop_parseIPRangeFailureWithInvalid_range :: Property
prop_parseIPRangeFailureWithInvalid_range = property $ do
  ipr@(IPRange ip1 ip2) <- forAll generateInvalidIPRange
  coversIPs "ip1" ip1
  coversIPs "ip2" ip2
  parseIPRange (unparseIPRange ipr) === Nothing

prop_lookupIP :: Property
prop_lookupIP = property $ do
  ipr@(IPRange ip1 ip2) <- forAll $ G.filter (\(IPRange l u) -> l > minBound && u < maxBound) generateIPRange
  let db = IPRangeDB [ipr]
  ipGood <- forAll $ generateIPBetween ip1 ip2
  ipLow <- forAll $ generateIPBetween minBound (ipModify ip1 (subtract 1))
  ipHigh <- forAll $ generateIPBetween (ipModify ip2 (+ 1)) maxBound
  coversIPs "ipGood" ipGood
  coversIPRange ipr
  assert $ lookupIP db ipGood
  assert $ not $ lookupIP db ipLow
  assert $ not $ lookupIP db ipHigh

prop_lookupIPBoundaries :: Property
prop_lookupIPBoundaries = property $ do
  db@(IPRangeDB l) <- forAll generateIPRangeDB
  ipr@(IPRange ip1 ip2) <- forAll $ G.element l
  coversIPRange ipr
  assert $ lookupIP db ip1
  assert $ lookupIP db ip2

prop_fastLookupIP :: Property
prop_fastLookupIP = property $ do
  db <- forAll generateIPRangeDB
  let dbFast = toFastRangeDB db
  ip <- forAll generateIP
  coversIPs "ip" ip
  lookupIP db ip === lookupIPFast dbFast ip

coversIPRange :: IPRange -> PropertyT IO ()
coversIPRange (IPRange ip1 ip2) = do
  let smallRange = 2 ^ 8
      largeRange = 2 ^ 24
      smallBound = IP smallRange
      largeBound = IP largeRange
      rangeSize = fromIntegral $ ip2 - ip1
  cover 5 ("small range: #IPs < " <> toLabelName smallRange) $ rangeSize < smallRange
  cover 5 ("large range: #IPs > " <> toLabelName largeRange) $ rangeSize > largeRange
  cover 5 ("small lower bound: IP < " <> toLabelName smallBound) $ ip1 < smallBound
  cover 5 ("large upper bound: IP > " <> toLabelName largeBound) $ ip2 > largeBound

coversIPs :: String -> IP -> PropertyT IO ()
coversIPs name = coversIPs_ (name, 10, 10, 10, 10)

coversIPs_ :: (String, CoverPercentage, CoverPercentage, CoverPercentage, CoverPercentage) -> IP -> PropertyT IO ()
coversIPs_ (name, e1, e2, e3, e4) ip = do
  let v0 = minBound
      v1 = IP $ 2 ^ 8
      v2 = IP $ 2 ^ 16
      v3 = IP $ 2 ^ 24
      v4 = maxBound
  cover e1 (fromString name <> ":" <> toLabelName v0 <> " ~ " <> toLabelName v1) $ v0 < ip && ip < v1
  cover e2 (fromString name <> ":" <> toLabelName v1 <> " ~ " <> toLabelName v2) $ v1 < ip && ip < v2
  cover e3 (fromString name <> ":" <> toLabelName v2 <> " ~ " <> toLabelName v3) $ v2 < ip && ip < v3
  cover e4 (fromString name <> ":" <> toLabelName v3 <> " ~ " <> toLabelName v4) $ v3 < ip && ip < v4
