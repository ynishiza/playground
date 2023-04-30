module Properties (group) where

import Gen
import Hedgehog
import qualified Hedgehog.Gen as G
import IPLookup
import IPLookupFast

group :: Group
group =
  Group
    "IP properties"
    [ ( "buildIP variants",
        property $ do
          ipSeq <- forAll generateByteSeq
          buildIP ipSeq === buildIP_foldl ipSeq
          buildIP ipSeq === buildIP_foldr ipSeq
          buildIP ipSeq === buildIP_foldl_shl ipSeq
      ),
      ( "Parse IP",
        property $ do
          ip <- forAll generateIP
          tripping ip unparseIP parseIP
      ),
      ( "Parse IPRange",
        property $ do
          ipr@(IPRange ip1 ip2) <- forAll generateIPRange
          assert (ip1 <= ip2)
          tripping ipr unparseIPRange parseIPRange
      ),
      ( "Parse IPRange failure with invalid range",
        property $ do
          ipr <- forAll generateInvalidIPRange
          parseIPRange (unparseIPRange ipr) === Nothing
      ),
      ( "lookup IP",
        property $ do
          ipr@(IPRange ip1 ip2) <- forAll $ G.filter (\(IPRange l u) -> l > minBound && u < maxBound) generateIPRange
          let db = IPRangeDB [ipr]
          ipGood <- forAll $ generateIPLinear ip1 ip2
          ipLow <- forAll $ generateIPLinear minBound (ipModify ip1 (subtract 1))
          ipHigh <- forAll $ generateIPLinear (ipModify ip2 (+ 1)) maxBound
          assert $ lookupIP db ipGood
          assert $ not $ lookupIP db ipLow
          assert $ not $ lookupIP db ipHigh
      ),
      ( "lookup IP boundaries",
        property $ do
          db@(IPRangeDB l) <- forAll generateIPRangeDB
          (IPRange ip1 ip2) <- forAll $ G.element l
          lookupIP db ip1 === True
          lookupIP db ip2 === True
      ),
      ( "lookup IP fast == lookup IP",
        property $ do
          db <- forAll generateIPRangeDB
          let dbFast = toFastRangeDB db
          ip <- forAll generateIP
          lookupIP db ip === lookupIPFast dbFast ip
      )
    ]
