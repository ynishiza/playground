module IPLookupFast
  ( toFastRangeDB,
    lookupIPFast,
  )
where

import Data.Foldable
import Data.IntervalMap.FingerTree
import IPTypes

type IPRangeDBFast = IntervalMap IP ()

toFastRangeDB :: IPRangeDB -> IPRangeDBFast
toFastRangeDB (IPRangeDB r) = foldl' (\m (IPRange i1 i2) -> insert (Interval i1 i2) () m) empty r

lookupIPFast :: IPRangeDBFast -> IP -> Bool
lookupIPFast m ip = not $ null $ search ip m
