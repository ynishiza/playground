module IPLookup
  ( lookupIP,
  )
where

import Data.List (find)
import Data.Maybe (isJust)
import IPParse

inIPRange :: IPRange -> IP -> Bool
inIPRange (IPRange ip1 ip2) ip = ip1 <= ip && ip <= ip2


lookupIP :: IPRangeDB -> IP -> Bool
lookupIP (IPRangeDB rs) ip = isJust $ find (`inIPRange` ip) rs
