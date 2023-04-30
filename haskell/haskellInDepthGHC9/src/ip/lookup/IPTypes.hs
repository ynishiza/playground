module IPTypes
  ( IP (..),
    IPRange (..),
    IPRangeDB (..),
    LineNumber,
    ParserError (..),
    Byte,
    ByteSeq,
    serializeIP,
    unserializeIP,
    ipModify,
    module X
  )
where

import Data.Foldable
import Data.Bits as X
import Data.List (intercalate)
import Data.Word as X
import Fmt
import Utils
import Control.DeepSeq

type LineNumber = Int

type Byte = Word8

type ByteSeq = [Byte]

newtype IP = IP {unIP :: Word32}
  deriving (Eq, Ord, Bounded)

instance NFData IP where
  rnf (IP p) = rnf p

instance NFData IPRange where
  rnf (IPRange ip1 ip2) = deepseq (rnf ip1) $ rnf ip2

instance NFData IPRangeDB where
  rnf (IPRangeDB r) = rnf r

serializeIP :: ByteSeq -> Word32
-- serializeIP = foldl' (\a x -> shiftb a 8 + fromIntegral x) 0
serializeIP v = sum $ zipWith shiftb v [24, 16, 8, 0]

shiftb :: (Integral a, Bits b, Integral b) => a -> Int -> b
shiftb x = shift (fromIntegral x)

unserializeIP :: Word32 -> ByteSeq
unserializeIP v0 = fromIntegral <$> [v4, v3, v2, v1]
  where
    v4 = extract 0
    v3 = extract 8
    v2 = extract 16
    v1 = extract 24
    extract v = shiftb @Word32 @Word32 (shiftb v0 v) (-24)

ipModify :: IP -> (Word32 -> Word32) -> IP
ipModify (IP v) f = IP (f v)

instance Show IP where
  show (IP v) = case unserializeIP v of
    a@[_, _, _, _] -> intercalate "." $ show <$> a
    _ -> shouldNeverHappen

instance Buildable IP where build = build . show

data IPRange = IPRange !IP !IP
  deriving (Eq)

instance Show IPRange where
  show (IPRange ip1 ip2) = ip1 |+ "," +| ip2 |+ ""

instance Ord IPRange where
  compare (IPRange (IP l1) (IP h1)) (IPRange (IP l2) (IP h2)) 
    | l1 /= l2 = compare l1 l2
    | otherwise = compare (h1 - l1) (h2 - l2)

instance Buildable IPRange where build = build . show

newtype IPRangeDB = IPRangeDB [IPRange]
  deriving (Eq, Show)

instance Buildable IPRangeDB where build = build . show

newtype ParserError = ParserError LineNumber
  deriving (Eq, Show)
