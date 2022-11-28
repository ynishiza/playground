module IPTypes
  ( IP (..),
    IPRange (..),
    IPRangeDB (..),
    LineNumber,
    ParserError (..),
    Byte,
    ByteSeq,
    minIP,
    maxIP,
    serializeIP,
    unserializeIP,
  )
where

import Data.Bits
import Data.List (intercalate)
import Data.Word
import Fmt
import Utils

minIP :: IP
minIP = IP minBound

maxIP :: IP
maxIP = IP maxBound

type LineNumber = Int

type Byte = Word8

type ByteSeq = [Byte]

newtype IP = IP {unIP :: Word32}
  deriving (Eq, Ord)

serializeIP :: ByteSeq -> Word32
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
    -- v4 = s' v0 (-24)
    -- v3 = s' (s' v0 8) (-24)
    -- v2 = s' (s' v0 16) (-24)
    -- v1 = s' (s' v0 24) (-24)
    -- s' :: Word32 -> Int -> Word32
    -- s' = shiftb
    extract v = shiftb @Word32 @Word32 (shiftb v0 v) (-24)

instance Show IP where
  show (IP v) = case unserializeIP v of
    a@[_, _, _, _] -> intercalate "." $ show <$> a
    _ -> shouldNeverHappen

instance Buildable IP where build = build . show

data IPRange = IPRange !IP !IP
  deriving (Eq)

instance Show IPRange where
  show (IPRange ip1 ip2) = ip1 |+ "," +| ip2 |+ ""

instance Buildable IPRange where build = build . show

newtype IPRangeDB = IPRangeDB [IPRange]
  deriving (Eq, Show)

instance Buildable IPRangeDB where build = build . show

newtype ParserError = ParserError LineNumber
  deriving (Eq, Show)
