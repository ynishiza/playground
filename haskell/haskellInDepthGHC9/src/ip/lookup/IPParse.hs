{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module IPParse
  ( buildIP,
    buildIP_foldr,
    buildIP_foldl,
    buildIP_foldl_shl,
    parseIP,
    unparseIP,
    parseIPRange,
    parseIPRangeDB,
    parseIPString,
    parseIPRangeString,
    parseIPRangeDBString,
    unparseIPRange,
    validateIPRangeIO,
    module X,
  )
where

import Control.Applicative as X
import Control.Monad as X
import Data.Bits
import Data.List.Extra
import qualified Data.Text as T
import IPTypes as X
import Text.Read (readMaybe)

buildIP :: ByteSeq -> IP
buildIP = IP . serializeIP

parseIP :: T.Text -> Maybe IP
parseIP = parseIPText

parseIPRange :: T.Text -> Maybe IPRange
parseIPRange = parseIPRangeText

parseIPRangeDB :: T.Text -> Either ParserError IPRangeDB
parseIPRangeDB = parseIPRangeDBText

parseIPText :: T.Text -> Maybe IP
parseIPText t = do
  vals <- guarded (isLengthOf 4) $ T.splitOn "." t
  buildIP <$> mapM (parseByte . T.unpack) vals
  where
    parseByte = readMaybe @Integer >=> toIntegralSized

parseIPString :: String -> Maybe IP
parseIPString t = do
  vals <- guarded (isLengthOf 4) $ splitOn "." t
  buildIP <$> mapM parseByte vals
  where
    parseByte = readMaybe @Integer >=> toIntegralSized

{-# INLINE buildIP_foldr #-}
buildIP_foldr :: ByteSeq -> IP
buildIP_foldr = IP . fst . foldr go (0, 1)
  where
    go b (s, k) = (s + fromIntegral b * k, k * 256)

{-# INLINE buildIP_foldl #-}
buildIP_foldl :: ByteSeq -> IP
buildIP_foldl = IP . foldl (\s b -> s * 256 + fromIntegral b) 0

{-# INLINE buildIP_foldl_shl #-}
buildIP_foldl_shl :: ByteSeq -> IP
buildIP_foldl_shl = IP . foldl (\s b -> shiftL s 8 + fromIntegral b) 0

unparseIP :: IP -> T.Text
unparseIP = T.pack . show

guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded gd v = if gd v then pure v else empty

isLengthOf :: Int -> [a] -> Bool
isLengthOf 0 [] = True
isLengthOf 0 _ = False
isLengthOf l v
  | l < 0 = False
  | _ : rs <- v = isLengthOf (l -1) rs
  | otherwise = False

parseIPRangeText :: T.Text -> Maybe IPRange
parseIPRangeText t = do
  [ip1, ip2] <-
    guarded (isLengthOf 2) (T.splitOn "," t)
      >>= mapM parseIPText
  guard (ip1 <= ip2)
  return $ IPRange ip1 ip2

parseIPRangeString :: String -> Maybe IPRange
parseIPRangeString t = do
  [ip1, ip2] <-
    guarded (isLengthOf 2) (splitOn "," t)
      >>= mapM parseIPString
  guard (ip1 <= ip2)
  return $ IPRange ip1 ip2

validIPRange :: IP -> IP -> Bool
validIPRange = (<=)

validateIPRangeIO :: IPRange -> IO ()
validateIPRangeIO (IPRange ip1 ip2) = guard (ip1 `validIPRange` ip2)

unparseIPRange :: IPRange -> T.Text
unparseIPRange = T.pack . show

parseIPRangeDBText :: T.Text -> Either ParserError IPRangeDB
parseIPRangeDBText t = IPRangeDB <$> mapM m (zip (T.lines t) [0 ..])
  where
    m (t', i) = case parseIPRangeText t' of
      Just r -> Right r
      Nothing -> Left (ParserError i)

parseIPRangeDBString :: String -> Either ParserError IPRangeDB
parseIPRangeDBString t = IPRangeDB <$> mapM m (zip (lines t) [0 ..])
  where
    m (t', i) = case parseIPRangeString t' of
      Just r -> Right r
      Nothing -> Left (ParserError i)
