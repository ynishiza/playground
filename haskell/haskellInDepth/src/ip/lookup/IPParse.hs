module IPParse
  ( buildIP,
    parseIP,
    unparseIP,
    parseIPRange,
    parseIPRangeDB,
    unparseIPRange,
    validateIPRangeIO,
    module X,
  )
where

import Control.Applicative as X
import Control.Monad as X
import Data.Bits
import qualified Data.Text as T
import IPTypes as X
import Text.Read (readMaybe)

buildIP :: ByteSeq -> IP
buildIP = IP . serializeIP

parseIP :: T.Text -> Maybe IP
parseIP t = do
  vals <- guarded (isLengthOf 4) $ T.splitOn "." t
  buildIP <$> mapM (parseByte . T.unpack) vals
  where
    parseByte = readMaybe @Integer >=> toIntegralSized

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

parseIPRange :: T.Text -> Maybe IPRange
parseIPRange t = do
  [ip1, ip2] <-
    guarded (isLengthOf 2) (T.splitOn "," t)
      >>= mapM parseIP
  guard (ip1 <= ip2)
  return $ IPRange ip1 ip2

validIPRange :: IP -> IP -> Bool
validIPRange = (<=)

validateIPRangeIO :: IPRange -> IO () 
validateIPRangeIO (IPRange ip1 ip2) = guard (ip1 `validIPRange` ip2) 

unparseIPRange :: IPRange -> T.Text
unparseIPRange = T.pack . show

parseIPRangeDB :: T.Text -> Either ParserError IPRangeDB
parseIPRangeDB t = IPRangeDB <$> mapM m (zip (T.lines t) [0 ..])
  where
    m (t', i) = case parseIPRange t' of
      Just r -> Right r
      Nothing -> Left (ParserError i)
