{-# LANGUAGE OverloadedStrings #-}
import QuoteData

import qualified Data.Csv as Csv
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Colonnade as C
import System.Environment
import Fmt

main :: IO ()
main = do
  args <- getArgs
  quotes <- readCsvFile (head args)
  fmtLn $ build $ toTable $ V.take 10 quotes

readCsvFile :: FilePath -> IO (Vector QuoteData)
readCsvFile filename = do
  text <- BSL.readFile filename
  let 
    parsed :: Either String (Csv.Header, Vector QuoteData)
    parsed = Csv.decodeByName text
    in 
      case parsed of
        Left e -> error e
        Right v -> return $ snd v

colQuoteData :: C.Colonnade C.Headed QuoteData String
colQuoteData = mconcat [
  C.headed "Day" (show.day),
  C.headed "Volume" (show.volume),
  C.headed "Open" (show.open),
  C.headed "High" (show.high),
  C.headed "Low" (show.low),
  C.headed "Close" (show.close)
                       ]

toTable :: Vector QuoteData -> Text
toTable qd = T.pack $ C.ascii colQuoteData qd
