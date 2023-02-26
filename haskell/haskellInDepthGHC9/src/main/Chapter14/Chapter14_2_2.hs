{-# LANGUAGE OverloadedStrings #-}

module Chapter14.Chapter14_2_2
  ( test,
    textField,
    record,
    csvFile,
    csvFilev2,
    readCsv,
  )
where

import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as PB
import Data.Attoparsec.ByteString.Char8 qualified as PC
import Data.ByteString.Char8 (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Streaming.Prelude qualified as S
import Streaming (Stream, Of)
import Streaming.ByteString qualified as SB
import Data.Attoparsec.ByteString.Streaming qualified as SB
import Control.Monad.Trans.Resource
import Data.Functor
import Data.Function

type Field = Text

rawField :: Parser ByteString
rawField = PB.takeWhile $ \c -> B.all (c /=) ",\n\r"

textField :: Parser Field
textField = T.decodeUtf8 <$> rawField

record :: Parser [Field]
record = textField `PC.sepBy1'` PC.char ','

csvFile :: Parser [[Field]]
csvFile = do
  res <- record `PC.sepBy1'` PC.endOfLine 
  PC.endOfInput
  return res

csvFilev2 :: Parser [[Field]]
csvFilev2 = (:) <$> record <*> PC.manyTill (PC.endOfLine >> record) PC.endOfInput

readCsv :: MonadResource m => FilePath -> Stream (Of [[Text]]) m (Either (SB.Message, SB.ByteStream m ()) ())
readCsv file = SB.parsed csvFile (SB.readFile file)

sampleCsv :: FilePath
sampleCsv = "./src/data/quotes.csv"

test :: IO ()
test = do
  runResourceT $ 
    readCsv sampleCsv
    & void
    & S.print
  -- runResourceT $ 
  --   SB.readFile sampleCsv
  --   & SB.parsed csvFile
  --   & void
  --   & S.print

