{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Chapter1
  ( run,
  )
where


import Control.Monad
import Data.Char
import Data.Ord
import Data.List(group,sortOn,sort)
import Data.Maybe
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt
import System.Environment
import Utils

run :: IO ()
run = do
  scratchSpace

  putStrLn "Chapter1"
  task1

task1 :: IO ()
task1 = do
  args <- getArgs
  case args of
    ["-a", filename, n] ->
      process filename True (read n)
    [filename, n] ->
      process filename False (read n)
    _ ->
      error "[-a] FILEPATH TOPN"

process :: FilePath -> Bool -> Int -> IO ()
process filename optAll n = do
  text <- TIO.readFile filename

  let vocab = extractVocab text
  TIO.putStrLn $ wordsCountReport vocab
  -- _ <- getChar
  TIO.putStrLn $ wordsByFrequencyReportN vocab n
  -- _ <- getChar
  when optAll $ do
    TIO.putStrLn $ allWordsReport vocab
    -- _ <- getChar
    pure ()

scratchSpace :: IO ()
scratchSpace = do
  pure ()


filterJust :: [Maybe a] -> [a]
filterJust list = fromJust <$> filter isJust list

cleanseWord :: T.Text -> Maybe T.Text
cleanseWord w
  | T.null w' = Nothing
  | otherwise = Just w'
  where
    w' = T.toCaseFold $ T.takeWhile isLetter w

type Entry = (T.Text, Int)
type Vocabulary = [Entry]

extractVocab :: T.Text -> Vocabulary
extractVocab text = toEntry <$> group cleansedWords
    where
      cleansedWords = sort $ filterJust $ cleanseWord <$> T.words text
      toEntry xs@(x:_) = (x, length xs)


allWords :: Vocabulary -> [Text]
allWords = (fst<$>)

wordsCount :: Vocabulary -> (Int, Int)
wordsCount vocabs = (sum $ snd <$> vocabs, length vocabs)

wordsByFrequency :: Vocabulary -> Vocabulary
wordsByFrequency = sortOn (Down . snd)
-- wordsByFrequency = reverse . sortOn snd 
--
--
allWordsReport :: Vocabulary -> Text
allWordsReport vocab = fmt $ nameF "all words:" $ unlinesF (allWords vocab)

wordsCountReport :: Vocabulary -> Text
wordsCountReport vocab = fmt $
    "Total number of words: "+|total|+
    "\nTotal number of unique words: "+|unique|+""
  where
    (total, unique) = wordsCount vocab

wordsByFrequencyReportN :: Vocabulary -> Int -> Text
wordsByFrequencyReportN vocab n = wordsByFrequencyReportUpto vocab (\(_, i) -> i < n)

wordsByFrequencyReportUpto :: Vocabulary -> ((Entry, Int) -> Bool) -> Text
wordsByFrequencyReportUpto vocab cond = printFrequencyReport $ fst <$> subset
  where
    subset = takeWhile cond $ zip (wordsByFrequency vocab) [0..]

printFrequencyReport :: Vocabulary -> Text
printFrequencyReport vocab = fmt $ nameF "frequency" formatted
  where
    formatted = blockListF' "*" blockEntry vocab
    blockEntry (word, count) = word|+":"+|count|+""

