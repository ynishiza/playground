{-# LANGUAGE OverloadedStrings #-}

module Properties
  ( properties,
    readPEquivalence,
  )
where

import Combinators
import Control.Applicative hiding (many, optional, some)
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.List (permutations)
import ParseNumber
import Debug.Trace
import Hedgehog
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as H
import Text.ParserCombinators.ReadP qualified as R

genChar :: Gen Char
genChar = H.alpha

genStringOfSize :: Int -> Int -> Gen Char -> Gen String
genStringOfSize l h = H.string (H.linear l h)

genString :: Gen String
genString = genStringOfSize 1 100 H.alpha

genIntInRange :: Int -> Int -> Gen Int
genIntInRange l h = fromIntegral <$> H.int (H.linear l h)

parseLast :: P a -> String -> (a, String)
parseLast p = last . parse p

parseReadP :: R.ReadP a -> ReadS a
parseReadP = R.readP_to_S

printDebug :: (Monad m, Show a) => a -> m ()
printDebug v = trace (show v <> "\n") pure ()

readpEqual :: (MonadTest m, Eq a, Show a) => P a -> R.ReadP a -> String -> m ()
readpEqual p1 p2 str = parse p1 str === parseReadP p2 str

testCase :: PropertyName -> PropertyT IO () -> (PropertyName, Property)
testCase name p = (name, property p)

properties :: Group
properties =
  Group
    "Number"
    [ testCase "Parses an integer" $ do
          n <- forAll $ genIntInRange (-10000) 10000
          annotate $ show $ parse int (show n)
          parseLast int (show n) === (n, ""),
      testCase "fails on non-int" $ do
          n <- forAll genString
          annotate $ show $ parse int (show n)
          parse int n === [],
      testCase "Parses a unary expression" $ do
          v1 <- forAll $ genIntInRange (-100) 100
          parseLast (unaryExp arithmeticUnaryOp int) (" -  " <> show v1)
            === (negate v1, ""),
      testCase "Parses a binary expression" $ do
          [v1, v2] <- forAll $ replicateM 2 $ genIntInRange (-100) 100
          parseLast (binaryExpr arithmeticBinaryOp int) (show v1 <> "+" <> show v2)
            === (v1 + v2, "")
          parseLast (binaryExpr arithmeticBinaryOp int) (show v1 <> "-" <> show v2)
            === (v1 - v2, "")
          parseLast (binaryExpr arithmeticBinaryOp int) (show v1 <> "*" <> show v2)
            === (v1 * v2, "")
    ]

readPEquivalence :: Group
readPEquivalence =
  Group
    "ReadP equivalence"
    [ testCase "[eof]" $ do
        str <- forAll $ genStringOfSize 0 10 H.ascii
        readpEqual @(PropertyT IO) eof R.eof str,
      testCase "[satisfy]" $ do
        c <- forAll genChar
        readpEqual (satisfy (== c)) (R.satisfy (== c)) $ c : "Hello"
        readpEqual (satisfy (== c)) (R.satisfy (== c)) "Hello",
      testCase "[<|>]" $ do
        str <- forAll $ genStringOfSize 0 10 H.ascii
        subset <- forAll $ H.subsequence str
        let cond = choice $ char <$> subset
            condP = R.choice $ R.char <$> subset
        readpEqual cond condP str,
      testCase "[gather]" $ do
        str <- forAll $ genStringOfSize 0 10 H.ascii
        subset <- forAll $ H.subsequence str
        readpEqual (gather (string subset)) (R.gather (R.string subset)) str,
      testCase "[string]" $ do
        str1 <- forAll $ genStringOfSize 1 20 H.alphaNum
        str2 <- forAll $ H.subsequence str1
        readpEqual (string str2) (R.string str2) ""
        readpEqual (string str2) (R.string str2) str2
        readpEqual (string str2) (R.string str2) str1,
      testCase "[munch]" $ do
        str <- forAll $ genStringOfSize 1 20 H.alphaNum
        substr <- forAll $ H.subsequence str
        readpEqual (munch (`elem` str)) (R.munch (`elem` str)) ""
        readpEqual (munch (`elem` substr)) (R.munch (`elem` substr)) str,
      testCase "[munch1]" $ do
        str <- forAll $ genStringOfSize 1 20 H.alphaNum
        substr <- forAll $ H.subsequence str
        readpEqual (munch1 (`elem` str)) (R.munch1 (`elem` str)) ""
        readpEqual (munch1 (`elem` substr)) (R.munch1 (`elem` substr)) str,
      testCase "[skipSpaces]" $ do
        str <- forAll $ H.subsequence $ ['a' .. 'd'] <> [' ', '\n', '\r']
        readpEqual skipSpaces R.skipSpaces str,
      testCase "[option]" $ do
        c <- forAll genString
        str <- forAll $ genStringOfSize 0 20 H.alphaNum
        substr <- forAll $ H.subsequence str
        readpEqual (option c (string substr)) (R.option c (R.string substr)) str,
      testCase "[optional]" $ do
        str <- forAll $ genStringOfSize 0 20 H.alphaNum
        substr <- forAll $ H.subsequence str
        let runTest = readpEqual (optional (string substr)) (R.optional (R.string substr)) 

        runTest str
        runTest ""
        runTest substr,
      testCase "[between]" $ do
        middle <- forAll $ genStringOfSize 0 10 H.alphaNum
        start <- forAll $ genStringOfSize 0 3 H.alphaNum
        end <- forAll $ genStringOfSize 0 3 H.alphaNum
        rest <- forAll $ genStringOfSize 0 10 H.alphaNum
        let 
          str = start <> middle <> end <> rest
          runTest = readpEqual 
            (between (string start) (string end) (string middle)) 
            (R.between (R.string start) (R.string end) (R.string middle))  

        runTest str
        runTest ""
        runTest start
        runTest end
        runTest middle
        ,
      testCase "[count]" $ do
        str <- forAll $ genStringOfSize 0 20 H.alphaNum
        substr <- forAll $ H.subsequence str
        n <- forAll $ genIntInRange 0 5
        readpEqual (count n (satisfy (`elem` substr))) (R.count n (R.satisfy (`elem` substr))) str,
      testCase "[many]" $ do
        c <- forAll genChar
        a <- forAll genChar
        let str = replicate 10 c <> [a]
        readpEqual (many $ char c) (R.many $ R.char c) str
        readpEqual (many1 $ char c) (R.many1 $ R.char c) str,
      testCase "[skipMany] empty" $ do
        c <- forAll genChar
        readpEqual (skipMany (char c)) (R.skipMany (R.char c)) "",
      testCase "[skipMany]" $ do
        c <- forAll genChar
        n <- forAll $ genIntInRange 0 100
        let str = replicate n c ++ "test"
        readpEqual (skipMany (char c)) (R.skipMany (R.char c)) str,
      testCase "[skipMany1]" $
        do
          c <- forAll genChar
          n <- forAll $ genIntInRange 0 100
          let str = replicate n c ++ "test"
          readpEqual (skipMany1 (char c)) (R.skipMany1 (R.char c)) str
    ]
