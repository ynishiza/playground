{-# LANGUAGE OverloadedStrings #-}

{- ORMOLU_DISABLE -}
module Properties
  ( properties,
    readPEquivalence,

    -- Export to avoid diagnostic warning
    genSmallAlphaNum,
    genSmallAlphaNum0,
    genMediumAlphaNum,
    genMediumAlphaNum0,
    genLargeAlphaNum,
    genLargeAlphaNum0,
    printDebug,
  )
where
{- ORMOLU_ENABLE -}

import Combinators
import Control.Monad
import Data.List (intercalate)
import Debug.Trace
import Hedgehog
import Hedgehog.Gen qualified as H
import Hedgehog.Range qualified as H
import ParseBasic qualified as B
import ReadPBasic qualified as R
import Text.ParserCombinators.ReadP qualified as R

genChar :: Gen Char
genChar = H.alpha

genStringOfSize :: Int -> Int -> Gen Char -> Gen String
genStringOfSize l h = H.string (H.linear l h)

genString :: Gen String
genString = genStringOfSize 1 100 H.alpha

genSmallAlphaNum :: Gen String
genSmallAlphaNum = genStringOfSize 1 5 H.alphaNum

genSmallAlphaNum0 :: Gen String
genSmallAlphaNum0 = genStringOfSize 0 5 H.alphaNum

genMediumAlphaNum :: Gen String
genMediumAlphaNum = genStringOfSize 1 20 H.alphaNum

genMediumAlphaNum0 :: Gen String
genMediumAlphaNum0 = genStringOfSize 0 20 H.alphaNum

genLargeAlphaNum :: Gen String
genLargeAlphaNum = genStringOfSize 1 100 H.alphaNum

genLargeAlphaNum0 :: Gen String
genLargeAlphaNum0 = genStringOfSize 0 100 H.alphaNum

genIntInRange :: Int -> Int -> Gen Int
genIntInRange l h = H.integral (H.linear l h)

genRest :: Gen String
genRest = genStringOfSize 0 10 (H.element ['!', '@', '#', '$', '%', '^', '&', '*'])

genReplicate :: Gen Int -> Gen String -> Gen (String, Int, String)
genReplicate pn pvalue = do
  (v, n, _, s) <- genReplicateWithSeparator pn pvalue (return "")
  return (v, n, s)

-- Output
--   (
--    String,         Replicated term
--    Int,            Number of replications
--    String,         Delimiter
--    String          Terms joined by delimiter
--   )
--
--  e.g.
--    ("a", 3, "|", "a|a|a")
--
genReplicateWithSeparator :: Gen Int -> Gen String -> Gen String -> Gen (String, Int, String, String)
genReplicateWithSeparator pn pval pdelim = do
  v <- pval
  (vs, delim, res) <- genDelimitedText pn (\n -> return $ replicate n v) pdelim
  return (v, length vs, delim, res)

-- Input
--   Gen Int                  Number of terms
--   Int -> Gen [String]      List of terms
--   Gen String               Delimiter
-- Output
--   (
--    [String],               List of terms
--    String,                 Delimiter
--    String                  Terms joined by delimiter
--   )
--
-- e.g.
--      (["a", "b", "c"], "|", "a|b|c")
--
genDelimitedText :: Gen Int -> (Int -> Gen [String]) -> Gen String -> Gen ([String], String, String)
genDelimitedText pn pvals pdelim = do
  n <- pn
  vals <- pvals n
  delim <- pdelim
  return
    ( vals,
      delim,
      intercalate delim vals
    )

parseLast :: P a -> String -> (a, String)
parseLast p = last . parse p

parseReadP :: R.ReadP a -> ReadS a
parseReadP = R.readP_to_S

printDebug :: (Monad m, Show a) => a -> m ()
printDebug v = trace (show v <> "\n") pure ()

readpEqual :: (MonadTest m, Eq a, Show a) => P a -> R.ReadP a -> String -> m ()
readpEqual p1 p2 toParse = do
  annotate $ "test string:" <> toParse
  parse p1 toParse === parseReadP p2 toParse

readpEqualWithNResults :: (MonadTest m, Eq a, Show a) => Int -> P a -> R.ReadP a -> String -> m ()
readpEqualWithNResults n p1 p2 toParse = do
  readpEqual p1 p2 toParse
  annotate $
    "test string:"
      <> toParse
      <> "\n"
      <> "result:"
      <> show (parse p1 toParse)
  length (parse p1 toParse) === n

testCase :: PropertyName -> PropertyT IO () -> (PropertyName, Property)
testCase name p = (name, property p)

properties :: Group
properties =
  Group
    "Number"
    [ --
      testCase "Parses an integer" $ do
        n <- forAll $ genIntInRange (-10000) 10000
        annotate $ show $ parse (B.integral @Int) (show n)
        parseLast B.integral (show n) === (n, ""),
      --
      testCase "fails on non-integral" $ do
        n <- forAll genString
        annotate $ show $ parse (B.integral @Int) (show n)
        parse (B.integral @Int) n === [],
      --
      testCase "Parses a unary expression" $ do
        v1 <- forAll $ genIntInRange (-100) 100
        parseLast (B.unaryExp B.arithmeticUnaryOp B.integral) (" -  " <> show v1)
          === (negate v1, ""),
      --
      testCase "Parses a binary expression" $ do
        [v1, v2] <- forAll $ replicateM 2 $ genIntInRange (-100) 100
        parseLast (B.binaryExpr B.arithmeticBinaryOp B.integral) (show v1 <> "+" <> show v2)
          === (v1 + v2, "")
        parseLast (B.binaryExpr B.arithmeticBinaryOp B.integral) (show v1 <> "-" <> show v2)
          === (v1 - v2, "")
        parseLast (B.binaryExpr B.arithmeticBinaryOp B.integral) (show v1 <> "*" <> show v2)
          === (v1 * v2, "")
    ]

readPEquivalence :: Group
readPEquivalence =
  Group
    "ReadP equivalence"
    [ testCase "[eof]" $ do
        toParse <- forAll $ genStringOfSize 0 10 H.ascii
        let n = if null toParse then 1 else 0
        readpEqualWithNResults @(PropertyT IO) n eof R.eof toParse,
      --
      testCase "[satisfy]" $ do
        toMatch <- forAll genChar
        let runTest =
              readpEqual
                (satisfy (== toMatch))
                (R.satisfy (== toMatch))
        runTest $ toMatch : "Hello"
        runTest "Hello",
      --
      testCase "[<|>]" $ do
        toParse <- forAll $ genStringOfSize 0 10 H.ascii
        subset <- forAll $ H.subsequence toParse
        let cond = choice $ char <$> subset
            condP = R.choice $ R.char <$> subset
        readpEqual cond condP toParse,
      --
      testCase "[gather]" $ do
        toParse <- forAll $ genStringOfSize 0 10 H.ascii
        subset <- forAll $ H.subsequence toParse
        readpEqual
          (gather (string subset))
          (R.gather (R.string subset))
          toParse,
      --
      testCase "[string]" $ do
        toParse <- forAll genLargeAlphaNum0
        toMatch <- forAll $ H.subsequence toParse
        let runTest =
              readpEqual
                (string toMatch)
                (R.string toMatch)
        runTest ""
        runTest toMatch
        runTest toParse,
      --
      testCase "[munch,munch1]" $ do
        rest <- forAll genLargeAlphaNum0
        (pattern, _, str) <-
          forAll $
            genReplicate
              (genIntInRange 0 10)
              genSmallAlphaNum
        let toParse = str <> rest
            runTest =
              readpEqual
                (munch (`elem` pattern))
                (R.munch (`elem` pattern))
            runTest1 =
              readpEqual
                (munch1 (`elem` toParse))
                (R.munch1 (`elem` toParse))

        runTest toParse
        runTest1 toParse,
      --
      testCase "[skipSpaces]" $ do
        toParse <- forAll $ H.subsequence $ ['a' .. 'd'] <> [' ', '\n', '\r']
        let runTest = readpEqual skipSpaces R.skipSpaces
        runTest toParse,
      --
      testCase "[option]" $ do
        c <- forAll genString
        toParse <- forAll genLargeAlphaNum0
        substr <- forAll $ H.subsequence toParse
        let runTest =
              readpEqual
                (option c (string substr))
                (R.option c (R.string substr))
        runTest toParse,
      --
      testCase "[optional]" $ do
        str <- forAll genLargeAlphaNum0
        substr <- forAll $ H.subsequence str
        let runTest =
              readpEqual
                (optional (string substr))
                (R.optional (R.string substr))

        runTest str
        runTest ""
        runTest substr,
      --
      testCase "[between]" $ do
        middle <- forAll genLargeAlphaNum0
        start <- forAll genSmallAlphaNum
        end <- forAll genSmallAlphaNum
        rest <- forAll $ genStringOfSize 0 10 H.alphaNum
        let str = start <> middle <> end <> rest
            runTest =
              readpEqual
                (between (string start) (string end) (string middle))
                (R.between (R.string start) (R.string end) (R.string middle))

        runTest str
        runTest ""
        runTest start
        runTest end
        runTest middle,
      --
      testCase "[count]" $ do
        toParse <- forAll genMediumAlphaNum0
        substr <- forAll $ H.subsequence toParse
        n <- forAll $ genIntInRange 0 5
        let runTest =
              readpEqual
                (count n (satisfy (`elem` substr)))
                (R.count n (R.satisfy (`elem` substr)))
        runTest toParse,
      --
      testCase "[many, many1]" $ do
        (pattern, n, str) <-
          forAll $
            genReplicate
              (genIntInRange 0 10)
              genSmallAlphaNum
        rest <- forAll genRest
        let toParse = str <> rest
            runTest0 =
              readpEqualWithNResults
                (if n == 0 then 1 else n + 1)
                (many $ string pattern)
                (R.many $ R.string pattern)
            runTest1 =
              readpEqualWithNResults
                n
                (many1 $ string pattern)
                (R.many1 $ R.string pattern)
        runTest0 toParse
        runTest1 toParse,
      --
      testCase "[many', many1']" $ do
        (pattern, n, str) <-
          forAll $
            genReplicate
              (genIntInRange 0 10)
              genSmallAlphaNum
        rest <- forAll genRest
        let toParse = str <> rest
            runTest0' =
              readpEqualWithNResults
                (if n == 0 then 1 else n + 1)
                (many' $ string pattern)
                (R.many $ R.string pattern)
            runTest1' =
              readpEqualWithNResults
                n
                (many1' $ string pattern)
                (R.many1 $ R.string pattern)
        runTest0' toParse
        runTest1' toParse,
      --
      testCase "[skipMany] empty" $ do
        c <- forAll genChar
        readpEqual (skipMany (char c)) (R.skipMany (R.char c)) "",
      --
      --
      testCase "[skipMany1]" $ do
        rest <- forAll genRest
        (pattern, n, str) <-
          forAll $
            genReplicate
              (genIntInRange 0 10)
              genSmallAlphaNum
        let toParse = str <> rest
            runTest =
              readpEqualWithNResults
                (if n == 0 then 1 else n + 1)
                (skipMany (string pattern))
                (R.skipMany (R.string pattern))
            runTest1 =
              readpEqualWithNResults
                n
                (skipMany1 (string pattern))
                (R.skipMany1 (R.string pattern))
        runTest toParse
        runTest1 toParse,
      --
      testCase "[sepBy, sepBy1]" $ do
        rest <- forAll genRest
        (pattern, n, sepc, str) <-
          forAll $
            genReplicateWithSeparator
              (genIntInRange 0 10)
              genSmallAlphaNum
              (H.element ["|", "_", ":", ";"])
        let toParse = str <> rest
            runTest =
              readpEqualWithNResults
                (if n == 0 then 1 else n + 1)
                (sepBy (string pattern) (string sepc))
                (R.sepBy (R.string pattern) (R.string sepc))
            runTest1 =
              readpEqualWithNResults
                (if n == 0 then 0 else n)
                (sepBy1 (string pattern) (string sepc))
                (R.sepBy1 (R.string pattern) (R.string sepc))

        runTest toParse
        runTest1 toParse,
      testCase "[endBy, endBy1]" $ do
        rest <- forAll genRest
        (pattern, n, delim, str) <-
          forAll $
            genReplicateWithSeparator
              (genIntInRange 0 10)
              genSmallAlphaNum
              (H.element [";", ":"])
        let toParse = str <> rest
            runTest =
              readpEqualWithNResults
                (if n == 0 then 1 else n)
                (endBy (string pattern) (string delim))
                (R.endBy (R.string pattern) (R.string delim))
            runTest1 =
              readpEqualWithNResults
                (if n == 0 then 0 else n - 1)
                (endBy1 (string pattern) (string delim))
                (R.endBy1 (R.string pattern) (R.string delim))

        runTest toParse
        runTest1 toParse,
      testCase "[chainr]" $ do
        (vals, _, toParse) <-
          forAll $
            genDelimitedText
              (genIntInRange 0 10)
              (`replicateM` (show <$> genIntInRange 0 9))
              (return ",")
        let n = length vals
            runTest =
              readpEqualWithNResults
                (if n == 0 then 1 else n + 1)
                (chainr (show <$> B.integral @Int) B.pbracket "#")
                (R.chainr (show <$> R.int @Int) R.bracket "#")
            runTest1 =
              readpEqualWithNResults
                n
                (chainr1 (show <$> B.integral @Int) B.pbracket)
                (R.chainr1 (show <$> R.int @Int) R.bracket)
        runTest toParse
        runTest1 toParse,
      --
      testCase "[chainl]" $ do
        (vals, _, toParse) <-
          forAll $
            genDelimitedText
              (genIntInRange 0 10)
              (`replicateM` (show <$> genIntInRange 0 9))
              (return ",")
        let n = length vals
            runTest =
              readpEqualWithNResults
                (if n == 0 then 1 else n + 1)
                (chainl (show <$> B.integral @Int) B.pbracket "#")
                (R.chainl (show <$> R.int @Int) R.bracket "#")
            runTest1 =
              readpEqualWithNResults
                n
                (chainl1 (show <$> B.integral @Int) B.pbracket)
                (R.chainl1 (show <$> R.int @Int) R.bracket)
        runTest toParse
        runTest1 toParse,
      testCase "[manyTill]" $ do
        (value, _, str) <-
          forAll $
            genReplicate
              (genIntInRange 0 10)
              genSmallAlphaNum
        end <- forAll $ H.element ["|", "_", "$"]
        rest <- forAll genLargeAlphaNum
        let toParse = str <> end <> rest
            runTest =
              readpEqualWithNResults
                1
                (manyTill (string value) (string end))
                (R.manyTill (R.string value) (R.string end))
        runTest toParse
    ]
