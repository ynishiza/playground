{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

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
import Data.Foldable
import Data.List (intercalate)
import Debug.Trace
import GHC.Exts (fromString)
import Hedgehog
import Hedgehog.Gen qualified as H hiding (constant)
import Hedgehog.Internal.Property (CoverPercentage)
import Hedgehog.Range qualified as H
import ParseBasic qualified as B
import ReadPBasic qualified as R
import Text.ParserCombinators.ReadP qualified as R

numTests :: TestLimit
numTests = 500

toLabelName :: Show a => a -> LabelName
toLabelName = fromString . show

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
genIntInRange l h = H.integral (H.constant l h)

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
readpEqual p1 p2 toParse = void $ readpEqual_ p1 p2 toParse

readpEqual_ :: (MonadTest m, Eq a, Show a) => P a -> R.ReadP a -> String -> m [(a, String)]
readpEqual_ p1 p2 toParse = do
  annotate $ "test string:" <> toParse
  let res = parse p1 toParse
  res === parseReadP p2 toParse
  return res

readpEqualWithNResults :: (MonadTest m, Eq a, Show a) => Int -> P a -> R.ReadP a -> String -> m ()
readpEqualWithNResults n p1 p2 toParse = void $ readpEqualWithNResults_ n p1 p2 toParse

readpEqualWithNResults_ :: (MonadTest m, Eq a, Show a) => Int -> P a -> R.ReadP a -> String -> m [(a, String)]
readpEqualWithNResults_ n p1 p2 toParse = do
  res <- readpEqual_ p1 p2 toParse
  annotate $
    "test string:"
      <> toParse
      <> "\n"
      <> "result:"
      <> show res
  length res === n
  return res

testCase :: PropertyName -> PropertyT IO () -> (PropertyName, Property)
testCase name p = (name, withTests numTests $ property p)

properties :: Group
properties =
  Group
    "Number"
    [ --
      testCase "Parses an integer" $ do
        n <- forAll $ genIntInRange (-10000) 10000
        annotate $ show $ parse (B.integral @Int) (show n)
        coverRange "int = " (-10000, 1000, replicate 20 1) n
        parseLast B.integral (show n) === (n, ""),
      --
      testCase "fails on non-integral" $ do
        n <- forAll genString
        annotate $ show $ parse (B.integral @Int) (show n)
        parse (B.integral @Int) n === [],
      --
      testCase "Parses a unary expression" $ do
        v1 <- forAll $ genIntInRange (-100) 100
        coverRange "int = " (-100, 10, replicate 20 1) v1
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
        (term, n, str) <-
          forAll $
            genReplicate
              (genIntInRange 0 10)
              genSmallAlphaNum
        let toParse = str <> rest
            runTest =
              readpEqual
                (munch (`elem` term))
                (R.munch (`elem` term))
            runTest1 =
              readpEqual
                (munch1 (`elem` toParse))
                (R.munch1 (`elem` toParse))

        label $ "n:" <> toLabelName n
        runTest toParse
        runTest1 toParse,
      --
      testCase "[skipSpaces]" $ do
        toParse <- forAll $ H.subsequence $ ['a' .. 'd'] <> [' ', '\n', '\r']
        let runTest = readpEqual_ skipSpaces R.skipSpaces
        [(_, rest)] <- runTest toParse
        label $ "# skipped = " <> toLabelName (length toParse - length rest),
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

        coverRange "n" (0, 1, replicate 6 5) n
        runTest toParse,
      --
      testCase "[many, many1]" $ do
        (term, n, str) <-
          forAll $
            genReplicate
              (genIntInRange 0 10)
              genSmallAlphaNum
        rest <- forAll genRest
        let toParse = str <> rest
            runTest0 =
              readpEqualWithNResults
                (n + 1)
                (many $ string term)
                (R.many $ R.string term)
            runTest1 =
              readpEqualWithNResults
                n
                (many1 $ string term)
                (R.many1 $ R.string term)

        coverRange "n" (0, 1, replicate 11 5) n
        runTest0 toParse
        runTest1 toParse,
      --
      testCase "[many', many1']" $ do
        (term, n, str) <-
          forAll $
            genReplicate
              (genIntInRange 0 10)
              genSmallAlphaNum
        rest <- forAll genRest
        let toParse = str <> rest
            runTest0' =
              readpEqualWithNResults
                (n + 1)
                (many' $ string term)
                (R.many $ R.string term)
            runTest1' =
              readpEqualWithNResults
                n
                (many1' $ string term)
                (R.many1 $ R.string term)

        coverRange "n" (0, 1, replicate 11 5) n
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
        (term, n, str) <-
          forAll $
            genReplicate
              (genIntInRange 0 10)
              genSmallAlphaNum
        let toParse = str <> rest
            runTest =
              readpEqualWithNResults
                (n + 1)
                (skipMany (string term))
                (R.skipMany (R.string term))
            runTest1 =
              readpEqualWithNResults
                n
                (skipMany1 (string term))
                (R.skipMany1 (R.string term))
        coverRange "n" (0, 1, replicate 11 5) n
        runTest toParse
        runTest1 toParse,
      --
      testCase "[sepBy, sepBy1]" $ do
        rest <- forAll genRest
        (term, n, sepc, str) <-
          forAll $
            genReplicateWithSeparator
              (genIntInRange 0 10)
              genSmallAlphaNum
              (H.element ["|", "_", ":", ";"])
        let toParse = str <> rest
            runTest =
              readpEqualWithNResults
                (n + 1)
                (sepBy (string term) (string sepc))
                (R.sepBy (R.string term) (R.string sepc))
            runTest1 =
              readpEqualWithNResults
                n
                (sepBy1 (string term) (string sepc))
                (R.sepBy1 (R.string term) (R.string sepc))

        coverRange "n" (0, 1, replicate 11 5) n
        runTest toParse
        runTest1 toParse,
      testCase "[endBy, endBy1]" $ do
        rest <- forAll genRest
        (term, n, delim, str) <-
          forAll $
            genReplicateWithSeparator
              (genIntInRange 0 10)
              genSmallAlphaNum
              (H.element [";", ":"])
        let toParse = str <> rest
            runTest =
              readpEqualWithNResults
                (max 1 n)
                (endBy (string term) (string delim))
                (R.endBy (R.string term) (R.string delim))
            runTest1 =
              readpEqualWithNResults
                (max 0 (n - 1))
                (endBy1 (string term) (string delim))
                (R.endBy1 (R.string term) (R.string delim))

        coverRange "n" (0, 1, replicate 11 5) n
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
                (n + 1)
                (chainr (show <$> B.integral @Int) B.pbracket "#")
                (R.chainr (show <$> R.int @Int) R.bracket "#")
            runTest1 =
              readpEqualWithNResults
                n
                (chainr1 (show <$> B.integral @Int) B.pbracket)
                (R.chainr1 (show <$> R.int @Int) R.bracket)

        coverRange "n" (0, 1, replicate 11 5) n
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
                (n + 1)
                (chainl (show <$> B.integral @Int) B.pbracket "#")
                (R.chainl (show <$> R.int @Int) R.bracket "#")
            runTest1 =
              readpEqualWithNResults
                n
                (chainl1 (show <$> B.integral @Int) B.pbracket)
                (R.chainl1 (show <$> R.int @Int) R.bracket)

        coverRange "n" (0, 1, replicate 11 5) n
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

coverRange :: (Show i, Integral i) => LabelName -> (i, i, [CoverPercentage]) -> i -> PropertyT IO ()
coverRange prefix (startValue, step, coverages) v = do
  traverse_ coverForStep $ zip [0 ..] coverages
  cover 0 (prefix <> " >= " <> toLabelName maxValue) $ maxValue <= v
  where
    numCoverages = length coverages
    maxValue = startValue + (step * fromIntegral numCoverages)
    coverForStep (i, requiredCoverage) = cover requiredCoverage (toLabelName l <> " <= " <> prefix <> " < " <> toLabelName u) $ l <= v && v < u
      where
        l = startValue + i * step
        u = l + step
