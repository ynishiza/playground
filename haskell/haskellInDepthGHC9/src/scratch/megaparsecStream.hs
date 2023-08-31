#!/usr/bin/env stack
{-
  Run with
    stack exec -- src/scratch/<name>.hs
    stack ghci -- src/scratch/<name>.hs
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Function
import Data.Functor
import Data.List (dropWhileEnd, intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Void
import Debug.Trace (trace)
import ScratchUtils
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

enableDebug :: Bool
enableDebug = False

trace_ :: String -> a -> a
trace_ = if enableDebug then trace else (\_ x -> x)

data AlphaNum
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | J
  | K
  | L
  | M
  | N
  | O
  | P
  | Q
  | R
  | S
  | T
  | U
  | V
  | W
  | X
  | Y
  | Z
  | Word [AlphaNum]
  deriving (Show, Eq, Ord)

instance VisualStream [AlphaNum] where
  showTokens _ = showTokens_ . NE.toList
  tokensLength _ cs =
    NE.toList cs
      <&> length . showAlphaNum
      & sum

showTokens_ :: [AlphaNum] -> String
showTokens_ = intercalate "" . (showAlphaNum <$>)

lineLength :: Int
lineLength = 10

instance TraversableStream [AlphaNum] where
  reachOffset offset p@(PosState {..}) =
    trace_
      debugInfo
      ( Just (showTokens_ tks),
        p
          { pstateOffset = pstateOffset + offset,
            pstateSourcePos = newPos
          }
      )
    where
      SourcePos {..} = pstateSourcePos

      (lineIdx, colIdx) = divMod pstateOffset lineLength
      (lineIdx', colIdx') = divMod offset lineLength
      numConsumed = lineLength * lineIdx' - pstateOffset
      tks =
        drop numConsumed pstateInput
          & take lineLength
      pre = take colIdx' tks

      newPos =
        SourcePos
          { sourceName = sourceName,
            sourceLine = mkPos (lineIdx' + 1),
            -- sourceColumn = mkPos (sourceColumn' + 1)
            sourceColumn = mkPos (length (showTokens_ pre) + 1)
          }

      debugInfo =
        "initial state:"
          <> show p
          <> "\n"
          <> "offset:"
          <> show offset
          <> "\n"
          <> "new pos:"
          <> show newPos
          <> "\n"
          <> "old (offset, lineIdx, colIdx)="
          <> show (pstateOffset, lineIdx, colIdx)
          <> "\n"
          <> "new (offset, lineIdx', colIdx')="
          <> show (offset, lineIdx', colIdx')
          <> "\n"
          <> "(dropped="
          <> show numConsumed
          <> "\n"

showAlphaNum :: AlphaNum -> String
showAlphaNum (Word content) =
  intercalate "" (showAlphaNum <$> content)
    & ("\'" <>)
    & (<> "\'")
showAlphaNum c = show c

type Parser = Parsec Void [AlphaNum]

abcde :: Parser [AlphaNum]
abcde = string [A, B, C, D, E]

abcdeSingles :: Parser [AlphaNum]
abcdeSingles = single A >> single B >> single C >> single D >> single E >> return [A, B, C, D, E]

abcdeWord :: Parser AlphaNum
abcdeWord = single (Word [A, B, C, D, E])

main :: IO ()
main = do
  parseTest abcde [A, B, C, D, E, F]
  parseTest abcde [A, B, D, E, F]
  parseTest abcdeSingles [A, B, D, E, F]
  parseTest abcdeWord [A, B, D, E, F]

  parseTest abcdeWord [Word [A, B, C, D, E], Z]
  parseTest abcdeWord [Z, Word [A, B, C, D, E], Z]
  parseTest abcdeWord [Word [A, B, F, D, E]]
  parseTest (count 3 abcdeWord) [Word [A, B, C, D, E], Z]

  parseTest (count 10 abcdeSingles) $ concat (replicate 3 [A, B, C, D, E]) <> [U, X, V]

  hspec spec

trimSpaces :: String -> String
trimSpaces = dropWhile f . dropWhileEnd f
  where
    f = flip elem " \n"

replicate_ :: Int -> [a] -> [a]
replicate_ n = concat . replicate n

spec :: Spec
spec = describe "stream" $ do
  let testError :: Parser a -> [AlphaNum] -> String -> Expectation
      testError p input msg = do
        case runParser p "" input of
          Left e -> trimSpaces (errorBundlePretty e) `shouldBe` trimSpaces msg
          Right _ -> expectationFailure $ "Parser did not fail with input " <> show input

  it "[Token]" $ do
    -- error at beginning
    testError
      abcdeSingles
      [U, A, B, C, D, E]
      [multilineString|
1:1:
  |
1 | UABCDE
  | ^
unexpected U
expecting A
      |]

    -- error in middle
    testError
      abcdeSingles
      [A, B, F, D, E]
      [multilineString|
1:3:
  |
1 | ABFDE
  |   ^
unexpected F
expecting C
      |]

    -- error at end
    testError
      (count 2 abcdeSingles)
      [A, B, C, D, E, A, B, F]
      [multilineString|
1:8:
  |
1 | ABCDEABF
  |        ^
unexpected F
expecting C
          |]

  it "[Tokens]" $ do
    -- error at beginning
    testError
      abcde
      [U, A, B, C, D, E]
      [multilineString|
1:1:
  |
1 | UABCDE
  | ^^^^^
unexpected UABCD
expecting ABCDE
        |]

    -- error in middle
    testError
      abcde
      [A, B, F, D, E]
      [multilineString|
1:1:
  |
1 | ABFDE
  | ^^^^^
unexpected ABFDE
expecting ABCDE
        |]

    testError
      (count 2 abcde)
      [A, B, C, D, E, A, B, F]
      [multilineString|
1:6:
  |
1 | ABCDEABF
  |      ^^^
unexpected ABF
expecting ABCDE
        |]

  it "[Token] different line" $ do
    testError
      (count 10 abcdeSingles)
      (replicate_ 3 [A, B, C, D, E] <> [F, A, B])
      [multilineString|
2:6:
  |
2 | ABCDEFAB
  |      ^
unexpected F
expecting A
|]

  it "[Tokens] different line" $ do
    testError
      (count 10 abcde)
      (replicate_ 3 [A, B, C, D, E] <> [F, A, B])
      [multilineString|
2:6:
  |
2 | ABCDEFAB
  |      ^^^
unexpected FAB
expecting ABCDE
|]
