{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Template.PrintfParser
  ( parseOnly,
    parseD,
    parseL,
    parseS,
    parseFormats,
    parseLiteralChar,
    isFormatValue,
    Format (..),
  )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 hiding (D)
import Data.Attoparsec.Combinator
import Data.String

data Format where
  Literal :: String -> Format
  D :: Format
  S :: Format
  deriving stock (Show, Eq)

dValue :: IsString s => s
dValue = "%d"

sValue :: IsString s => s
sValue = "%s"

isFormatValue :: (Eq s, IsString s) => s -> Bool
isFormatValue s
  | s == dValue = True
  | s == sValue = True
  | otherwise = False

parseD :: Parser Format
parseD = string dValue >> pure D <?> "D"

parseS :: Parser Format
parseS = string sValue >> pure S <?> "S"

parseL :: Parser Format
parseL = Literal <$> many1 parseLiteralChar <?> "Literal"

-- note: parse exactly one character that isn't a format character
parseLiteralChar :: Parser Char
parseLiteralChar =
  ( do
      v <- lookAhead $ count 1 (parseD <|> parseS) <|> pure []
      if null v
        then anyChar
        else fail ("Encountered format " <> show (head v))
  )
    <?> "literal char"

parseFormats :: Parser [Format]
parseFormats = many1 (parseD <|> parseS <|> parseL)
