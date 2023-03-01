module CsvParser
  ( fields,
    dayInfo,
    countryStat,
    dateField,
    countryData,
    maybeCountryData,
  )
where

import Control.Applicative
import Control.Monad
import CovidData
import Data.Attoparsec.ByteString (Parser, (<?>))
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Functor
import Data.Text (Text)
import Data.Text.Encoding
import Data.Time

delimiter :: Char
delimiter = ','

fields :: Parser [ByteString]
fields = P.sepBy1 (P.takeWhile (/= delimiter)) (P.char delimiter)

rawField :: Parser ByteString
rawField = P.takeWhile1 (/= delimiter)

textField :: Parser Text
textField = decodeUtf8 <$> rawField <?> "Text"

skipField :: Parser ()
skipField = void $ P.takeWhile (/= delimiter) >> P.take 1

intField :: Integral a => Parser a
intField = round <$> P.double <?> "Number"

dateField :: Parser Day
dateField =
  rawField
    >>= parseTimeM True defaultTimeLocale "%F" . B.unpack
    <?> "Date"

countryStat :: Parser CountryStat
countryStat =
  CountryStat
    <$> P.decimal
    <* skipField
    <*> optional P.double
    <?> "CountryStat"

dayInfo :: Parser DayInfo
dayInfo =
  DayInfo
    <$> pcases
    <* skipField
    <*> pdeaths
  where
    pcases =
      DayCases
        <$> intField
        <* skipField
        <*> intField
        <?> "DayCases"
    pdeaths =
      DayDeaths
        <$> intField
        <* skipField
        <*> intField
        <?> "DayDeaths"

pany :: Parser Char
pany = P.satisfy (const True)

skipRestOfLine :: Parser ()
skipRestOfLine = void $ P.manyTill pany pend

pend :: Parser ()
pend = P.endOfLine <|> P.endOfInput

countryData :: Parser CountryData
countryData =
  CountryData
    <$> (rawField <?> "iso")
    <* skipField
    <*> (textField <?> "country")
    <* skipField
    <*> (textField <?> "name")
    <* skipField
    <*> pure 0
    <*> pure 0
    <*> (singleinfo <$> dateField <* skipField <*> dayInfo <* skipField)
    <* P.count 14 skipField
    <*> countryStat
    <* skipRestOfLine
    <?> "CountryData"
  where
    singleinfo x y = [(x, y)]

maybeCountryData :: Parser (Maybe CountryData)
maybeCountryData = Just <$> countryData <|> skipRestOfLine $> Nothing
