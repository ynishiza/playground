module ProcessRequest
  ( processInteractive,
    processFile,
    processInput,
    processManyLines,
    RequestError,
  )
where

import App
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format
import Data.Time.LocalTime
import Fmt
import GeoCoordsReq
import STExcept
import SunTimes
import Types

type Request = (Address, When)

requestFormat :: T.Text
requestFormat = "[yyyy-mm-dd@]ADDRESS"

whenFormat :: String
whenFormat = "%Y-%m-%d"

lineDelimiter :: Char
lineDelimiter = '@'

parseInput :: T.Text -> Either RequestError Request
parseInput line =
  case T.findIndex (== lineDelimiter) line of
    Just i -> (,) <$> processAddr (T.drop (i + 1) line) <*> processDay (T.take i line)
    Nothing -> (,) <$> processAddr line <*> processDay ""
  where
    cleanse = T.strip
    createMessage :: T.Text -> T.Text
    createMessage msg = "Failed to process line='" +| line |+ "'\t " +| msg |+ ""
    processAddr addr = case cleanse addr of
      "" -> Left (BadAddress $ createMessage "Missing address")
      _ -> Right addr
    processDay :: T.Text -> Either RequestError When
    processDay v = case cleanse v of
      "" -> Right Now
      t ->
        ( case parseTimeM False defaultTimeLocale whenFormat (T.unpack t) of
            Nothing -> Left (BadDay $ createMessage $ "Invalid date = " +| t |+ "")
            Just d -> Right (On d)
        )

processInput :: T.Text -> SuntimesApp (GeoCoords, SunTimes ZonedTime)
processInput line = either (throwM . FormatError) proc (parseInput line) 
  where
    proc rq = do 
      x <- processRequest rq 
      liftIO $ prettyLn $ formatResult rq x
      return x

processRequest :: Request -> SuntimesApp (GeoCoords, SunTimes ZonedTime)
processRequest (location, w) = do
  g <- getGeoCoords location
  t <- getSuntimesLocal g w
  return (g, t)

formatResult :: Request -> (GeoCoords, SunTimes ZonedTime) -> T.Text
formatResult (addr, _) (GeoCoords {..}, SunTimes {..}) =
  fmtDate sunrise |+ "@" +| addr |+ ""
    +| "\t("
    +| lat |+ ","
    +| lon |+ ")\n"
      <> indentF
        2
        ( ""
            +| fmtTime sunrise |+ "\n"
            +| fmtTime sunset |+ "\n"
        )
  where
    fmtDate = formatTime defaultTimeLocale "%F"
    fmtTime = formatTime defaultTimeLocale "%X %EZ"

processManyLines :: [T.Text] -> SuntimesApp ()
processManyLines = traverse_ (handle handleError . procOne)
  where
    procOne :: T.Text -> SuntimesApp ()
    procOne line = case T.uncons (T.strip line) of
      Just ('#', _) -> pure ()
      _ -> processInput line >> pure ()

handleError :: SuntimeException -> SuntimesApp ()
handleError e
  | (UnknownLocation _) <- e = p e
  | (UnknownTime _) <- e = p e
  | (FormatError _) <- e = p e
  | otherwise = throwM e
  where
    p e' = liftIO (print e')

processInteractive :: SuntimesApp ()
processInteractive = mainProc `catch` handleError >> onDone
  where
    p :: T.Text -> SuntimesApp ()
    p = liftIO . prettyLn

    mainProc = do
      p $ "Enter request: " +| requestFormat |+ ""
      l <- liftIO T.getLine
      processInput l >> pure () `catch` handleError 
    onDone = do
      p "Another request? (y/n)"
      l <- liftIO T.getLine
      when (l == "y" || l == "yes") processInteractive

processFile :: FilePath -> SuntimesApp ()
processFile fpath = liftIO (T.lines <$> T.readFile fpath) >>= processManyLines
