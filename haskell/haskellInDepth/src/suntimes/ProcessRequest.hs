module ProcessRequest
  ( processInteractive,
    processManyLines,
    RequestError,
    parseLine,
  )
where

import App
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Format
import Data.Time.LocalTime
import Fmt
import GeoCoordsReq
import STExcept
import SunTimes
import Types

parseLine :: T.Text -> Either RequestError (Address, When)
parseLine line = case T.breakOn "@" (cleanse line) of
  (addr, "") -> onlyAddress addr
  (da, addr) -> toDate (cleanse (T.tail addr), cleanse da)
  where
    cleanse = T.strip
    onlyAddress addr = Right (addr, Now)

    toDate :: (Address, T.Text) -> Either RequestError (Address, When)
    toDate (addr, v) = case v of
      "" -> onlyAddress addr
      t ->
        ( case parseTimeM False defaultTimeLocale "%Y-%m-%d" (T.unpack t) of
            Nothing -> Left (BadDay $ "Invalid date = '" +| t |+ "''. Must be of the form yyyy-mm-dd.")
            Just d -> Right (addr, On d)
        )

processRequest :: (Address, When) -> SuntimesApp (SunTimes ZonedTime)
processRequest (location, w) = do
  g <- getGeoCoords location
  getSuntimesLocal g w

processInteractive :: SuntimesApp ()
processInteractive = catch @SuntimesApp @SomeException (action >> prompt) handle
  where
    action = do
      p "Enter request"

      l <- liftIO T.getLine
      case parseLine l of
        Right v -> do
          r <- processRequest v
          p $ "result=" +| r |+ ""
        Left e -> do
          throwM e
    prompt = do
      p "Another request? (y/n)"
      l <- liftIO T.getLine
      when (l == "y" || l == "yes") processInteractive

    handle :: Exception e => e -> SuntimesApp ()
    handle e = do
      p $ "error" +|| e ||+ ""
      prompt
    p = liftIO . fmtLn

processManyLines :: [T.Text] -> SuntimesApp ()
processManyLines = undefined
