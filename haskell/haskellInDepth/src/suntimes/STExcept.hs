{-# LANGUAGE DeriveAnyClass #-}

module STExcept
  ( RequestError (..),
    SuntimeException (..),
  )
where

import Control.Monad.Catch
import qualified Data.Text as T
import Fmt
import Types

data RequestError
  = BadDay !T.Text
  | BadAddress !T.Text
  deriving (Eq, Exception)

instance Show RequestError where
  show (BadDay v) = "Invalid date format: " +| v |+ ""
  show (BadAddress v) = "Invalid address: " +| v |+ ""

data SuntimeException
  = UnknownLocation !T.Text
  | UnknownTime !GeoCoords
  | FormatError !RequestError
  | ServiceAPIError !T.Text
  | NetworkError !SomeException
  | ConfigError !T.Text
  | ParamError !T.Text
  deriving (Exception)

instance Show SuntimeException where
  show (UnknownLocation m) = "Failed while determining coordinates :" +| m |+ ""
  show (UnknownTime v) = "Failed while determining sunrise/sunset times coords: " +|| v ||+ ""
  show (FormatError v) = show v
  show (ServiceAPIError v) = "Error while communicating with external services: " +| v |+ ""
  show (NetworkError v) = "Network communication error: " +|| v ||+ ""
  show (ConfigError v) = "Error parsing configuration file: " +| v |+ ""
  show (ParamError v) = "Error parsing configuration file: " +| v |+ ""
