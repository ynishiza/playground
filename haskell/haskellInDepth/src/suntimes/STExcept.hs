{-# LANGUAGE DeriveAnyClass #-} 

module STExcept ( 
  RequestError(..),
  SuntimeException(..),
  ) where

import Types
import qualified Data.Text as T
import Control.Monad.Catch

data RequestError = BadDay T.Text deriving (Show, Eq, Exception)

data SuntimeException = UnknownLocation !T.Text
  | UnknownTime !GeoCoords
  | FormatError !RequestError
  | ServiceAPIError !T.Text
  | NetworkError !SomeException
  | ConfigError
  deriving (Show, Exception)
