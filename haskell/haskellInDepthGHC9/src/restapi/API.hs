{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module API
  ( Request,
    HandlerAction,
    APIService,
    APIError (..),
    APIAction,
    HasAPIService (..),
    ServiceStatus (..),
    execAPIAction,
    Get,
    Capture,
    (:<|>) (..),
    (:>),
    module X,
  )
where

import Control.Exception hiding (catch)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Kind
import Data.Maybe
import Data.Proxy as X
import Fmt as X
import GHC.Generics
import GHC.TypeLits
import Text.Read

type Request = [String]

type HandlerAction a = APIAction a

type Capture a = Maybe a

data ServiceStatus where
  Up :: ServiceStatus
  Down :: ServiceStatus
  deriving (Eq, Show, Read)

type Get :: Type -> Type
data Get a

type (:<|>) :: Type -> Type -> Type
data a :<|> b where
  (:<|>) :: a -> b -> a :<|> b

infixr 8 :<|>

type (:>) :: forall a. a -> Type -> Type
data a :> b

infixr 9 :>

newtype APIAction a = MkAPIAction (LoggingT IO a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadLogger
    )
deriving via (LoggingT IO) instance MonadCatch APIAction 

type APIService :: Type -> Type
type family APIService api where
  APIService (a :<|> b) = APIService a :<|> APIService b
  APIService ((a :: Symbol) :> b) = APIService b
  APIService (Capture a :> b) = a -> APIService b
  APIService (Get a) = APIAction a

data APIError where
  RouteError :: Request -> String -> APIError
  CaptureError :: Request -> String -> APIError
  RequestError :: Request -> String -> APIError
  deriving stock (Generic, Show, Eq)
  deriving anyclass (Exception)

class HasAPIService (api :: Type) where
  route :: Proxy api -> APIService api -> Request -> APIAction String

instance {-# OVERLAPPING #-} HasAPIService (Get String) where
  route _ result [] = result
  route _ _ req = throwM $ RequestError req $ "Non empty data:" +|| req ||+ ""

instance (Show a) => HasAPIService (Get a) where
  route _ result [] = show <$> result
  route _ _ req = throwM $ RequestError req $ "Non empty data:" +|| req ||+ ""

instance (HasAPIService a, HasAPIService b) => HasAPIService (a :<|> b) where
  route _ (h1 :<|> h2) req =
    route (Proxy @a) h1 req
      `catch` next
        (route (Proxy @b) h2 req `catch` onErr)
    where
      next r (RouteError _ _) = r
      next _ e = throwM e
      onErr (RouteError _ _) = throwM $ RouteError req "Route not found"
      onErr e = throwM e

instance (Show a, Read a, HasAPIService b) => HasAPIService (Capture a :> b) where
  route _ handler req@(y : ys) = do
    let x = fromMaybe (throw $ CaptureError req $ "Failed to parse " +| y |+ "") $ readMaybe @a y
    logDebugN $ "captured " +| y |+ " -> " +|| x ||+ ""
    route (Proxy @b) (handler x) ys
  route _ _ req = throwM $ CaptureError req "No data to capture"

instance (KnownSymbol a, HasAPIService b) => HasAPIService ((a :: Symbol) :> b) where
  route _ handler req@(x : xs) = do
    if r == x
      then
        logDebugN ("route:" +| r |+ "")
          >> route (Proxy @b) handler xs
      else throwM $ RouteError req $ "Route mismatch. Expected " +| r |+ " but found " +| x |+ ""
    where
      r = symbolVal (Proxy @a)
  route _ _ req = throwM $ RouteError req "Route not specified"

execAPIAction :: forall a. APIAction a -> IO a
execAPIAction (MkAPIAction action) = runStdoutLoggingT (action `catch` onErr)
  where
    onErr (SomeException e) =
      logErrorN (pretty (show e)) >> throwM e
