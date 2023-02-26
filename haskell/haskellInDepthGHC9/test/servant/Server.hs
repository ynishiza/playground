{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( application,
    myAPI,
  )
where

import Common
import Servant
import TestAPI qualified
import UserAPI qualified

myAPI :: Proxy ServerAPI
myAPI = Proxy @ServerAPI

type ServerAPI =
  "api"
    :> Description "API"
    :> Summary "API"
    :> ( BaseAPI
           :<|> UserAPI.MainAPI
           :<|> TestAPI.API
       )

type BaseAPI = Get '[JSON] String

newtype APIHandler m a = APIHandler (ReaderT ServerEnv (LoggingT m) a)
  deriving stock
    ( Functor
    )
  deriving newtype
    ( Applicative,
      Monad,
      MonadReader ServerEnv,
      MonadLogger,
      MonadIO
    )

deriving instance MonadError ServerError (APIHandler Handler)

instance MonadTrans APIHandler where
  lift x = APIHandler $ lift $ lift x

mainHandler :: ServerT ServerAPI (APIHandler Handler)
mainHandler =
  baseHandler
    :<|> UserAPI.mainHandler
    :<|> TestAPI.handler

baseHandler :: ServerT BaseAPI (APIHandler Handler)
baseHandler = do
  logDebugN "Hello API"
  return "Hello"

mainServer :: ServerEnv -> Server ServerAPI
mainServer senv = hoistServer myAPI run mainHandler
  where
    run (APIHandler s) = runStdoutLoggingT $ runReaderT s senv

application :: ServerEnv -> Application
application senv = serve myAPI (mainServer senv)
