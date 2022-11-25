{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use fromMaybe" #-}

module App
  ( SuntimesApp (..),
    requestM,
    runApp,
    appGET,
    module Exports,
  )
where

import Control.Monad.Catch as Exports
import Control.Monad.Logger as Exports
import Control.Monad.Reader as Exports
import Control.Monad.State as Exports
import Data.Proxy
import qualified Data.Text as T
import Fmt
import qualified Network.HTTP.Client as L
import Network.HTTP.Req as Exports
import STExcept
import Types

appRequestTimeout :: Int
appRequestTimeout = 10 * 1000 * 1000 -- 10s

newtype SuntimesAppState = SuntimesAppState
  { lastRequest :: Maybe L.Request
  }

newtype SuntimesApp a = SuntimesApp
  { runSuntimesApp :: ReaderT WebAPIAuth (StateT SuntimesAppState (LoggingT IO)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader WebAPIAuth,
      MonadState SuntimesAppState,
      MonadIO,
      MonadLogger,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

instance MonadHttp SuntimesApp where
  handleHttpException excp
    | (JsonHttpException s) <- excp = logAndRethrow (ServiceAPIError $ T.pack s)
    | (VanillaHttpException (L.HttpExceptionRequest _ (L.StatusCodeException resp _))) <- excp =
      logAndRethrow $ ServiceAPIError $ T.pack $ show $ L.responseStatus resp
    | otherwise = logAndRethrow excp
    where
      logAndRethrow e = do
        get >>= maybe (pure ()) (logErrorN . fmt . logRequest) . lastRequest
        logErrorN $ T.pack $ show e
        throwM e

  getHttpConfig = return defaultHttpConfig

defaultState :: SuntimesAppState
defaultState = SuntimesAppState Nothing

runApp :: SuntimesApp a -> LogLevel -> WebAPIAuth -> IO a
runApp app level config =
  runStdoutLoggingT $ filterLogger withLog (evalStateT (runReaderT r config) defaultState)
  where
    withLog _ l = l >= level
    (SuntimesApp r) = do
      logDebugN $ fmtLn $ "Using config:" +| config |+ ""
      app

appGET ::
  HttpResponse response =>
  Url scheme ->
  Option scheme ->
  Proxy response ->
  SuntimesApp (L.Request, response)
appGET uri opts respType = do
  res <- requestM GET uri NoReqBody respType (opts <> responseTimeout appRequestTimeout) onRequest
  jrq <- gets lastRequest
  return (maybe (error "Should never happen") id jrq, res)

onRequest :: L.Request -> SuntimesApp ()
onRequest rq = do
  modify (\s -> s {lastRequest = Just rq})
  logDebugN $ fmt $ logRequest rq

logRequest :: L.Request -> Builder
logRequest rq =
  "HTTP request:\n"
    <> indentF
      2
      ( blockMapF @[(Builder, String)]
          [ ("version", show (L.requestVersion rq)),
            ("method", show (L.method rq)),
            ("uri", show (L.getUri rq)),
            ("host", show (L.host rq)),
            ("headers", show (L.requestHeaders rq))
          ]
      )

logResponse :: L.Response a -> Builder
logResponse res =
  "HHTP response:\n"
    <> indentF
      2
      ( blockMapF @[(Builder, String)]
          [ ("version", show (L.responseVersion res)),
            ("status", show (L.responseStatus res)),
            ("headers", show (L.responseHeaders res))
          ]
      )

requestM ::
  (MonadHttp m, MonadLogger m, HttpMethod method, HttpResponse response, HttpBody body, HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
  method ->
  Url scheme ->
  body ->
  Proxy response ->
  Option scheme ->
  (L.Request -> m ()) ->
  m response
requestM method uri body respType opts onRq = do
  res <-
    reqCb
      method
      uri
      body
      respType
      opts
      ( \rq -> do
          onRq rq
          return rq
      )
  logDebugN $ fmt $ logResponse $ toVanillaResponse res
  return res
