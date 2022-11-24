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
    module Req,
    MyLog (..),
  )
where

import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
-- import qualified Data.ByteString as B
import Data.Kind
import Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fmt
import qualified Network.HTTP.Client as L
import Network.HTTP.Req as Req
import Types

newtype SuntimesAppState = SuntimesAppState
  { lastRequest :: Maybe L.Request
  }

newtype SuntimesApp a = SuntimesApp
  { runSuntimesApp :: ReaderT WebAPIAuth (StateT SuntimesAppState IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader WebAPIAuth,
      MonadState SuntimesAppState,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

class MyLog (m :: Type -> Type) where
  writeLog :: MyLogLevel -> T.Text -> m ()

  logInfo :: T.Text -> m ()
  logInfo = writeLog LGInfo

  logError :: T.Text -> m ()
  logError = writeLog LGError

data MyLogLevel = LGInfo | LGError

instance MyLog SuntimesApp where
  writeLog _ = liftIO . T.putStrLn

instance MonadHttp SuntimesApp where
  handleHttpException e = do
    liftIO $ print e
    case e of 
      JsonHttpException m -> liftIO $ print m
      _ -> pure ()
    throwM e
  getHttpConfig = return defaultHttpConfig

defaultState :: SuntimesAppState
defaultState = SuntimesAppState Nothing

runApp :: SuntimesApp a -> WebAPIAuth -> IO a
runApp (SuntimesApp a) config = evalStateT (runReaderT a config) defaultState

appGET ::
  HttpResponse response =>
  Url scheme ->
  Option scheme ->
  Proxy response ->
  SuntimesApp (L.Request, response)
appGET uri opts respType = do
  res <- requestM GET uri NoReqBody respType opts onRequest
  jrq <- gets lastRequest
  return (maybe (error "Should never happen") id jrq, res)

onRequest :: L.Request -> SuntimesApp ()
onRequest rq = do
  modify (\s -> s {lastRequest = Just rq})
  logInfo $ fmt $ logRequest rq

logRequest :: L.Request -> Builder
logRequest rq =
  "HTTP request:\n" 
    <> indentF 2 (blockMapF @[(Builder, String)]
      [ 
        ("version", show (L.requestVersion rq)),
        ("method", show (L.method rq)),
        ("uri", show (L.getUri rq)),
        ("host", show (L.host rq)),
        ("headers", show (L.requestHeaders rq))
      ])

logResponse :: L.Response a -> Builder
logResponse res =
  "HHTP response:\n"
    <> indentF 2 (blockMapF @[(Builder, String)]
      [ 
      ("version", show (L.responseVersion res)),
      ("status", show (L.responseStatus res)),
      ("headers", show (L.responseHeaders res))
      ])

requestM ::
  (MonadHttp m, MyLog m, HttpMethod method, HttpResponse response, HttpBody body, HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
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
  logInfo $ fmt $ logResponse $ toVanillaResponse res
  return res
