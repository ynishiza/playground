{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use void" #-}

module Playground
  ( run,
    printRequestInfo,
    printResponseInfo,
    printHtppResponse,
    requestGoogle,
    myAppRequestGoogle,
    myAppRun,
    MyHttpApp,
    simpleRequestWithLog,
    simpleRequestWithReqRes,
  )
where

import Control.Exception
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Fmt
import qualified Network.HTTP.Client as C
import Network.HTTP.Req
import Utils

createGoogleRequest :: MonadHttp m => T.Text -> m BsResponse
createGoogleRequest = createGoogleRequestBase return

createGoogleRequestBase :: MonadHttp m => (C.Request -> m C.Request) -> T.Text -> m BsResponse
createGoogleRequestBase f path = reqCb GET (https "www.google.com" /~ path) NoReqBody bsResponse (responseTimeout 10000000) f

requestGoogle :: MonadIO m => T.Text -> m BsResponse
requestGoogle path = runReq defaultHttpConfig $ createGoogleRequest path

printRequestInfo :: C.Request -> IO ()
printRequestInfo rq =
  printBannerWrap "Request info: start" $ do
    "uri:" +|| C.getUri rq ||+ "\n"
    +|"method:" +|| C.method rq ||+ "\n"
    +|"host:" +|| C.host rq ||+ "\n"
    +|"path:" +|| C.path rq ||+ "\n"

printResponseInfo :: C.Response ByteString -> IO ()
printResponseInfo resp = printBannerWrap "ResponseInfo" $ do
  fmt $
    "status:" +|| C.responseStatus resp ||+ "\n"
      +|| B.take 101 (C.responseBody resp) ||+ ""
      +| "headers" <> blockListF' "" (build . show) (C.responseHeaders resp) <> ""

printHtppResponse :: (HttpResponse response, HttpResponseBody response ~ ByteString) => response -> IO ()
printHtppResponse resp = printBannerWrap "ResponseInfo" $ printResponseInfo $ toVanillaResponse resp

runAndSuppress :: IO () -> IO ()
runAndSuppress = handle (\(e :: SomeException) -> ("error:" +|| e ||+ "\n") >> return ())

run :: IO ()
run = do
  putStrLn ""
  runAndSuppress $ requestGoogle "images" >>= printHtppResponse
  runAndSuppress $ requestGoogle "abcde" >>= printHtppResponse
  runAndSuppress $ requestGoogle "." >>= printHtppResponse
  return ()

type MyError = String

newtype MyHttpApp a = MyHttpApp {runMyHttpApp :: ExceptT MyError IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadError MyError
    )

instance MonadHttp MyHttpApp where
  handleHttpException e = do
    liftIO $ "HTTP error:" +|| e ||+ ""
    throw e
  getHttpConfig = return defaultHttpConfig

myAppRequestGoogle :: T.Text -> MyHttpApp BsResponse
myAppRequestGoogle path = do
  res <-
    createGoogleRequestBase
      ( \r -> do
          let uri = C.getUri r
          liftIO $
            printBannerWrap "Request info: start" $ do
              "request uri=" +|| uri ||+ "\n"
          return r
      )
      path
  liftIO $ printHtppResponse res
  return res

myAppRun :: MyHttpApp a -> IO (Either MyError a)
myAppRun app = do
  runExceptT (runMyHttpApp app)

newtype SimpleHttp a = SimpleHttp {runSimpleHttp :: IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO
    )

instance MonadHttp SimpleHttp where
  handleHttpException e = do
    liftIO $ "HTTP error:" +|| e ||+ ""
    throw e

simpleRequestWithLog :: T.Text -> IO BsResponse
simpleRequestWithLog path = runSimpleHttp $ do
  res <-
    createGoogleRequestBase
      (\r -> liftIO (printRequestInfo r) >> return r)
      path
  liftIO $ printHtppResponse res
  return res

simpleRequestWithReqRes :: T.Text -> IO (C.Request, C.Response ByteString)
simpleRequestWithReqRes path = runSimpleHttp $ do
  (rq, mng) <-
    req' GET (https "www.google.com" /~ path) NoReqBody mempty (curry return)
  liftIO $ printRequestInfo rq
  res <- liftIO $ (LB.toStrict <$>) <$> C.httpLbs rq mng
  liftIO $ printResponseInfo res
  return (rq, res)
