{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module TestClient
  ( call,
    getJSON,
    TestContext (..),
    (/#),
  )
where

import Control.Exception (throw)
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Req

instance MonadHttp IO where
  handleHttpException = throw

data TestContext = TestContext
  { portNumber :: Int,
    baseUrl :: Url 'Http
  }
  deriving (Show, Eq)

(/#) :: (Url scheme -> Url scheme) -> Text -> (Url scheme -> Url scheme)
f /# t = (/: t) . f

infixl 1 /#

call ::
  (HttpMethod method, HttpBody body, HttpBodyAllowed (AllowsBody method) (ProvidesBody body), HttpResponse response) =>
  TestContext ->
  method ->
  (Url 'Http -> Url 'Http) ->
  body ->
  Proxy response ->
  Option 'Http ->
  IO response
call TestContext {..} method url body response opts = req method (url baseUrl) body response (port portNumber <> opts)

getJSON ::
  (FromJSON a) =>
  TestContext ->
  (Url 'Http -> Url 'Http) ->
  Option 'Http ->
  IO (JsonResponse a)
getJSON TestContext {..} url opts = req GET (url baseUrl) NoReqBody jsonResponse (port portNumber <> opts)
