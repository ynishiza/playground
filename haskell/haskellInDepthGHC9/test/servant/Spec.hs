{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec
  ( spec,
  )
where

import Common
import Control.Concurrent
import Control.Exception
import Data.IORef
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text.IO qualified as T
import Network.HTTP.Req
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Servant (layout)
import Server
import System.Random
import Test.Hspec
import TestAPI (SomeComplexData (..))
import TestClient

vanillaHttpExceptionSelector :: Selector HttpException
vanillaHttpExceptionSelector = \case
  (VanillaHttpException _) -> True
  _ -> False

expectResponse :: (HttpResponse response, Show (HttpResponseBody response), Eq (HttpResponseBody response)) => IO response -> HttpResponseBody response -> IO ()
expectResponse res expected = res >>= (`shouldBe` expected) . responseBody

spec :: Bool -> Spec
spec debug =
  aroundAll
    ( \runTest -> do
        ctx@(TestContext {..}) <-
          TestContext
            <$> getStdRandom (uniformR (10000 :: Int, 20000))
            <*> return (http "localhost")
        senv <-
          ServerEnv
            <$> newIORef emptyServerState
        T.putStrLn $ layout myAPI

        bracket
          (forkIO $ run portNumber (middleware $ application senv))
          killThread
          (const $ runTest ctx)
    )
    specBase
  where
    middleware = if debug then logStdoutDev else id

specBase :: SpecWith TestContext
specBase = describe "servant" $ do
  it "base" $ \ctx -> do
    call ctx GET (/: "api") NoReqBody (jsonResponse @String) mempty
      `expectResponse` "Hello"

  userAPISpecs
  testSpecs

userAPISpecs :: SpecWith TestContext
userAPISpecs = describe "User API" $ do
  let getUsers c = responseBody <$> getJSON @[User] c (id /# "api" /# "user") mempty

  it "GET /user" $ \ctx -> do
    users <- getUsers ctx
    length users `shouldBe` 1

  it "POST /user" $ \ctx -> do
    _ <- call ctx POST (id /# "api" /# "user") (ReqBodyJson $ User "" "Test person") ignoreResponse mempty
    users <- getUsers ctx
    find (\User {..} -> userName == "Test person") users `shouldSatisfy` isJust

  it "GET /user/:uid" $ \ctx -> do
    getJSON @User ctx (id /# "api" /# "user" /# userId defaultUser) mempty
      `expectResponse` defaultUser
    getJSON @User ctx (id /# "api" /# "user" /# "-1") mempty
      `shouldThrow` vanillaHttpExceptionSelector

testSpecs :: SpecWith TestContext
testSpecs = describe "" $ do
  let urlBuilder = id /# "api" /# "test"
  describe "response" $ do
    it "JSON response" $ \ctx ->
      do
        call ctx GET (urlBuilder /# "response" /# "multitype") NoReqBody (jsonResponse @String) (header "Accept" "application/json")
        `expectResponse` "Hello"

    it "Plain text response" $ \ctx -> do
      call ctx GET (urlBuilder /# "response" /# "multitype") NoReqBody bsResponse (header "Accept" "text/plain; charset=utf-8")
        `expectResponse` "Hello"

    it "throws an error if unsupported response type" $ \ctx -> do
      responseBody <$> call ctx GET (urlBuilder /# "response") NoReqBody (jsonResponse @String) (header "Accept" "text/plain")
        `shouldThrow` vanillaHttpExceptionSelector

  it "header" $ \ctx -> do
    res <- call ctx GET (urlBuilder /# "response" /# "header") NoReqBody ignoreResponse mempty
    responseHeader res "A" `shouldBe` Just "1"
    responseHeader res "B" `shouldBe` Just "true"
    responseHeader res "C" `shouldBe` Nothing

  describe "QueryParam" $ do
    it "basic" $ \ctx -> do
      -- case: with parameter
      getJSON @String ctx (urlBuilder /# "queryparam") (queryParam "value" (Just @String "hello"))
        `expectResponse` "param:hello"

      -- case: without parameter
      getJSON @String ctx (urlBuilder /# "queryparam") (queryParam "value" (Nothing @String))
        `expectResponse` "NA"

    it "list parameter" $ \ctx -> do
      getJSON @String
        ctx
        (urlBuilder /# "queryparam" /# "list")
        ( queryParam "value" (Just @String "ab")
            <> queryParam "value" (Just @String "cd")
        )
        `expectResponse` "ab,cd"

    it "modifier" $ \ctx -> do
      -- case: without optional
      getJSON @(String, String)
        ctx
        (urlBuilder /# "queryparam" /# "modifier")
        (queryParam "v1" (Just @String "hello"))
        `expectResponse` ("hello", "NA")

      -- case: with optional
      getJSON @(String, String)
        ctx
        (urlBuilder /# "queryparam" /# "modifier")
        ( queryParam "v1" (Just @String "hello")
            <> queryParam "v2" (Just @String "world")
        )
        `expectResponse` ("hello", "world")

    it "error if parameter is required" $ \ctx -> do
      responseBody
        <$> getJSON @(String, String) ctx (urlBuilder /# "queryparam" /# "modifier") mempty
        `shouldThrow` vanillaHttpExceptionSelector

    it "combination" $ \ctx -> do
      getJSON @(String, String, [Int], Bool)
        ctx
        (urlBuilder /# "queryparam" /# "combined")
        ( queryParam "v1" (Just @String "hello")
            <> queryParam "v2" (Just @String "world")
            <> queryParam "list" (Just @Int 1)
            <> queryParam "list" (Just @Int 2)
            <> queryParam "flag" (Just True)
        )
        `expectResponse` ("hello", "world", [1, 2], True)

  describe "ReqBody" $ do
    it "JSON body" $ \ctx -> do
      call ctx POST (urlBuilder /# "requestBody") (ReqBodyJson @String "TEST") (jsonResponse @String) mempty
        `expectResponse` "body:TEST"

    it "Plain text body" $ \ctx -> do
      call ctx POST (urlBuilder /# "requestBody") (ReqBodyBs "TEST") (jsonResponse @String) (header "Content-Type" "text/plain; charset=utf-8")
        `expectResponse` "body:TEST"

    it "object as body" $ \ctx -> do
      call ctx POST (id /# "api" /# "test" /# "requestBody" /# "complex") (ReqBodyJson $ SomeComplexData "abc" 1) (jsonResponse @SomeComplexData) mempty
        `expectResponse` SomeComplexData "abc" 1
