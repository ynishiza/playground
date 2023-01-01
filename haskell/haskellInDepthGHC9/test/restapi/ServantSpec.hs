{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module ServantSpec
  ( spec,
  )
where

import APIWithServant.API
import Control.Concurrent
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Lazy.Char8 qualified as LBC
import Data.List (isInfixOf)
import Fmt
import Network.HTTP.Req
import Network.Wai.Handler.Warp
import System.Random
import Test.Hspec

instance MonadHttp IO where
  handleHttpException = throw

oneSecond :: IO ()
oneSecond = threadDelay $ 1 * 1000 * 1000

spec :: Spec
spec =
  ( \runTest -> do
      (portNum :: Int) <- getStdRandom (uniformR (10000, 20000))
      threadId <- forkIO $ run portNum server
      putStrLn $ "Port:" +| portNum |+ ""
      oneSecond
      let sendReq :: (Url 'Http -> Url 'Http) -> IO LB.ByteString
          sendReq path = responseBody <$> req GET (path $ http "localhost") NoReqBody lbsResponse (port portNum)
      runTest sendReq
      killThread threadId
  )
    `aroundAll` baseSpec

baseSpec :: SpecWith ((Url 'Http -> Url 'Http) -> IO LB.ByteString)
baseSpec = describe "" $ do
  it "should return JSON" $ \sendReq -> do
    decode @Rating <$> sendReq (\u -> u /: "rating" /: "0")
      >>= (`shouldBe` Just Great)
    decode @Book <$> sendReq (\u -> u /: "book" /: "0")
      >>= (`shouldBe` Just (MkBook 0 "Haskell in depth" 2021))
    decode @Int <$> sendReq (\u -> u /: "year" /: "0")
      >>= (`shouldBe` Just 2021)

  it "should return HTML" $ \sendReq ->
    sendReq (\u -> u /: "title" /: "0")
      >>= (`shouldBe` "<html><body><h1>Title</h1><div>Haskell in depth</div></body></html>") . LBC.unpack

  it "should throw an error if the id is invalid" $ \sendReq ->
    sendReq (\u -> u /: "title" /: "a")
      `shouldThrow` (\(e :: HttpException) -> "could not parse: `a'" `isInfixOf` show e)
  pure ()
