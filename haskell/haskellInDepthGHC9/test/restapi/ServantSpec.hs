{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module ServantSpec
  ( spec,
  )
where

import APIWithServant.API
import APIWithServant.Client
import Control.Concurrent
import Control.Exception
import Data.ByteString qualified as B
import Data.List (isInfixOf)
import Fmt
import Network.HTTP.Req
import Network.Wai.Handler.Warp
import Servant.Client qualified as S
import System.Random
import Test.Hspec
import Text.Blaze.Html.Renderer.String

instance MonadHttp IO where
  handleHttpException = throw

oneSecond :: IO ()
oneSecond = threadDelay $ 1 * 1000 * 1000

data TestArgs = MkTestArgs S.BaseUrl ((Url 'Http -> Url 'Http) -> IO B.ByteString)

spec :: Spec
spec =
  ( \runTest -> do
      (portNum :: Int) <- getStdRandom (uniformR (10000, 20000))
      threadId <- forkIO $ run portNum serverApp
      putStrLn $ "Port:" +| portNum |+ ""
      oneSecond
      let url = S.BaseUrl S.Http "localhost" portNum ""
          sendReq :: (Url 'Http -> Url 'Http) -> IO B.ByteString
          sendReq path = responseBody <$> req GET (path $ http "localhost") NoReqBody bsResponse (port portNum)
      runTest (MkTestArgs url sendReq)
      killThread threadId
  )
    `aroundAll` baseSpec

baseSpec :: SpecWith TestArgs
baseSpec = describe "" $ do
  it "should return JSON" $ \(MkTestArgs url _) -> do
    runClient url (getRating 0)
      >>= (`shouldBe` Right Great)
    runClient url (getBook 0)
      >>= (`shouldBe` Right (MkBook 0 "Haskell in depth" 2021))
    runClient url (getYear 0)
      >>= (`shouldBe` Right 2021)

  it "should return HTML" $ \(MkTestArgs url _) -> do
    runClient url (getTitle 1)
      >>= pure . (renderHtml <$>)
      >>= (`shouldBe` Right "<html><body><h1>Title</h1><div>Haskell in depth</div></body></html>")

  it "should throw an error if the id is invalid" $ \(MkTestArgs _ sendReq) ->
    sendReq (\u -> u /: "title" /: "a")
      `shouldThrow` (\(e :: HttpException) -> "could not parse: `a'" `isInfixOf` show e)
  pure ()
