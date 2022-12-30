{-# HLINT ignore "Use section" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MessagingSpec (specs) where

import Common
import Data.List (isSubsequenceOf)
import Network.Connection
import RPC.Base
import Test.Hspec

specs :: SpecWith ()
specs =
  testBootstrap
    (flip startOperationServer (sampleTable @()))
    `aroundAll` baseSpec

baseSpec :: SpecWith RPCParams
baseSpec =
  describe
    "test server"
    ( do
        it "can process operations" $ \params -> do
          requestOperation params "hello" ()
            >>= shouldBe "World"
          requestOperation @(Int, Int) params "add" (1, 2)
            >>= shouldBe (3 :: Int)
          requestOperation @(Int, Int) params "add" (3, 2)
            >>= shouldBe (5 :: Int)

        it "should throw an error if the parameters are invalid" $ \params -> do
          requestOperation @Int @() params "sum" 1
            `shouldThrow` ( \e@(OperationCallFail _) ->
                              "Failed to decode at stage Stage2 with error:\"too few bytes" `isSubsequenceOf` show e
                          )

        it "should exit immediately if the server is not reachable" $ \params -> do
          let badCon = (connectionParams params) {connectionPort = 11111}
          requestOperation @(Double, Double) @Double (params {connectionParams = badCon}) "divide" (1.0, 0.0)
            `shouldThrow` ( \e@(HostCannotConnect _ _) ->
                              "Connection refused" `isSubsequenceOf` show e
                          )

        it "should raise an error if the operation crashes" $ \params ->
          requestOperation @(Double, Double) @Double params "divide" (1.0, 0.0)
            `shouldThrow` ( \e@(OperationCallFail _) ->
                              "OperationCallFail : error: divide by zero" `isSubsequenceOf` show e
                          )

        it "should raise an error if the operation is not supported" $ \params ->
          requestOperation @() @() params "BAD" ()
            `shouldThrow` (\(OperationNotFound _) -> True)
    )
