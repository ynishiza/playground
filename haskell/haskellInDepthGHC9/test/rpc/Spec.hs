{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Spec (specs) where

import Control.Concurrent
import Data.List (isSubsequenceOf)
import Network.Connection
import RPC.Base
import Test.Hspec

specs :: SpecWith ()
specs =
  ( \start -> do
      thread <- forkIO $ startOperationServer defaultRPCParams (sampleTable @())
      putStrLn "Waiting until server is ready"
      threadDelay $ 1 * 1000 * 1000
      start
      killThread thread
  )
    `aroundAll_` baseSpec

baseSpec :: Spec
baseSpec =
  describe
    "test server"
    ( do
        it "can process operations" $ \_ -> do
          requestOperation defaultRPCParams "hello" ()
            >>= shouldBe "World"
          requestOperation @(Int, Int) defaultRPCParams "add" (1, 2)
            >>= shouldBe (3 :: Int)
          requestOperation @(Int, Int) defaultRPCParams "add" (3, 2)
            >>= shouldBe (5 :: Int)

        it "should throw an error if the parameters are invalid" $ \_ -> do
          requestOperation @() @() defaultRPCParams "sum" ()
            `shouldThrow` (\e@(OperationCallFail _) -> 
              "Failed to decode at stage Stage2 with error:\"too few bytes" `isSubsequenceOf` show e)

        it "should exit immediately if the server is not reachable" $ \_ -> do
          let badCon = (connectionParams defaultRPCParams) {connectionPort = 11111}
          requestOperation @(Double, Double) @Double (defaultRPCParams {connectionParams = badCon}) "divide" (1.0, 0.0)
            `shouldThrow` ( \e@(HostCannotConnect _ _) ->
                              "Connection refused" `isSubsequenceOf` show e
                          )

        it "should raise an error if the operation crashes" $ \_ ->
          requestOperation @(Double, Double) @Double defaultRPCParams "divide" (1.0, 0.0)
            `shouldThrow` ( \e@(OperationCallFail _) ->
                              "OperationCallFail : error: divide by zero" `isSubsequenceOf` show e
                          )

        it "should raise an error if the operation is not supported" $ \_ ->
          requestOperation @() @() defaultRPCParams "BAD" ()
            `shouldThrow` (\(OperationNotFound _) -> True)
    )
