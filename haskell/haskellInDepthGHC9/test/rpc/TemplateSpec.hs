{-# HLINT ignore "Use section" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module TemplateSpec
  ( spec,
  )
where

import Common
import GHC.Real
import RPC.Base
import Test.Hspec
import TestClient qualified as C
import TestServer qualified as S

spec :: SpecWith ()
spec =
  testBootstrap
    (flip startOperationServer S.generatedTable)
    `aroundAll` baseSpec

baseSpec :: SpecWith RPCParams
baseSpec = describe "template generated client and server stubs" $ do
  it "should process a remote operation" $ \params -> do
    requestRSIO params (C.echo "Hello") >>= (`shouldBe` "echo:Hello")

  it "should process an operation with no input" $ \params ->
    requestRSIO params C.reset
      >> requestRSIO params C.ping
      >>= (`shouldBe` 1)

  it "should process an operation with multiple inputs" $ \params ->
    requestRSIO params (C.to3DPoint 1 2 3)
      >>= (`shouldBe` (1, 4, 9))

  it "should process an operation with complex inputs" $ \params ->
    requestRSIO params (C.unwrapAll (Just (Just (Right 10))))
      >>= (`shouldBe` 10)
      >> requestRSIO params (C.unwrapAll (Just Nothing))
      >>= (`shouldBe` fromRational infinity)

  it "should be able to perform multiple calls in a single connection" $ \params -> do
    res <- requestRSIO params $ do
      l <- C.reset >> replicateM 5 C.ping
      h <- C.echo "hello"
      return (l, h)
    res `shouldBe` ([1, 2, 3, 4, 5], "echo:hello")

  it "should raise an error if the operation is not supported" $ \params ->
    requestRSIO params C.predictFuture
      `shouldThrow` ( \(OperationNotFound m) ->
                        m == "predictFuture"
                    )

  describe "client only" $ do
    it "should support operations with parameterized inputs" $ \params ->
      requestRSIO params (C.trivial @Int 1)
        >>= (`shouldBe` 1)
        >> requestRSIO params (C.trivial "hello")
        >>= (`shouldBe` "hello")

    it "should support forall" $ \params ->
      requestRSIO params (C.trivialForAll @Int 1)
        >>= (`shouldBe` 1)
