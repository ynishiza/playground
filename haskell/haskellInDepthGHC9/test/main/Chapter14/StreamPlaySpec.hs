{-# LANGUAGE OverloadedStrings #-}

module Chapter14.StreamPlaySpec
  ( spec,
  )
where

import Chapter14.StreamPlay
import Control.Monad.Trans.Writer
import Streaming.Prelude qualified as S
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B
import System.Random
import Test.Hspec

spec :: SpecWith ()
spec = describe "stream play" $ do
  describe "tabulate" $ do
    it "should tabulate" $ do
      tabulateCollapse 2 (S.each [1 .. 10 :: Int]) >>= (`shouldBe` "1\t2\t\n3\t4\t\n5\t6\t\n7\t8\t\n9\t10\t\n")

    it "should tabulate and sum" $ do
      tabulateAndSum 2 (S.each [1 .. 10 :: Int]) >>= (`shouldBe` ("1\t2\t\n3\t4\t\n5\t6\t\n7\t8\t\n9\t10\t\n", 55))

  describe "logChar" $ do
    it "should log in parallel with IO" $ do
      ref <- newIORef @Text ""
      logCharRawAndCodeWithRef ref $ S.each ("haskell" :: String)
      readIORef ref >>= (`shouldBe` "104 h 97 a 115 s 107 k 101 e 108 l 108 l ")

    it "should write in parallel with Writer" $ do
      r <- newIORef (pure () :: WriterT Text IO ())
      let writeLog :: forall n. MonadIO n => Text -> n ()
          writeLog txt = liftIO $ modifyIORef r (>> tell (txt |+ " "))

      (_, txt) <- runWriterT @Text $ do
        logCharRawAndCodeWith writeLog (S.each ("haskell" :: String))
        join $ liftIO $ readIORef r
      txt `shouldBe` "104 h 97 a 115 s 107 k 101 e 108 l 108 l "

  it "log" $ do
    let w :: Stream (Of Char) (WriterT Text IO) ()
        w = logCharCodeAndRawWriter $ S.each ("haskell" :: String)
    (_, txt) <- runWriterT $ S.effects w
    txt `shouldBe` "104h97a115s107k101e108l108l"

  describe "Bytestream" $ do
    it "copies a file" $ do
      (n :: Int) <- getStdRandom uniform
      let src = "/tmp/test.txt"
          dst = "/tmp/test2.txt"
          txt = "data:" +| n |+ ""
          txtLength = B.length $ B.pack txt
      putStrLn txt
      writeFile src txt
      l <- copyFile src dst
      l `shouldBe` txtLength
      readFile dst >>= (`shouldBe` txt)
