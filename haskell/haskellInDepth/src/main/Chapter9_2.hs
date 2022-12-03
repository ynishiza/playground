{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Chapter9_2 (run) where

import Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Fmt
import Utils

bt :: a
bt = undefined

run :: TestState
run =
  createChapterTest
    "9.2"
    "Strict"
    ( do
        testBang @Int "bottom" bt >>= assertFalse
        testSeq @Int "bottom" bt >>= assertFalse
        testBang "undefined * 2" (bt * 2) >>= assertFalse
        testSeq "undefined * 2" (bt * 2) >>= assertFalse

        testBang "Just undefined" (Just bt) >>= assertSuccess
        testSeq "Just undefined" (Just bt) >>= assertSuccess
        testDone
    )
  where
    assertSuccess = assertIsEqual True . snd
    assertFalse = assertIsEqual False . snd

testBang :: T.Text -> a -> IO (T.Text, Bool)
testBang expr x = testStrict  ("! (" +| expr |+ ")") $ let !y = x in return ()

testSeq :: T.Text -> a -> IO (T.Text, Bool)
testSeq expr x = testStrict ("seq " +|| expr ||+ "") $ return $ seq x ()

testStrict :: Show a => T.Text -> IO a -> IO (T.Text, Bool)
testStrict label io =
  ( do
      v <- io
      fmtLn ("OK "+| label|+"" <> indentF 1 (build (show v)))
      return (label, True)
  )
    `catch` f
  where
    f :: SomeException -> IO (T.Text, Bool)
    f e =
      let msg = fmt $ nameF "ERROR" (build label) <> indentF 1 (build $ show e)
       in T.putStrLn msg >> return (msg, False)
