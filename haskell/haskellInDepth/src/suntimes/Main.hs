{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Main (main) where

import App
import ProcessRequest
import qualified Playground
import Types

main :: IO ()
main = do
  Playground.run
  runApp ProcessRequest.processInteractive defaultWebAPIAuth
  return ()
