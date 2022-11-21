{-# LANGUAGE OverloadedStrings #-} 
module Main (main) where

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import GHC.Exts

a = "123" :: ByteString
b = fromString @ByteString "as"

main = putStrLn ""
