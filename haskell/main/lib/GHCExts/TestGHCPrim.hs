{-# LANGUAGE MagicHash #-} 
module GHCExts.TestGHCPrim (testBasic) where
  
import GHC.Exts
import Data.Typeable
import TestUtils

testBasic = createTest (do
  let 
    x = I# 1#
    y = I# (undefined :: Int#)
    z :: Int#
    z = undefined

    in do
      assertIsEqual x 1
      assertIsEqual (typeOf x) (typeOf @Int 1)
  testDone) "GHCPrim.basic"

