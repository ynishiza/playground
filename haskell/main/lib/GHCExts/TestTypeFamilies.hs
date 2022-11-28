{-# LANGUAGE TypeFamilies #-}
module GHCExts.TestTypeFamilies (testTypeFamily) where

import GHC.Types
import TestUtils

class MyConts a where 
  data MyContainer a :: Type 

  myEmpty :: MyContainer a 
  myAppend :: a -> MyContainer a -> MyContainer a 

instance MyConts Int where
  data MyContainer Int = MyContainer [Int]
  myEmpty = MyContainer []
  myAppend x (MyContainer l) = MyContainer (x:l)

deriving instance Show (MyContainer Int)

testTypeFamily :: TestState
testTypeFamily = createTest (do
  let 
    x = myEmpty :: MyContainer Int
    y = 1 `myAppend` (2 `myAppend` x)

  print y

  testDone) "testTypeFamily"
