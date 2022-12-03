{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Chapter12.Overlapping (
  run
  ) where

import Utils

run :: TestState
run = createChapterTest "12" "Overlapping instances" (do
  assertIsEqual (myShow ()) "()"
  assertIsEqual (myShow @Double 1.1) "1.1"
  assertIsEqual (myShow @Int 1) "INT 1"
  assertIsEqual (myShow True) "BOOL True"

  assertIsEqual (myShow2 ()) "()"
  assertIsEqual (myShow2 @Double 1.1) "1.1"
  assertIsEqual (myShow2 @Int 1) "INT 1"
  assertIsEqual (myShow2 True) "BOOL True"
  testDone)

class MyShow a where
  myShow :: a -> String
  default myShow :: Show a => a -> String
  myShow = show

instance Show a => MyShow a where
  myShow = show 

instance {-# OVERLAPPING #-} MyShow Int where
  myShow v = "INT " ++ show v

instance {-# OVERLAPPING #-} MyShow Bool where
  myShow v = "BOOL " ++ show v

class MyShow2 a where
  myShow2 :: a -> String
  default myShow2 :: Show a => a -> String
  myShow2 = show

deriving instance Show a => MyShow2 a

instance  {-# OVERLAPPING #-}MyShow2 Int where
  myShow2 v = "INT " ++ show v

instance {-# OVERLAPPING #-} MyShow2 Bool where
  myShow2 v = "BOOL " ++ show v
