{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ScratchSpace
  ( MyShow (..),
    MyShow2 (..),
  )
where

import Data.Foldable
import Data.List (intercalate)
import Data.Coerce
import Unsafe.Coerce

type family IdF a where
  IdF a = Wrap1 a

data Wrap1 a = Wrap1 a deriving (Show, Eq)

newtype WrapFamily a = WrapFamily (IdF a)

deriving instance Eq a => Eq (WrapFamily a)

deriving instance Show a => Show (WrapFamily a)

unsafeWrap :: WrapFamily Int -> WrapFamily (Wrap1 Int)
-- unsafeWrap = coerce
unsafeWrap = unsafeCoerce

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

instance {-# OVERLAPPING #-} MyShow2 Int where
  myShow2 v = "INT " ++ show v

instance {-# OVERLAPPING #-} MyShow2 Bool where
  myShow2 v = "BOOL " ++ show v

data Showable = forall a. Show a => MkShowable a

instance Show Showable where show (MkShowable x) = show x

list =
  [ MkShowable 1,
    MkShowable True,
    MkShowable $ Just 1,
    MkShowable "A"
  ]

io :: IO ()
io = traverse_ print
  list
c :: String
c = intercalate "," (show <$> list) 
