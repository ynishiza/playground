{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Chapter12.DerivingVia
  ( run,
    Age,
    MyList,
  )
where

import Control.Applicative
import Data.Monoid
import Fmt
import Utils

run :: TestState
run =
  createChapterTest
    "12.1.2"
    "Deriving via"
    ( do
        let i :: Int
            i = 20
            l :: [Int]
            l = [1 .. 4]
         in do
              assertIsEqual (show $ Age 1) "1"

              assertIsEqual (show $ MyList l) (show l)
              assertIsEqual (pure i) (MyList [i])
              assertIsEqual (return i) (MyList [i])
              assertIsEqual ((* 2) <$> MyList l) (MyList $ (* 2) <$> l)
              assertIsEqual ((,) <$> MyList l <*> MyList l) (MyList $ (,) <$> l <*> l)
              assertIsEqual (MyList l >>= pure . (* 2)) (MyList $ (* 2) <$> l)

              assertIsEqual (pure i) (MyMaybe (Just i))
              assertIsEqual mempty (MyMaybe @Int Nothing)
              assertIsEqual (MyMaybe (Just i) <> MyMaybe Nothing) (MyMaybe (Just i <|> Nothing))
              assertIsEqual (MyMaybe (Just i) <> MyMaybe (Just 100)) (MyMaybe (Just i <|> Just 100))
              assertIsEqual (MyMaybe (Just 100) <> MyMaybe (Just i)) (MyMaybe (Just 100 <|> Just i))
              assertIsEqual (MyMaybe Nothing <> MyMaybe (Just i)) (MyMaybe (Nothing <|> Just i))
              assertIsEqual (MyMaybe @Int Nothing <> MyMaybe Nothing) (MyMaybe (Nothing <|> Nothing))
        testDone
    )

newtype Age = Age Int
  deriving (Eq, Show, Buildable) via Int

newtype MyList a = MyList [a]
  deriving (Eq, Show) via [a]
  deriving (Functor, Applicative, Monad) via []

newtype MyMaybe a = MyMaybe (Maybe a)
  deriving (Eq, Show) via (Maybe a)
  deriving (Functor, Applicative, Monad, Alternative) via Maybe
  deriving (Semigroup, Monoid) via (Alt Maybe a)

newtype MyMaybe2 a = MyMaybe2 (Maybe a)
deriving via (Maybe a) instance Show a => Show (MyMaybe2 a)
deriving via (Maybe a) instance Eq a => Eq (MyMaybe2 a)
deriving via Maybe instance Functor MyMaybe2
deriving via (Alt Maybe a) instance Semigroup (MyMaybe2 a)
deriving via (Alt Maybe a) instance Monoid (MyMaybe2 a)
