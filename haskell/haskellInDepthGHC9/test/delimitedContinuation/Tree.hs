{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
module Tree
  ( Tree (..),
    walkTree,
    gprint,
    sameFringeG,
  )
where

import CtT
import Data.Function

data Tree a where
  Empty :: Tree a
  Node :: Tree a -> a -> Tree a -> Tree a
  deriving (Functor)

data CGenerator a where
  Done :: CGenerator a
  Next :: a -> (() -> CGenerator a) -> CGenerator a

instance Foldable CGenerator where
  foldr _ v Done = v
  foldr f v (Next a k) = f a (foldr f v (k ()))

walkTree' :: Tree a -> Ct (CGenerator a) (CGenerator a) ()
walkTree' Empty = ct ($ ())
walkTree' (Node l a r) =
  walkTree' l
    #>>= const (shift $ \k -> exitT (Next a k))
    #>>= const (walkTree' r)

walkTree :: Tree a -> CGenerator a
walkTree = ($ const Done) . runCt . walkTree'

gprint :: Show a => CGenerator a -> IO ()
gprint = foldr (\a io -> putStrLn ("value:" <> show a) >> io) (putStrLn "Done")

gEq :: Eq a => CGenerator a -> CGenerator a -> Bool
gEq Done Done = True
gEq (Next a1 k1) (Next a2 k2) = a1 == a2 && gEq (k1 ()) (k2 ())
gEq _ _ = False

sameFringeG :: Eq a => Tree a -> Tree a -> Bool
sameFringeG = on gEq walkTree
