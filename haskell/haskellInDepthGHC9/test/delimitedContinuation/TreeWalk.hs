{-# LANGUAGE GADTs #-}

module TreeWalk
  ( Tree (..),
    walkTree,
    tree0,
    tree1,
    tree2,
    tree3,
    tree4,
    printGen,
    collectGen,
    sameFringe,
  )
where

import Ct
import Data.Foldable

data Tree a where
  Empty :: Tree a
  Node :: Tree a -> a -> Tree a -> Tree a

tree0 :: Tree a
tree0 = Empty

tree1 :: Tree Int
tree1 = Node Empty 1 Empty

tree2 :: Tree Int
tree2 = Node Empty 2 Empty

tree3 :: Tree Int
tree3 = Node Empty 3 Empty

tree4 :: Tree Int
tree4 = Node (Node tree1 4 (Node tree2 5 tree3)) 6 tree1

data Generator a where
  Done :: Generator a
  Next :: a -> (() -> Generator a) -> Generator a

instance Foldable Generator where
  foldr _ v0 Done = v0
  foldr f v0 (Next a k) = f a $ foldr f v0 $ k ()

instance Functor Generator where
  fmap _ Done = Done
  fmap f (Next a k) = Next (f a) $ fmap f . k

instance Traversable Generator where
  sequenceA Done = pure Done
  sequenceA (Next a k) = Next <$> a <*> (const <$> sequenceA (k ()))

walkTree' :: Tree a -> Ctf (Generator a) (Generator a) ()
walkTree' Empty = liftct ($ ())
walkTree' (Node l n r) = do
  walkTree' l
  liftct $ \k -> Next n k
  walkTree' r

walkTree :: Tree a -> Generator a
walkTree = evalCtfWith (const Done) id . walkTree'

printGen :: Show a => Generator a -> IO ()
printGen = traverse_ (putStrLn . ("value:" <>) . show)

collectGen :: Generator a -> [a]
collectGen = concatMap (: [])

sameFringe :: Eq a => Tree a -> Tree a -> Bool
sameFringe t1 t2 = eq (walkTree t1) (walkTree t2)
  where
    eq Done Done = True
    eq (Next a1 k1) (Next a2 k2) = a1 == a2 && eq (k1 ()) (k2 ())
    eq _ _ = False
