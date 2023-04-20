{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Lens.Proofs () where

import Data.Function ((&))
import Lens
import Data.Functor.Const
import Data.Functor.Contravariant

traversalIsSetter :: Traversal s t a b -> Setter s t a b
traversalIsSetter = id

traversalIsFold :: Traversal s s a a -> Fold s a
traversalIsFold = id

getterIsFold :: Getter s s a a -> Fold s a
getterIsFold = id

lensIsGetter :: Lens s t a b -> Getter s t a b
lensIsGetter = id

lensIsSetter :: Lens s t a b -> Setter s t a b
lensIsSetter = id

lensIsFold :: Lens s s a a -> Fold s a
lensIsFold = id

lensIsTraversal :: Lens s t a b -> Traversal s t a b
lensIsTraversal = id

trT :: Traversable t => Traversal (t a) (t b) a b
trT = traverse

trF :: Traversable t => Fold (t a) a
trF = trT

trG :: Traversable t => Setter (t a) (t b) a b
trG = trT

gtG :: Getter (a, c) (b, c) a b
gtG = _1

gtF :: Fold (a, c) a
gtF = gtG

lensL :: Lens (a, c) (b, c) a b
lensL = _1

lensG :: Getter (a, c) (b, c) a b
lensG = lensL

lensT :: Traversal (a, c) (b, c) a b
lensT = lensL

lensF :: Setter (a, c) (b, c) a b
lensF = lensL

lensS :: Fold (a, c) a
lensS = lensL

setterToFrom :: ((a -> b) -> s -> t) -> (a -> b) -> s -> t
setterToFrom = fromSetter . toSetter

setterFromTo :: ASetter s t a b -> Setter s t a b
setterFromTo = toSetter . fromSetter

getterFromTo :: Getting r s a -> Getting r s a
getterFromTo = toGetter . fromGetter

getterToFrom :: ((a -> r) -> s -> r) -> (a -> r) -> s -> r
getterToFrom = fromGetter . toGetter

foldToFrom :: ((a -> r) -> s -> r) -> (a -> r) -> s -> r
foldToFrom = fromFoldMap . toFoldMap

foldFromTo :: Getting a s a -> Getting a s a
foldFromTo = toFoldMap . fromFoldMap

lensTraverseLens :: LensLike f s t a b -> LensLike f s t a b
lensTraverseLens = fromTraverse . toTraverse

traverseLensTraverse :: ((a -> f b) -> s -> f t) -> ((a -> f b) -> s -> f t)
traverseLensTraverse = toTraverse . fromTraverse
