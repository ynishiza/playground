{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Lens
  ( Field1 (..),
    Field2 (..),
    Field3 (..),
    module X,
  )
where

import Data.Function ((&))
import Lens.Fold as X
import Lens.Get as X
import Lens.Index as X
import Lens.Lens as X
import Lens.Set as X
import Lens.Prism as X
import Lens.Traverse as X

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _1 :: (ProfunctorArrow p, Functor f) => Optic p f s t a b

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: (ProfunctorArrow p, Functor f) => Optic p f s t a b

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _3 :: (ProfunctorArrow p, Functor f) => Optic p f s t a b

instance Field1 (a, b) (a', b) a a' where
  _1 p =
    lmap fst p
      & strong (\(_, b) x -> (,b) <$> x)

instance Field2 (a, b) (a, b') b b' where
  _2 p =
    lmap snd p
      & strong (\(a, _) x -> (a,) <$> x)

instance Field1 (a, b, c) (a', b, c) a a' where
  _1 p =
    lmap (\(a, _, _) -> a) p
      & strong (\(_, b, c) x -> (,b,c) <$> x)

instance Field2 (a, b, c) (a, b', c) b b' where
  _2 p =
    lmap (\(_, b, _) -> b) p
      & strong (\(a, _, c) x -> (a,,c) <$> x)

instance Field3 (a, b, c) (a, b, c') c c' where
  _3 p =
    lmap (\(_, _, x) -> x) p
      & strong (\(a, b, _) x -> (a,b,) <$> x)

instance Field3 (a, b, c, d) (a, b, c', d) c c' where
  _3 p =
    lmap (\(_, _, c, _) -> c) p
      & strong (\(a, b, _, d) x -> (a,b,,d) <$> x)
