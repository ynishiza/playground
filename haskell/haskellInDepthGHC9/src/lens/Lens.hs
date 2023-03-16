{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Lens
  ( Field1 (..),
    Field2 (..),
    module X,
  )
where

import Lens.Fold as X
import Lens.Get as X
import Lens.Lens as X
import Lens.Traverse as X

type P s t a b = forall f p. (Functor f, NormalProfunctor p) => p a (f b) -> p s (f t)

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _1 :: P s t a b

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: P s t a b

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _3 :: P s t a b

instance Field1 (a, b) (a', b) a a' where
  _1 =
    normalDimap
      fst
      (\(_, b) x -> (,b) <$> x)

instance Field2 (a, b) (a, b') b b' where
  _2 =
    normalDimap
      snd
      (\(a, _) x -> (a,) <$> x)

instance Field1 (a, b, c) (a', b, c) a a' where
  _1 =
    normalDimap
      (\(a, _, _) -> a)
      (\(_, b, c) x -> (,b,c) <$> x)

instance Field2 (a, b, c) (a, b', c) b b' where
  _2 =
    normalDimap
      (\(_, b, _) -> b)
      (\(a, _, c) x -> (a,,c) <$> x)

instance Field3 (a, b, c) (a, b, c') c c' where
  _3 =
    normalDimap
      (\(_, _, c) -> c)
      (\(a, b, _) x -> (a,b,) <$> x)

instance Field3 (a, b, c, d) (a, b, c', d) c c' where
  _3 =
    normalDimap
      (\(_, _, c, _) -> c)
      (\(a, b, _, d) x -> (a,b,,d) <$> x)
