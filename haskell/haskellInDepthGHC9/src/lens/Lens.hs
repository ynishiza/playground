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

import Lens.Class as X
import Lens.Fold as X
import Lens.Get as X
import Lens.Proofs as X

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

class Field1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _1 :: Lens s t a b

class Field2 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _2 :: Lens s t a b

class Field3 s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _3 :: Lens s t a b

instance Field1 (a, b) (a', b) a a' where _1 f (a, b) = (,b) <$> f a

instance Field2 (a, b) (a, b') b b' where _2 f (a, b) = (a,) <$> f b

instance Field1 (a, b, c) (a', b, c) a a' where _1 f (a, b, c) = (,b,c) <$> f a

instance Field2 (a, b, c) (a, b', c) b b' where _2 f (a, b, c) = (a,,c) <$> f b

instance Field3 (a, b, c, d) (a, b', c, d) b b' where _3 f (a, b, c, d) = (a,,c,d) <$> f b
