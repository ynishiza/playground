{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Lens.Monoid.Monoid
  ( 
    XFirst (..),
    getXFirst,
    module X
  )
where

import Lens.Monoid.Indexing as X
import Lens.Monoid.FreeAp as X
import Lens.Monoid.FreeTake as X

data XFirst a where
  XFirst :: Maybe a -> XFirst a

getXFirst :: XFirst a -> Maybe a
getXFirst (XFirst x) = x

instance Semigroup (XFirst a) where
  (XFirst Nothing) <> x = x
  x <> _ = x

instance Monoid (XFirst a) where
  mempty = XFirst Nothing
