{-# LANGUAGE RankNTypes #-}

module Lens.Prism
  ( _Just,
    _Nothing,
    _Left,
    _Right,
  )
where

import Data.Function ((&))
import Lens.Lens

_Just :: Prism (Maybe a) (Maybe b) a b
_Just ka =
  right' ka
    & dimap
      (maybe (Left Nothing) Right)
      (either (const (pure Nothing)) (Just <$>))

_Nothing :: Prism (Maybe a) (Maybe a) () ()
_Nothing = _Nothing' ()

_Nothing' :: b -> Prism (Maybe a) (Maybe a) b c
_Nothing' b ka =
  right' ka
    & dimap
      (maybe (Right b) Left)
      (either (const (pure Nothing)) (const Nothing <$>))

_Left :: Prism (Either a b) (Either c b) a c
_Left ka =
  left' ka
    & dimap
      id
      (either (Left <$>) (pure . Right))

_Right :: Prism (Either a b) (Either a c) b c
_Right ka =
  right' ka
    & dimap
      id
      (either (pure . Left) (Right <$>))
