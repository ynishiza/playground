{-# LANGUAGE FunctionalDependencies #-}

module CPS.Class
  ( Invertible (..),
  )
where

class Invertible m n | m -> n where
  from :: m a -> n a
  to :: n a -> m a
