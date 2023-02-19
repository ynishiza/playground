{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Collapse lambdas" #-}
{-# HLINT ignore "Avoid lambda" #-}

module State
  ( 
    tick,
    CtTState,
    module X,
  )
where

import Control.Monad
import Control.Monad.State.Class as X hiding (state) 
import Control.Monad.Writer.Class as X
import Control.Monad.Reader.Class as X
import CtT

-- Representing state as a continuation
-- i.e.
--      continuation k = a -> state -> state        a = result of current
--                                                  first state = current state
--                                                  second state = new state

type CtTState state m = CtT (state -> m state) (state -> m state) m

instance Monad m => MonadState s (CtTState s m) where
  get = CtT $ \k -> return $ \s -> k s >>= ($ s)
  put s = CtT $ \k -> return $ \_ -> k () >>= ($ s)

instance (Monoid w, Monad m) => MonadWriter w (CtT w w m) where
  tell w = CtT $ \k -> (<> w) <$> k () 
  listen = undefined
  pass = undefined

instance Monad m => MonadReader e (CtT (e -> m e) (e -> m e) m) where
  ask = CtT $ \k -> return $ \e -> k e >>= ($ e)
  local fn (CtT c) = CtT $ c >=> \f -> return (f . fn)

tick :: Monad m => CtTState Int m ()
tick = modify (+1)
