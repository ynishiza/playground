module ContState
  ( get,
    put,
    modify,
    tick,
    ContState,
    module X,
  )
where

import Control.Monad.Trans.Cont as X

type ContState s m a = ContT (s -> m s) m a

get :: Monad m => ContState s m s
get = ContT $ \k -> return $ \state -> do
  next <- k state
  next state

modify :: Monad m => (s -> s) -> ContState s m ()
modify f = ContT $ \k -> return $ \state -> do
  next <- k ()
  next $ f state

put :: Monad m => s -> ContState s m ()
put = modify . const

tick :: Monad m => ContState Int m ()
tick = modify (+ 1)
