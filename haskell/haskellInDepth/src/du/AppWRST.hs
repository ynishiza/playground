{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module AppWRST
  ( DuApp (..),
    runAll,
  )
where

import AppTypes
import Control.Monad.IO.Class
import Control.Monad.Reader 
import Control.Monad.State 
import Control.Monad.Writer

newtype DuApp l s a = DuApp {runDuApp :: ReaderT AppEnv (WriterT [l] (StateT s IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad
    )

instance forall l s . MonadWriter [l] (DuApp l s) where
  writer = DuApp . lift . writer
  listen (DuApp (ReaderT r)) = DuApp $ ReaderT $ listen <$> r
  pass (DuApp (ReaderT r)) = DuApp $ ReaderT $ pass <$> r

instance forall l s. MonadState s (DuApp l s) where
  get = DuApp $ lift $ lift get
  put = DuApp . lift . lift . put

instance forall l s. MonadReader AppEnv (DuApp l s) where
  ask = DuApp ask
  local f (DuApp app) = DuApp (local f app)

instance forall l s. MonadIO (DuApp l s) where
  liftIO io = DuApp (lift $ lift $ lift io)

runAll :: DuApp l s a -> AppEnv -> s -> IO (a, [l], s)
runAll (DuApp app) env st = do
  ((res, logs), st') <- runStateT (runWriterT (runReaderT app env)) st
  return (res, logs, st')
