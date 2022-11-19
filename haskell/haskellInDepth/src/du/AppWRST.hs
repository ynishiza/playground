{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# OPTIONS_GHC -Wno-missing-methods #-}

module AppWRST
  ( DuApp (..),
    runAll,
  )
where


import AppTypes
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Writer.Class

newtype DuApp l s a = DuApp { runDuApp :: ReaderT AppEnv (WriterT [l] (StateT s IO)) a }
  deriving
    ( Functor,
      Applicative,
      Monad,
      -- MonadIO,
      MonadReader AppEnv,
      MonadWriter [l],
      MonadState s
    )

instance MonadIO (DuApp l s) where
  liftIO io = DuApp (lift $ lift $ lift io)

runAll :: DuApp l s a -> AppEnv -> s -> IO (a, [l], s)
runAll (DuApp app) env st = do
  ((res, logs), st') <- runStateT (runWriterT (runReaderT app env)) st
  return (res, logs, st')
