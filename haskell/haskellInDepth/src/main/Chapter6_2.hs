{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Chapter6_2 (MyApp(..), processMyApp, myTask, myMain) where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State (StateT (..))
import Control.Monad.Trans.Writer (WriterT (..))
import Control.Monad.Writer.Class

newtype MyApp r w s a = MyApp {runMyApp :: ReaderT r (WriterT [w] (StateT s IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader r,
      MonadState s,
      MonadWriter [w],
      MonadIO
    )

processMyApp :: MyApp r w s a -> r -> s -> IO (a, [w], s)
processMyApp (MyApp app) r st = do
  ((res, logs), st') <- runStateT (runWriterT (runReaderT app r)) st
  return (res, logs, st')

myTask :: MyApp Bool Int String Bool
myTask = do
  x <- ask
  tell [1]
  put "test"
  liftIO $ putStrLn "Hello"
  return x

-- e.g. without transformer class
-- i.e. manual lifting
task2 :: MyApp Bool Int String ()
task2 = MyApp $ do
  x <- ask                             -- ReaderT at top level
  lift $ tell [0]                      -- WriterT at 2nd level
  lift $ lift $ put "Hello"            -- StateT at 3rd level
  lift $ lift $ lift $ putStr "Hello"  -- IO

-- same with transformer class
task1 :: MyApp Bool Int String ()
task1 = do
  x <- ask                            -- MonadReader
  tell [0]                            -- MonadWriter
  put "Hello"                         -- MonadState
  liftIO $ putStrLn "Hello"           -- MonadIO

newtype MyApp2 r w s a = MyApp2 {runMyApp2 :: ReaderT r (WriterT [w] (StateT s IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad
    )

instance MonadWriter [w] (MyApp2 r w s) where
  writer = MyApp2 . lift . writer
  listen (MyApp2 (ReaderT r)) = MyApp2 $ ReaderT $ listen <$> r
  pass (MyApp2 (ReaderT r)) = MyApp2 $ ReaderT $ pass <$> r

instance MonadState s (MyApp2 r w s) where
  get = MyApp2 $ lift $ lift get
  put = MyApp2 . lift . lift . put

instance MonadReader r (MyApp2 r w s) where
  ask = MyApp2 ask
  local f (MyApp2 app) = MyApp2 (local f app)

instance MonadIO (MyApp2 r w s) where
  liftIO io = MyApp2 (lift $ lift $ lift io)

myMain :: IO ()
myMain = processMyApp myTask True "" >>= print
