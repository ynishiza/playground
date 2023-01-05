{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Chapter6_2
  ( MyApp1 (..),
    MyApp2 (..),
    processMyApp,
    myTask,
    run,
    -- mapLevel1,
    -- mapLevel2,
    -- mapLevel3,
    -- mapLevel4,
    taskWithManualLift,
    taskWithTransformLift,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

type MyApp0 r w s e = ReaderT r (WriterT [w] (StateT s (ExceptT e IO)))

newtype MyApp1 r w s e a = MyApp1 {runMyApp :: ReaderT r (WriterT [w] (StateT s (ExceptT e IO))) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader r,
      MonadState s,
      MonadWriter [w],
      MonadError e,
      MonadIO
    )

newtype MyApp2 r w s e a = MyApp2 {runMyApp2 :: ReaderT r (WriterT [w] (StateT s (ExceptT e IO))) a}
  deriving
    ( Functor,
      Applicative,
      Monad
    )

processMyApp0 :: MyApp0 r w s e a -> r -> s -> IO (Either e (a, [w], s))
processMyApp0 app r st = do
  x <- runExceptT $ runStateT (runWriterT (runReaderT app r)) st
  -- Right ((res, logs), st') <- runExceptT $ runStateT (runWriterT (runReaderT app r)) st
  return $ either Left (\((res, logs), st') -> Right (res, logs, st')) x

processMyApp :: MyApp1 r w s e a -> r -> s -> IO (a, [w], s)
processMyApp (MyApp1 app) r st = do
  Right ((res, logs), st') <- runExceptT $ runStateT (runWriterT (runReaderT app r)) st
  return (res, logs, st')

-- mapLevel1 :: MyApp1 r w s a -> (ReaderT r (WriterT [w] (StateT s IO)) a -> ReaderT r (WriterT [w] (StateT s IO)) b) -> MyApp1 r w s b
-- mapLevel1 (MyApp1 r) f = MyApp1 $ f r

-- mapLevel2 :: MyApp1 r w s a -> (WriterT [w] (StateT s IO) a -> WriterT [w] (StateT s IO) b) -> MyApp1 r w s b
-- mapLevel2 (MyApp1 (ReaderT r)) f = MyApp1 $ ReaderT (f <$> r)

-- mapLevel3 :: MyApp1 r w s a -> (StateT s IO (a, [w]) -> StateT s IO (b, [v])) -> MyApp1 r v s b
-- mapLevel3 (MyApp1 (ReaderT r)) f = MyApp1 $ ReaderT (WriterT . f . runWriterT <$> r)

-- mapLevel4 :: MyApp1 r w s a -> ((a, [w]) -> (b, [v])) -> MyApp1 r v s b
-- mapLevel4 (MyApp1 (ReaderT r)) f = MyApp1 $ ReaderT (WriterT . (f<$>) . runWriterT <$> r)

myTask :: MyApp0 Bool Int String String Bool
myTask = do
  x <- ask
  tell [1]
  put "test"
  -- throwError "oOPs"
  liftIO $ putStrLn "Hello"
  return x

-- e.g. without transformer class
-- i.e. manual lifting
taskWithManualLift :: MyApp0 Bool Int String String Bool
taskWithManualLift = do
  x <- ask -- level1: ReaderT
  lift $ tell [0] -- level2: WriterT
  lift $ lift $ put "Hello" -- level3: StateT
  _ <- lift $ lift $ lift $ throwError "Oops" -- level4: ExceptT
  lift $ lift $ lift $ lift $ putStr "Hello" -- level5: IO
  return x

-- same with transformer class
taskWithTransformLift :: MyApp0 Bool Int String String Bool
taskWithTransformLift = do
  x <- ask -- MonadReader
  tell [0] -- MonadWriter
  put "Hello" -- MonadState
  _ <- throwError "Oops" -- MonadError
  liftIO $ putStrLn "Hello" -- MonadIO
  return x

instance MonadWriter [w] (MyApp2 r w s e) where
  writer = MyApp2 . lift . writer
  listen (MyApp2 r) = MyApp2 $ listen r
  pass (MyApp2 r) = MyApp2 $ pass r

instance MonadState s (MyApp2 r w s e) where
  get = MyApp2 $ lift $ lift get
  put = MyApp2 . lift . lift . put

instance MonadReader r (MyApp2 r w s e) where
  ask = MyApp2 ask
  local f (MyApp2 app) = MyApp2 (local f app)

instance MonadIO (MyApp2 r w s e) where
  liftIO io = MyApp2 (lift $ lift $ lift $ lift io)

instance MonadError e (MyApp2 r w s e) where
  throwError e = MyApp2 $ throwError e
  catchError (MyApp2 p) h = MyApp2 $ catchError p (runMyApp2 . h)

run :: IO ()
run = do
  processMyApp0 myTask True "" >>= print
  main0

newtype App0 r w a = App0 {runApp0 :: ReaderT r (Writer [w]) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadWriter [w],
      MonadReader r
    )

newtype App0V1 r w a = App0V1 {runApp0v1 :: WriterT [w] (Reader r) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadWriter [w],
      MonadReader r
    )

newtype App0v2 e r w a = App0v2 {runApp0v2 :: WriterT [w] (ReaderT r (Except e)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadWriter [w],
      MonadReader r
    )

useApp0Only :: App0 w w w
useApp0Only = App0 $ do
  x <- ask
  lift $ tell [x]
  return x

useApp0Class :: (MonadReader w m, MonadWriter [w] m) => m w
useApp0Class = do
  x <- ask
  tell [x]
  return x

main0 :: IO ()
main0 = do
  let x0 = useApp0Only @String
      x = useApp0Class @String @(App0 String String)
      y = useApp0Class @String @(App0V1 String String)
      z = useApp0Class @String @(App0v2 () String String)

  print $ runWriter (runReaderT (runApp0 x0) "Hello")
  print $ runWriter (runReaderT (runApp0 x) "Hello")
  print $ runReader (runWriterT (runApp0v1 y)) "Hello"
  print $ runExcept (runReaderT (runWriterT (runApp0v2 z)) "Hello")
  return ()
