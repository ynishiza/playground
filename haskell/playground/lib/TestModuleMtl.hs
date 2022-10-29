module TestModuleMtl (
  testMyIOState
  --
) where

import Data.Functor.Identity
import Control.Monad.Trans
import Control.Monad.State
import TestUtils

newtype MyIOState a = MyIOState (IO a)
getMyIOState :: MyIOState a -> IO a
getMyIOState (MyIOState x) = x

instance Functor MyIOState where
  fmap f (MyIOState io) = MyIOState (fmap f io)

instance Applicative MyIOState where
  pure = MyIOState . pure
  (MyIOState f) <*> (MyIOState x) = MyIOState (f <*> x)

instance Monad MyIOState where
  (MyIOState io) >>= k = MyIOState (io >>= (getMyIOState . k))

instance MonadState String MyIOState where
  -- state f = MyIOState (do
  --   x <- getLine
  --   let (a, s) = f x
  --   putStrLn s
  --   return a
                      -- )
  get = MyIOState getLine
  put = MyIOState . putStrLn


testMyIOState = callTest (do
  let 
    query :: MyIOState ()
    query = do
      put "Enter value"
      s <- get
      put $ "Current value:" ++ s

  getMyIOState (do
    put "Hello"
    query
    query
    query
    )


  testDone) "testMyIOState"
