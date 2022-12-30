{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module ScratchSpace
  ( 
  )
where

import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Logger
import TextShow
import qualified Data.ByteString as B
import qualified Data.Text as T

newtype MyApp s a = MkMyApp (LoggingT (StateT s IO) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadState s,
      MonadLogger
    )

runMyApp :: MyApp s a -> s -> IO a
runMyApp (MkMyApp app) st =
  fst <$> runStateT (runLoggingT app fmt) st

fmt :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
fmt _ text lv bd = B.putStr $ fromLogStr $ 
  toLogStr ("LOG [" <> T.pack (show lv) <> "]" <> text) <> bd <> "\n"

