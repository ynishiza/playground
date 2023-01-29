{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SimpleStream.Extra
  ( ssumM,
    printStream,
    promptOne,
    zipPair,
    withEffect,
    withEffectMap,
    ssum,
    logStreamInWriter,
  )
where

import Control.Applicative as M
import Control.Monad.IO.Class
import Control.Monad.Writer.Class
import Data.Bifunctor
import Data.Coerce
import Data.Monoid qualified as M
import Fmt
import SimpleStream.Prelude
import SimpleStream.Stream as S
import Text.Read (readMaybe)
import Prelude hiding (map, mapM_)

ssumM :: (Monad m, Monoid a) => StreamOf a m r -> m (Of a r)
ssumM (Return r) = pure $ mempty :> r
ssumM (Step (a :> str)) = first (a <>) <$> ssumM str
ssumM (Effect e) = e >>= ssumM

printStream :: (MonadIO m, Show a, Show r) => StreamOf a m r -> m ()
printStream (Return r) = liftIO $ fmtLn $ "Return:" +|| r ||+ ""
printStream (Step (a :> str)) = liftIO (fmtLn $ "Item:" +|| a ||+ "") >> printStream str
printStream (Effect e) = e >>= printStream

promptOne :: forall a m. (MonadIO m, Read a) => StreamOf a m ()
promptOne = Effect $ do
  liftIO $ fmtLn "Enter number"
  input <- liftIO getLine
  case readMaybe input of
    (Just a) -> pure $ yield a
    Nothing -> liftIO (fmtLn $ "Failed at parse input" +|| input ||+ "") >> pure empty_

zipPair :: Monad m => StreamOf a m r -> StreamOf b m r -> StreamOf (a, b) m r
zipPair = zipsWith' (\f (a :> x) (b :> y) -> (a, b) :> f x y)

withEffect :: forall a m r. Monad m => (a -> m ()) -> StreamOf a m r -> StreamOf a m r
withEffect f = mapped (\v@(a :> _) -> f a >> pure v)

withEffectMap :: forall a b m r. Monad m => (a -> m b) -> StreamOf a m r -> StreamOf b m r
withEffectMap f = mapped (\(a :> as) -> f a >>= pure . (:> as))

ssum :: forall a m r. (Monad m, Num a) => StreamOf a m r -> m (Of a r)
ssum = (coerce <$>) . ssumM . map M.Sum

logStreamInWriter :: (MonadWriter w m) => (a -> w) -> StreamOf a m r -> StreamOf a m r
logStreamInWriter toLog = mapped (\(a :> s) -> tell (toLog a) >> return (a :> s))

