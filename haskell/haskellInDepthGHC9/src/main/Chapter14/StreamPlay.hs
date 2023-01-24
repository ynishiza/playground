{-# LANGUAGE OverloadedStrings #-}
module Chapter14.StreamPlay (
  tabulate,
  tabulateCollapse,
  tabulateAndSum,
  tabulatePrint,
  tabulatePrintAndSum,
  ) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Fmt
import Streaming
import Streaming.Prelude qualified as S

withTab :: Show a => a -> T.Text
withTab x = x ||+ "\t"

tabulate :: forall a m r. (Monad m, Show a) => Int -> Stream (Of a) m r -> Stream (Of T.Text) m r
tabulate n str = S.mapped S.mconcat c
  where
    c :: Stream (Stream (Of T.Text) m) m r
    c = chunksOf n $ S.map withTab str

tabulateCollapse :: forall a m r. (MonadIO m, Show a) => Int -> Stream (Of a) m r -> m T.Text
tabulateCollapse n = S.foldMap_ (`T.append` "\n") . tabulate n 

tabulatePrint :: forall a m . (MonadIO m, Show a) => Int -> Stream (Of a) m () -> m ()
tabulatePrint n str = tabulateCollapse n str >>= liftIO . T.putStrLn 

tabulateAndSum :: forall a m . (MonadIO m, Show a, Num a) => Int -> Stream (Of a) m () -> m (T.Text, a)
tabulateAndSum n str = do
  (txt :> (a :> _)) <- y
  return (txt, a)
  where
    x = S.store S.sum str
    y = S.foldMap (`T.append` "\n") $ tabulate n x

tabulatePrintAndSum :: forall a m . (MonadIO m, Show a, Num a) => Int -> Stream (Of a) m () -> m a
tabulatePrintAndSum n str = do
  (txt, a) <- tabulateAndSum n str
  liftIO $ T.putStrLn txt
  return a
