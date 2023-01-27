{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Chapter14.StreamPlay
  ( tabulate,
    tabulateCollapse,
    tabulateAndSum,
    tabulatePrint,
    tabulatePrintAndSum,
    logCharRaw,
    logCharCode,
    logCharRawAndCodeWith,
    logCharRawAndCodeWithRef,
    logCharCodeAndRawWriter,

    -- Byte
    copyFile, 
    module X,
    T.Text,
  )
where

import Control.Monad.Trans.Resource
import Control.Monad.Writer
import Data.Char
import Data.IORef as X
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Fmt as X
import Streaming as X
import qualified Streaming.ByteString as SB
import Streaming.Prelude qualified as S
import Control.Arrow ((>>>))

withTab :: Show a => a -> T.Text
withTab x = x ||+ "\t"

tabulate :: forall a m r. (Monad m, Show a) => Int -> Stream (Of a) m r -> Stream (Of T.Text) m r
tabulate n str = S.mapped S.mconcat c
  where
    c :: Stream (Stream (Of T.Text) m) m r
    c = chunksOf n $ S.map withTab str

tabulateCollapse :: forall a m r. (MonadIO m, Show a) => Int -> Stream (Of a) m r -> m T.Text
tabulateCollapse n = S.foldMap_ (`T.append` "\n") . tabulate n

tabulatePrint :: forall a m. (MonadIO m, Show a) => Int -> Stream (Of a) m () -> m ()
tabulatePrint n str = tabulateCollapse n str >>= liftIO . T.putStrLn

tabulateAndSum :: forall a m. (MonadIO m, Show a, Num a) => Int -> Stream (Of a) m () -> m (T.Text, a)
tabulateAndSum n str = do
  (txt :> (a :> _)) <- y
  return (txt, a)
  where
    x = S.store S.sum str
    y = S.foldMap (`T.append` "\n") $ tabulate n x

tabulatePrintAndSum :: forall a m. (MonadIO m, Show a, Num a) => Int -> Stream (Of a) m () -> m a
tabulatePrintAndSum n str = do
  (txt, a) <- tabulateAndSum n str
  liftIO $ T.putStrLn txt
  return a

logCharRaw :: (MonadIO m) => (forall n. MonadIO n => Char -> n ()) -> Stream (Of Char) m r -> m r
logCharRaw = logCharWith

logCharCode :: (MonadIO m) => (forall n. MonadIO n => Int -> n ()) -> Stream (Of Char) m r -> m r
logCharCode l = logCharWith (l . ord)

logCharRawAndCodeWithRef :: forall m r. (MonadIO m) => IORef T.Text -> Stream (Of Char) m r -> m r
logCharRawAndCodeWithRef ref = logCharRawAndCodeWith writeLog
  where
    writeLog :: MonadIO n => T.Text -> n ()
    writeLog txt = liftIO $ modifyIORef ref (`T.append` ("" +| txt |+ " "))

logCharRawAndCodeWith :: forall m r. (MonadIO m) => (forall n. MonadIO n => T.Text -> n ()) -> Stream (Of Char) m r -> m r
logCharRawAndCodeWith f str = logCharCode (writeLog . pretty) x
  where
    writeLog :: forall n. MonadIO n => T.Text -> n ()
    writeLog txt = liftIO $ pretty txt >> f txt
    x = S.store (logCharRaw (writeLog . pretty)) str

logCharWith :: (Monad m) => (Char -> m ()) -> Stream (Of Char) m r -> m r
logCharWith = S.mapM_

logCharRawWriter :: (MonadWriter T.Text m) => Stream (Of Char) m r -> Stream (Of Char) m r
logCharRawWriter = logCharWithWriter (pretty . build)

logCharCodeWriter :: (MonadWriter T.Text m) => Stream (Of Char) m r -> Stream (Of Char) m r
logCharCodeWriter = logCharWithWriter (pretty . build . ord)

logCharWithWriter :: (MonadWriter T.Text m) => (Char -> T.Text) -> Stream (Of Char) m r -> Stream (Of Char) m r
logCharWithWriter l = S.mapM (\c -> tell (l c) >> pure c)

logCharCodeAndRawWriter :: forall m r. (MonadWriter T.Text m) => Stream (Of Char) m r -> Stream (Of Char) m r
logCharCodeAndRawWriter = logCharRawWriter . logCharCodeWriter

copyFile :: FilePath -> FilePath -> IO Int
copyFile src dst = runResourceT (f src)
  where
    f :: FilePath -> ResourceT IO Int
    f = SB.readFile 
      >>> SB.copy
      >>> SB.length_
      >>> SB.writeFile dst
