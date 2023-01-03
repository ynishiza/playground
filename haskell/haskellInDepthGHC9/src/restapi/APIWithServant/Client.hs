{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module APIWithServant.Client
  ( getStatus,
    getTitle,
    getYear,
    getRating,
    getBook,
    runClient,
  )
where

import APIWithServant.API
import Network.HTTP.Client
import Servant
import Servant.Client
import Servant.HTML.Blaze
import Text.Blaze.Html qualified as H

runClient :: BaseUrl -> ClientM a -> IO (Either ClientError a)
runClient url r = do
  mg <- newManager defaultManagerSettings
  runClientM r (mkClientEnv mg url)

getTitle :: Int -> ClientM H.Html
getYear :: Int -> ClientM Int
getRating :: Int -> ClientM Rating
getBook :: Int -> ClientM Book
getStatus :: ClientM ServiceStatus

instance MimeUnrender HTML H.Markup where
  mimeUnrender _ s = Right $ H.unsafeLazyByteString s

getStatus :<|> getTitle :<|> getYear :<|> getRating :<|> getBook = client bookAPI
