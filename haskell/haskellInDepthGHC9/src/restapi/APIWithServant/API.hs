{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module APIWithServant.API (
  serverApp,
  serverAppBasic,
  BookAPI,
  Book(..),
  Rating(..),
  ServiceStatus(..),
) where

import Servant
import Servant.HTML.Blaze
import Data.Aeson
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Logger
import qualified Text.Blaze.Html as H
import qualified Text.Blaze.Html4.Strict as H

data Rating where
  Great :: Rating
  Good :: Rating
  Bad :: Rating
  deriving stock (Eq, Show, Read, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

data ServiceStatus where
  Up :: ServiceStatus
  Down :: ServiceStatus
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Book where
  MkBook :: { bookId:: !Int, title :: !String, year :: !Int } -> Book
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

type BookAPI = Get '[JSON] ServiceStatus
  :<|> "title" :> Capture "id" Int :> Get '[HTML] H.Html
  :<|> "year" :> Capture "id" Int :> Get '[JSON] Int
  :<|> "rating" :> Capture "id" Int :> Get '[JSON] Rating
  :<|> "book" :> Capture "id" Int :> Get '[JSON] Book

bookAPI :: Proxy BookAPI
bookAPI = Proxy

bookAPIServerT :: ServerT BookAPI (LoggingT Handler)
bookAPIServerT = 
  pure Up
  :<|> (\_ -> do 
    logDebugN "title"
    pure $ H.html $ 
      H.body $ do
        H.h1 "Title"
        H.div $ do
          "Haskell in depth")
  :<|> (\i -> do 
    logDebugN "year called"
    liftIO $ putStrLn "test"
    pure $ 2021 + i)
  :<|> (\i -> logDebugN "rating" >> pure (toEnum (i `mod` 3)))
  :<|> (\i -> logDebugN "book" >> pure (MkBook i "Haskell in depth" 2021))

serverWithLogger :: Server BookAPI
serverWithLogger = hoistServer bookAPI runStdoutLoggingT bookAPIServerT

serverApp :: Application
serverApp = serve bookAPI serverWithLogger

bookAPIServerBasic :: Server BookAPI
bookAPIServerBasic = 
  pure Up
  :<|> (\_ -> pure $ H.html $ 
      H.body $ do
        H.h1 "Title"
        H.div $ do
          "Haskell in depth")
  :<|> (\i -> pure $ 2021 + i)
  :<|> (\i -> pure (toEnum (i `mod` 3)))
  :<|> (\i -> pure $ MkBook i "Haskell in depth" 2021)

serverAppBasic :: Application
serverAppBasic = serve bookAPI bookAPIServerBasic
