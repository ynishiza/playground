{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

import Control.Monad.IO.Class
import Data.Aeson
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

-- Data
data User where
  User :: {userId :: UserId, name :: String, email :: String} -> User
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

type UserId = String

api :: Proxy API
api = Proxy

-- Interfaces
type API =
  "api"
    :> Summary "Main API"
    :> Description
         "This is a sample API\
         \5 endpoints \
         \ GET      /api/user \
         \ POST     /api/user \
         \ GET      /api/user/:uid \
         \ POST     /api/user/:uid \
         \ DELETE   /api/user/:uid "
    :> ( BaseAPI
           :<|> UserAPI
       )

type BaseAPI =
  Summary "Base API"
    :> Get '[JSON] String

type UserAPI =
  "user"
    :> Summary "User API"
    :> ( Get '[JSON] [User]
           :<|> ReqBody '[JSON] User :> Post '[JSON] ()
           :<|> TargetUserAPI
       )

type TargetUserAPI =
  Capture "uid" UserId
    :> ( Get '[JSON] User
           :<|> ReqBody '[JSON] User :> Post '[JSON] ()
           :<|> Delete '[JSON] ()
       )

-- Handlers
mainHandler :: Server API
mainHandler =
  baseAPIHandler
    :<|> userAPIHandler

baseAPIHandler :: Server BaseAPI
baseAPIHandler = return "Hello"

userAPIHandler :: Server UserAPI
userAPIHandler =
  ( do
      liftIO $ putStrLn "GET user"
      return []
  )
    :<|> ( \user -> do
             liftIO $ putStrLn $ "POST user:" <> show user
             return ()
         )
    :<|> targetUserAPIHandler

targetUserAPIHandler :: Server TargetUserAPI
targetUserAPIHandler uid =
  ( do
      liftIO $ putStrLn $ "GET uid:" <> uid
      return $ User "123" "Yui" "test@test.com"
  )
    :<|> ( \user -> do
             liftIO $ putStrLn $ "POST uid:" <> uid <> " user:" <> show user
             return ()
         )
    :<|> ( do
             liftIO $ putStrLn $ "DELETE uid:" <> uid
             throwError err403
         )

start :: IO ()
start = run 1234 (serve api mainHandler)
