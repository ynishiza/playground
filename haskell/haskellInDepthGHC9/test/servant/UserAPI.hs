{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module UserAPI
  ( mainHandler,
    MainAPI,
  )
where

import Common
import Data.Map.Strict
import Data.Text (Text)
import Data.Text qualified as T
import Servant

type MainAPI =
  "user" 
  :> Description "User API"
  :> Summary "Users"
  :> (UsersAPI :<|> SingleUserAPI)

type UsersAPI =
  Get '[JSON] [User]
    :<|> ReqBody '[JSON] User :> Post '[JSON] ()

type SingleUserAPI =
  Capture "id" Text
  :> Description "Single User API"
  :> Summary "Single User"
    :> ( Get '[JSON] User
           :<|> ReqBody '[JSON] User :> PutAccepted '[JSON] ()
       )

mainHandler :: ServerHandler t Handler => ServerT MainAPI (t Handler)
mainHandler = userHandler :<|> singleUserHandler

toText :: Show a => a -> Text
toText = T.pack . show

userHandler :: ServerHandler t Handler => ServerT UsersAPI (t Handler)
userHandler =
  (logUsers >> getUserDB >>= listUsers)
    :<|> ( \dat -> do
             (db, _) <- getUserDB >>= flip createUser dat
             modifyUserDB (const db)
         )

logUsers :: ServerHandler t Handler => t Handler ()
logUsers = do
  u <- getUserDB
  logDebugN $ "users:" <> toText (toList u)

singleUserHandler :: ServerHandler t Handler => ServerT SingleUserAPI (t Handler)
singleUserHandler uid =
  (getUserDB >>= flip getUser uid)
    :<|> ( \u -> do
             logDebugN $ "PUT user:" <> toText u
             pure ()
         )
