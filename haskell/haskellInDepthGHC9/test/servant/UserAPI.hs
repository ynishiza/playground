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
    :> Summary "Users API"
    :> Description
         "This is an API for managing user data\
         \Two types of API \
         \    /user/*       Base user API\
         \    /user/:uid    Specific user API"
    :> (UsersAPI :<|> SingleUserAPI)

type UsersAPI =
  Get '[JSON] [User]
    :<|> ReqBody '[JSON] User :> Post '[JSON] ()

type SingleUserAPI =
  Capture "id" Text
    :> Summary "Single User API"
    :> ( Description
           "GET /user/:uid\
           \  gets specified user"
           :> Get '[JSON] User
           :<|> Description
                  " PUT /user:uid\
                  \  updates specified user"
             :> ReqBody '[JSON] User
             :> PutAccepted '[JSON] ()
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
             _ <- getUserDB >>= flip getUser uid
             logDebugN $ "PUT user:" <> toText u
             modifyUserDB (insert uid (u {userId = uid}))
             pure ()
         )
