{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

{- ORMOLU_ENABLE -}
module Common
  ( User (..),
    defaultUser,
    ServerEnv (..),
    ServerHandler,
    ServerState (..),
    emptyServerState,
    modifyServerState,
    putServerState,
    getServerState,
    UserDB,
    getUserDB,
    modifyUserDB,
    listUsers,
    createUser,
    getUser,
    module X,
  )
where
{- ORMOLU_DISABLE -}

import Control.Monad.Logger as X
import Control.Monad.Reader as X
import Control.Monad.Except as X
import Data.Aeson
import Data.IORef
import Data.Map.Strict
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Prelude hiding (lookup)
import GHC.Types (Constraint, Type)
import Servant (ServerError, err404)
import System.Random

type ServerHandler :: ((Type -> Type) -> Type -> Type) -> (Type -> Type) -> Constraint
type ServerHandler t m = (MonadTrans t, MonadError ServerError (t m), MonadIO (t m), MonadReader ServerEnv (t m), MonadLogger (t m))

data ServerEnv where
  ServerEnv :: {ioref :: IORef ServerState} -> ServerEnv

data ServerState where
  ServerState :: {users :: UserDB} -> ServerState

emptyServerState :: ServerState
emptyServerState = ServerState (insert (userId defaultUser) defaultUser empty)

type UserDB = Map Text User

getUserDB :: ServerHandler t m => t m UserDB
getUserDB = users <$> getServerState 

modifyUserDB :: ServerHandler t m => (UserDB -> UserDB) -> t m ()
modifyUserDB fn = modifyServerState (\s -> s {users = fn (users s)})

getServerState :: ServerHandler t m => t m ServerState
getServerState = asks ioref >>= liftIO . readIORef

modifyServerState :: ServerHandler t m => (ServerState -> ServerState) -> t m ()
modifyServerState fn = asks ioref >>= liftIO . flip modifyIORef fn

putServerState :: ServerHandler t m => ServerState -> t m ()
putServerState s = modifyServerState (const s)

data User where
  User ::
    { userId :: Text,
      userName :: Text
    } ->
    User
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

defaultUser :: User
defaultUser = User "123" "Default"

listUsers :: Monad m => UserDB -> m [User]
listUsers = return . (snd <$>) . toList

getUser :: MonadError ServerError m => UserDB -> Text -> m User
getUser db uid = maybe (throwError err404) return $ lookup uid db

createUser :: (MonadIO m) => UserDB -> User -> m (UserDB, User)
createUser db u = do
  newId <- liftIO $ T.pack <$> randomStr
  let newUser = u {userId = newId}
  return (insert newId newUser db, newUser)

randomStr :: IO String
randomStr = show <$> getStdRandom (uniform @StdGen @Int)
