{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Logger
import Control.Monad.State
import Servant
import TextShow

type MyAPI =
  "api"
    :> Description "API"
    :> Summary "API"
    :> "base"
    :> "user"
    :> "data"
    :> ( "name" :> Get '[JSON] String
           :<|> "name" :> ReqBody '[JSON] String :> Post '[JSON] ()
           :<|> "age" :> Get '[JSON] Int
           :<|> "age" :> ReqBody '[JSON] Int :> Post '[JSON] ()
           :<|> "sum" :> QueryParam' '[Required] "x" Int :> QueryParam' '[Required, Strict] "y" Int :> Get '[JSON] Int
       )

myAPI :: Proxy MyAPI
myAPI = Proxy @MyAPI

data ServerState = MyState
  { name :: String,
    age :: Int
  }

newtype MyServer a = MySerevr (StateT ServerState (LoggingT Handler) a)
  deriving stock
    ( Functor
    )
  deriving newtype
    ( Applicative,
      Monad,
      MonadState ServerState,
      MonadLogger
    )

server :: ServerT MyAPI (StateT ServerState (LoggingT Handler))
server =
  ( do
      gets name
  )
    :<|> ( \name -> do
             logDebugN $ "PUT name:" <> showt name
             modify (\x -> x {name = name})
         )
    :<|> ( do
             gets age
         )
    :<|> ( \age -> do
             logDebugN $ "PUT age:" <> showt age
             modify (\x -> x {age = age})
         )
    :<|> ( \x y -> do
             return $ x + y
         )

mainServer :: Server MyAPI
mainServer = hoistServer myAPI f server
  where
    f x = runStdoutLoggingT $ evalStateT x (MyState "" 0)
