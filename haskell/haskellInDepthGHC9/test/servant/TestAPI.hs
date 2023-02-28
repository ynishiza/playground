{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module TestAPI
  ( handler,
    API,
    SomeComplexData (..),
  )
where

import Common (ServerHandler)
import Data.Aeson
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Servant

data SomeComplexData = SomeComplexData {value :: String, key :: Int}
  deriving (Generic, FromJSON, ToJSON, Show, Eq)

type API =
  "test"
    :> ( ( "response"
             :> ( Get '[JSON] Int
                    :<|> "multitype" :> Get '[JSON, PlainText] String
                    :<|> "header" :> Get '[JSON, PlainText] (Headers '[Header "A" Int, Header "B" Bool] String)
                )
         )
           :<|> "queryparam"
             :> ( QueryParam "value" String
                    :> Get '[JSON] String
                    :<|> "list"
                      :> QueryParams "value" String
                      :> Get '[JSON] String
                    :<|> "modifier"
                      :> QueryParam' '[Required, Strict] "v1" String
                      :> QueryParam' '[Optional, Strict] "v2" String
                      :> Get '[JSON] (String, String)
                    :<|> "combined"
                      :> QueryParam "v1" String
                      :> QueryParam' '[Required] "v2" String
                      :> QueryParams "list" Int
                      :> QueryFlag "flag"
                      :> Get '[JSON] (String, String, [Int], Bool)
                )
           :<|> "requestBody"
             :> ( ReqBody '[JSON, PlainText] String :> Post '[JSON] String
                    :<|> "complex" :> ReqBody '[JSON] SomeComplexData :> Post '[JSON] SomeComplexData
                )
       )

handler :: ServerHandler t Handler => ServerT API (t Handler)
handler =
  ( return 1
      :<|> return "Hello"
      :<|> return (addHeader 1 (addHeader True "Hello"))
  )
    :<|> queryParamHandler
    :<|> bodyHandler
  where
    queryParamHandler =
      (return . maybe "NA" ("param:" <>))
        :<|> (return . intercalate ",")
        :<|> (\v1 v2 -> return (v1, fromMaybe "NA" v2))
        :<|> (\v1 v2 l f -> return (fromMaybe "" v1, v2, l, f))
    bodyHandler = (return . ("body:" <>)) :<|> return
