module GeoCoordsReq
  ( getGeoCoords,
  )
where

import App
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Fmt
import STExcept
import Types

baseUri :: T.Text
baseUri = "nominatim.openstreetmap.org"

getGeoCoords :: Address -> SuntimesApp GeoCoords
getGeoCoords addr =
  do
    wauth <- ask
    let uri = https baseUri /: "search"
        params =
          "q" =: addr
            <> "format" =: ("json" :: T.Text)
            <> "limit" =: (1 :: Int)
            <> "email" =: email wauth
            <> header "User-Agent" (T.encodeUtf8 $ agent wauth)
    res <- responseBody . snd <$> appGET @(JsonResponse [GeoCoords]) uri params jsonResponse
    case res of
      (c : _) -> return c
      [] -> throwM (UnknownLocation addr)
    `catch` onErr
  where
    onErr (ServiceAPIError m) = do
      throwM (ServiceAPIError $ "Failed to lookup coordinates. Are email and agent set in your config?\n" +| m |+ "")
    onErr e = throwM e
