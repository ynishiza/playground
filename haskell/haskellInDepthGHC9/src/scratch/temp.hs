#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as B
import GHC.Generics (Generic)
import Data.Map qualified as M
import Data.Char (toUpper)

data Person = Person { name :: String, value :: Int }
  deriving stock (Show, Eq, Ord, Generic)
  -- deriving anyclass (ToJSONKey, FromJSONKey)

-- options :: Options
-- options = defaultOptions { fieldLabelModifier = (toUpper <$>) }

-- instance ToJSON Person where 
--   toJSON = genericToJSON options
-- instance FromJSON Person where 
--   parseJSON = genericParseJSON options

instance FromJSON Person where
  -- parseJSON (Object obj) = Person <$> obj .: "name" <*> obj .: "value"
  -- parseJSON _ = fail "Invalid"
  parseJSON = withObject "Person" $ \obj -> Person <$> obj .: "name" <*> obj .: "value"

instance ToJSON Person where
  toJSON Person {..} = Object $ "name" .= name <> "value" .= value
  -- toJSON Person {..} = object [ "name" .= name, "value" .= value]
  toEncoding Person {..} = pairs $ "name" .= name <> "value" .= value

main :: IO ()
main = do
  B.putStrLn $ encode $ Person "a" 1
  print $ decode @Person "{\"name\":\"a\", \"value\": 1 }"
  print $ decode @Person "{\"NAME\":\"a\", \"VALUE\": 1 }"
  -- B.putStrLn $ encode $ M.singleton (Person "a" 1) True
  B.putStrLn $ encode $ M.singleton (1 :: Int) True
  B.putStrLn $ encode $ M.singleton ("1" :: String) True
  B.putStrLn $ encode $ M.singleton (1 :: Int,2 :: Int) True
  B.putStrLn $ encode $ M.singleton (1 :: Int) True
  B.putStrLn $ encode $ M.singleton [ 1 :: Int, 2 ] True
  -- B.putStrLn $ encode $ M.singleton (Just (1 :: Int)) True
  -- print $ decode @(M.Map Person ()) $ "[[{\"name\":\"a\",\"value\":1},true]]"
  pure ()
