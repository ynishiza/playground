{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module API3.BookAPI
  ( BookAPI,
    bookAPI,
    bookAPIProxy,
    Rating,
    module X,
  )
where

import API3.API as X

data Rating where
  Good :: Rating
  Bad :: Rating
  deriving (Eq, Show, Read)

type BookID = Int

type BTitle = "title"

type BYear = "year"

type BRating = "rating"

type Root = "_"

type BookAPI =
  Root :> Get ServiceStatus
    :<|> BTitle :> Capture BookID :> Get String
    :<|> BYear :> Capture BookID :> Get Int
    :<|> BRating :> Capture BookID :> Get Rating

bookAPI :: APIService BookAPI
bookAPI =
  pure Up
    :<|> title
    :<|> year
    :<|> rating
  where
    title _ = pure "Haskell in depth"
    year _ = pure (2021 :: Int)
    rating _ = pure Good

bookAPIProxy :: Proxy BookAPI
bookAPIProxy = Proxy
