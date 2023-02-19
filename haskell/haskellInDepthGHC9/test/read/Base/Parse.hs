{-# LANGUAGE GADTs #-}

{- ORMOLU_DISABLE -}
module Base.Parse
  ( 
    Parse(..),
    parse,
    parseMaybe,
    final,
  )
where
{- ORMOLU_ENABLE -}

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Data.List.NonEmpty

data Parse a where
  -- Get: consume single character
  Get :: (Char -> Parse a) -> Parse a
  -- Look: peek string without consuming
  Look :: (String -> Parse a) -> Parse a
  -- Stop: End
  Stop :: Parse a
  -- Result: one result
  Result :: a -> Parse a -> Parse a
  Final :: (NonEmpty (a, String)) -> Parse a

instance Functor Parse where
  fmap f p = case p of
    Get g -> Get $ (f <$>) . g
    Look l -> Look $ (f <$>) . l
    Stop -> Stop
    Result a rest -> Result (f a) (f <$> rest)
    Final result -> Final $ first f <$> result

instance Applicative Parse where
  pure a = Result a Stop
  (<*>) = ap

instance Monad Parse where
  Get g >>= k = Get $ k <=< g
  Look l >>= k = Look $ k <=< l
  Stop >>= _ = Stop
  Result a rest >>= k = k a <|> (rest >>= k)
  Final res >>= k = Final $ do
    (a, str) <- res
    fromList $ parse (k a) str

instance Alternative Parse where
  empty = Stop
  Stop <|> p = p
  p <|> Stop = p
  (Result a p) <|> q = Result a (p <|> q)
  p <|> (Result a q) = Result a (p <|> q)

  (Get g) <|> (Get g') = Get $ \c -> g c <|> g' c
  (Get g) <|> p = Get $ (<|> p) . g
  (Look l) <|> (Look l') = Look $ \str -> l str <|> l' str
  p <|> (Look l) = Look $ (<|> p) . l
  (Look l) <|> p = Look $ (<|> p) . l

  (Final l) <|> (Final m) = Final $ l <> m
  p@(Final _) <|> _ = p


parse :: Parse a -> String -> [(a, String)]
parse (Get f) (c : rest) = parse (f c) rest
parse (Get _) [] = []
parse (Look f) str = parse (f str) str
parse Stop _ = []
parse (Result a alt) str = (a, str) : parse alt str
parse (Final l) _ = toList l

parseMaybe :: Parse a -> String -> Maybe a
parseMaybe p s = case parse p s of
  (x : _) -> Just $ fst x
  _ -> Nothing

final :: [(a, String)] -> Parse a
final [] = Stop
final (x : xs) = Final $ x :| xs

