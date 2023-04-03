{-# LANGUAGE LambdaCase #-}

module Base.Combinators
  ( char,
    look,
    digit,
    int,
    test,
  )
where

import Base.Parse as X

char :: Parse Char
char = Get pure

look :: Parse String
look = Look pure

digit :: Integral n => Parse n
digit =
  char >>= \case
    '1' -> pure 1
    '2' -> pure 2
    '3' -> pure 3
    '4' -> pure 4
    '5' -> pure 5
    '6' -> pure 6
    '7' -> pure 7
    '8' -> pure 8
    '9' -> pure 9
    '0' -> pure 0
    _ -> Stop

int :: Parse Int
int =
  look >>= \case
    (_ : xs) ->
      foldr
        ( \_ aggr -> do
            r <- aggr
            x <- digit
            return $ x + 10 * r
        )
        digit
        xs
    _ -> Stop

test :: IO ()
test = do
  print $ parse int "123287"


