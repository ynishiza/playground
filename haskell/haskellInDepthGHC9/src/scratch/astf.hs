{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

import Control.Monad.Free

data ASTF a where
  Add :: Int -> Int -> (Int -> a) -> ASTF a
  Print :: !String -> a -> ASTF a
  Input :: (Int -> a) -> ASTF a
  deriving (Functor)

type AST a = Free ASTF  a

addF :: Int -> Int -> AST Int
addF x y = liftF $ Add x y id

inputF :: AST Int
inputF = liftF $ Input id

printF :: String -> AST ()
printF s = liftF $ Print s ()

interpretInt :: AST a -> IO a
interpretInt = foldFree f
  where
    f :: ASTF a -> IO a
    f (Add x y g) = do
      putStrLn $ "sum:" <> show (x + y)
      return $ g (x + y)
    f (Input g) = do
      putStr "Enter number:"
      g . read <$> getLine
    f (Print s a) = putStrLn s >> pure a

test :: IO ()
test = do
  let
    c1 = do
      printF "Hello"
      x <- inputF
      y <- inputF
      z <- addF x y
      return (x + y + z)

  interpretInt c1 >>= print
  pure ()
