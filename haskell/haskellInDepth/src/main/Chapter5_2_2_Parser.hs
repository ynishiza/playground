{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Eta reduce" #-}

module Chapter5_2_2_Parser
  ( parse,
    logState,
    createParser,
    processParser,
    Expr (..),
    SYError (..),
  )
where

import Control.Arrow
import Control.Monad.Except
import Control.Monad.State
import Data.Char
import Data.Foldable
import Data.Kind
import Data.List (groupBy)
import Data.Maybe
import Fmt
import Text.Read (readMaybe)
import Utils

parse :: String -> Either SYError (Expr Int, SYState)
parse = processExpression

data Expr (a :: Type) where
  Lit :: a -> Expr a
  Add :: Expr a -> Expr a -> Expr a
  Mult :: Expr a -> Expr a -> Expr a

deriving instance Eq a => Eq (Expr a)

deriving instance Show a => Show (Expr a)

type Token = String

type Stack = [Token]

type Output = [Expr Int]

type SYState = (Stack, Output)

data SYError where
  SYError :: SYState -> String -> SYError

deriving instance Show SYError

deriving instance Eq SYError

newtype SYParser (a :: Type) = SYParser {runSYParser :: StateT SYState (Except SYError) a}

deriving instance Functor SYParser

deriving instance Applicative SYParser

deriving instance Monad SYParser

instance MonadState SYState SYParser where
  state f = SYParser $ state f

instance MonadError SYError SYParser where
  throwError e = SYParser $ throwError e
  catchError (SYParser p) handler = SYParser $ catchError p (runSYParser . handler)

addOp :: Token
addOp = "+"

multOp :: Token
multOp = "*"

lB :: Token
lB = "("

rB :: Token
rB = ")"

isOp :: Token -> Bool
isOp token
  | token == addOp = True
  | token == multOp = True
  | otherwise = False

getOp :: Token -> Expr a -> Expr a -> Expr a
getOp token
  | token == addOp = Add
  | token == multOp = Mult
  | otherwise = error "Not an operator"

getPrecedence :: Token -> Int
getPrecedence token
  | token == multOp = 2
  | token == addOp = 1
  | otherwise = 0

precGTE :: Token -> Token -> Bool
x `precGTE` y = getPrecedence x >= getPrecedence y

logState :: SYState -> Builder
logState (stack, output) = "state (stack=" +| stack |+ " output" +|| output ||+ ")"

createError :: SYState -> String -> SYError
createError s m = trace ("ERROR:" +| logState s |+ " message=" +| m |+ "") $ SYError s m

-- createError s m = SYError s m

myThrowError :: SYState -> String -> SYParser ()
myThrowError s m = throwError $ createError s m

updateStack :: (Stack -> Stack) -> SYParser ()
updateStack f = modify (first f)

updateOutput :: (Output -> Output) -> SYParser ()
updateOutput f = modify (second f)

popStack :: SYParser Token
popStack = do
  s@(stack, _) <- get
  case stack of
    (x : xs) -> updateStack (const xs) >> return x
    [] -> myThrowError s "popStack: empty stack" >> return ""

pushStack :: Token -> SYParser ()
pushStack t = updateStack (t :)

pushOutput :: Token -> SYParser ()
pushOutput t =
  if isOp t
    then get >>= pushOp
    else get >>= pushLiteral
  where
    readError s = myThrowError s $ "Failed to read token. token=" +| t |+ ""
    outputError s = myThrowError s $ "Not enough output. token=" +| t |+ ""
    pushLiteral s = maybe (readError s) createLiteral (readMaybe t)
    createLiteral value = updateOutput (Lit value :)
    pushOp s@(stack, output)
      | (e1 : e2 : es) <- output = put (stack, getOp t e1 e2 : es)
      | otherwise = outputError s

doWhileState :: (SYState -> Bool) -> SYParser () -> SYParser ()
doWhileState predicate = whileLoop (get >>= return . predicate)

tryGetHead :: [a] -> String -> SYParser (Maybe a)
tryGetHead l m = do
  s <- get
  case l of
    (x : _) -> return $ Just x
    [] -> myThrowError s m >> return Nothing

processExpression :: String -> Either SYError (Expr Int, SYState)
processExpression = processParser . createParser

processParser :: SYParser (Expr Int) -> Either SYError (Expr Int, SYState)
processParser (SYParser p) = runExcept (runStateT p ([], []))

createParser :: String -> SYParser (Expr Int)
createParser str = proc
  where
    tokens = reverse $ tokenizer str
    isD x = isDigit x || x == '-'
    tokenizer =
      groupBy (\x y -> isD x && isD y)
        . filter (not . isSpace)

    proc = do
      traverse_ processToken tokens
      doWhileState (not . null . fst) transfer

      output <- gets snd
      h <- tryGetHead output "Empty output"
      return $ fromMaybe (Lit 0) h

-- >> doWhileState (\st@(stack, _) -> trace (logState st |+ "") $ not $ null stack) transfer

processToken :: Token -> SYParser ()
processToken token
  | token == rB = pushStack rB
  | token == lB = doWhileTop (/= rB) transfer >> popStack >> return ()
  | isOp token = doWhileTop (`precGTE` token) transfer >> pushStack token
  | otherwise = pushOutput token
  where
    doWhileTop predicate = doWhileState (\(stack, _) -> not (null stack) && predicate (head stack))

transfer :: SYParser ()
transfer = popStack >>= pushOutput
