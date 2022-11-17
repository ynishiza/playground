{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}

module Chapter5Parser
  ( parse,
    logState,
    Expr (..),
    SYError (..),
  )
where

import Control.Arrow
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Char
import Data.Foldable
import Data.List (groupBy)
import Data.Maybe
import Fmt
import Text.Read (readMaybe)
import Utils

parse :: String -> Either SYError (Expr Int, SYState)
parse = processExpression

data Expr a = Lit !a | Add !(Expr a) !(Expr a) | Mult !(Expr a) !(Expr a) deriving (Show, Eq)

type Token = String

type Stack = [Token]

type Output = [Expr Int]

type SYState = (Stack, Output)

data SYError = SYError !SYState !String deriving (Show, Eq)

type SYParser a = StateT SYState (Except SYError) a

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

throwError :: SYState -> String -> SYParser ()
throwError s m = lift $ throwE $ createError s m

updateStack :: (Stack -> Stack) -> SYParser ()
updateStack f = modify (first f)

updateOutput :: (Output -> Output) -> SYParser ()
updateOutput f = modify (second f)

popStack :: SYParser Token
popStack = do
  s@(stack, _) <- get
  case stack of
    (x : xs) -> updateStack (const xs) >> return x
    [] -> throwError s "popStack: empty stack" >> return ""

pushStack :: Token -> SYParser ()
pushStack t = updateStack (t :)

pushOutput :: Token -> SYParser ()
pushOutput t =
  if isOp t
    then get >>= pushOp
    else get >>= pushLiteral
  where
    readError s = throwError s $ "Failed to read token. token=" +| t |+ ""
    outputError s = throwError s $ "Not enough output. token=" +| t |+ ""
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
    [] -> throwError s m >> return Nothing

processExpression :: String -> Either SYError (Expr Int, SYState)
processExpression str = runExcept (runStateT proc ([], []))
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

transfer :: StateT SYState (Except SYError) ()
transfer = popStack >>= pushOutput
