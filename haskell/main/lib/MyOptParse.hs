{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module MyOptParse
  ( Opt (..),
    ParserBase,
    ParserError (..),
    readMaybe,
    runParser,
    createStringOption,
    createSimpleOption,
    parseOption,
  )
where

-- import Control.Monad.Trans.Class
-- import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Text.Read (readMaybe)
import Data.List (findIndex)
import Fmt
import TestUtils

type Arg = String

type ParserState = [Arg]

data ParserError
  = POptionReadError Int String
  | PRequierdOptionError String
  | PError String
  deriving (Show, Eq)

newtype ParserBase a = P (State ParserState (Either ParserError a))

getParser :: ParserBase a -> State ParserState (Either ParserError a)
getParser (P st) = st

data Opt a = Opt
  { short :: Char,
    long :: String,
    defaultValue :: Maybe a,
    readValue :: String -> Maybe a
  }

class HasDefault a where
  getDefault :: a
instance HasDefault String where
  getDefault = ""
instance HasDefault Int where
  getDefault = 0
instance HasDefault Integer where
  getDefault = 0
instance HasDefault Double where
  getDefault = 0
instance HasDefault Float where
  getDefault = 0
instance HasDefault Bool where
  getDefault = False

instance Functor ParserBase where
  fmap f p = p >>= return . f

instance Applicative ParserBase where
  pure = P  . pure . pure
  sf <*> st = sf >>= (<$>st)

instance Monad ParserBase where 
  (P st) >>= k = P $ do
    res <- st
    case res of 
      Right x -> getParser $ k x
      Left e -> return $ Left e


runParser :: ParserBase a -> [Arg] -> (Either ParserError a, ParserState)
runParser parser args = (res', pstate)
  where 
    (res, pstate) = runState (getParser parser) args 
    res' = res >>
      if null pstate
        then res
        else Left (PError (fmtLn $ "Unused args:" +| pstate |+ ""))

instance Show a => Buildable (Either ParserError a) where
  build = build . show

createStringOption :: Char -> String -> Opt String
createStringOption short long = Opt short long (Just "") Just

createSimpleOption :: forall a. (Read a, HasDefault a) => Char -> String -> Opt a
createSimpleOption short l = Opt short l (Just (getDefault @a)) readMaybe

parseOption :: Opt a -> ParserBase a
-- parseOption = undefined
parseOption opt = P $ do
  pstate <- get
  case processOption opt pstate of
    Left e -> return $ Left e
    Right (pstate', value) -> do
      put pstate'
      return $ Right value

processOption :: Opt a -> ParserState -> Either ParserError (ParserState, a)
-- processOption = undefined
processOption (Opt {..}) pstate = case parse of
  -- case: use default if set
  Left e@(PRequierdOptionError _) -> errorOrDefault e
  _ -> parse
  where
    longOpt :: String
    longOpt = "--" +| long |+ ""
    shortOpt :: String
    shortOpt = "-" +| short |+ ""
    optionName :: String
    optionName = shortOpt |+ "|" +| longOpt |+ ""

    isOptionMatch arg = arg == longOpt || arg == shortOpt
    errorOrDefault e = case defaultValue of
      Just v -> return (pstate, v)
      Nothing -> Left e

    errorMessage :: String -> String
    errorMessage msg = "Error in " +| optionName |+ ":" +| msg |+ ""

    parse = do
      trace (fmt $ indentF 2 $ pstate |+ " short:" +| optionName |+ "") return ()
      -- step: look for option
      nameIdx <- case findIndex isOptionMatch pstate of
        Just i -> return i
        Nothing -> Left $ PRequierdOptionError $ errorMessage "Missing required option"

      -- step: get value index
      valueIdx <-
        if nameIdx >= length pstate - 1
          then Left $ POptionReadError nameIdx $ errorMessage "Missing option value"
          else return $ nameIdx + 1

      -- step: read value
      let rawVal = pstate !! valueIdx
      val <- case readValue rawVal of
        Just v -> return v
        Nothing -> Left $ POptionReadError valueIdx $ errorMessage $ "Failed to parse value:" +| rawVal |+ ""
      return (removeArg nameIdx 2 pstate, val)

removeArg :: Int -> Int -> [Arg] -> [Arg]
removeArg startIdx n args = s1 ++ drop n s2
  where
    (s1, s2) = splitAt startIdx args
