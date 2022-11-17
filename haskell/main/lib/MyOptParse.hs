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

import Control.Monad
import Control.Monad.Trans.State
import Data.List (findIndex)
import Fmt
import TestUtils
import Text.Read (readMaybe)

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
  fmap f parser = parser >>= return . f

instance Applicative ParserBase where
  pure = P . pure . pure
  parserf <*> parser = parserf >>= (<$> parser)

instance Monad ParserBase where
  (P parser) >>= k = P $ do
    pres <- parser
    either (return . Left) (getParser . k) pres

runParser :: ParserBase a -> [Arg] -> (Either ParserError a, ParserState)
runParser parser args = (pres >> validate >> pres, pstate)
  where
    (pres, pstate) = runState (getParser parser) args
    validate = unless (null pstate) $ Left (PError (fmtLn $ "Unused args:" +| pstate |+ ""))

instance Show a => Buildable (Either ParserError a) where
  build = build . show

createStringOption :: Char -> String -> Opt String
createStringOption short long = Opt short long (Just "") Just

createSimpleOption :: forall a. (Read a, HasDefault a) => Char -> String -> Opt a
createSimpleOption short l = Opt short l (Just (getDefault @a)) readMaybe

parseOption :: Opt a -> ParserBase a
parseOption opt = P $ do
  pstate <- get
  either
    (return . Left)
    (\(pstate', value) -> put pstate' >> return (Right value))
    $ processOption opt pstate

processOption :: Opt a -> ParserState -> Either ParserError (ParserState, a)
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

    errorOrDefault e = maybe (Left e) (return . (pstate,)) defaultValue

    errorMessage :: String -> String
    errorMessage msg = "Error in " +| optionName |+ ":" +| msg |+ ""

    parse = do
      trace (fmt $ indentF 2 $ pstate |+ " short:" +| optionName |+ "") return ()
      -- step: look for option
      nameIdx <-
        let e0 = PRequierdOptionError $ errorMessage "Missing required option"
         in maybeToEither e0 $ findIndex isOptionMatch pstate

      -- step: get value index
      valueIdx <-
        if nameIdx >= length pstate - 1
          then Left $ POptionReadError nameIdx $ errorMessage "Missing option value"
          else return $ nameIdx + 1

      -- step: read value
      val <-
        let rawVal = pstate !! valueIdx
            e = POptionReadError valueIdx $ errorMessage $ "Failed to parse value:" +| rawVal |+ ""
         in maybeToEither e $ readValue rawVal
      return (removeArg nameIdx 2 pstate, val)

removeArg :: Int -> Int -> [Arg] -> [Arg]
removeArg startIdx n args = s1 ++ drop n s2
  where
    (s1, s2) = splitAt startIdx args
