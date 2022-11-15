{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Modules.TestOptparseApplicative (test) where

import Fmt
import Options.Applicative
import System.Environment
import TestUtils

type Args = [String]

instance Buildable a => Buildable (ParserResult a) where
  build x = build $ show $ show . build <$> x

test :: TestState
test =
  createTest
    ( do
        let pString :: Parser String
            pString = strOption (short 'v')

            pStringInString :: Parser String
            pStringInString = option auto (short 'v')
         in do
            printBanner "Test string option"
            testHelp pStringInString
            _ <- testParser ["-v", "hello"] $ infoSimple pString
            _ <- testParser ["-v", "hello"] $ infoSimple pStringInString
            _ <- testParser ["-v", "\"hello\""] $ infoSimple pStringInString
            pure ()


        let 
            parser =
              (,,,,,,)
                -- <$> argument str (metavar "args")
                -- <$> optional (argument str (metavar "args"))
                <$> argument str (metavar "args" <> value "")
                -- <$> strArgument (value "")
                <*> flag 0 1 (long "f1")
                <*> optional (flag' (1::Int) (long "f2"))
                <*> switch (long "f3")

                <*> optional (option (auto @Int) (long "o1"))
                <*> option (auto @Int) (long "o12" <> value 0)
                <*> optional (strOption (long "o2"))

            commandParser = subparser (command "start" (i "add") <> command "stop" (i "do"))
                    where p = switch (short 'f')
                          i desc = info (helper <*> p) (progDesc desc)
         in ( do
                testHelp parser
                pauseIO

                printBanner "Basic"
                _ <- testParser [] $ infoSimple parser
                _ <- testParser ["--f1", "--f2", "--f3"] $ infoSimple parser
                _ <- testParser ["--o1", "1", "--o2", "apple"] $ infoSimple parser
                pauseIO

                printBanner "Command"
                _ <- testParser ["start"] $ info commandParser mempty
                _ <- testParser ["stop"] $ info commandParser mempty
                _ <- testParser ["start", "abc"] $ infoSimple commandParser
                _ <- testParser ["stop", "-f"] $ infoSimple commandParser
                _ <- testParser ["start", "-h"] $ infoSimple commandParser
                pauseIO
                return ()
            )

        printBanner "Test help"
        testHelp 
          ((,,) 
            <$> argument str (metavar "args")
            <*> option auto (short 'n')
            <*> option auto (short 'v') :: Parser (String, Int, String))

        printBanner "Test abort transform"
        _ <- let pbase = switch (short 'f')
          in do
            _ <- testParser ["-a"] $ infoSimple $ abortOption (ErrorMsg "NOT IMPLEMENTED") (short 'a') <*> pbase
            _ <- testParser [] $ infoSimple $ abortOption (ErrorMsg "NOT IMPLEMENTED") (short 'a') <*> pbase
            _ <- testParser ["-a"] $ infoSimple $ infoOption "ABORT" (short 'a') <*> pbase
            pauseIO
            return ()

        pure ()
    )
    "TestOptparseApplicative"


infoSimple :: Parser a -> ParserInfo a
infoSimple p = info (helper <*> p) fullDesc

testHelp :: Show a => Parser a -> IO ()
testHelp parser = do
  _ <- testParser ["--help"] $ info (helper <*> parser) mempty
  pure ()

testParser :: Show a => Args -> ParserInfo a -> IO (ParserResult a)
testParser args pinfo = withArgs args $ do
  let result = execParserPure defaultPrefs pinfo args
  fmtLn $
    "args:"
      +| indentF 2 (listF args)
      |+ ""
      +| "result:\n"
      +| indentF 6 (build $ show result)
      |+ ""
  return result
