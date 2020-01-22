{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Config
import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Validation
import System.Directory
import System.Environment
import System.Exit

import Db
import LowLevelDb
import Exec
import Xml

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Options.Applicative as Opt

{- HLINT ignore Options -}
data Options = Options
  { optFilenames :: NonEmpty String
  , optProcessMode :: ProcessMode
  , optDatabasePath :: FilePath
  }

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> NE.some1 (Opt.strArgument (Opt.metavar "FILE"))
    <*> Opt.flag
          Execute
          PrintOnly
          (Opt.long "print"
             <> Opt.short 'p'
             <> Opt.help "print the builder's interpretation, but do not execute it")
    <*> Opt.strOption
          (Opt.long "database"
            <> Opt.help "The sqlite database to use"
            <> Opt.value "state.sqlite"
            <> Opt.metavar "DATABASE"
            <> Opt.showDefault)

optionsParserInfo :: Opt.ParserInfo Options
optionsParserInfo = Opt.info (optionsParser <**> Opt.helper) Opt.fullDesc

runConcreteStack
  :: Database
  -> UsingIOForExec
       ( UsingLowLevelDb
           (UsingIOForDb (ReaderT Database (ExceptT ExitCode IO)))
       )
       a
  -> IO (Either ExitCode a)
runConcreteStack db =
  runExceptT
    . flip runReaderT db
    . runUsingIOForDb
    . runUsingLowLevelDb
    . runUsingIOForExec

main :: IO ()
main = do
  Options{optFilenames, optProcessMode, optDatabasePath} <-
    Opt.execParser optionsParserInfo
  env <- ProcessEnv <$> getCurrentDirectory <*> getEnvironment
  xmls <- traverse readFile optFilenames
  withDatabase optDatabasePath $ \db -> do
    res <- runConcreteStack db $ traverse (process optProcessMode env) xmls
    case res of
      Left code -> exitWith code
      _ -> return ()
