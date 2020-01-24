{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Applicative
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Data.List.NonEmpty (NonEmpty)
import Data.Function
import System.Directory
import System.Environment
import System.Exit

import Config
import Db
import Exec
import LowLevelDb

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
  -> Eff '[Exec, DbOperations, Reader Database, Error ExitCode, IO] a
  -> IO (Either ExitCode a)
runConcreteStack db action =
  action
    & runExecWithIO
    & interpretDbOpsAsLowLevelDbOps
    & runLowLevelDbOpsWithSQLite
    & runReader db
    & runError
    & runM

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
