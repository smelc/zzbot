{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Applicative
import Control.Effect
import Data.List.NonEmpty (NonEmpty)
import Data.Function
import Data.List.NonEmpty.Extra (maximum1)
import Data.Maybe
import Data.Validation
import System.Directory
import System.Environment
import System.Exit

import Common
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

runConcreteStack db action =
  action
    & runMonadExecWithIO
    & interpretDbOpsAsLowLevelDbOps
    & runLowLevelDbOpsWithSQLite db

main :: IO ()
main = do
  Options{optFilenames, optProcessMode, optDatabasePath} <-
    Opt.execParser optionsParserInfo
  env <- ProcessEnv <$> getCurrentDirectory <*> getEnvironment
  xmls <- traverse readFile optFilenames
  withDatabase optDatabasePath $ \db -> do
    statuses <- runConcreteStack db $ traverse (process optProcessMode env) xmls
    exitWith $ toExitCode $ maximum1 statuses

-- This function makes sense solely here, that's why it's not in Common.hs
toExitCode :: Status -> ExitCode
toExitCode Common.Success = ExitSuccess
toExitCode Common.Warning = ExitSuccess
toExitCode Common.Cancellation = ExitSuccess
toExitCode Common.Failure = ExitFailure 1
toExitCode Common.Error = ExitFailure 2
