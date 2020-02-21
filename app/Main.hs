{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Config
import Control.Applicative
import Control.Concurrent.Async
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty.Extra (maximum1)
import Data.Maybe
import Data.Validation
import System.Directory
import System.Environment
import System.Exit

import Common
import Db
import LowLevelDb
import Exec
import Web
import Xml

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Options.Applicative as Opt

data WebMode = WebYes | WebNo

data Options = Options
  { optFilenames :: [String]
  , optProcessMode :: ProcessMode
  , optWebMode :: WebMode
  , optDatabasePath :: FilePath
  }

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> some (Opt.strArgument (Opt.metavar "FILE"))
    <*> Opt.flag
          Execute
          PrintOnly
          (Opt.long "print"
             <> Opt.short 'p'
             <> Opt.help "print the builder's interpretation, but do not execute it")
    <*> Opt.flag
          WebNo
          WebYes
          (Opt.long "web"
             <> Opt.short 'w'
             <> Opt.help "runs a web server to crawl builds' data")
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
  -> ReaderT Database IO a
  -> IO a
runConcreteStack = flip runReaderT

launchBuild :: Database
            -> ProcessMode
            -> ProcessEnv
            -> [String]
            -> IO [Status]
launchBuild db processMode processEnv xmls =
  runConcreteStack db $ traverse
    (process @UsingIOForExec @(UsingLowLevelDb UsingIOForDb) processMode processEnv) xmls

launchWeb :: Database
          -> WebMode
          -> IO()
launchWeb db mode = 
  case mode of
    WebNo -> return ()
    WebYes -> web 8010 db

launchAll :: Database
          -> ProcessMode
          -> WebMode
          -> ProcessEnv
          -> [String]
          -> IO Status
launchAll db processMode webMode processEnv xmls = do
  (_, statuses :: [Status]) <- concurrently
                                 (launchWeb db webMode)
                                 (launchBuild db processMode processEnv xmls)
  return $ foldr max Common.Success statuses

main :: IO ()
main = do
  Options{optFilenames, optProcessMode, optWebMode, optDatabasePath} <-
    Opt.execParser optionsParserInfo
  env <- ProcessEnv <$> getCurrentDirectory <*> getEnvironment
  xmls <- traverse readFile optFilenames
  withDatabase optDatabasePath $ \db -> do
    status <- launchAll db optProcessMode optWebMode env xmls
    exitWith $ toExitCode status

-- This function makes sense solely here, that's why it's not in Common.hs
toExitCode :: Status -> ExitCode
toExitCode Common.Success = ExitSuccess
toExitCode Common.Warning = ExitSuccess
toExitCode Common.Cancellation = ExitSuccess
toExitCode Common.Failure = ExitFailure 1
toExitCode Common.Error = ExitFailure 2