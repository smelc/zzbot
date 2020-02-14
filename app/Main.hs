{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Config
import Control.Applicative
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
  , optWeb :: WebMode
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

launchWeb :: WebMode -> Database -> IO()
launchWeb mode db = 
  case mode of
    WebNo -> return ()
    WebYes -> web 8010 db

main :: IO ()
main = do
  Options{optFilenames, optProcessMode, optWeb, optDatabasePath} <-
    Opt.execParser optionsParserInfo
  env <- ProcessEnv <$> getCurrentDirectory <*> getEnvironment
  xmls <- traverse readFile optFilenames
  withDatabase optDatabasePath $ \db -> do
    -- FIXME smelc: parallelize launchWeb and runConcreteStack
    -- (we want the web server to start simultaneously with processing the build file (if any))
    -- if web is requested and no build: join on webserver, return webserver status (is there one?)
    -- if web is not requested and builds are: join on builds; return exit code corresponding to build status
    -- if both are requested: join on both, quit when both have finished; return webserver status (rationale: if you wanted
    -- the build' status, you should not launch the webserver)
    launchWeb optWeb db
    statuses :: [Status] <- runConcreteStack db $ traverse
      (process @UsingIOForExec @(UsingLowLevelDb UsingIOForDb) optProcessMode env) xmls
    exitWith $ toExitCode $ foldr max Common.Success statuses

-- This function makes sense solely here, that's why it's not in Common.hs
toExitCode :: Status -> ExitCode
toExitCode Common.Success = ExitSuccess
toExitCode Common.Warning = ExitSuccess
toExitCode Common.Cancellation = ExitSuccess
toExitCode Common.Failure = ExitFailure 1
toExitCode Common.Error = ExitFailure 2