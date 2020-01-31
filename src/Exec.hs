{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- File to execute a build specified as a Builder
module Exec
  ( ProcessEnv(..)
  , Exec(..)
  , LogLevel(..)
  , ProcessMode(..)
  , runExecWithIO
  , runBuild
  , process
   ) where

import Control.Applicative
import Control.Arrow (left)
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.Kind
import Data.Function
import Data.List
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Validation
import Data.Void
import System.Exit
import System.IO
import System.Process

import Common (StepStreams(StepStreams), toStatus)
import Config
import Db
import Xml

import qualified Common
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.IO.Handle as Handle

parsingErrorCode, substitutionErrorCode, failureStatusErrorCode, errorStatusErrorCode :: ExitCode

-- | Configuration cannot be parsed
parsingErrorCode = ExitFailure 1
-- | Application of static substitution failed
substitutionErrorCode = ExitFailure 2
-- | Build returned 'Common.Failure'
failureStatusErrorCode = ExitFailure 3
-- | Build returned 'Common.Error'
errorStatusErrorCode = ExitFailure 4

type Properties = Map.Map String String

-- | The second element maps build variables to their values
data BuildContext = BuildContext
  { buildState :: BuildState -- ^ Build identifier and build status (so far)
  , properties :: Properties -- ^ Build properties
  }

data LogLevel = InfoLevel | ErrorLevel
  deriving (Eq, Show)

data Exec :: Type -> Type where
  ZzLog :: LogLevel -> String -> Exec ()
  RunShellCommand
    :: String                       -- ^ The working directory
    -> Command                      -- ^ The command to execute
    -> Exec (ExitCode, String, String) -- ^ return code, stdout, stderr
  PutOut :: String -> Exec ()
  PutErr :: String -> Exec ()

zzLog :: Member Exec effs => LogLevel -> String -> Eff effs ()
zzLog logLevel logEntry = send (ZzLog logLevel logEntry)

runShellCommand :: Member Exec effs => String -> Command -> Eff effs (ExitCode, String, String)
runShellCommand workDir command = send (RunShellCommand workDir command)

putOut :: Member Exec effs => String -> Eff effs ()
putOut str = send (PutOut str)

putErr :: Member Exec effs => String -> Eff effs ()
putErr str = send (PutErr str)

runExecWithIO
  :: LastMember IO effs
  => Eff (Exec ': effs)
  ~> Eff effs
runExecWithIO = interpret execToIO

execToIO
  :: LastMember IO effs
  => Exec
  ~> Eff effs
execToIO (ZzLog logLevel logEntry) =
  sendM $ hPutDoc handle (annotate style doc)
 where
  doc = "ZZ>" <+> pretty logEntry <> hardline
  handle
    | InfoLevel <- logLevel = stdout
    | ErrorLevel <- logLevel = stderr
  style
    | InfoLevel <- logLevel = color Green
    | ErrorLevel <- logLevel = color Red
execToIO (RunShellCommand workdir Command{cmdString}) =
  sendM $ readCreateProcessWithExitCode createProcess ""
 where
  createProcess = (shell cmdString) { cwd = Just workdir }
execToIO (PutOut str) = sendM (putStr str)
execToIO (PutErr str) = sendM (hPutStr stderr str)

putOutLn
  :: Member Exec effs
  => String
  -> Eff effs ()
putOutLn str = putOut (str ++ "\n")

putErrLn
  :: Member Exec effs
  => String
  -> Eff effs ()
putErrLn str = putErr (str ++ "\n")

zzLogInfo
  :: Member Exec effs
  => String
  -> Eff effs ()
zzLogInfo = zzLog InfoLevel

zzLogError
  :: Member Exec effs
  => String
  -> Eff effs ()
zzLogError = zzLog ErrorLevel

dynSubstDelimiters = ("«", "»")

runSteps
  :: Members '[Exec, DbOperations, Error ExitCode] effs
  => BuildContext
  -> [Step Substituted]
  -> Eff effs BuildContext
runSteps ctxt [] = return ctxt
runSteps ctxt@BuildContext{buildState, properties} (step:steps) =
  case substitute dynSubstDelimiters (Map.toList properties) step  of
    Failure errors -> do
      zzLogError (buildErrorMsg errors)
      return $ BuildContext (withMaxStatus buildState Common.Failure) properties
    Success step' -> do
      stepID <- startStep buildState step'
      -- We must call endStep now, no matter what happens. Could we handle that like a resource?
      (properties', streams, status, continue) <- runStep properties step'
      buildState' <- endStep buildState stepID streams status
      let ctxt' = BuildContext buildState' properties'
      if continue then runSteps ctxt' steps
      else return ctxt'

runStep
  :: Member Exec effs
  => Properties
  -> Step Substituted -- ^ The step to execute
  -> Eff effs (Properties, StepStreams, Common.Status, Bool) -- ^ Last Bool indicates if build should go on
runStep properties (SetPropertyFromValue prop value) =
  return (properties', StepStreams Nothing Nothing, Common.Success, True)
  where properties' = Map.insert prop value properties
runStep properties (ShellCmd workdir cmd@Command{cmdString} mprop haltOnFailure) = do
  zzLogInfo (cmdString ++ maybe "" (" → " ++) mprop)
  (rc, outmsg, errmsg) <- runShellCommand workdir cmd
  unless (null outmsg) $ putOut outmsg -- show step standard output, if any
  unless (null errmsg) $ putErr errmsg -- show step error output, if any
  unless (rc == ExitSuccess) $
    zzLogError (cmdString ++ " failed: " ++ show rc)
  let properties' = properties & maybe id (`Map.insert` normalize outmsg) mprop
  let streams = StepStreams (Just outmsg) (Just errmsg)
  return (properties', streams, toStatus rc, not haltOnFailure || rc == ExitSuccess)
 where
  normalize "" = ""
  normalize str = if last str == '\n' then init str else str
runStep _ (Ext ext) = absurd ext

runBuild
  :: Members '[Exec, DbOperations, Error ExitCode] effs
  => Builder Substituted
  -> Eff effs ()
runBuild (Builder () name steps) = do
  initialState <- startBuild name
  finalCtxt <- runSteps (BuildContext initialState Map.empty) steps
  endBuild (buildState finalCtxt)
  return ()

data ProcessEnv = ProcessEnv { workdir :: FilePath, -- ^ The working directory
                               sysenv :: [(String, String)] -- ^ The system's environment
                             }

data ProcessMode = PrintOnly | Execute


buildErrorMsg :: Show e => Set.Set e -> String
buildErrorMsg errors = unlines $ map show $ Set.toList errors

errorToString :: Show e => Validation (Set.Set e) a -> Validation String a
errorToString = first buildErrorMsg

prepareConfig :: ProcessMode
              -> ProcessEnv -- ^ The system's environment
              -> String -- ^ The content of the XML file to process
              -> Validation String (Config Substituted) -- ^ An error message or tHe configuration to execute
prepareConfig mode ProcessEnv { Exec.workdir, sysenv } xml =
  errorToString (parseXmlString xml) `bindValidation` \config ->
    errorToString (substAll sysenv (normalize workdir config))

process
  :: Members '[Exec, DbOperations, Error ExitCode] effs
  => ProcessMode -- ^ Whether to print or execute the builder
  -> ProcessEnv -- ^ The system's environment
  -> String -- ^ The content of the XML file to process
  -> Eff effs ()
process mode env xml =
  case prepareConfig mode env xml of
    Failure errMsg -> zzLogError errMsg
    Success substitutedConfig@Config{builders} ->
      case mode of
        PrintOnly -> putOutLn (renderAsXml substitutedConfig)
        Execute -> traverse_ runBuild builders
