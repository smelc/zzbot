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
import Data.Foldable (traverse_)
import Data.Kind
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Validation
import Data.Void
import System.Exit
import System.IO
import System.Process

import Common (StepStreams(StepStreams), toExitCode)
import Config
import Db
import Xml

import qualified Common
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.IO.Handle as Handle

parsingErrorCode, substitutionErrorCode, subprocessErrorCode :: ExitCode

parsingErrorCode = ExitFailure 1
substitutionErrorCode = ExitFailure 2
subprocessErrorCode = ExitFailure 3

-- The second element maps build variables to their values
data BuildContext = BuildContext
  { buildState :: BuildState
  , properties :: Map.Map String String
  }

withProperties :: BuildContext -> Map.Map String String -> BuildContext
withProperties BuildContext{buildState} = BuildContext buildState

data LogLevel = LogLevelInfo | LogLevelError
  deriving (Eq, Show)

data Exec :: Type -> Type where
  ZzLog :: LogLevel -> String -> Exec ()
  RunShellCommand
    :: String                       -- ^ The working directory
    -> Command                      -- ^ The command to execute
    -> Exec (ExitCode, String, String) -- ^ return code, stdout, stderr
  PutOut   :: String -> Exec ()
  PutErr   :: String -> Exec ()

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
    | LogLevelInfo <- logLevel = stdout
    | LogLevelError <- logLevel = stderr
  style
    | LogLevelInfo <- logLevel = color Green
    | LogLevelError <- logLevel = color Red
execToIO (RunShellCommand workdir Command{cmdFilename, cmdArgs}) =
  sendM $ readCreateProcessWithExitCode createProcess ""
 where
  createProcess = (proc cmdFilename cmdArgs) { cwd = Just workdir }
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

inject
  :: Members '[Exec, Error ExitCode] effs
  => Show e
  => ExitCode
  -> Validation (Set.Set e) a
  -> Eff effs a
inject code validation =
  case validation of
    Success res -> return res
    Failure errors -> do
      zzLog LogLevelError (buildErrorMsg errors)
      throwError code
 where
  buildErrorMsg :: forall e . Show e => Set.Set e -> String
  buildErrorMsg errors = unlines $ map show $ Set.toList errors

dynSubstDelimiters = ("«", "»")

runSteps
  :: Members '[Exec, DbOperations, Error ExitCode] effs
  => BuildContext
  -> [Step Substituted]
  -> Eff effs BuildContext
runSteps ctxt [] = return ctxt
runSteps ctxt@BuildContext{buildState, properties} (step:steps) = do
  step' <- inject
             substitutionErrorCode
             (substitute dynSubstDelimiters (Map.toList properties) step)
  stepID <- startStep buildState step'
  (BuildContext buildState' properties', streams, status, continue) <-
    runStep ctxt step'
  buildState'' <- endStep buildState' stepID streams status
  unless continue $ throwError subprocessErrorCode
  runSteps (BuildContext buildState'' properties') steps

prettyCommand :: Command -> String
prettyCommand (Command cmd []) = cmd
prettyCommand (Command cmd args) = cmd ++ " " ++ unwords args

runStep
  :: Member Exec effs
  => BuildContext
  -> Step Substituted -- ^ The step to execute
  -> Eff effs (BuildContext, StepStreams, Common.Status, Bool) -- ^ Last Bool indicates if build should go on
runStep ctxt@BuildContext{properties} (SetPropertyFromValue prop value) =
  return (ctxt', StepStreams Nothing Nothing, Common.Success, True)
  where ctxt' = withProperties ctxt $ Map.insert prop value properties
runStep ctxt@BuildContext{properties} (ShellCmd workdir cmd mprop haltOnFailure) = do
  let infoSuffix :: String = case mprop of Nothing -> ""
                                           Just prop -> " → " ++ prop
  zzLog LogLevelInfo (prettyCommand cmd ++ infoSuffix)
  (rc, outmsg, errmsg) <- runShellCommand workdir cmd
  unless (null outmsg) $ putOut outmsg -- show step normal output, if any
  unless (null errmsg) $ putErr errmsg -- show step error output, if any
  let ctxt' = case mprop of Nothing -> ctxt
                            Just prop -> withProperties ctxt $ Map.insert prop outmsg properties
      streams = StepStreams (Just outmsg) (Just errmsg)
      status = toExitCode rc
  unless (rc == ExitSuccess) $
    zzLog LogLevelError (prettyCommand cmd ++ " failed: " ++ show rc)
  return (ctxt', streams, toExitCode rc, not haltOnFailure || not (haltBuilds status))
  where haltBuilds Common.Success = False
        haltBuilds Common.Warning = False
        haltBuilds Common.Cancellation = True
        haltBuilds Common.Failure = True
        haltBuilds Common.Error = True
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

process
  :: Members '[Exec, DbOperations, Error ExitCode] effs
  => ProcessMode -- ^ Whether to print or execute the builder
  -> ProcessEnv -- ^ The system's environment
  -> String -- ^ The content of the XML file to process
  -> Eff effs ()
process mode ProcessEnv{Exec.workdir, sysenv} xml = do
  parsedConfig <- inject parsingErrorCode (parseXmlString xml)
  let normalizedConfig = normalize workdir parsedConfig
  substitutedConfig@Config{builders} <-
    inject substitutionErrorCode (substAll sysenv normalizedConfig)
  case mode of
    PrintOnly -> putOutLn (renderAsXml substitutedConfig)
    Execute -> traverse_ runBuild builders
