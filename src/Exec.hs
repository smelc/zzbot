{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

-- File to execute a build specified as a Builder
module Exec
  ( ProcessEnv(..)
  , MonadExec(..)
  , LogLevel(..)
  , ProcessMode(..)
  , UsingIOForExec(..)
  , runBuild
  , process
   ) where

import Control.Applicative
import Control.Arrow (left)
import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.Function
import Data.List
import Data.List.NonEmpty.Extra (maximum1)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Validation
import Data.Void
import System.Exit
import System.IO
import System.Process

import Common (StepStreams(StepStreams), toStatus, BuildID, Status)
import Config
import Db
import Xml

import qualified Common
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Properties = Map.Map String String

data LogLevel = InfoLevel | ErrorLevel
  deriving (Eq, Show)

class Monad m => MonadExec s m where
  zzLog :: LogLevel -> String -> m ()
  runShellCommand
    :: String                       -- ^ The working directory
    -> Command                      -- ^ The command to execute
    -> m (ExitCode, String, String) -- ^ return code, stdout, stderr
  putOut   :: String -> m ()
  putErr   :: String -> m ()

data UsingIOForExec

instance (Monad m, MonadIO m) => MonadExec UsingIOForExec m where
  zzLog logLevel logEntry = liftIO $ hPutDoc handle (annotate style doc)
   where
    doc = "ZZ>" <+> pretty logEntry <> hardline
    handle
      | InfoLevel <- logLevel = stdout
      | ErrorLevel <- logLevel = stderr
    style
      | InfoLevel <- logLevel = color Green
      | ErrorLevel <- logLevel = color Red

  runShellCommand workdir Command{cmdString} =
    liftIO $ readCreateProcessWithExitCode createProcess ""
   where
    createProcess = (shell cmdString) { cwd = Just workdir }

  putOut = liftIO . putStr
  putErr = liftIO . hPutStr stderr

putOutLn :: forall s m . MonadExec s m => String -> m ()
putOutLn str = putOut @s (str ++ "\n")

putErrLn :: forall s m . MonadExec s m => String -> m ()
putErrLn str = putErr @s (str ++ "\n")

zzLogInfo :: forall s m . MonadExec s m => String -> m ()
zzLogInfo = zzLog @s InfoLevel

zzLogError :: forall s m . MonadExec s m => String -> m ()
zzLogError = zzLog @s ErrorLevel

dynSubstDelimiters = ("«", "»")

runSteps
  :: forall s1 s2 m
   . (MonadExec s1 m, DbOperations s2 m)
  => BuildID
  -> Properties
  -> [Step Substituted]
  -> m Status
runSteps _ ctxt [] = return Common.Success
runSteps buildId properties (step:steps) =
  case substitute dynSubstDelimiters (Map.toList properties) step  of
    Failure errors -> do
      zzLogError @s1 (buildErrorMsg errors)
      return Common.Failure
    Success step' -> do
      stepID <- startStep @s2 buildId step'
      -- We must call endStep now, no matter what happens. Could we handle that like a resource?
      (properties', streams, status, continue) <- runStep @s1 properties step'
      endStep @s2 buildId stepID streams status
      if continue
        then max status <$> runSteps @s1 @s2 buildId properties' steps
        else return status

runStep
  :: forall s m
   . MonadExec s m
  => Properties
  -> Step Substituted -- ^ The step to execute
  -> m (Properties, StepStreams, Status, Bool) -- ^ Last Bool indicates if build should go on
runStep properties (SetPropertyFromValue prop value) =
  return (properties', StepStreams Nothing Nothing, Common.Success, True)
  where properties' = Map.insert prop value properties
runStep properties (ShellCmd workdir cmd@Command{cmdString} mprop haltOnFailure ignoreFailure) = do
  zzLogInfo @s (cmdString ++ maybe "" (" → " ++) mprop)
  (rc, outmsg, errmsg) <- runShellCommand @s workdir cmd
  unless (null outmsg) $ putOut @s outmsg -- show step standard output, if any
  unless (null errmsg) $ putErr @s errmsg -- show step error output, if any
  unless (rc == ExitSuccess) $
    zzLogError @s (cmdString ++ " failed: " ++ show (Common.exitCodetoInt rc))
  let properties' = properties & maybe id (`Map.insert` normalize outmsg) mprop
      streams = StepStreams (Just outmsg) (Just errmsg)
      status = if ignoreFailure then Common.Success else toStatus rc
  return (properties', streams, status, not haltOnFailure || rc == ExitSuccess)
 where
  normalize "" = ""
  normalize str = if last str == '\n' then init str else str
runStep _ (Ext ext) = absurd ext

runBuild
  :: forall s1 s2 m
   . (MonadExec s1 m, DbOperations s2 m)
  => Builder Substituted
  -> m Status
runBuild (Builder () name steps) = do
  buildId <- startBuild @s2 name
  finalStatus <- runSteps @s1 @s2 buildId Map.empty steps
  endBuild @s2 buildId finalStatus
  return finalStatus

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
  :: forall s1 s2 m
   . (MonadExec s1 m, DbOperations s2 m)
  => ProcessMode -- ^ Whether to print or execute the builder
  -> ProcessEnv -- ^ The system's environment
  -> String -- ^ The content of the XML file to process
  -> m Common.Status
process mode env xml =
  case prepareConfig mode env xml of
    Failure errMsg -> do
      zzLogError @s1 errMsg
      return Common.Failure
    Success substitutedConfig@Config{builders} ->
      case mode of
        PrintOnly -> do
          putOutLn @s1 (renderAsXml substitutedConfig)
          return Common.Success
        Execute -> do
          statuses <- traverse (runBuild @s1 @s2) builders
          return $ maximum1 statuses