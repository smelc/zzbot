{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- File to execute a build specified as a Builder
module Exec
  ( ProcessEnv(..)
  , MonadExec(..)
  , MonadExecIOT
  , LogLevel(..)
  , ProcessMode(..)
  , runMonadExecWithIO
  , runBuild
  , process
   ) where

import Control.Applicative
import Control.Arrow (left)
import Control.Effect
import Control.Monad
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.Kind
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

class Monad m => MonadExec m where
  zzLog :: LogLevel -> String -> m ()
  runShellCommand
    :: String                       -- ^ The working directory
    -> Command                      -- ^ The command to execute
    -> m (ExitCode, String, String) -- ^ return code, stdout, stderr
  putOut   :: String -> m ()
  putErr   :: String -> m ()

instance (Monad (t m), Send MonadExec t m) => MonadExec (EffT t m) where
  zzLog logLevel logEntry = send @MonadExec (zzLog logLevel logEntry)
  runShellCommand workDir command = send @MonadExec (runShellCommand workDir command)
  putOut str = send @MonadExec (putOut str)
  putErr str = send @MonadExec (putErr str)

data MonadExecIO
type MonadExecIOT = HandlerT MonadExecIO '[]
type instance Handles MonadExecIOT eff = eff == MonadExec

runMonadExecWithIO
  :: EffT MonadExecIOT m a
  -> m a
runMonadExecWithIO = runHandlerT . runEffT

instance MonadIO m => MonadExec (MonadExecIOT m) where
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

putOutLn :: MonadExec m => String -> m ()
putOutLn str = putOut (str ++ "\n")

putErrLn :: MonadExec m => String -> m ()
putErrLn str = putErr (str ++ "\n")

zzLogInfo :: MonadExec m => String -> m ()
zzLogInfo = zzLog InfoLevel

zzLogError :: MonadExec m => String -> m ()
zzLogError = zzLog ErrorLevel

dynSubstDelimiters = ("«", "»")

runSteps
  :: MonadExec m
  => DbOperations m
  => BuildID
  -> Properties
  -> [Step Substituted]
  -> m Status
runSteps _ ctxt [] = return Common.Success
runSteps buildId properties (step:steps) =
  case substitute dynSubstDelimiters (Map.toList properties) step  of
    Failure errors -> do
      zzLogError (buildErrorMsg errors)
      return Common.Failure
    Success step' -> do
      stepID <- startStep buildId step'
      -- We must call endStep now, no matter what happens. Could we handle that like a resource?
      (properties', streams, status, continue) <- runStep properties step'
      endStep buildId stepID streams status
      if continue
        then max status <$> runSteps buildId properties' steps
        else return status

runStep
  :: MonadExec m
  => Properties
  -> Step Substituted -- ^ The step to execute
  -> m (Properties, StepStreams, Status, Bool) -- ^ Last Bool indicates if build should go on
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
  :: MonadExec m
  => DbOperations m
  => Builder Substituted
  -> m Status
runBuild (Builder () name steps) = do
  buildId <- startBuild name
  finalStatus <- runSteps buildId Map.empty steps
  endBuild buildId finalStatus
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
  :: MonadExec m
  => DbOperations m
  => ProcessMode -- ^ Whether to print or execute the builder
  -> ProcessEnv -- ^ The system's environment
  -> String -- ^ The content of the XML file to process
  -> m Common.Status
process mode env xml =
  case prepareConfig mode env xml of
    Failure errMsg -> do
      zzLogError errMsg
      return Common.Failure
    Success substitutedConfig@Config{builders} ->
      case mode of
        PrintOnly -> do
          putOutLn (renderAsXml substitutedConfig)
          return Common.Success
        Execute -> do
          statuses <- traverse runBuild builders
          return $ maximum1 statuses
