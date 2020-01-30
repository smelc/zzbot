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
import Control.Monad.Except
import Data.Bifunctor
import Data.Foldable (traverse_)
import Data.List
import Data.List.Extra
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

parsingErrorCode = ExitFailure 1 -- ^ Configuration cannot be parsed
substitutionErrorCode = ExitFailure 2 -- ^ Application of static substitution failed
failureStatusErrorCode = ExitFailure 3 -- ^ Build returned 'Common.Failure'
errorStatusErrorCode = ExitFailure 4 -- ^ Build returned 'Common.Error'

type Properties = Map.Map String String

-- | The second element maps build variables to their values
data BuildContext = BuildContext
  { buildState :: BuildState -- ^ Build identifier and build status (so far)
  , properties :: Map.Map String String -- ^ Build properties
  }

data LogLevel = Info | Error
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
      | Info <- logLevel = stdout
      | Error <- logLevel = stderr
    style
      | Info <- logLevel = color Green
      | Error <- logLevel = color Red

  runShellCommand workdir Command{cmdString} =
    liftIO $ readCreateProcessWithExitCode createProcess ""
   where
    createProcess = (shell cmdString) { cwd = Just workdir }

  putOut = liftIO . putStr
  putErr = liftIO . hPutStr stderr

instance forall e s m . MonadExec s m => MonadExec s (ExceptT e m) where
  zzLog textColor logEntry = lift (zzLog @s textColor logEntry)
  runShellCommand workdir command = lift (runShellCommand @s workdir command)
  putOut   str = lift (putOut @s str)
  putErr   str = lift (putErr @s str)

putOutLn :: forall s m . MonadExec s m => String -> m ()
putOutLn str = putOut @s (str ++ "\n")

putErrLn :: forall s m . MonadExec s m => String -> m ()
putErrLn str = putErr @s (str ++ "\n")

dynSubstDelimiters = ("«", "»")

runSteps
  :: forall s1 s2 m
   . (MonadExec s1 m, DbOperations s2 m)
  => BuildContext
  -> [Step Substituted]
  -> m BuildContext
runSteps ctxt [] = return ctxt
runSteps ctxt@BuildContext{buildState, properties} (step:steps) =
  let vstep' = substitute dynSubstDelimiters (Map.toList properties) step in
  case vstep' of
    Failure errors -> do
      zzLog @s1 Error (buildErrorMsg errors)
      return $ BuildContext (withMaxStatus buildState Common.Failure) properties
    Success step' -> do
      stepID <- startStep @s2 buildState step'
      -- We must call endStep now, no matter what happens. Could we handle that like a resource?
      (properties', streams, status, continue) <- runStep @s1 properties step'
      buildState' <- endStep @s2 buildState stepID streams status
      let ctxt' = BuildContext buildState' properties'
      if continue then runSteps @s1 @s2 ctxt' steps
      else return ctxt'
 where
  buildErrorMsg :: forall e . Show e => Set.Set e -> String
  buildErrorMsg errors = unlines $ map show $ Set.toList errors

runStep
  :: forall s m
   . MonadExec s m
  => Properties
  -> Step Substituted -- ^ The step to execute
  -> m (Properties, StepStreams, Common.Status, Bool) -- ^ Last Bool indicates if build should go on
runStep properties (SetPropertyFromValue prop value) =
  return (properties', StepStreams Nothing Nothing, Common.Success, True)
  where properties' = Map.insert prop value properties
runStep properties (ShellCmd workdir cmd mprop haltOnFailure) = do
  let infoSuffix :: String = case mprop of Nothing -> ""
                                           Just prop -> " → " ++ prop
  zzLog @s Info (prettyCommand cmd ++ infoSuffix)
  (rc, outmsg, errmsg) <- runShellCommand @s workdir cmd
  unless (null outmsg) $ putOut @s outmsg -- show step normal output, if any
  unless (null errmsg) $ putErr @s errmsg -- show step error output, if any
  let properties' = case mprop of Nothing -> properties
                                  Just prop -> Map.insert prop (trim outmsg) properties
      streams = StepStreams (Just outmsg) (Just errmsg)
  unless (rc == ExitSuccess) $
    zzLog @s Error (prettyCommand cmd ++ " failed: " ++ show rc)
  return (properties', streams, toStatus rc, not haltOnFailure || rc == ExitSuccess)
 where
  prettyCommand Command{cmdString} = cmdString
runStep _ (Ext ext) = absurd ext

runBuild
  :: forall s1 s2 m
   . (MonadExec s1 m, DbOperations s2 m)
  => Builder Substituted
  -> m ()
runBuild (Builder () name steps) = do
  initialState <- startBuild @s2 name
  finalCtxt <- runSteps @s1 @s2 (BuildContext initialState Map.empty) steps
  endBuild @s2 (buildState finalCtxt)
  return ()

data ProcessEnv = ProcessEnv { workdir :: FilePath, -- ^ The working directory
                               sysenv :: [(String, String)] -- ^ The system's environment
                             }

data ProcessMode = PrintOnly | Execute

errorToString :: Show e => Validation (Set.Set e) a -> Validation String a
errorToString = first (unlines . map show . Set.toList )

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
  -> m ()
process mode env xml =
  case prepareConfig mode env xml of
    Failure errMsg -> zzLog @s1 Error errMsg
    Success substitutedConfig@Config{builders} ->
      case mode of
        PrintOnly -> putOutLn @s1 (renderAsXml substitutedConfig)
        Execute -> traverse_ (runBuild @s1 @s2) builders