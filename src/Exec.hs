{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

-- File to execute a build specified as a Builder
module Exec
  ( ProcessEnv(..)
  , MonadExec(..)
  , LogLevel(..)
  , ProcessMode(..)
  , runBuild
  , process
   ) where

import Control.Applicative
import Control.Arrow (left)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Except
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Validation
import System.Exit
import System.IO
import System.Process

import Config
import Db
import Xml

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified GHC.IO.Handle as Handle

parsingErrorCode, substitutionErrorCode, subprocessErrorCode :: ExitCode

parsingErrorCode = ExitFailure 1
substitutionErrorCode = ExitFailure 2
subprocessErrorCode = ExitFailure 3

-- Maps build variables to their values
type BuildContext = Map.Map String String

data LogLevel = Info | Error
  deriving (Eq, Show)

class Monad m => MonadExec m where
  zzLog :: LogLevel -> String -> m ()
  runShellCommand
    :: String                       -- ^ The working directory
    -> Command                      -- ^ The command to execute
    -> m (ExitCode, String, String) -- ^ return code, stdout, stderr
  putOut   :: String -> m ()
  putErr   :: String -> m ()

instance MonadExec IO where
  zzLog logLevel logEntry = hPutDoc handle (annotate style doc)
   where
    doc = "ZZ>" <+> pretty logEntry <> hardline
    handle
      | Info <- logLevel = stdout
      | Error <- logLevel = stderr
    style
      | Info <- logLevel = color Green
      | Error <- logLevel = color Red

  runShellCommand workdir Command{cmdFilename, cmdArgs} =
    readCreateProcessWithExitCode createProcess ""
   where
    createProcess = (proc cmdFilename cmdArgs) { cwd = Just workdir }

  putOut   = putStr
  putErr   = hPutStr stderr

instance MonadExec m => MonadExec (ExceptT e m) where
  zzLog textColor logEntry = lift (zzLog textColor logEntry)
  runShellCommand workdir command = lift (runShellCommand workdir command)
  putOut   str = lift (putOut str)
  putErr   str = lift (putErr str)


putOutLn :: MonadExec m => String -> m ()
putOutLn s = putOut (s ++ "\n")

putErrLn :: MonadExec m => String -> m ()
putErrLn s = putErr (s ++ "\n")

inject
  :: (Show e, MonadExec m, MonadError ExitCode m)
  => ExitCode
  -> Validation (Set.Set e) a
  -> m a
inject code validation =
  case validation of
    Success res -> return res
    Failure errors -> do
      zzLog Error (buildErrorMsg errors)
      throwError code
 where
  buildErrorMsg :: Show a => Set.Set a -> String
  buildErrorMsg errors = unlines $ map show $ Set.toList errors

dynSubstDelimiters = ("«", "»")

runSteps
  :: (MonadExec m, MonadError ExitCode m)
  => BuildContext
  -> [Step Substituted]
  -> m ()
runSteps ctxt [] = return ()
runSteps ctxt (step:steps) = do
  step' <- inject
             substitutionErrorCode
             (substitute dynSubstDelimiters (Map.toList ctxt) step)
  ctxt' <- runStep ctxt step'
  runSteps ctxt' steps

runStep :: (MonadExec m, MonadError ExitCode m)
        => BuildContext
        -> Step Substituted -- ^ The step to execute
        -> m BuildContext
runStep ctxt (SetPropertyFromValue prop value) =
  return $ Map.insert prop value ctxt
runStep ctxt (ShellCmd workdir cmd mprop haltOnFailure) = do
  let infoSuffix :: String = case mprop of Nothing -> ""
                                           Just prop -> " → " ++ prop
  zzLog Info (show cmd ++ infoSuffix)
  (rc, outmsg, errmsg) <- runShellCommand workdir cmd
  unless (null outmsg) $ putOut outmsg -- show step normal output, if any
  unless (null errmsg) $ putErr errmsg -- show step error output, if any
  let ctxt' = case mprop of Nothing -> ctxt
                            Just prop -> Map.insert prop outmsg ctxt
  unless (rc == ExitSuccess) $ zzLog Error (show cmd ++ " failed: " ++ show rc)
  when (haltOnFailure && rc /= ExitSuccess ) $ throwError subprocessErrorCode
  return ctxt'

runBuild :: (MonadExec m, MonadError ExitCode m) => Builder Substituted -> m ()
runBuild (Builder () _ steps) = runSteps Map.empty steps

data ProcessEnv = ProcessEnv { workdir :: FilePath, -- ^ The working directory
                               sysenv :: [(String, String)] -- ^ The system's environment
                             }

data ProcessMode = PrintOnly | Execute

process
  :: (MonadExec m, MonadError ExitCode m)
  => ProcessMode -- ^ Whether to print or execute the builder
  -> ProcessEnv -- ^ The system's environment
  -> String -- ^ The content of the XML file to process
  -> m ()
process mode ProcessEnv{Exec.workdir, sysenv} xml = do
  parsedConfig <- inject parsingErrorCode (parseXmlString xml)
  let normalizedConfig = normalize workdir parsedConfig
  substitutedConfig@Config{builders} <-
    inject substitutionErrorCode (substAll sysenv normalizedConfig)
  case mode of
    PrintOnly -> putOutLn (renderAsXml substitutedConfig)
    Execute -> traverse_ runBuild builders
