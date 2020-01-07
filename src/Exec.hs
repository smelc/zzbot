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
  putOutLn :: String -> m ()
  putErrLn :: String -> m ()

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

  putOutLn = putStrLn

  putErrLn = hPutStrLn stderr

instance MonadExec m => MonadExec (ExceptT e m) where
  zzLog textColor logEntry = lift (zzLog textColor logEntry)
  runShellCommand workdir command = lift (runShellCommand workdir command)
  putOutLn str = lift (putOutLn str)
  putErrLn str = lift (putErrLn str)

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
runStep ctxt (ShellCmd workdir cmd mprop) = do
  let infoSuffix :: String = case mprop of Nothing -> ""
                                           Just prop -> " → " ++ prop
  zzLog Info (show cmd ++ infoSuffix)
  (rc, outmsg, errmsg) <- runShellCommand workdir cmd
  unless (null outmsg) $ putOutLn outmsg -- show step normal output, if any
  unless (null errmsg) $ putErrLn errmsg -- show step error output, if any
  case rc of
    ExitSuccess ->
      -- step succeeded, execution will continue
      let ctxt' = case mprop of Nothing -> ctxt
                                Just prop -> Map.insert prop outmsg ctxt in
      return ctxt'
    _ -> do
      -- step failed, execution will stop
      zzLog Error (show cmd ++ " failed: " ++ show rc)
      throwError subprocessErrorCode

runBuild :: (Monad m, MonadExec m, MonadError ExitCode m) => Builder Substituted -> m ()
runBuild (Builder () _ steps) = runSteps Map.empty steps

data ProcessEnv = ProcessEnv { workdir :: FilePath, -- ^ The working directory
                               sysenv :: [(String, String)] -- ^ The system's environment
                             }

process
  :: (MonadExec m, MonadError ExitCode m)
  => Bool -- ^ Whether to print (True) or execute the builder (False)
  -> ProcessEnv -- ^ The system's environment
  -> String -- ^ The content of the XML file to process
  -> m ()
process printOnly env xml = do
  config <- inject parsingErrorCode (parseXmlString xml)
  let config' = normalize (Exec.workdir env) config
  sconfig@Config{builders} <- inject substitutionErrorCode (substAll (sysenv env) config')
  if printOnly
    then putOutLn (renderAsXml sconfig)
    else traverse_ runBuild builders
