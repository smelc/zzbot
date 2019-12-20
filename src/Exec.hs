{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- File to execute a build specified as a Builder
module Exec
  ( MonadExec(..)
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
import XmlParse

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as LT
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
    :: Maybe String                 -- ^ Optional path to the working directory
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
    createProcess = (proc cmdFilename cmdArgs) { cwd = workdir }

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
  => Maybe String
  -> BuildContext
  -> [Step]
  -> m ()
runSteps _ ctxt [] = return ()
runSteps builderWorkdir ctxt (step:steps) = do
  step' <- inject
             substitutionErrorCode
             (substitute dynSubstDelimiters (Map.toList ctxt) step)
  ctxt' <- runStep builderWorkdir ctxt step'
  runSteps builderWorkdir ctxt' steps

runStep :: (MonadExec m, MonadError ExitCode m)
        => Maybe String -- ^ The builder's workdir, if any
        -> BuildContext
        -> Step         -- ^ The step to execute
        -> m BuildContext
runStep builderWorkdir ctxt (SetPropertyFromValue prop value) =
  return $ Map.insert prop value ctxt
runStep builderWorkdir ctxt (ShellCmd workdir cmd) = do
  zzLog Info (show cmd)
  (rc, outmsg, errmsg) <- runShellCommand (workdir <|> builderWorkdir) cmd
  unless (null outmsg) $ putOutLn outmsg -- show step normal output, if any
  unless (null errmsg) $ putErrLn errmsg -- show step error output, if any
  case rc of
    ExitSuccess ->
      -- step succeeded, execution will continue
      return ctxt
    _ -> do
      -- step failed, execution will stop
      zzLog Error (show cmd ++ " failed: " ++ show rc)
      throwError rc

runBuild :: (Monad m, MonadExec m, MonadError ExitCode m) => Builder -> m ()
runBuild (Builder workdir _ steps) = runSteps workdir Map.empty steps


process
  :: (MonadExec m, MonadError ExitCode m)
  => Bool -- ^ Whether to print (True) or execute the builder (False)
  -> [(String, String)] -- ^ The environment
  -> String -- ^ The content of the XML file to process
  -> m ()
process printOnly env xml = do
  config <- inject parsingErrorCode (parseXmlString xml)
  sconfig@Config{builders} <- inject substitutionErrorCode (substAll env config)
  if printOnly
    then putOutLn (LT.unpack $ renderAsXml sconfig)
    else traverse_ runBuild builders
