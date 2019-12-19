{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- File to execute a build specified as a Builder
module Exec (
  MonadExec(..)
  , ExecutionError(..)
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

-- Maps build variables to their values
type BuildContext = Map.Map String String

class Monad m => MonadExec m where
  zzLog :: Color -> String -> m ()
  runShellCommand
    :: Maybe String                 -- ^ Optional path to the working directory
    -> Command                      -- ^ The command to execute
    -> m (ExitCode, String, String) -- ^ return code, stdout, stderr
  putOutLn :: String -> m ()
  putErrLn :: String -> m ()

instance MonadExec IO where
  zzLog textColor logEntry = putDoc (annotate style doc)
   where
    doc = "ZZ>" <+> pretty logEntry <> hardline
    style = color textColor

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

data ExecutionError = ExecutionError (Maybe String) ExitCode
  deriving (Eq, Show)

type ExecError m = ExceptT ExecutionError m

inject :: (Show e, Monad m) => Validation (Set.Set e) a -> ExecError m a
inject = ExceptT . return . left makeError . toEither
 where
  makeError errors =
    ExecutionError (Just $ buildErrorMsg errors) (ExitFailure 1)

dynSubstDelimiters = ("«", "»")

runSteps
  :: MonadExec m
  => Maybe String
  -> BuildContext
  -> [Step]
  -> ExecError m ()
runSteps _ ctxt [] = return ()
runSteps builderWorkdir ctxt (step:steps) = do
  step' <- inject (substitute dynSubstDelimiters (Map.toList ctxt) step)
  ctxt' <- runStep builderWorkdir ctxt step'
  runSteps builderWorkdir ctxt' steps

runStep :: MonadExec m
        => Maybe String -- ^ The builder's workdir, if any
        -> BuildContext
        -> Step         -- ^ The step to execute
        -> ExecError m BuildContext
runStep builderWorkdir ctxt (SetPropertyFromValue prop value) =
  return $ Map.insert prop value ctxt
runStep builderWorkdir ctxt (ShellCmd workdir cmd) = do
  zzLog Green (show cmd)
  (rc, outmsg, errmsg) <- runShellCommand (workdir <|> builderWorkdir) cmd
  unless (null outmsg) $ putOutLn outmsg -- show step normal output, if any
  unless (null errmsg) $ putErrLn errmsg -- show step error output, if any
  case rc of
    ExitSuccess ->
      -- step succeeded, execution will continue
      return ctxt
    _ -> do
      -- step failed, execution will stop
      zzLog Red (show cmd ++ " failed: " ++ show rc)
      throwError (ExecutionError Nothing rc)

runBuild :: (Monad m, MonadExec m) => Builder -> ExecError m ()
runBuild (Builder workdir _ steps) = runSteps workdir Map.empty steps

buildErrorMsg :: Show a => Set.Set a -> String
buildErrorMsg errors = unlines $ map show $ Set.toList errors

process'
  :: MonadExec m
  => Bool -- ^ Whether to print (True) or execute the builder (False)
  -> [(String, String)] -- ^ The environment
  -> String -- ^ The content of the XML file to process
  -> ExecError m ()
process' printOnly env xml = do
  config <- inject (parseXmlString xml)
  sconfig@Config{builders} <- inject (substAll env config)
  if printOnly
    then do
      putOutLn (LT.unpack $ renderAsXml sconfig)
      return ()
    else
      traverse_ runBuild builders

process
  :: MonadExec m
  => Bool -- ^ Whether to print (True) or execute the builder (False)
  -> [(String, String)] -- ^ The environment
  -> String -- ^ The content of the XML file to process
  -> m ExitCode
process printOnly env xml = do
  result <- runExceptT (process' printOnly env xml)
  case result of
    Left (ExecutionError msg code) -> do
      traverse_ putErrLn msg
      return code
    Right () ->
      return ExitSuccess
