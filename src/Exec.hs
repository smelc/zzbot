{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- File to execute a build specified as a Builder
module Exec (
  MonadExec(..)
  , runBuild
  , process
  , andExitCodes
 ) where

import Control.Applicative
import Control.Monad
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
    :: Maybe String -- ^ Optional path to the working directory
    -> Command -- ^ The command to execute
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

runSteps
  :: MonadExec m
  => Maybe String
  -> BuildContext
  -> [Step]
  -> m ExitCode
runSteps _ ctxt [] = return ExitSuccess
runSteps builderWorkdir ctxt (SetPropertyFromValue prop value : steps) =
  runSteps builderWorkdir (Map.insert prop value ctxt) steps
runSteps builderWorkdir ctxt (ShellCmd workdir cmd : steps) = do
  zzLog Green (show cmd)
  (rc, outmsg, errmsg) <- runShellCommand (workdir <|> builderWorkdir) cmd
  unless (null outmsg) $ putOutLn outmsg -- show step normal output, if any
  unless (null errmsg) $ putErrLn errmsg -- show step error output, if any
  case rc of
    ExitSuccess ->
      -- step succeeded, continue execution
      runSteps builderWorkdir ctxt steps
    _ -> do
      -- step failed, stop execution
      zzLog Red (show cmd ++ " failed: " ++ show rc)
      return rc

runBuild :: (Monad m, MonadExec m) => Builder -> m ExitCode
runBuild (Builder workdir _ steps) = runSteps workdir Map.empty steps

andExitCodes :: NonEmpty ExitCode -> ExitCode
andExitCodes = foldr1 andExitCode

andExitCode :: ExitCode -> ExitCode -> ExitCode
andExitCode (ExitFailure i) (ExitFailure j) = ExitFailure (max i j)
andExitCode (ExitFailure i) _ = ExitFailure i
andExitCode _ (ExitFailure j) = ExitFailure j
andExitCode c1 c2 = c1

process
  :: MonadExec m
  => Bool -- ^ Whether to print (True) or execute the builder (False)
  -> [(String, String)] -- ^ The environment
  -> String -- ^ The content of the XML file to process
  -> m ExitCode
process printOnly env xml =
  case parseXmlString xml of
    Failure errors -> do
      displayErrors errors
      return (ExitFailure 1)
    Success config ->
      let msconfig = substAll env config in
      case msconfig of
        Failure errors -> do
          displayErrors errors
          return (ExitFailure 1)
        Success sconfig@Config{builders} ->
          if printOnly
            then do
              putOutLn (LT.unpack $ renderAsXml sconfig)
              return ExitSuccess
            else
              andExitCodes <$> traverse runBuild builders
 where
  displayErrors :: (MonadExec m, Show a) => Set.Set a -> m ()
  displayErrors errors = putErrLn $ unlines $ map show $ Set.toList errors

