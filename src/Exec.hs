{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- File to execute a build specified as a Builder
module Exec (
  MonadExec(..)
  , runBuild
 ) where

import Control.Applicative
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.Exit
import System.IO
import System.Process

import Config

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified GHC.IO.Handle as Handle

-- Maps build variables to their values
type BuildContext = Map.Map String String

class MonadExec m where
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
  :: (Monad m, MonadExec m)
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
