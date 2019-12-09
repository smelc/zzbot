{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

-- File to execute a build specified as a Builder
module Exec (
  MonadExec(..)
  , runBuild
 ) where

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
  -- return code, stdout, stderr
  runShellCommand :: Command -> m (ExitCode, String, String)
  putOutLn :: String -> m ()
  putErrLn :: String -> m ()

instance MonadExec IO where
  zzLog textColor logEntry = putDoc (annotate style doc)
   where
    doc = "ZZ>" <+> pretty logEntry <> hardline
    style = color textColor

  runShellCommand Command{cmdFilename, cmdArgs} =
    readProcessWithExitCode cmdFilename cmdArgs ""

  putOutLn = putStrLn

  putErrLn = hPutStrLn stderr

runSteps :: (Monad m, MonadExec m) => BuildContext -> [Step] -> m ExitCode
runSteps ctxt [] = return ExitSuccess
runSteps ctxt (SetPropertyFromValue prop value : steps) =
  runSteps (Map.insert prop value ctxt) steps
runSteps ctxt (ShellCmd cmd : steps) = do
  zzLog Green (show cmd)
  (rc, outmsg, errmsg) <- runShellCommand cmd
  unless (null outmsg) $ putOutLn outmsg -- show step normal output, if any
  unless (null errmsg) $ putErrLn errmsg -- show step error output, if any
  case rc of
    ExitSuccess -> runSteps ctxt steps -- step succeeded, continue execution
    _           -> do                  -- step failed, stop execution
      zzLog Red (show cmd ++ " failed: " ++ show rc)
      return rc

runBuild :: (Monad m, MonadExec m) => Builder -> m ExitCode
runBuild (Builder _ steps) = runSteps Map.empty steps
