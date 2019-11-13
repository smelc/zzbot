{-# LANGUAGE ScopedTypeVariables #-}

-- File to exec a build specified as a Builder
module Exec where

import Control.Monad
import System.Console.ANSI
import System.Exit
import System.IO
import System.Process

import Config

import qualified Data.Map.Strict as Map
import qualified GHC.IO.Handle as Handle

-- Maps build variables to their values
type BuildContext = Map.Map String String

zzLog :: String -> IO()
zzLog log = putStrLn $ "ZZ> " ++ log

-- return code, stdout, stderr
runShellCommand :: [String] -> IO (ExitCode, String, String)
runShellCommand cmd = do
    setSGR [SetColor Foreground Vivid Green]
    zzLog $ unwords cmd
    setSGR []
    readProcessWithExitCode "/bin/sh" ("-c" : cmd) ""

runSteps :: BuildContext -> [Step] -> IO ExitCode
runSteps ctxt [] = return ExitSuccess
runSteps ctxt (SetPropertyFromValue prop value : steps) =
  runSteps (Map.insert prop value ctxt) steps
runSteps ctxt (ShellCmd cmd : steps) = do
  (rc, outmsg :: String, errmsg) <- runShellCommand cmd
  unless (null outmsg) $ putStr outmsg -- show step normal output, if any
  unless (null errmsg) $ hPutStrLn stderr errmsg -- show step error output, if any
  case rc of
    ExitSuccess -> runSteps ctxt steps -- step succeeded, continue execution
    _           -> do                  -- step failed, stop execution
      setSGR [SetColor Foreground Vivid Red]
      zzLog $ unwords cmd ++ " failed: " ++ show rc
      setSGR []
      return rc

runBuild :: Builder -> IO ExitCode
runBuild (Builder _ steps) = runSteps Map.empty steps
