{-# LANGUAGE ScopedTypeVariables #-}

-- File to exec a build specified as a Builder
module Exec where

import Control.Monad
import System.Exit
import System.IO
import System.Process

import Config

import qualified Data.Map.Strict as Map
import qualified GHC.IO.Handle as Handle

-- Maps build variables to their values
type BuildContext = Map.Map String String

createStepProcess :: [String] -> IO (Maybe Handle.Handle, Maybe Handle.Handle, Maybe Handle.Handle, ProcessHandle)
createStepProcess cmd = do
    print $ "Starting: " ++ cmdString
    createProcess (shell cmdString){ std_out = CreatePipe }
    where cmdString = unwords cmd

-- return code, stdout, stderr
runShellCommand :: [String] -> IO (ExitCode, String, String)
runShellCommand cmd = readProcessWithExitCode "/bin/sh" ("-c" : cmd) ""

runStep :: BuildContext -> Step -> Either BuildContext (IO (ExitCode, String, String)) 
runStep ctxt step = 
    case step of
        SetPropertyFromValue prop value -> Left $ Map.insert prop value ctxt
        ShellCmd cmd -> Right $ runShellCommand cmd

runSteps :: BuildContext -> [Step] -> IO ExitCode
runSteps ctxt [] = return ExitSuccess
runSteps ctxt (step : tl) =
    case runStep ctxt step of
        Left ctxt' -> runSteps ctxt' tl 
        Right io -> do
            (rc, outmsg:: String, errmsg) <- io
            unless (null outmsg) $ print outmsg -- show step normal output, if any
            unless (null errmsg) $ hPutStrLn stderr errmsg -- show step error output, if any
            case rc of
                ExitSuccess -> runSteps ctxt tl -- step succeeded, continue execution
                _           -> return rc        -- step failed; stop execution

runBuild :: Builder -> IO ExitCode
runBuild (Builder _ steps) = runSteps Map.empty steps