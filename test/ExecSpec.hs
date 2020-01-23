{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ExecSpec (spec) where

import Common (Status(Success))
import Config
import Control.Monad.Writer
import Control.Monad.Except
import Db
import Data.Text.Prettyprint.Doc.Render.Terminal
import Exec
import System.Exit
import Test.Hspec

-- Mock behavior for shell commands, used by both LoggingMockExec and
-- TracingMockExec

mockShellCommand (Command "ls" ["a"]) = (ExitSuccess, "foo bar", "")
mockShellCommand (Command "ls" ["b"]) = (ExitSuccess, "bar baz", "")
mockShellCommand _ = (ExitFailure 127, "", "command not found")

-- Logging mock exec

data LogEntry
  = Message LogLevel String
  | StdOut   String
  | StdErr   String
  deriving (Eq, Show)

newtype LoggingMockExec a = LoggingMockExec (Writer [LogEntry] a)
  deriving (Functor, Applicative, Monad, MonadWriter [LogEntry])

runLoggingMockExec :: LoggingMockExec a -> (a, [LogEntry])
runLoggingMockExec (LoggingMockExec m) = runWriter m

instance MonadExec LoggingMockExec where
  zzLog level entry = tell [Message level entry]
  runShellCommand _ cmd = return (mockShellCommand cmd)
  putOut   str = tell [StdOut str]
  putErr   str = tell [StdErr str]

instance DbOperations LoggingMockExec where
   startBuild name = return (BuildState 0 [])
   startStep state desc = return 0 -- FIXME smelc
   endStep state stepID streams status = return state -- FIXME smelc
   endBuild state = return Success

-- Tracing mock exec

data Execution = Execution String Command
  deriving (Eq, Show)

newtype TracingMockExec a = TracingMockExec (Writer [Execution] a)
  deriving (Functor, Applicative, Monad, MonadWriter [Execution])

runTracingMockExec :: TracingMockExec a -> (a, [Execution])
runTracingMockExec (TracingMockExec m) = runWriter m

instance MonadExec TracingMockExec where
  zzLog color entry = return ()
  runShellCommand workdir command = do
    tell [Execution workdir command]
    return (mockShellCommand command)
  putOut   str = return ()
  putErr   str = return ()

instance DbOperations TracingMockExec where
   startBuild name = return (BuildState 0 [])
   startStep state desc = return 0 -- FIXME smelc
   endStep state stepID streams status = return state -- FIXME smelc
   endBuild state = return Success

-- Tests

spec =
  describe "runBuild" $ do
    it "should log what it's doing" $
      runLoggingMockExec (runExceptT (process Execute env testXml)) `shouldBe` expectedOutput
    it "should set the working directory as specified" $
      runTracingMockExec (runExceptT (process Execute env testXml)) `shouldBe` expectedTrace
  where
    env = ProcessEnv "/test/workdir" [("ENV_VAR", "a")]
    testXml =
      "<config>\
      \  <builder name=\"test\" workdir=\"dir1\">\
      \    <shell command=\"ls ${ENV_VAR}\"/>\
      \    <shell workdir=\"dir2\" command=\"ls b\"/>\
      \    <shell workdir=\"/absolute/dir\" command=\"ls b\"/>\
      \    <shell command=\"some junk 1\" haltOnFailure=\"False\"/>\
      \    <shell command=\"some junk 2\"/>\
      \    <shell command=\"some junk 3\"/>\
      \  </builder>\
      \</config>"
    expectedOutput =
      ( Left (ExitFailure 3)
      , [ Message Info "ls a"
        , StdOut "foo bar"
        , Message Info "ls b"
        , StdOut "bar baz"
        , Message Info "ls b"
        , StdOut "bar baz"
        , Message Info "some junk 1"
        , StdErr "command not found"
        , Message Error "some junk 1 failed: ExitFailure 127"
        , Message Info "some junk 2"
        , StdErr "command not found"
        , Message Error "some junk 2 failed: ExitFailure 127"
        ]
      )
    expectedTrace =
      ( Left (ExitFailure 3)
      , [ Execution "/test/workdir/dir1" (Command "ls" ["a"])
        , Execution "/test/workdir/dir1/dir2" (Command "ls" ["b"])
        , Execution "/absolute/dir" (Command "ls" ["b"])
        , Execution "/test/workdir/dir1" (Command "some" ["junk", "1"])
        , Execution "/test/workdir/dir1" (Command "some" ["junk", "2"])
        ]
      )
