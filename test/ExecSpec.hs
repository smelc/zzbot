{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module ExecSpec (spec) where

import Common (Status(Success))
import Config
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Writer
import Data.Function
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

runExecAsDisplayLog
  :: Eff (Exec ': effs) a
  -> Eff effs (a, [LogEntry])
runExecAsDisplayLog = runWriter . execToWriter
 where
  execToWriter :: Eff (Exec ': effs) ~> Eff (Writer [LogEntry] ': effs)
  execToWriter = reinterpret $ \case
    ZzLog level entry -> tell [Message level entry]
    RunShellCommand _ cmd -> return (mockShellCommand cmd)
    PutOut str -> tell [StdOut str]
    PutErr str -> tell [StdErr str]

data Execution = Execution String Command
  deriving (Eq, Show)

runExecAsExecutionLog
  :: Eff (Exec ': effs) a
  -> Eff effs (a, [Execution])
runExecAsExecutionLog = runWriter . execToWriter
 where
  execToWriter :: Eff (Exec ': effs) ~> Eff (Writer [Execution] ': effs)
  execToWriter = reinterpret $ \case
    ZzLog color entry -> return ()
    RunShellCommand workdir command -> do
      tell [Execution workdir command]
      return (mockShellCommand command)
    PutOut str -> return ()
    PutErr str -> return ()

runStubDbOps
  :: Eff (DbOperations ': effs)
  ~> Eff effs
runStubDbOps = interpret $ \case
   StartBuild name -> return (BuildState 0 Success)
   StartStep state desc -> return 0
   EndStep state stepID streams status -> return state
   EndBuild state -> return Success

-- Tests

spec =
  describe "runBuild" $ do
    it "should log what it's doing" $
      (process Execute env testXml & runError & runExecAsDisplayLog & runStubDbOps & run)
        `shouldBe` expectedOutput
    it "should set the working directory as specified" $
      (process Execute env testXml & runError & runExecAsExecutionLog & runStubDbOps & run)
        `shouldBe` expectedTrace
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
      , [ Message LogLevelInfo "ls a"
        , StdOut "foo bar"
        , Message LogLevelInfo "ls b"
        , StdOut "bar baz"
        , Message LogLevelInfo "ls b"
        , StdOut "bar baz"
        , Message LogLevelInfo "some junk 1"
        , StdErr "command not found"
        , Message LogLevelError "some junk 1 failed: ExitFailure 127"
        , Message LogLevelInfo "some junk 2"
        , StdErr "command not found"
        , Message LogLevelError "some junk 2 failed: ExitFailure 127"
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