{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ExecSpec (spec) where

import Common hiding (ErrorLevel)
import Config
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer
import Data.Function
import Db
import Data.Text.Prettyprint.Doc.Render.Terminal
import Exec
import System.Exit
import Test.Hspec

import qualified Data.Map as M
import qualified Data.Set as S

-- Mock behavior for shell commands, used by both LoggingMockExec and
-- TracingMockExec
mockShellCommand (Command "ls a") = (ExitSuccess, "foo bar", "")
mockShellCommand (Command "ls b") = (ExitSuccess, "bar baz", "")
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

instance MonadExec () LoggingMockExec where
  zzLog level entry = tell [Message level entry]
  runShellCommand _ cmd = return (mockShellCommand cmd)
  putOut str = tell [StdOut str]
  putErr str = tell [StdErr str]

instance DbOperations () LoggingMockExec where
   startBuild name = return (BuildState 0 Success)
   startStep state desc = return 0 -- FIXME smelc, makes the tests fail!
   endStep state stepID streams status = return state -- FIXME smelc, makes the tests fail!
   endBuild state = return Success

-- Tracing mock exec

data Execution = Execution String Command
  deriving (Eq, Show)

newtype TracingMockExec a = TracingMockExec (Writer [Execution] a)
  deriving (Functor, Applicative, Monad, MonadWriter [Execution])

runTracingMockExec :: TracingMockExec a -> (a, [Execution])
runTracingMockExec (TracingMockExec m) = runWriter m

instance MonadExec () TracingMockExec where
  zzLog color entry = return ()
  runShellCommand workdir command = do
    tell [Execution workdir command]
    return (mockShellCommand command)
  putOut str = return ()
  putErr str = return ()

instance DbOperations () TracingMockExec where
   startBuild name = return (BuildState 0 Success)
   startStep state desc = return 0
   endStep state stepID streams status = return state
   endBuild state = return Success

-- FakeDb mock exec
data BuildEntry = BuildEntry
  { buildEntryName :: String
  , buildEntryStatus :: Maybe Status
  }
  deriving (Eq, Ord, Show)

data StepEntry = StepEntry
  { stepEntryStep :: Step Substituted
  , stepEntryStreams :: Maybe StepStreams
  , stepEntryStatus :: Maybe Status
  }
  deriving (Eq, Ord, Show)

data FakeDb = FakeDb
  { _fakeDbCounter :: Int
  , _fakeDbBuilds :: M.Map BuildID BuildEntry
  , _fakeDbSteps :: M.Map (StepID, BuildID) StepEntry
  }
  deriving (Eq, Ord, Show)

makeLenses ''FakeDb

emptyFakeDb = FakeDb 0 mempty mempty

type NormalizedFakeDb = (M.Map BuildEntry (S.Set StepEntry))

normalizeFakeDb :: FakeDb -> NormalizedFakeDb
normalizeFakeDb (FakeDb _ builds steps) =
  M.fromList [(build, stepsFor id) | (id, build) <- M.toList builds]
 where
  stepsFor parentId = S.fromList
    [step | ((_, buildId), step) <- M.toList steps, buildId == parentId]

newtype FakeDbMockExec a = FakeDbMockExec (State FakeDb a)
  deriving (Functor, Applicative, Monad, MonadState FakeDb)

runFakeDbMockExec :: FakeDbMockExec a -> FakeDb -> (a, FakeDb)
runFakeDbMockExec (FakeDbMockExec m) = runState m

freshId :: FakeDbMockExec Int
freshId = do
  fakeDbCounter += 1
  gets _fakeDbCounter

instance MonadExec () FakeDbMockExec where
  zzLog color entry = return ()
  runShellCommand workdir command = return (mockShellCommand command)
  putOut str = return ()
  putErr str = return ()

instance DbOperations () FakeDbMockExec where
  startBuild name = do
    buildId <- freshId
    fakeDbBuilds . at buildId ?= BuildEntry name Nothing
    return (BuildState buildId Success)
  endBuild (BuildState buildId status) = do
    fakeDbBuilds . ix buildId %= updateBuildEntry status
    return status
   where
    updateBuildEntry status (BuildEntry name _) =
      BuildEntry name (Just status)
  startStep (BuildState buildId _) step = do
    stepId <- freshId
    fakeDbSteps . at (stepId, buildId) ?= StepEntry step Nothing Nothing
    return stepId
  endStep (BuildState buildId buildStatus) stepId streams status = do
    fakeDbSteps . ix (stepId, buildId) %= updateStepEntry streams status
    return (BuildState buildId (max buildStatus status))
   where
    updateStepEntry streams status (StepEntry step _ _) =
      StepEntry step (Just streams) (Just status)

-- Tests

spec =
  describe "runBuild" $ do
    it "should log what it's doing" $
      runLoggingMockExec (process @() @() Execute env testXml)
        `shouldBe` expectedOutput
    it "should set the working directory as specified" $
      runTracingMockExec (process @() @() Execute env testXml)
        `shouldBe` expectedTrace
    it "should write the expected information in the DB" $
      (process @() @() Execute env testXml
        & flip runFakeDbMockExec emptyFakeDb
        & snd -- we don't care for the return value, we just want the db
        & normalizeFakeDb)
      `shouldBe` expectedDb
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
      ( Failure
      , [ Message InfoLevel "ls a"
        , StdOut "foo bar"
        , Message InfoLevel "ls b"
        , StdOut "bar baz"
        , Message InfoLevel "ls b"
        , StdOut "bar baz"
        , Message InfoLevel "some junk 1"
        , StdErr "command not found"
        , Message ErrorLevel "some junk 1 failed: ExitFailure 127"
        , Message InfoLevel "some junk 2"
        , StdErr "command not found"
        , Message ErrorLevel "some junk 2 failed: ExitFailure 127"
        ]
      )
    expectedTrace =
      ( Failure
      , [ Execution "/test/workdir/dir1" (Command "ls a")
        , Execution "/test/workdir/dir1/dir2" (Command "ls b")
        , Execution "/absolute/dir" (Command "ls b")
        , Execution "/test/workdir/dir1" (Command "some junk 1")
        , Execution "/test/workdir/dir1" (Command "some junk 2")
        ]
      )
    expectedDb =
      M.fromList
        [ ( BuildEntry
              { buildEntryName = "test"
              , buildEntryStatus = Just Failure
              }
          , S.fromList
              [ StepEntry
                  { stepEntryStep = ShellCmd
                      { workdir = "/test/workdir/dir1"
                      , cmd = Command "ls a"
                      , mprop = Nothing
                      , haltOnFailure = True
                      }
                  , stepEntryStreams =
                      Just (StepStreams (Just "foo bar") (Just ""))
                  , stepEntryStatus = Just Success
                  }
              , StepEntry
                  { stepEntryStep = ShellCmd
                      { workdir = "/test/workdir/dir1/dir2"
                      , cmd = Command "ls b"
                      , mprop = Nothing
                      , haltOnFailure = True
                      }
                  , stepEntryStreams =
                      Just (StepStreams (Just "bar baz") (Just ""))
                  , stepEntryStatus = Just Success
                  }
              , StepEntry
                  { stepEntryStep = ShellCmd
                      { workdir = "/absolute/dir"
                      , cmd = Command "ls b"
                      , mprop = Nothing
                      , haltOnFailure = True
                      }
                  , stepEntryStreams =
                      Just (StepStreams (Just "bar baz") (Just ""))
                  , stepEntryStatus = Just Success
                  }
              , StepEntry
                  { stepEntryStep = ShellCmd
                      { workdir = "/test/workdir/dir1"
                      , cmd = Command "some junk 1"
                      , mprop = Nothing
                      , haltOnFailure = False
                      }
                  , stepEntryStreams =
                      Just (StepStreams (Just "") (Just "command not found"))
                  , stepEntryStatus = Just Failure
                  }
              , StepEntry
                  { stepEntryStep = ShellCmd
                      { workdir = "/test/workdir/dir1"
                      , cmd = Command "some junk 2"
                      , mprop = Nothing
                      , haltOnFailure = True
                      }
                  , stepEntryStreams =
                      Just (StepStreams (Just "") (Just "command not found"))
                  , stepEntryStatus = Just Failure
                  }
              ]
          )
        ]
