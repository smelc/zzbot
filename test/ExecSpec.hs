{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module ExecSpec (spec) where

import Common hiding (Error)
import Config
import Control.Lens
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
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

runStubExec
  :: Eff (Exec ': effs) a
  -> Eff effs a
runStubExec = interpret $ \case
  ZzLog color entry -> return ()
  RunShellCommand workdir command ->
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

freshId
  :: Member (State FakeDb) effs
  => Eff effs Int
freshId = do
  modify (fakeDbCounter +~ 1)
  gets _fakeDbCounter

runDbOpsWithFakeDb
  :: FakeDb
  -> Eff (DbOperations ': effs) a
  -> Eff effs (a, FakeDb)
runDbOpsWithFakeDb db = runState db . dbToState

dbToState
  :: Eff (DbOperations ': effs)
  ~> Eff (State FakeDb ': effs)
dbToState = reinterpret $ \case
  (StartBuild name) -> do
    buildId <- freshId
    modify $ fakeDbBuilds . at buildId ?~ BuildEntry name Nothing
    return (BuildState buildId Success)
  (EndBuild (BuildState buildId status)) -> do
    modify $ fakeDbBuilds . ix buildId %~ updateBuildEntry status
    return status
  (StartStep (BuildState buildId _) step) -> do
    stepId <- freshId
    modify $ fakeDbSteps . at (stepId, buildId) ?~ StepEntry step Nothing Nothing
    return stepId
  (EndStep (BuildState buildId buildStatus) stepId streams status) -> do
    modify $ fakeDbSteps . ix (stepId, buildId) %~ updateStepEntry streams status
    return (BuildState buildId (max buildStatus status))
 where
  updateStepEntry streams status (StepEntry step _ _) =
    StepEntry step (Just streams) (Just status)
  updateBuildEntry status (BuildEntry name _) =
    BuildEntry name (Just status)

-- Tests

spec =
  describe "runBuild" $ do
    it "should log what it's doing" $
      (process Execute env testXml
         & runError
         & runExecAsDisplayLog
         & runStubDbOps
         & run)
      `shouldBe` expectedOutput
    it "should set the working directory as specified" $
      (process Execute env testXml
         & runError
         & runExecAsExecutionLog
         & runStubDbOps
         & run)
      `shouldBe` expectedTrace
    it "should write the expected information in the DB" $
      (process Execute env testXml
         & runError @ExitCode
         & runStubExec
         & runDbOpsWithFakeDb emptyFakeDb
         & run
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
