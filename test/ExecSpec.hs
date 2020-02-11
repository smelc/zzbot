{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module ExecSpec (spec) where

import Common hiding (ErrorLevel)
import Config
import Control.Effect
import Control.Effect.State
import Control.Effect.Writer
import Control.Lens
import Data.Function
import Db
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Tuple
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

data MonadExecDisplayLog
type MonadExecDisplayLogT = HandlerT MonadExecDisplayLog '[WriterT [LogEntry]]
type instance Handles MonadExecDisplayLogT eff = eff == MonadExec

runExecAsDisplayLog
  :: Monad m
  => EffT MonadExecDisplayLogT m a
  -> m ([LogEntry], a)
runExecAsDisplayLog = runWriter . runHandlerT . runEffT

instance Monad m => MonadExec (MonadExecDisplayLogT m) where
  zzLog level entry = HandlerT $ tell [Message level entry]
  runShellCommand _ cmd = HandlerT $ return (mockShellCommand cmd)
  putOut str = HandlerT $ tell [StdOut str]
  putErr str = HandlerT $ tell [StdErr str]

data Execution = Execution String Command
  deriving (Eq, Show)

data MonadExecExecutionLog
type MonadExecExecutionLogT = HandlerT MonadExecExecutionLog '[WriterT [Execution]]
type instance Handles MonadExecExecutionLogT eff = eff == MonadExec

runExecAsExecutionLog
  :: Monad m
  => EffT MonadExecExecutionLogT m a
  -> m ([Execution], a)
runExecAsExecutionLog = runWriter . runHandlerT . runEffT

instance Monad m => MonadExec (MonadExecExecutionLogT m) where
  zzLog level entry = HandlerT $ return ()
  runShellCommand workdir cmd = HandlerT $ do
    tell [Execution workdir cmd]
    return (mockShellCommand cmd)
  putOut str = HandlerT $ return ()
  putErr str = HandlerT $ return ()

data MonadExecStub
type MonadExecStubT = HandlerT MonadExecStub '[]
type instance Handles MonadExecStubT eff = eff == MonadExec

runStubExec
  :: EffT MonadExecStubT m a
  -> m a
runStubExec = runHandlerT . runEffT

instance Monad m => MonadExec (MonadExecStubT m) where
  zzLog level entry = HandlerT $ return ()
  runShellCommand _ cmd = HandlerT $ return (mockShellCommand cmd)
  putOut str = HandlerT $ return ()
  putErr str = HandlerT $ return ()

data DbOperationsStub
type DbOperationsStubT = HandlerT DbOperationsStub '[]
type instance Handles DbOperationsStubT eff = eff == DbOperations

runStubDbOps
  :: EffT DbOperationsStubT m a
  -> m a
runStubDbOps = runHandlerT . runEffT

instance Monad m => DbOperations (DbOperationsStubT m) where
   startBuild name = HandlerT $ return 0
   startStep state desc = HandlerT $ return 0
   endStep state stepID streams status = HandlerT $ return ()
   endBuild buildId status = HandlerT $ return ()

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
  :: State FakeDb m
  => m Int
freshId = do
  modify (fakeDbCounter +~ 1)
  _fakeDbCounter <$> get

data DbOperationsFakeDb
type DbOperationsFakeDbT = HandlerT DbOperationsFakeDb '[StateT FakeDb]
type instance Handles DbOperationsFakeDbT eff = eff == DbOperations

runDbOpsWithFakeDb
  :: Monad m
  => FakeDb
  -> EffT DbOperationsFakeDbT m a
  -> m (FakeDb, a)
runDbOpsWithFakeDb db = runState db . runHandlerT . runEffT

instance Monad m => DbOperations (DbOperationsFakeDbT m) where
  startBuild name = HandlerT $ do
    buildId <- freshId
    modify (fakeDbBuilds . at buildId ?~ BuildEntry name Nothing)
    return buildId
  endBuild buildId status = HandlerT $
    modify (fakeDbBuilds . ix buildId %~ updateBuildEntry status)
   where
    updateBuildEntry status (BuildEntry name _) =
      BuildEntry name (Just status)
  startStep buildId step = HandlerT $ do
    stepId <- freshId
    modify (fakeDbSteps . at (stepId, buildId) ?~ StepEntry step Nothing Nothing)
    return stepId
  endStep buildId stepId streams status = HandlerT $
    modify (fakeDbSteps . ix (stepId, buildId) %~ updateStepEntry streams status)
   where
    updateStepEntry streams status (StepEntry step _ _) =
      StepEntry step (Just streams) (Just status)

-- Tests

spec =
  describe "runBuild" $ do
    it "should log what it's doing" $
      (process Execute env testXml
         & runExecAsDisplayLog
         & runStubDbOps
         & runIdentity
         & swap)
      `shouldBe` expectedOutput
    it "should set the working directory as specified" $
      (process Execute env testXml
         & runExecAsExecutionLog
         & runStubDbOps
         & runIdentity
         & swap)
      `shouldBe` expectedTrace
    it "should write the expected information in the DB" $
      (process Execute env testXml
         & runStubExec
         & runDbOpsWithFakeDb emptyFakeDb
         & runIdentity
         & fst -- we don't care for the return value, we just want the db
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
      \    <shell command=\"some junk 2\" ignoreFailure=\"True\"/>\
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
                      , ignoreFailure = False
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
                      , ignoreFailure = False
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
                      , ignoreFailure = False
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
                      , ignoreFailure = False
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
                      , ignoreFailure = True
                      }
                  , stepEntryStreams =
                      Just (StepStreams (Just "") (Just "command not found"))
                  , stepEntryStatus = Just Success
                  }
              ]
          )
        ]
