{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module LowLevelDb (
  Database(),
  LowLevelDbOperations(..),
  LowLevelDbOperationsIOT,
  runLowLevelDbOpsWithSQLite,
  withDatabase
) where

import Control.Effect
import Control.Effect.Reader
import Control.Monad.IO.Class
import Database.SQLite.Simple
import Data.Maybe
import Data.Kind

import Common
import Text.Printf

import qualified Data.ByteString.Lazy as LBS
import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Data.Text as Text

-- smelc: I have little experience with databases and hence wanna write
-- the queries myself, to get a better grasp of the database commands;
-- before moving on to a DSL for typesafe queries. Hence the use of sqlite-simple
-- for the moment

-- | The database's structure is documented at the repo's root DEV.md file
class Monad m => LowLevelDbOperations m where
  -- | Records start of build with given name, returns the new build's unique identifier
  startBuild :: String -> m BuildID
  -- | End a build: records the end time and the status
  endBuild :: BuildID -> Status -> m ()
  -- | Records the start of a step, requires its description, returns its unique identifier
  startStep :: BuildID -> LBS.ByteString -> m StepID
  -- | Records the end of a step, requires its identifier, streams outputs, and its status
  endStep :: StepID -> StepStreams -> Status -> m ()

instance (Monad (t m), Send LowLevelDbOperations t m) => LowLevelDbOperations (EffT t m) where
  startBuild name = send @LowLevelDbOperations (startBuild name)
  endBuild buildId status = send @LowLevelDbOperations (endBuild buildId status)
  startStep buildId desc = send @LowLevelDbOperations (startStep buildId desc)
  endStep stepId streams status = send @LowLevelDbOperations (endStep stepId streams status)

data LowLevelDbOperationsIO
type LowLevelDbOperationsIOT = HandlerT LowLevelDbOperationsIO '[ReaderT Database]
type instance Handles LowLevelDbOperationsIOT eff = eff == LowLevelDbOperations

data Database = Database RWL.RWLock Connection

withDatabase :: FilePath -> (Database -> IO a) -> IO a
withDatabase filepath action =
  withConnection filepath $ \connexion -> do
    execute_ connexion "PRAGMA journal_mode=WAL;"
    execute_ connexion "PRAGMA foreign_keys=ON;"
    execute_ connexion createBuildTable
    execute_ connexion createStepsTable
    lock <- RWL.new
    action (Database lock connexion)
 where
  createBuildTable :: Query =
    "CREATE TABLE IF NOT EXISTS build\
    \  ( id INTEGER PRIMARY KEY NOT NULL\
    \  , builder TEXT NOT NULL\
    \  , start TEXT NOT NULL\
    \  , end TEXT\
    \  , status TEXT\
    \  )"
  createStepsTable :: Query =
    "CREATE TABLE IF NOT EXISTS step\
    \  ( id INTEGER PRIMARY KEY NOT NULL\
    \  , build_id INTEGER NOT NULL\
    \  , description TEXT NOT NULL\
    \  , start TEXT NOT NULL\
    \  , end TEXT\
    \  , stdout TEXT\
    \  , stderr TEXT\
    \  , status TEXT\
    \  , FOREIGN KEY(build_id) REFERENCES build(id)\
    \  )"

withReadConnection
  :: Reader Database m
  => MonadIO m
  => (Connection -> IO a)
  -> m a
withReadConnection action = do
  Database lock conn <- ask
  liftIO (RWL.withRead lock (action conn))

withWriteConnection
  :: Reader Database m
  => MonadIO m
  => (Connection -> IO a)
  -> m a
withWriteConnection action = do
  Database lock conn <- ask
  liftIO (RWL.withWrite lock (action conn))

runLowLevelDbOpsWithSQLite
  :: MonadIO m
  => Database
  -> EffT LowLevelDbOperationsIOT m a
  -> m a
runLowLevelDbOpsWithSQLite db = runReader db . runHandlerT . runEffT

instance MonadIO m => LowLevelDbOperations (LowLevelDbOperationsIOT m) where
    startBuild builderName = HandlerT $
      withWriteConnection $ \conn -> do
        executeNamed conn query args
        fromIntegral <$> lastInsertRowId conn
     where
      query =
        "INSERT\
        \  INTO build (builder, start)\
        \  VALUES (:builder, STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'))"
      args =
        [ ":builder" := builderName ]

    startStep buildID desc = HandlerT $
      withWriteConnection $ \conn -> do
        executeNamed conn query args
        fromIntegral <$> lastInsertRowId conn
     where
      query :: Query =
        "INSERT\
        \  INTO step (build_id, description, start)\
        \  VALUES (:build_id, :description, STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'))"
      args =
        [ ":build_id" := buildID
        , ":description" := desc
        ]

    endStep stepID streams status = HandlerT $
      withWriteConnection $ \conn ->
        executeNamed conn query args
     where
      query :: Query =
        "UPDATE step SET\
        \  end = STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'),\
        \  stdout = :stdout, \
        \  stderr = :stderr, \
        \  status = :status \
        \WHERE id = :id"
      args =
        [ ":stdout" := streamToValue (stdout streams)
        , ":stderr" := streamToValue (stderr streams)
        , ":status" := show status
        , ":id" := stepID
        ]
      streamToValue = fromMaybe "NULL"

    endBuild buildID status = HandlerT $
      withWriteConnection $ \conn ->
        executeNamed conn query args
     where
      query =
        "UPDATE build SET\
        \  end = STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'),\
        \  status = :status \
        \WHERE id = :id"
      args =
        [ ":status" := show status
        , ":id" := buildID
        ]


