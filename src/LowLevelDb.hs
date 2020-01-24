{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module LowLevelDb (
  Database(),
  LowLevelDbOperations(..),
  startBuild,
  endBuild,
  startStep,
  endStep,
  runLowLevelDbOpsWithSQLite,
  withDatabase
) where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
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
data LowLevelDbOperations :: Type -> Type where
  StartBuild :: String -> LowLevelDbOperations BuildID
  EndBuild :: BuildID -> Status -> LowLevelDbOperations ()
  StartStep :: BuildID -> LBS.ByteString -> LowLevelDbOperations StepID
  EndStep :: StepID -> StepStreams -> Status -> LowLevelDbOperations ()

-- | Records start of build with given name, returns the new build's unique identifier
startBuild :: Member LowLevelDbOperations effs => String -> Eff effs BuildID
startBuild name = send (StartBuild name)

-- | End a build: records the end time and the status
endBuild :: Member LowLevelDbOperations effs => BuildID -> Status -> Eff effs ()
endBuild buildId status = send (EndBuild buildId status)

-- | Records the start of a step, requires its description, returns its unique identifier
startStep :: Member LowLevelDbOperations effs => BuildID -> LBS.ByteString -> Eff effs StepID
startStep buildId desc = send (StartStep buildId desc)

-- | Records the end of a step, requires its identifier, streams outputs, and its status
endStep :: Member LowLevelDbOperations effs => StepID -> StepStreams -> Status -> Eff effs ()
endStep stepId streams status = send (EndStep stepId streams status)

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
  :: (Member (Reader Database) effs, LastMember IO effs)
  => (Connection -> IO a)
  -> Eff effs a
withReadConnection action = do
  Database lock conn <- ask
  sendM (RWL.withRead lock (action conn))

withWriteConnection
  :: (Member (Reader Database) effs, LastMember IO effs)
  => (Connection -> IO a)
  -> Eff effs a
withWriteConnection action = do
  Database lock conn <- ask
  sendM (RWL.withWrite lock (action conn))

runLowLevelDbOpsWithSQLite
  :: (Member (Reader Database) effs, LastMember IO effs)
  => Eff (LowLevelDbOperations ': effs)
  ~> Eff effs
runLowLevelDbOpsWithSQLite = interpret dbToIO

dbToIO
  :: (Member (Reader Database) effs, LastMember IO effs)
  => LowLevelDbOperations
  ~> Eff effs
dbToIO (StartBuild builderName) =
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
dbToIO (StartStep buildID desc) =
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
dbToIO (EndStep stepID streams status) =
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
dbToIO (EndBuild buildID status) =
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