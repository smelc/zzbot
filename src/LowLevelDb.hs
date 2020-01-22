{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LowLevelDb where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Database.SQLite.Simple
import Debug.Trace

import Common
import Text.Printf

import qualified Data.Text as Text

-- smelc: I have little experience with databases and hence wanna write
-- the queries myself, to get a better grasp of the database commands;
-- before moving on to a DSL for typesafe queries. Hence the use of sqlite-simple
-- for the moment

-- | The database's structure is documented at the repo's root DEV.md file
class Monad m => LowLevelDbOperations m where
    startBuild :: String -> m BuildID -- ^ Records start of build with given name, returning the new build's ID.
    recordStep :: BuildID -> String -> String -> Status -> m () -- ^ Records a step's execution
    endBuild :: BuildID -> Status -> m () -- ^ End a build: records the end time and the status

newtype UsingIOForDb m a = UsingIOForDb { runUsingIOForDb :: m a }
 deriving (Functor, Applicative, Monad)

instance MonadError e m => MonadError e (UsingIOForDb m) where
  throwError e = UsingIOForDb (throwError e)
  catchError m h = UsingIOForDb $ catchError (runUsingIOForDb m) (runUsingIOForDb . h)

instance MonadIO m => MonadIO (UsingIOForDb m) where
  liftIO f = UsingIOForDb (liftIO f)

-- For now just an alias for a Connection.
-- Can be extended with a mutex for concurrent writes.
newtype Database = Database Connection

createDatabase :: IO Database
createDatabase = do
  connexion <- open dbFile
  execute_ connexion "PRAGMA journal_mode=WAL;"
  execute_ connexion "PRAGMA foreign_keys=ON;"
  execute_ connexion createBuildTable
  execute_ connexion createStepsTable
  return (Database connexion)
 where
  createBuildTable :: Query = "CREATE TABLE IF NOT EXISTS build (id INTEGER PRIMARY KEY NOT NULL, builder TEXT NOT NULL, start TEXT NOT NULL, end TEXT, status TEXT)"
  createStepsTable :: Query = "CREATE TABLE IF NOT EXISTS step (id INTEGER PRIMARY KEY NOT NULL, build_id TEXT NOT NULL, description TEXT NOT NULL, stdout TEXT NOT NULL, stderr TEXT NOT NULL, status TEXT NOT NULL, FOREIGN KEY(build_id) REFERENCES build(builder))"

instance (MonadReader Database m, MonadIO m) => LowLevelDbOperations (UsingIOForDb m) where
    startBuild builderName = UsingIOForDb $ do
      Database connexion <- ask
      liftIO $ do
        execute connexion "INSERT INTO build (builder, start) VALUES (?, STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'))" $ Only builderName
        fromIntegral <$> lastInsertRowId connexion
    recordStep = undefined
    endBuild buildID status = UsingIOForDb $ do
      Database connexion <- ask
      liftIO $ do
        let argsList = [":status" := show status, ":id" := buildID]
        -- TODO smelc: this makes sqlite crash with "SQLite3 returned ErrorIO while attempting to perform step: disk I/O error"
        -- executeNamed connexion "UPDATE build SET end = STRFTIME('%Y-%m-%d %H:%M:%f', 'NOW'), status = :status WHERE id = :id " argsList
        -- close connexion
        return ()