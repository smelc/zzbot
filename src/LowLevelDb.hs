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

import Common
import Text.Printf

import qualified Data.Text as Text

data BuilderField = BuilderField Int Text.Text
  deriving Show

instance FromRow BuilderField where
    fromRow = BuilderField <$> field <*> field

-- smelc: I have little experience with databases and hence wanna write
-- the queries myself, to get a better grasp of the database commands;
-- before moving on to a DSL for typesafe queries. Hence the use of sqlite-simple
-- for the moment

-- | The database's structure is documented at the repo's root DEV.md file
class Monad m => LowLevelDbOperations m where
    getBuilderID :: String -> m BuilderID -- ^ Given a builder's name, get the primary key for it
    startBuild :: BuilderID -> m BuildID -- ^ Given a builder's ID, get a fresh ID for a starting build. Sets the start date.
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
  execute_ connexion createBuilderTable
  execute_ connexion createBuildTable
  execute_ connexion createStepsTable
  return (Database connexion)
 where
  createBuilderTable :: Query = "CREATE TABLE IF NOT EXISTS builder (id INTEGER PRIMARY KEY, name TEXT)"
  createBuildTable :: Query = "CREATE TABLE IF NOT EXISTS build (id INTEGER PRIMARY KEY, builder_id INTEGER, start TEXT, end TEXT, status TEXT, FOREIGN KEY(builder_id) REFERENCES builder(id))"
  createStepsTable :: Query = "CREATE TABLE IF NOT EXISTS step (id INTEGER PRIMARY KEY, build_id INTEGER, description TEXT, stdout TEXT, stderr TEXT, status TEX, FOREIGN KEY(build_id) REFERENCES build(id))"

instance (MonadReader Database m, MonadIO m) => LowLevelDbOperations (UsingIOForDb m) where
    getBuilderID builderName = UsingIOForDb $ do
        Database connexion <- ask
        liftIO $ do
          rows :: [Only Int] <- queryNamed connexion "SELECT id FROM builder WHERE NAME=:name LIMIT 1" [":name" := builderName]
          case rows of
            (Only id : _) -> return id
            [] -> do
             execute connexion "INSERT INTO builder (name) VALUES (?)" (Only builderName)
             fromIntegral <$> lastInsertRowId connexion
    startBuild builderId = undefined
    recordStep = undefined
    endBuild = undefined