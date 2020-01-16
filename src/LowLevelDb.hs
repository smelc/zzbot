{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LowLevelDb where

import Control.Monad.Except
import Database.SQLite.Simple

import Common
import Data.Text
import Text.Printf

data BuilderField = BuilderField Int Text
  deriving Show

instance FromRow BuilderField where
    fromRow = BuilderField <$> field <*> field

-- smelc: I have little experience with databases and hence wanna write
-- the queries myself, to get a better grasp of the database commands;
-- before moving on to a DSL for typesafe queries. Hence the use of sqlite-simple
-- for the moment

-- | The database's structure is documented at the repo's root DEV.md file
class Monad m => LowLevelDbOperations m where
    ensureDB :: m () -- ^ Creates the database if missing. All other operations assume this has been called beforehand.
    getBuilderID :: String -> m BuilderID -- ^ Given a builder's name, get the primary key for it
    startBuild :: BuilderID -> m BuildID -- ^ Given a builder's ID, get a fresh ID for a starting build. Sets the start date.
    recordStep :: BuildID -> String -> String -> Status -> m () -- ^ Records a step's execution
    endBuild :: BuildID -> Status -> m () -- ^ End a build: records the end time and the status

instance LowLevelDbOperations IO where
    ensureDB = do
        connexion <- open dbFile
        execute_ connexion "PRAGMA journal_mode=WAL;"
        execute_ connexion "PRAGMA foreign_keys=ON;"
        execute_ connexion createBuilderTable
        execute_ connexion createBuildTable
        execute_ connexion createStepsTable
        close connexion
      where
        createBuilderTable :: Query = "CREATE TABLE IF NOT EXISTS builder (id INTEGER PRIMARY KEY, name TEXT)"
        createBuildTable :: Query = "CREATE TABLE IF NOT EXISTS build (id INTEGER PRIMARY KEY, builder_id INTEGER, start TEXT, end TEXT, status TEXT, FOREIGN KEY(builder_id) REFERENCES builder(id))"
        createStepsTable :: Query = "CREATE TABLE IF NOT EXISTS step (id INTEGER PRIMARY KEY, build_id INTEGER, description TEXT, stdout TEXT, stderr TEXT, status TEX, FOREIGN KEY(build_id) REFERENCES build(id))"
    getBuilderID builderName = do
        connexion <- open dbFile
        r :: [BuilderField] <- queryNamed connexion "SELECT * FROM builder WHERE NAME=:name" [":name" := builderName]
        -- TODO smelc Condition over r's being non empty
        -- TODO smelc get at most one result
        print $ show r
        close connexion
        return 0
    startBuild builderId = undefined
    recordStep = undefined
    endBuild = undefined