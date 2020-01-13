{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db where

import Database.SQLite.Simple

import Common
import Data.Text
import Text.Printf

type BuilderID = Int
type BuildID = Int

-- smelc: I have little experience with databases and hence wanna write
-- the queries myself, to get a better grasp of the database commands;
-- before moving on to a DSL for typesafe queries. Hence the use of sqlite-simple
-- for the moment

-- | The database's structure is documented at the repo's root
-- DEV.md file
class DbOperations m where
    ensureDB :: m () -- ^ Creates the database if missing
    getBuilderID :: String -> BuilderID -> m () -- ^ Given a builder's name, get the primary key for it; creating the appropriate table if required
    startBuild :: BuilderID -> BuildID -> m() -- ^ Given a builder's ID, get a fresh ID for a starting build. Sets the start date.
    recordStep :: BuildID -> String -> String -> Status -> m () -- ^ Records a step's execution
    endBuild :: BuildID -> Status -> m () -- ^ End a build: records the end time and the status

tableNameBuilder :: String = "builder"
tableNameBuild = "build"
tableNameStep = "step"

tableBuilderName :: String = "name"

instance DbOperations IO where
    ensureDB = do
        connexion <- open dbFile
        execute_ connexion createBuilderTable
        close connexion
      where
        createBuilderTable :: Query = "CREATE TABLE IF NOT EXISTS builder (id INTEGER PRIMARY KEY, str name)"
    getBuilderID = undefined
    startBuild = undefined
    recordStep = undefined
    endBuild = undefined
