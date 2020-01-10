{-# LANGUAGE MultiParamTypeClasses #-}

module Db where

import Common

type BuilderID = Int
type BuildID = Int

-- | The database's structure is documented at the repo's root
-- DEV.md file
class DbOperations where
    getBuilderID :: String -> BuilderID -- ^ Given a builder's name, get the primary key for it; creating the appropriate table if required
    startBuild :: BuilderID -> BuildID -- ^ Given a builder's ID, get a fresh ID for a starting build. Sets the start date.
    recordStep :: BuildID -> String -> String -> Status -> () -- ^ Records a step's execution
    endBuild :: BuildID -> Status -> () -- ^ End a build: records the end time and the status
