{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Db where

import Data.List.Extra

import Common
import LowLevelDb

data StepState = StepState String String Status -- ^ stdout, stderr, status
data BuildState = BuildState BuilderID BuildID [StepState]

snoc :: BuildState -> StepState -> BuildState
snoc (BuildState builderID buildID steps) stepState =
   BuildState builderID buildID $ Data.List.Extra.snoc steps stepState

class DbOperations m where
   startBuild :: String -> m BuildState -- ^ The string is the builder's name
   addStep :: BuildState -> String -> String -> Status -> m BuildState -- ^ stdout, stderr, step status
   endBuild :: BuildState -> m Status -- ^ Returns the overall state of the build

newtype UsingLowLevelDb m a = UsingLowLevelDb { unUsingLowLevelDb :: m a }
 deriving (Functor, Applicative, Monad)

instance LowLevelDbOperations m => DbOperations (UsingLowLevelDb m) where
    startBuild builderName = UsingLowLevelDb $ do
       ensureDB
       builderID <- getBuilderID builderName
       buildID <- LowLevelDb.startBuild builderID
       return $ BuildState builderID buildID []
    addStep buildState@(BuildState _ buildID _) stdout stderr status = UsingLowLevelDb $ do
       recordStep buildID stdout stderr status
       return $ Db.snoc buildState $ StepState stdout stderr status
    endBuild buildState@(BuildState _ buildID steps) = UsingLowLevelDb $ do
       let statuses = map (\(StepState _ _ s) -> s) steps
           status = foldr max Success statuses
       LowLevelDb.endBuild buildID status
       return status