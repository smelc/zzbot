{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Db where

import Control.Monad.Except
import Data.List.Extra

import Common
import LowLevelDb

data BuildState = BuildState BuildID [Status]

snoc :: BuildState -> Status -> BuildState
snoc (BuildState buildID statuses) status =
   BuildState buildID $ Data.List.Extra.snoc statuses status

class Monad m => DbOperations m where
   startBuild :: String -> m BuildState -- ^ The string is the builder's name
   startStep :: BuildState -> String -> m StepID -- ^ The string is the step's description
   endStep :: BuildState -> StepID -> String -> String -> Status -> m BuildState -- ^ stdout, stderr, step status
   endBuild :: BuildState -> m Status -- ^ Returns the overall state of the build

newtype UsingLowLevelDb m a = UsingLowLevelDb { runUsingLowLevelDb :: m a }
 deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (UsingLowLevelDb m) where
  liftIO f = UsingLowLevelDb (liftIO f)

instance MonadError e m => MonadError e (UsingLowLevelDb m) where
  throwError e = UsingLowLevelDb (throwError e)
  catchError m h = UsingLowLevelDb $ catchError (runUsingLowLevelDb m) (runUsingLowLevelDb . h)

instance DbOperations m => DbOperations (ExceptT a m) where
   startBuild name = lift (Db.startBuild name)
   startStep state desc = lift (Db.startStep state desc)
   endStep state stepID stdout stderr status = lift (Db.endStep state stepID stdout stderr status)
   endBuild state = lift (Db.endBuild state)

instance LowLevelDbOperations m => DbOperations (UsingLowLevelDb m) where
    startBuild builderName = UsingLowLevelDb $ do
       buildID <- LowLevelDb.startBuild builderName
       return $ BuildState buildID []
    startStep buildState@(BuildState buildID _) desc = UsingLowLevelDb $
       LowLevelDb.startStep buildID desc
    endStep buildState@(BuildState buildID steps) stepID stdout stderr status = UsingLowLevelDb $ do
       LowLevelDb.endStep stepID stdout stderr status
       return $ Db.snoc buildState status
    endBuild buildState@(BuildState buildID statuses) = UsingLowLevelDb $ do
       let status = foldr max Success statuses
       LowLevelDb.endBuild buildID status
       return status