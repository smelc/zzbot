{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Db where

import Control.Monad.Except
import Data.List.Extra

import Common
import LowLevelDb

data StepState = StepState String String Status -- ^ stdout, stderr, status
data BuildState = BuildState BuildID [StepState]

snoc :: BuildState -> StepState -> BuildState
snoc (BuildState buildID steps) stepState =
   BuildState buildID $ Data.List.Extra.snoc steps stepState

class Monad m => DbOperations m where
   startBuild :: String -> m BuildState -- ^ The string is the builder's name
   addStep :: BuildState -> String -> String -> Status -> m BuildState -- ^ stdout, stderr, step status
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
   addStep state stdout stderr status = lift (addStep state stdout stderr status)
   endBuild state = lift (Db.endBuild state)

instance LowLevelDbOperations m => DbOperations (UsingLowLevelDb m) where
    startBuild builderName = UsingLowLevelDb $ do
       buildID <- LowLevelDb.startBuild builderName
       return $ BuildState buildID []
    addStep buildState@(BuildState buildID _) stdout stderr status = UsingLowLevelDb $ do
       recordStep buildID stdout stderr status
       return $ Db.snoc buildState $ StepState stdout stderr status
    endBuild buildState@(BuildState buildID steps) = UsingLowLevelDb $ do
       let statuses = map (\(StepState _ _ s) -> s) steps
           status = foldr max Success statuses
       LowLevelDb.endBuild buildID status
       return status