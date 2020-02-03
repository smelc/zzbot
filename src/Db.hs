{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Db (
  DbOperations(..),
  DbOperationsUsingLowLevelDbT,
  BuildState(..), -- visible for testing
  interpretDbOpsAsLowLevelDbOps,
  withMaxStatus
) where

import Control.Effect
import Data.Kind
import qualified Data.Aeson as J

import Common
import Config
import qualified LowLevelDb as Low

-- | The build identifier and its status (so far)
data BuildState = BuildState BuildID Status

withMaxStatus :: BuildState -> Status -> BuildState
withMaxStatus (BuildState buildID s1) s2 =
   BuildState buildID (max s1 s2)

class Monad m => DbOperations m where
   startBuild :: String -> m BuildState -- ^ The string is the builder's name
   startStep :: BuildState -> Step Substituted -> m StepID -- ^ The string is the step's description
   endStep :: BuildState -> StepID -> StepStreams -> Status -> m BuildState -- ^ stdout, stderr, step status
   endBuild :: BuildState -> m Status -- ^ Returns the overall state of the build

instance (Monad (t m), Send DbOperations t m) => DbOperations (EffT t m) where
  startBuild name = send @DbOperations (startBuild name)
  startStep state desc = send @DbOperations (startStep state desc)
  endStep state stepID streams status = send @DbOperations (endStep state stepID streams status)
  endBuild state = send @DbOperations (endBuild state)

data DbOperationsUsingLowLevelDb
type DbOperationsUsingLowLevelDbT = HandlerT DbOperationsUsingLowLevelDb '[]
type instance Handles DbOperationsUsingLowLevelDbT eff = eff == DbOperations

instance Low.LowLevelDbOperations m => DbOperations (DbOperationsUsingLowLevelDbT m) where
  startBuild builderName = HandlerT $ do
    buildID <- Low.startBuild builderName
    return $ BuildState buildID Success
  startStep buildState@(BuildState buildID _) step = HandlerT $
    Low.startStep buildID (J.encode step)
  endStep (BuildState buildId buildStatus) stepID streams status = HandlerT $ do
    Low.endStep stepID streams status
    return $ BuildState buildId (max buildStatus status)
  endBuild buildState@(BuildState buildID status) = HandlerT $ do
    Low.endBuild buildID status
    return status

interpretDbOpsAsLowLevelDbOps
  :: EffT DbOperationsUsingLowLevelDbT m a
  -> m a
interpretDbOpsAsLowLevelDbOps = runHandlerT . runEffT