{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Db (
  DbOperations(..),
  BuildState(..), -- visible for testing
  interpretDbOpsAsLowLevelDbOps,
  startBuild,
  endBuild,
  startStep,
  endStep,
  withMaxStatus
) where

import Control.Monad.Freer
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

data DbOperations :: Type -> Type where
   StartBuild :: String -> DbOperations BuildState -- ^ The string is the builder's name
   StartStep :: BuildState -> Step Substituted -> DbOperations StepID -- ^ The string is the step's description
   EndStep :: BuildState -> StepID -> StepStreams -> Status -> DbOperations BuildState -- ^ stdout, stderr, step status
   EndBuild :: BuildState -> DbOperations Status -- ^ Returns the overall state of the build

startBuild :: Member DbOperations effs => String -> Eff effs BuildState
startBuild name = send (StartBuild name)

startStep :: Member DbOperations effs => BuildState -> Step Substituted -> Eff effs StepID -- ^ The string is the step's description
startStep state desc = send (StartStep state desc)

endStep :: Member DbOperations effs => BuildState -> StepID -> StepStreams -> Status -> Eff effs BuildState -- ^ stdout, stderr, step status
endStep state stepID streams status = send (EndStep state stepID streams status)

endBuild :: Member DbOperations effs => BuildState -> Eff effs Status -- ^ Returns the overall state of the build
endBuild state = send (EndBuild state)

interpretDbOpsAsLowLevelDbOps
  :: Eff (DbOperations ': effs)
  ~> Eff (Low.LowLevelDbOperations ': effs)
interpretDbOpsAsLowLevelDbOps = reinterpret dbToLowLevelDb

dbToLowLevelDb
  :: DbOperations
  ~> Eff (Low.LowLevelDbOperations ': effs)
dbToLowLevelDb (StartBuild builderName) = do
   buildID <- Low.startBuild builderName
   return $ BuildState buildID Success
dbToLowLevelDb (StartStep buildState@(BuildState buildID _) step) =
   Low.startStep buildID (J.encode step)
dbToLowLevelDb (EndStep (BuildState buildId buildStatus) stepID streams status) = do
   Low.endStep stepID streams status
   return $ BuildState buildId (max buildStatus status)
dbToLowLevelDb (EndBuild buildState@(BuildState buildID status)) = do
   Low.endBuild buildID status
   return status
