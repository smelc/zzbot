{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db where

import Control.Monad.Except
import Data.List.Extra
import qualified Data.Aeson as J

import Common
import Config
import LowLevelDb

-- | The build identifier and its status (so far)
data BuildState = BuildState BuildID Status

withMaxStatus :: BuildState -> Status -> BuildState
withMaxStatus (BuildState buildID s1) s2 =
   BuildState buildID (max s1 s2)

class Monad m => DbOperations s m where
   startBuild :: String -> m BuildState -- ^ The string is the builder's name
   startStep :: BuildState -> Step Substituted -> m StepID -- ^ The string is the step's description
   endStep :: BuildState -> StepID -> StepStreams -> Status -> m BuildState -- ^ stdout, stderr, step status
   endBuild :: BuildState -> m Status -- ^ Returns the overall state of the build

instance forall a s m . DbOperations s m => DbOperations s (ExceptT a m) where
   startBuild name = lift (Db.startBuild @s name)
   startStep state desc = lift (Db.startStep @s state desc)
   endStep state stepID streams status =
     lift (Db.endStep @s state stepID streams status)
   endBuild state = lift (Db.endBuild @s state)

data UsingLowLevelDb s

instance (Monad m, LowLevelDbOperations s m) => DbOperations (UsingLowLevelDb s) m where
    startBuild builderName = do
       buildID <- LowLevelDb.startBuild @s builderName
       return $ BuildState buildID Success
    startStep buildState@(BuildState buildID _) step =
       LowLevelDb.startStep @s buildID (J.encode step)
    endStep (BuildState buildId buildStatus) stepID streams status = do
       LowLevelDb.endStep @s stepID streams status
       return $ BuildState buildId (max buildStatus status)
    endBuild buildState@(BuildState buildID status) = do
       LowLevelDb.endBuild @s buildID status
       return status
