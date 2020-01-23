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

-- | The state of a build, as exposed to clients of this file
-- TODO smelc keep only the maximum of statuses gathered so far, no need to keep the list
-- This'll get rid of the snoc business whose complexity is bad
data BuildState = BuildState BuildID [Status]

snoc :: BuildState -> Status -> BuildState
snoc (BuildState buildID statuses) status =
   BuildState buildID $ Data.List.Extra.snoc statuses status

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
       return $ BuildState buildID []
    startStep buildState@(BuildState buildID _) step =
       LowLevelDb.startStep @s buildID (J.encode step)
    endStep buildState stepID streams status = do
       LowLevelDb.endStep @s stepID streams status
       return $ Db.snoc buildState status
    endBuild buildState@(BuildState buildID statuses) = do
       let status = foldr max Success statuses
       LowLevelDb.endBuild @s buildID status
       return status
