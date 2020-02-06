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
  interpretDbOpsAsLowLevelDbOps,
) where

import Control.Effect
import Data.Kind
import qualified Data.Aeson as J

import Common
import Config
import qualified LowLevelDb as Low

class Monad m => DbOperations m where
   startBuild
     :: String -- ^ The builder's name
     -> m BuildID
   startStep :: BuildID -> Step Substituted -> m StepID
   endStep :: BuildID -> StepID -> StepStreams -> Status -> m ()
   endBuild :: BuildID -> Status -> m ()

instance (Monad (t m), Send DbOperations t m) => DbOperations (EffT t m) where
  startBuild name = send @DbOperations (startBuild name)
  startStep buildId step = send @DbOperations (startStep buildId step)
  endStep buildId stepID streams status = send @DbOperations (endStep buildId stepID streams status)
  endBuild buildId status = send @DbOperations (endBuild buildId status)

data DbOperationsUsingLowLevelDb
type DbOperationsUsingLowLevelDbT = HandlerT DbOperationsUsingLowLevelDb '[]
type instance Handles DbOperationsUsingLowLevelDbT eff = eff == DbOperations

instance Low.LowLevelDbOperations m => DbOperations (DbOperationsUsingLowLevelDbT m) where
  startBuild name = HandlerT $ Low.startBuild name
  startStep buildID step = HandlerT $ Low.startStep buildID (J.encode step)
  endStep buildId stepId streams status = HandlerT $ Low.endStep stepId streams status
  endBuild buildId status = HandlerT $ Low.endBuild buildId status

interpretDbOpsAsLowLevelDbOps
  :: EffT DbOperationsUsingLowLevelDbT m a
  -> m a
interpretDbOpsAsLowLevelDbOps = runHandlerT . runEffT
