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

class Monad m => DbOperations s m where
   startBuild
     :: String -- ^ The builder's name
     -> m BuildID
   startStep :: BuildID -> Step Substituted -> m StepID
   endStep :: BuildID -> StepID -> StepStreams -> Status -> m ()
   endBuild :: BuildID -> Status -> m ()

instance forall a s m . DbOperations s m => DbOperations s (ExceptT a m) where
   startBuild name = lift (Db.startBuild @s name)
   startStep state desc = lift (Db.startStep @s state desc)
   endStep state stepID streams status =
     lift (Db.endStep @s state stepID streams status)
   endBuild buildId status = lift (Db.endBuild @s buildId status)

data UsingLowLevelDb s

instance (Monad m, LowLevelDbOperations s m) => DbOperations (UsingLowLevelDb s) m where
    startBuild = LowLevelDb.startBuild @s
    startStep buildID step = LowLevelDb.startStep @s buildID (J.encode step)
    endStep _ = LowLevelDb.endStep @s
    endBuild = LowLevelDb.endBuild @s
