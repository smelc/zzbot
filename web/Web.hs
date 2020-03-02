{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Web where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Text.Lazy
import Web.Scotty.Trans

import qualified Data.Text.Lazy as Text

import Db
import Html
import LowLevelDb

runWeb
 :: Database
 -> Int
 -> IO ()
runWeb db port = scottyT port (`runReaderT` db) (web @(UsingLowLevelDb UsingIOForDb))

web
  :: forall s m
   . DbOperations s m
  => MonadIO m
  => ScottyT Text m ()
web =
  get "/" $ do
    builders :: [String] <- lift $ Db.getAllBuilders @s
    let builders' :: [Text] = Prelude.map pack builders
    html $ append start $ buildersText builders'
  where start = Text.concat ["<h1>zzbot</h1>", "\n", "Here's the list of builders:"]
        buildersText builders' =
          if Prelude.null builders'
          then "oops, no builders!"
          else listify builders'