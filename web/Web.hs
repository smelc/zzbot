{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Web where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Text.Lazy
import Web.Scotty

import qualified Data.Text.Lazy as Text

import Db
import Html
import LowLevelDb

runDbStack :: Database
          -> ReaderT Database IO a
          -> IO a
runDbStack = flip runReaderT

web :: Database
    -> Int -- The port for the server
    -> IO ()
web db port = scotty port $ -- do
  get "/" $ do
    builders :: [String] <- lift $ runDbStack db (Db.getAllBuilders @(UsingLowLevelDb UsingIOForDb))
    let builders' :: [Text] = Prelude.map pack builders
    html $ append start $ buildersText builders'
  where start = Text.concat ["<h1>zzbot</h1>", "\n", "Here's the list of builders:"]
        buildersText builders' =
          if Prelude.null builders'
          then "oops, no builders!"
          else listify builders'