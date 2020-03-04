{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Web where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Text.Lazy
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty.Trans

import qualified Data.Text.Lazy as Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Db
import LowLevelDb

runWeb
 :: Database
 -> Int
 -> IO ()
runWeb db port = scottyT port (`runReaderT` db) (web @(UsingLowLevelDb UsingIOForDb))

web
  :: forall s m
   . (MonadIO m, DbOperations s m)
  => ScottyT Text m ()
web = do
  get "/" $ do
    builders :: [String] <- lift $ Db.getAllBuilders @s
    html $ renderHtml $ do
      H.h1 "zzbot"
      H.p "Here's the list of builders:"
      if Prelude.null builders
        then H.p "oops, no builders!"
        else H.ul $ forM_ builders (H.li . H.toHtml)
  get "/builder/:builder" $ do
    builder :: Text <- param "builder"
    builders :: [String] <- lift $ Db.getAllBuilders @s -- XXX Use a query method, to avoid retrieving all builders
    let builder' :: H.Html = H.i $ H.toHtml builder
    html $ renderHtml $
      if builder `notElem` Prelude.map pack builders
      then H.p (H.toHtml ("Builder " :: Text)
                <> builder'
                <> H.toHtml (" doesn't exist" :: Text))
      else undefined