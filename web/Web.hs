{-# LANGUAGE OverloadedStrings #-}

module Web where

import Web.Scotty

import LowLevelDb

{-# ANN web ("HLint: ignore" :: String) #-}
web :: Int -- The port for the server
    -> Database
    -> IO ()
web port db = scotty port $ do
  get "/" $ do
    html "<h1>zzbot</h1>" -- TODO smelc: first thing: list the builders