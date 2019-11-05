{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Map.Strict as Map

import Config

main :: IO ()
main = do
  putStrLn $ show shellCmd0
  putStrLn $ show builder
  let substedBuilder :: Either String Builder = substitute subst builder
  putStrLn $ show substedBuilder
  where shellCmd0 :: Step = ShellCmd ["ls", "$[mydir]"]
        shellCmd1 = ShellCmd ["cd", "$[mydir]"]
        builder :: Builder = Builder "bname" [shellCmd0, shellCmd1]
        subst :: Subst = Map.fromList [("mydir", "/home/churlin")]
