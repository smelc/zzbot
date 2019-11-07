{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Map.Strict as Map

import Config

showSubstErrors :: [String] -> String
showSubstErrors = unlines

main :: IO ()
main = do
  print shellCmd0
  print builder
  let substedBuilder :: Either [String] Builder = substitute ("$[", "]") subst builder
  print substedBuilder
  where shellCmd0 :: Step = ShellCmd ["ls", "$[mydir]"]
        shellCmd1 = ShellCmd ["cd", "$[mydir]"]
        builder :: Builder = Builder "bname" [shellCmd0, shellCmd1]
        subst :: Subst = Map.fromList [("mydir", "/home/churlin")]
