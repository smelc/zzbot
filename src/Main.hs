{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Map.Strict as Map


showSubstErrors :: [String] -> String
showSubstErrors = unlines

main :: IO ()
main = do
  LT.putStrLn (renderAsXml shellCmd0)
  LT.putStrLn (renderAsXml builder)
  let substedBuilder :: Either [String] Builder = substitute ("$[", "]") subst builder
  LT.putStrLn (either (LT.pack . unlines) renderAsXml substedBuilder)
  where shellCmd0 :: Step = ShellCmd ["ls", "$[mydir]"]
        shellCmd1 = ShellCmd ["cd", "$[mydir]"]
        builder :: Builder = Builder "bname" [shellCmd0, shellCmd1]
        subst :: Subst = Map.fromList [("mydir", "/home/churlin")]
