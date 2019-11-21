{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config
import Exec
import System.Exit

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Map.Strict as Map

runSubstedBuild :: Either [ValidationError] Builder -> IO ExitCode
runSubstedBuild eBuilder =
  case eBuilder of
     Left _        -> return $ ExitFailure 1
     Right builder -> runBuild builder

main :: IO ()
main = do
  LT.putStrLn (renderAsXml shellCmd0)
  LT.putStrLn (renderAsXml builder)
  let substedBuilder :: Either [ValidationError] Builder = substitute ("$[", "]") subst builder
  LT.putStrLn (either (LT.pack . unlines . map show) renderAsXml substedBuilder)
  rc :: ExitCode <- runSubstedBuild substedBuilder
  putStrLn $ "Builder finished with " ++ show rc
  where shellCmd0 :: Step = ShellCmd ["ls", "$[mydir]"]
        shellCmd1 = ShellCmd ["tree", "-L", "1", "$[mydir]"]
        shellCmd2 = ShellCmd ["wrongcmd"]
        builder :: Builder = Builder "bname" [shellCmd0, shellCmd1, shellCmd2]
        subst :: Subst = Map.fromList [("mydir", "/")]
