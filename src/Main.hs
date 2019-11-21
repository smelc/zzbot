{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config
import Data.Validation
import Exec
import System.Exit

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

runSubstedBuild :: Validation (Set.Set ValidationError) Builder -> IO ExitCode
runSubstedBuild eBuilder =
  case eBuilder of
     Failure _       -> return $ ExitFailure 1
     Success builder -> runBuild builder

renderErrors :: Set.Set ValidationError -> LT.Text
renderErrors = LT.pack . unlines . map show . Set.toList

main :: IO ()
main = do
  LT.putStrLn (renderAsXml shellCmd0)
  LT.putStrLn (renderAsXml builder)
  LT.putStrLn (validation renderErrors renderAsXml substedBuilder)
  rc :: ExitCode <- runSubstedBuild substedBuilder
  putStrLn $ "Builder finished with " ++ show rc
  where shellCmd0 :: Step = ShellCmd ["ls", "$[mydir]"]
        shellCmd1 = ShellCmd ["tree", "-L", "1", "$[mydir]"]
        shellCmd2 = ShellCmd ["wrongcmd"]
        builder :: Builder = Builder "bname" [shellCmd0, shellCmd1, shellCmd2]
        subst :: Subst = Map.fromList [("mydir", "/")]
        substedBuilder = substitute ("$[", "]") subst builder
