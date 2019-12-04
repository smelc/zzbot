{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config
import Data.Maybe
import Data.Validation
import Exec
import System.Environment
import System.Exit
import XmlParse

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

data Argument =
    File String -- ^ A filename
  | Help -- ^ Help argument (-h and --help)

parseArg :: String -> Argument
parseArg "-h" = Help
parseArg "--help" = Help
parseArg s = File s

process :: Argument -> IO ExitCode
process Help = do
  putStrLn "Usage: zzbot [-h|--help] xml_file0 xml_file1 .."
  return ExitSuccess
process (File path) = do
  mbuilders :: XmlValidation[Builder] <- parseXmlFile path
  case mbuilders of
      Failure (err::Set.Set XmlParsingError) -> do
        putStrLn $ unlines $ Set.toList serr
        return $ ExitFailure 1
        where serr :: Set.Set String = Set.map show err
      Success (builders::[Builder]) -> do
        codes :: [ExitCode] <- mapM runBuild builders
        return $ andExitCodes (ExitFailure 1) codes -- If there are no builders, return a failure

andExitCode (ExitFailure i) (ExitFailure j) = ExitFailure (max i j)
andExitCode (ExitFailure i) _ = ExitFailure i
andExitCode _ (ExitFailure j) = ExitFailure j
andExitCode c1 c2 = c1

andExitCodes :: ExitCode -- ^ The value to return if the second argument is empty
            -> [ExitCode] -- ^ The list of return codes to combine
            -> ExitCode
andExitCodes default_ [] = default_
andExitCodes _ (hd:tl) = foldr andExitCode hd tl

main :: IO ()
main = do
  args :: [String] <- getArgs
  let zargs :: [Argument] = map parseArg args
      exits :: [IO ExitCode] = map process zargs
  codes :: [ExitCode] <- sequence exits
  exitWith $ andExitCodes (ExitFailure 1) codes

-- kept for future reference:
main0 :: IO ()
main0 = do
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