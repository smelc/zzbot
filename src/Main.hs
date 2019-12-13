{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config
import Control.Applicative
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Validation
import Exec
import System.Environment
import System.Exit
import XmlParse

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Options.Applicative as Opt

process :: Bool -- ^ Whether to print (True) or execute the builder (False)
        -> String -- ^ The file containing the builder's description
        -> IO ExitCode
process printOrExec filepath  = do
  mconfig :: XmlValidation Config <- parseXmlFile filepath
  case mconfig of
      Failure (err :: Set.Set XmlParsingError) -> do
        putStrLn $ unlines $ map show $ Set.toList err
        return (ExitFailure 1)
      Success config@Config{subst, builders} ->
        if printOrExec
        then do
          LT.putStrLn $ renderAsXml config
          return ExitSuccess
        else andExitCodes <$> traverse runBuild builders

andExitCode :: ExitCode -> ExitCode -> ExitCode
andExitCode (ExitFailure i) (ExitFailure j) = ExitFailure (max i j)
andExitCode (ExitFailure i) _ = ExitFailure i
andExitCode _ (ExitFailure j) = ExitFailure j
andExitCode c1 c2 = c1

andExitCodes
  :: NonEmpty ExitCode -- ^ The list of return codes to combine
  -> ExitCode
andExitCodes = foldr1 andExitCode

{- HLINT ignore Options -}
data Options = Options { optFilenames :: NonEmpty String, optPrint :: Bool }

optionsParser :: Opt.Parser Options
optionsParser = Options <$> NE.some1 (Opt.strArgument (Opt.metavar "FILE"))
                        <*> Opt.switch (Opt.long "print" <> Opt.short 'p' <> Opt.help "print the builder's interpretation, but do not execute it")

optionsParserInfo :: Opt.ParserInfo Options
optionsParserInfo = Opt.info (optionsParser <**> Opt.helper) Opt.fullDesc

main :: IO ()
main = do
  Options{optFilenames, optPrint} <- Opt.execParser optionsParserInfo
  codes <- traverse (process optPrint) optFilenames
  exitWith $ andExitCodes codes