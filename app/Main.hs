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
  env <- getEnvironment
  xmls <- traverse readFile optFilenames
  codes <- traverse (process optPrint env) xmls
  exitWith $ andExitCodes codes
