{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Config
import Control.Applicative
import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Validation
import System.Directory
import System.Environment
import System.Exit

import Db
import Exec
import Xml

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Options.Applicative as Opt

{- HLINT ignore Options -}
data Options = Options
  { optFilenames :: NonEmpty String
  , optProcessMode :: ProcessMode
  }

optionsParser :: Opt.Parser Options
optionsParser =
  Options
    <$> NE.some1 (Opt.strArgument (Opt.metavar "FILE"))
    <*> Opt.flag
          Execute
          PrintOnly
          (Opt.long "print"
             <> Opt.short 'p'
             <> Opt.help "print the builder's interpretation, but do not execute it")

optionsParserInfo :: Opt.ParserInfo Options
optionsParserInfo = Opt.info (optionsParser <**> Opt.helper) Opt.fullDesc

main :: IO ()
main = do
  Options{optFilenames, optProcessMode} <- Opt.execParser optionsParserInfo
  env <- ProcessEnv <$> getCurrentDirectory <*> getEnvironment
  xmls <- traverse readFile optFilenames
  res <- runUsingLowLevelDb $ runExceptT $ traverse (process optProcessMode env) xmls
  case res of
    Left code -> exitWith code
    _ -> return ()
