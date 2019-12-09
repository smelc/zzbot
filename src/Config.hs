{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Config (
  Builder(..)
  , Command(..)
  , parseVars
  , renderAsXml
  , splitAround
  , splitDelimiters
  , Step(..)
  , Subst
  , Substable(..)
  , ValidationError(..)
) where

import Data.Either
import Data.List
import Data.Maybe
import Data.Validation
import Text.Printf
import Text.XML

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

-- AST
data Command = Command { cmdFilename :: String, cmdArgs :: [String] }
  deriving (Eq)

data Step
    = SetPropertyFromValue { prop :: String, value :: String }
    | ShellCmd             { workdir :: Maybe String, cmd :: Command }
  deriving (Eq, Show)

data Builder = Builder { workdir :: Maybe String, name :: String, steps :: [Step] }
  deriving (Eq, Show)

data Config = Config { builders :: [Builder], subst :: Subst }
  deriving (Eq, Show)

instance Show Command where
  show (Command cmd []) = cmd
  show (Command cmd args) = cmd ++ " " ++ unwords args

-- types
type Subst = Map.Map String String

data ValidationError = KeyNotFound Subst String
  deriving (Eq, Ord)

instance Show ValidationError where
  show (KeyNotFound subst key) =
    printf "key not mapped by substitution: %s. Substitution's domain is: %s" key domain
   where
    domain = unwords (Map.keys subst)

class Substable a where
    -- The result of applying a substitution (Right) or errors (Left), using the delimiters given as first argument
    substitute :: (String, String) -> Subst -> a -> Validation (Set.Set ValidationError) a

-- split_around 'b' 'foobar' = Just(("foo", "ar"))
-- splitAround 'f' 'foobar' = Just(("", "oobar"))
-- splitAround 'r' 'foobar' = Just(("fooba", "")
-- splitAround 'c' 'foobar' = Nothing
splitAround :: String -> String -> Maybe (String, String)
splitAround sep text =
  if sep `isPrefixOf` text
  then Just ("", drop (length sep) text)
  else let fail = (length sep > length text || null text) in
    if fail then Nothing
    else let fst = head text
             tailResult = splitAround sep (tail text)
          in case tailResult of
                Nothing             -> Nothing
                Just(before, after) -> Just(fst : before, after)

-- splitDelimiters ("(", ")") "foo(var)bar" returns Just("foo", "var", "bar")
-- splitDelimiters ("(", ")") "(var)bar" returns Just("", "var", "bar")
-- splitDelimiters ("(", ")") "foo(var)" returns Just("foo", "var")
-- splitDelimiters ("(", ")") "foobar" returns Nothing
splitDelimiters :: (String, String) -> String -> Maybe (String, String, String)
splitDelimiters delimiters text = do
  (beforeOpen, afterOpen) <- splitAround (fst delimiters) text
  (beforeClose, afterClose) <- splitAround (snd delimiters) afterOpen
  return (beforeOpen, beforeClose, afterClose)

type VarName = String

-- | > parseVars ("(", ")") "foo(bar)chose" = [Left "foo", Right "bar", Left "chose"]
parseVars :: (String, String) -- ^ The pair of opening and closing delimiters
          -> String           -- ^ The string to parse
          -> [Either String VarName] -- ^ The parsed string, concatenating String content and variable names
parseVars delimiters text =
    case splitDelimiters delimiters text of
        Nothing -> [Left text | not (null text)]
        Just (before, var, after) -> [Left before, Right var] ++ parseVars delimiters after

validateVars :: (String, String) -- ^ The pair of opening and closing delimiters
             -> Subst            -- ^ The substitution
             -> String           -- ^ The text to substitute
             -> [ValidationError]         -- ^ A list of errors
validateVars delimiters subst text =
    map (KeyNotFound subst) missingVars
    where allVars = rights (parseVars delimiters text)
          missingVars = filter (`Map.notMember` subst) allVars
          domain = unwords $ Map.keys subst

-- Replace variables enclosed in delimiters and return the resulting string (Right)
-- or a list of errors (Left) if some keys are not mapped by the substitution
applySubstitution :: (String, String) -> Subst -> String -> Validation (Set.Set ValidationError) String
applySubstitution delimiters subst text =
    if not (null errors)
      then Failure (Set.fromList errors)
      else Success (concatMap substApplier pieces)
    where errors :: [ValidationError] = validateVars delimiters subst text
          pieces :: [Either String VarName] = parseVars delimiters text
          substApplier (Left text) = text
          substApplier (Right varName) = subst Map.! varName

---------
-- XML --
---------

class ToXml a where
    toXml :: a -> Element

simpleName :: String -> Name
simpleName name = Name (T.pack name) Nothing Nothing

(=:) :: String -> String -> Map.Map Name T.Text
attr =: value = Map.singleton (simpleName attr) (T.pack value)

(=?) :: String -> Maybe String -> Map.Map Name T.Text
attr =? Nothing = Map.empty
attr =? Just value = attr =: value

instance ToXml Step where
    toXml (SetPropertyFromValue prop value) = Element "set_property" (prop =: value) []
    toXml (ShellCmd workdir cmd) =
      Element "shell" ("command" =: show cmd <> "workdir" =? workdir) []

instance ToXml Builder where
    toXml (Builder workdir name steps) =
      Element "builder" ("workdir" =? workdir) (map (NodeElement . toXml) steps)

renderAsXml :: ToXml a => a -> LT.Text
renderAsXml x = renderText settings doc
 where
  settings = def {rsPretty=True, rsXMLDeclaration=False}
  doc = Document (Prologue [] Nothing []) (toXml x) []

---------------------------------
-- Implementation of Substable --
---------------------------------

-- Creates an instance overlapping with the one for lists, because hey
-- String is [Char]!
-- instance Substable String where
--    substitute delimiters subst text = applySubstitution delimiters subst text

-- Lift substitute to traversables
instance (Traversable t, Substable a) => Substable (t a) where
    substitute delimiters subst = traverse (substitute delimiters subst)

instance Substable Command where
  substitute delimiters subst (Command cmd args) =
    Command
      <$> applySubstitution delimiters subst cmd
      <*> traverse (applySubstitution delimiters subst) args

instance Substable Step where
  substitute delimiters subst (SetPropertyFromValue prop value) =
    SetPropertyFromValue prop <$> applySubstitution delimiters subst value
  substitute delimiters subst (ShellCmd workdir cmd) =
    ShellCmd
      <$> traverse (applySubstitution delimiters subst) workdir
      <*> substitute delimiters subst cmd

instance Substable Builder where
  substitute delimiters subst (Builder workdir name steps) =
    Builder
      <$> traverse (applySubstitution delimiters subst) workdir
      <*> applySubstitution delimiters subst name
      <*> substitute delimiters subst steps
