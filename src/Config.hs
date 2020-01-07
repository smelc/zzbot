{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Config (
  applySubstitution
  , Builder(..)
  , Config(..)
  , ConfigValidation
  , Command(..)
  , duplicates
  , normalize
  , parseVars
  , Phase(..)
  , splitAround
  , splitDelimiters
  , Step(..)
  , Subst
  , Substable(..)
  , substAll
  , ValidationError(..)
) where

import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Validation
import Text.Printf

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- AST

data Phase = Parsed | Normalized | Substituted

type family StepWorkDirType (p :: Phase) :: * where
  StepWorkDirType Parsed = Maybe String
  StepWorkDirType Normalized = String
  StepWorkDirType Substituted = String

type family BuilderWorkDirType (p :: Phase) :: * where
  BuilderWorkDirType Parsed = Maybe String
  BuilderWorkDirType Normalized = ()
  BuilderWorkDirType Substituted = ()


type family ConfigSubstType (p :: Phase) :: * where
  ConfigSubstType Parsed = Subst
  ConfigSubstType Normalized = Subst
  ConfigSubstType Substituted = ()

data Command = Command { cmdFilename :: String, cmdArgs :: [String] }
  deriving (Eq)

data Step (p :: Phase)
    = SetPropertyFromValue { prop :: String, value :: String }
    | ShellCmd             { workdir :: StepWorkDirType p,
                             cmd :: Command,
                             mprop :: Maybe String -- ^ The build property to set (if any) from the command's output
                           }

deriving instance Eq (Step Parsed)
deriving instance Eq (Step Normalized)
deriving instance Eq (Step Substituted)
deriving instance Show (Step Parsed)
deriving instance Show (Step Normalized)
deriving instance Show (Step Substituted)

data Builder (p :: Phase) = Builder { workdir :: BuilderWorkDirType p, name :: String, steps :: [Step p] }

deriving instance Eq (Builder Parsed)
deriving instance Eq (Builder Normalized)
deriving instance Eq (Builder Substituted)
deriving instance Show (Builder Parsed)
deriving instance Show (Builder Normalized)
deriving instance Show (Builder Substituted)

data Config (p :: Phase) = Config { builders :: NE.NonEmpty (Builder p), subst :: ConfigSubstType p }

deriving instance Eq (Config Parsed)
deriving instance Eq (Config Normalized)
deriving instance Eq (Config Substituted)
deriving instance Show (Config Parsed)
deriving instance Show (Config Normalized)
deriving instance Show (Config Substituted)

instance Show Command where
  show (Command cmd []) = cmd
  show (Command cmd args) = cmd ++ " " ++ unwords args

-- types
type Subst = [(String,String)]

data ValidationError
  = DuplicateSubstEntries [String]
  | KeyNotFound Subst String
  deriving (Eq, Ord)

type ConfigValidation = Validation (Set.Set ValidationError)

failWith :: ValidationError -> ConfigValidation a
failWith error = Failure (Set.singleton error)

instance Show ValidationError where
  show (DuplicateSubstEntries entries) =
    "Some substitutions members are mapped more thance once: "
      ++ intercalate "," entries
  show (KeyNotFound subst key) =
    printf
      "key not mapped by substitution: %s. Substitution's domain is: %s"
      key
      domain
   where
    domain = unwords (map fst subst)

class Substable a b where
    -- The result of applying a substitution (Right) or errors (Left), using the delimiters given as first argument
    substitute :: (String, String) -> Subst -> a -> ConfigValidation b

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
             -> [ValidationError] -- ^ A list of errors
validateVars delimiters subst text =
    map (KeyNotFound subst) missingVars
    where allVars = rights (parseVars delimiters text)
          missingVars = filter (`notElem` map fst subst) allVars

-- Replace variables enclosed in delimiters and return the resulting string (Right)
-- or a list of errors (Left) if some keys are not mapped by the substitution
applySubstitution :: (String, String) -> Subst -> String -> ConfigValidation String
applySubstitution delimiters subst text =
    if not (null errors)
      then Failure (Set.fromList errors)
      else Success (concatMap substApplier pieces)
    where errors :: [ValidationError] = validateVars delimiters subst text
          pieces :: [Either String VarName] = parseVars delimiters text
          substApplier :: Either String VarName -> String
          substApplier (Left text) = text
          substApplier (Right varName) = fromJust $ lookup varName subst

----------------
-- Validation --
----------------

duplicates
  :: (Eq a, Ord a)
  => [a] -- ^ The list to look for duplicates in
  -> [a]
duplicates elems = Map.keys (Map.filter (>1) counts)
 where
  counts = Map.unionsWith (+) (map singleton elems)
  singleton elem = Map.singleton elem 1

---------------------------------
-- Implementation of Substable --
---------------------------------

-- Creates an instance overlapping with the one for lists, because hey
-- String is [Char]!
-- instance Substable String where
--    substitute delimiters subst text = applySubstitution delimiters subst text

-- Lift substitute to traversables
instance (Traversable t, Substable a b) => Substable (t a) (t b) where
    substitute delimiters subst = traverse (substitute delimiters subst)

instance Substable Command Command where
  substitute delimiters subst (Command cmd args) =
    Command
      <$> applySubstitution delimiters subst cmd
      <*> traverse (applySubstitution delimiters subst) args

instance (StepWorkDirType a ~ String) => Substable (Step a) (Step Substituted) where
  substitute delimiters subst (SetPropertyFromValue prop value) =
    SetPropertyFromValue prop <$> applySubstitution delimiters subst value
  substitute delimiters subst (ShellCmd workdir cmd mprop) =
    ShellCmd
      <$> applySubstitution delimiters subst workdir
      <*> substitute delimiters subst cmd
      <*> Success mprop

instance (StepWorkDirType a ~ String) => Substable (Builder a) (Builder Substituted) where
  substitute delimiters subst (Builder _ name steps) =
    Builder ()
      <$> applySubstitution delimiters subst name
      <*> substitute delimiters subst steps

checkNoDuplicates :: Subst -> ConfigValidation ()
checkNoDuplicates subst =
  case duplicates (map fst subst) of
    [] -> Success ()
    dupes -> failWith $ DuplicateSubstEntries dupes

-- |Substitute $[...] variables
substSquare :: Config Normalized -> ConfigValidation (Config Substituted)
substSquare Config{builders, subst} = do
  checkNoDuplicates subst
  substedBuilders <- traverse (substitute ("$[", "]") subst) builders
  return $ Config substedBuilders ()

-- |Substitute environment variables (${})
substEnv :: [(String, String)] -- ^ The environment
         -> Config Substituted -- ^ The configuration to substitute
         -> ConfigValidation (Config Substituted)
substEnv env Config{builders, subst} = do
  substedBuilders <- traverse (substitute ("${", "}") env) builders
  return $ Config substedBuilders ()

-- |Substitute $[..] variables and environment variables (${})
substAll :: [(String, String)]
         -> Config Normalized
         -> ConfigValidation (Config Substituted)
substAll env config =
  substSquare config `bindValidation` substEnv env

normalize :: FilePath -- ^ The working directory
          -> Config Parsed -- ^ The input configuration
          -> Config Normalized
normalize workdir Config{builders, subst} =
  Config builders' subst
  where builders' = NE.map normalizeBuilder builders
        normalizeBuilder :: Builder Parsed -> Builder Normalized
        normalizeBuilder Builder{workdir=mworkdir, name, steps} =
          Builder () name (map (normalizeStep (fromMaybe workdir mworkdir)) steps)
        normalizeStep :: String -> Step Parsed -> Step Normalized
        normalizeStep _ SetPropertyFromValue{prop, value} = SetPropertyFromValue{prop, value}
        normalizeStep newWorkDir ShellCmd{workdir=mworkdir, cmd, mprop} = ShellCmd (fromMaybe newWorkDir mworkdir) cmd mprop
