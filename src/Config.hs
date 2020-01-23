{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Config (
  applySubstitution
  , Builder(..)
  , Config(..)
  , ConfigValidation
  , Command(..)
  , duplicates
  , ForeachExtension(..)
  , normalize
  , parseVars
  , Phase(..)
  , Step(..)
  , Subst
  , Substable(..)
  , substAll
  , ValidationError(..)
) where

import Data.Either
import Data.Foldable
import Data.List
import Data.Kind
import Data.Maybe
import Data.Validation
import Data.Void
import Safe
import System.FilePath
import Text.Megaparsec.Char
import Text.Megaparsec
import Text.Printf

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- AST

data Phase = Parsed | Normalized | Substituted

type family BuilderWorkDirType (p :: Phase) :: Type where
  BuilderWorkDirType Parsed = Maybe String
  BuilderWorkDirType Normalized = ()
  BuilderWorkDirType Substituted = ()

type family ConfigSubstType (p :: Phase) :: Type where
  ConfigSubstType Parsed = Subst
  ConfigSubstType Normalized = Subst
  ConfigSubstType Substituted = ()

type family StepHaltOnFailureType (p :: Phase) :: Type where
  StepHaltOnFailureType Parsed = Maybe Bool
  StepHaltOnFailureType Normalized = Bool
  StepHaltOnFailureType Substituted = Bool

type family StepWorkDirType (p :: Phase) :: Type where
  StepWorkDirType Parsed = Maybe String
  StepWorkDirType Normalized = String
  StepWorkDirType Substituted = String

type family StepExtensionType (p :: Phase) :: Type where
  StepExtensionType Parsed = ForeachExtension Parsed
  StepExtensionType Normalized = Void
  StepExtensionType Substituted = Void

type Forall (c :: Type -> Constraint) (p :: Phase) =
  ( c (BuilderWorkDirType p)
  , c (ConfigSubstType p)
  , c (StepHaltOnFailureType p)
  , c (StepWorkDirType p)
  , c (StepExtensionType p)
  )

data Command = Command { cmdFilename :: String, cmdArgs :: [String] }
  deriving (Eq, Show)

data Step (p :: Phase)
  = SetPropertyFromValue { prop :: String, value :: String }
  | ShellCmd             { workdir :: StepWorkDirType p,
                           cmd :: Command,
                           mprop :: Maybe String, -- ^ The build property to set (if any) from the command's output
                           haltOnFailure :: StepHaltOnFailureType p
                         }
  | Ext (StepExtensionType p)

deriving instance Forall Eq p => Eq (Step p)
deriving instance Forall Show p => Show (Step p)

{- HLINT ignore ForeachExtension -}
data ForeachExtension (p :: Phase) = Foreach
  { steps :: [Step p]
  }

deriving instance Forall Eq p => Eq (ForeachExtension p)
deriving instance Forall Show p => Show (ForeachExtension p)

data Builder (p :: Phase) = Builder
  { workdir :: BuilderWorkDirType p
  , name :: String
  , steps :: [Step p]
  }

deriving instance Forall Eq p => Eq (Builder p)
deriving instance Forall Show p => Show (Builder p)

data Config (p :: Phase) = Config
  { builders :: NE.NonEmpty (Builder p)
  , subst :: ConfigSubstType p
  }

deriving instance Forall Eq p => Eq (Config p)
deriving instance Forall Show p => Show (Config p)

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
  substitute :: (String, String) -> Subst -> a -> ConfigValidation b

type VarName = String

parseVars :: (String, String) -- ^ The pair of opening and closing delimiters
          -> String           -- ^ The string to parse
          -> [Either String VarName] -- ^ The parsed string, concatenating String content and variable names
parseVars (open, close) str =
  fromJustNote "vars should always succeed" (parseMaybe @Void vars str)
 where
  vars = do
    prefix <- many (try textThenVar)
    suffix <- many anySingle
    return (concat prefix ++ [Left suffix | not (null suffix)])

  textThenVar = do
    before <- manyTill anySingle (string open)
    inside <- manyTill anySingle (string close)
    return [Left before, Right inside]

-- Replace variables enclosed in delimiters and return the resulting string (Right)
-- or a list of errors (Left) if some keys are not mapped by the substitution
applySubstitution :: (String, String) -> Subst -> String -> ConfigValidation String
applySubstitution delimiters subst text =
  concat <$> traverse substApplier (parseVars delimiters text)
 where
  substApplier (Left text) = pure text
  substApplier (Right varName)
    | Just value <- lookup varName subst = pure value
    | otherwise = failWith (KeyNotFound subst varName)

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

-- Lift substitute to traversables
instance (Traversable t, Substable a b) => Substable (t a) (t b) where
    substitute delimiters subst = traverse (substitute delimiters subst)

instance Substable Command Command where
  substitute delimiters subst (Command cmd args) =
    Command
      <$> applySubstitution delimiters subst cmd
      <*> traverse (applySubstitution delimiters subst) args

instance (StepWorkDirType a ~ String, StepHaltOnFailureType a ~ Bool, StepExtensionType a ~ Void) => Substable (Step a) (Step Substituted) where
  substitute delimiters subst (SetPropertyFromValue prop value) =
    SetPropertyFromValue prop <$> applySubstitution delimiters subst value
  substitute delimiters subst (ShellCmd workdir cmd mprop haltOnFailure) =
    ShellCmd
      <$> applySubstitution delimiters subst workdir
      <*> substitute delimiters subst cmd
      <*> Success mprop
      <*> Success haltOnFailure
  substitute _ _ (Ext ext) = absurd ext

instance (StepWorkDirType a ~ String, StepHaltOnFailureType a ~ Bool, StepExtensionType a ~ Void) => Substable (Builder a) (Builder Substituted) where
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
normalize userWorkDir Config{builders, subst} =
  Config (fmap normalizeBuilder builders) subst
 where
  prependIfRelative :: FilePath -> FilePath -> FilePath
  prependIfRelative prefix filepath =
    if isAbsolute filepath
      then filepath
      else prefix </> filepath

  normalizeBuilder :: Builder Parsed -> Builder Normalized
  normalizeBuilder builder@Builder{workdir=builderWorkDir, name, steps} =
    builder
      { workdir = ()
      , steps = concatMap (normalizeStep absoluteWorkDir) steps
      }
   where
    relativeWorkDir = fromMaybe "" builderWorkDir
    absoluteWorkDir = prependIfRelative userWorkDir relativeWorkDir

  normalizeStep :: FilePath -> Step Parsed -> [Step Normalized]
  normalizeStep _ SetPropertyFromValue{..} = [SetPropertyFromValue {..}]
  normalizeStep defaultWorkDir step@ShellCmd{workdir=stepWorkDir, mprop, haltOnFailure} =
    [ step
        { workdir = absoluteWorkDir
        , haltOnFailure = fromMaybe defaultHaltOnFailure haltOnFailure
        }
    ]
   where
    relativeWorkDir = fromMaybe "" stepWorkDir
    absoluteWorkDir = prependIfRelative defaultWorkDir relativeWorkDir
    -- haltOnFailure defaults to True for <shell> and to False for <setPropertyFromCommand>
    defaultHaltOnFailure = isNothing mprop
  normalizeStep defaultWorkDir (Ext (Foreach steps)) =
    concatMap (normalizeStep defaultWorkDir) steps
