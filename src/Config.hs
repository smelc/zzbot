{-# LANGUAGE ScopedTypeVariables #-}
module Config where

import Data.Either
import Data.List
import Text.Printf
import qualified Data.Map.Strict as Map

-- AST
data Step =
      SetPropertyFromValue { prop :: String, value :: String }
    | ShellCmd             { cmd :: [String] }
data Builder = Builder { name :: String, steps :: [Step] }
data Config = Config { builders :: [Builder], subst :: Subst }

-- types
type Subst = Map.Map String String

class Substable a where
    -- The result of applying a substitution (Right) or an error message (Left)
    substitute :: Subst -> a -> Either String a

-- split_around 'b' 'foobar' = Just(("foo", "ar"))
-- splitAround 'f' 'foobar' = Just(("", "oobar"))
-- splitAround 'r' 'foobar' = Just(("fooba", "")
-- splitAround 'c' 'foobar' = Nothing
splitAround :: String -> String -> Maybe (String, String)
splitAround sep text =
  if isPrefixOf sep text
  then Just(("", drop (length sep) text))
  else let fail = (length(sep) > length(text) || length(text) == 0) in
    if fail then Nothing
    else let fst = head text
             tailResult = splitAround sep (tail text)
          in case tailResult of
                Nothing             -> Nothing
                Just(before, after) -> Just(fst : before, after)

-- splitDelimiters "(" ")" "foo(var)bar" returns Just("foo", "var", "bar")
-- splitDelimiters "(" ")" "(var)bar" returns Just("", "var", "bar")
-- splitDelimiters "(" ")" "foo(var)" returns Just("foo", "var")
-- splitDelimiters "(" ")" "foobar" returns Nothing
splitDelimiters :: String -> String -> String -> Maybe (String, String, String)
-- This implementation is not very haskellish. I suppose we could use some Functor
-- to simplify Nothing handling
splitDelimiters open close text = do
  (beforeOpen, afterOpen) <- splitAround open text
  (beforeClose, afterClose) <- splitAround close afterOpen
  return (beforeOpen, beforeClose, afterClose)

type VarName = String

-- First argument is the pair of opening and closing delimiters
-- Second argument is the string to parse
-- Returns the string parsed, concatening String content and variable names
-- For example: parseVars ("(", ")") "foo(bar)chose" =
--                [Left "foo", Right "bar", Left "chose"]
parseVars :: (String, String) -> String -> [Either String VarName]
parseVars delimiters text =
    case splitDelimiters (fst delimiters) (snd delimiters) text of
        Nothing -> if length(text) == 0 then [] else [Left text]
        Just (before, var, after) -> [Left before, Right var] ++ parseVars delimiters after

-- Replace $[key] by subst[key] for every $[key] found
-- Fail (return Left) if some key is not mapped by the substitution
applySubstitution :: Subst -> String -> Either String String
applySubstitution subst text =
    case splitDelimiters "$[" "]" text of
        Nothing -> Right text
        Just (before, key, after) ->
            case Map.lookup key subst of
                Nothing -> Left ("$[" ++ key ++ "] appears in " ++ text ++ " but " ++ key ++ " is not bound by the substitution (" ++ show subst ++ ")")
                Just image -> applySubstitution subst (before ++ image ++ after)

attrValueString :: String -> String -> String
attrValueString attr value = printf "%s=\"%s\"" attr value

----------------------------
-- Implementation of Show --
----------------------------

instance Show Step where
    show (SetPropertyFromValue prop value)= printf "<set_property %s/>" (attrValueString prop value)
    show (ShellCmd cmdList)= "<shell command=\"" ++ (unwords cmdList) ++ "\"/>"

instance Show Builder where
    show (Builder name steps) = 
        let shownSteps :: [String] = map show steps
            indentedSteps :: [String] = map (\x -> " " ++ x) shownSteps
            linedSteps :: String = intercalate "\n" indentedSteps
         in "<builder>\n" ++ linedSteps ++ "\n</builder>"

---------------------------------
-- Implementation of Substable --
---------------------------------

-- Lift substitute to lists
instance Substable a => Substable [a] where
    substitute subst list = 
        let base :: [Either String a] = map (substitute subst) list
            (failures, images) = partitionEithers base in
                if length(failures) > 0
                then Left(intercalate "\n" failures) -- report all errors at once, not only the first one
                else Right images

instance Substable Step where
    substitute subst (SetPropertyFromValue prop value) = do
        valueImage <- applySubstitution subst value
        Right (SetPropertyFromValue prop valueImage)
    substitute subst (ShellCmd cmdList) =
        let cmdListImage :: [Either String String] = map (applySubstitution subst) cmdList
            (failures, images) = partitionEithers cmdListImage in
                if length(failures) > 0
                then Left(intercalate "\n" failures)
                else Right (ShellCmd images)

instance Substable Builder where
    substitute subst (Builder name steps) = do
        substedName <- applySubstitution subst name
        substedSteps <- substitute subst steps
        return (Builder substedName substedSteps)