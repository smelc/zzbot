{-# LANGUAGE ScopedTypeVariables #-}
module Config where

import Data.List
import Text.Printf
import qualified Data.Map.Strict as Map

data Step =
      SetPropertyFromValue { prop :: String, value :: String }
    | ShellCmd { cmd :: [String] }

class Substable a where
    substitute :: Subst -> a -> a

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
splitDelimiters :: String -> String -> String -> Maybe(String, String, String)
-- This implementation is not very haskellish. I suppose we could use some Functor
-- to simplify Nothing handling
splitDelimiters open close text = 
  let splitOpen = splitAround open text
  in case splitOpen of
        Nothing -> Nothing
        Just(beforeOpen, afterOpen) ->
            let splitClose = splitAround close afterOpen
            in case splitClose of
                Nothing -> Nothing
                Just(beforeClose, afterClose) -> Just(beforeOpen, beforeClose, afterClose)

substituteString :: Subst -> String -> String
substituteString subst s = s -- FIXME

attrValueString :: String -> String -> String
attrValueString attr value = printf "%s=\"%s\"" attr value

instance Show Step where
    show (SetPropertyFromValue prop value)= printf "<set_property %s/>" (attrValueString prop value)
    show (ShellCmd cmdList)= "<shell command=\"" ++ (unwords cmdList) ++ "\"/>"

instance Substable Step where
    substitute subst (SetPropertyFromValue prop value) = SetPropertyFromValue prop (substituteString subst value)
    substitute subst whatever = whatever

data Builder = Builder { name :: String, steps :: [Step] }

instance Show Builder where
    show (Builder name steps) = 
        let shownSteps :: [String] = map show steps
            indentedSteps :: [String] = map (\x -> " " ++ x) shownSteps
            linedSteps :: String = intercalate "\n" indentedSteps
         in "<builder>\n" ++ linedSteps ++ "\n</builder>"

type Subst = Map.Map String String

data Config = Config { builders :: [Builder], subst :: Subst }