{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}

module XmlParse (
  aProperty
  , aValue
  , failWith
  , parseXmlFile
  , parseXmlString
  , tBuilder
  , tSetProperty
  , tSetPropertyFromCommand
  , tShell
  , XmlParsingError(..)
  , XmlValidation
) where

import Data.Char
import Data.Either
import Data.Either.Combinators
import Data.Foldable
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Validation
import System.IO
import Text.Printf
import Text.XML.Light.Types

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.XML.Light.Input as XmlInput

import Config

---------------------------------------------------
-- Parsing of XML to create instances of Builder --
---------------------------------------------------

-- Attributes
aProperty = "property"
aValue = "value"

-- Tags
tBuilder, tSetProperty, tShell, tConfig :: String
tBuilder = "builder"
tEntry = "entry"
tSetProperty = "setProperty"
tSetPropertyFromCommand = "setPropertyFromCommand"
tShell = "shell"
tSubstitution = "substitution"
tConfig = "config"

giveLineInMsg :: String -> Maybe Line -> String
giveLineInMsg errMsg Nothing = errMsg
giveLineInMsg errMsg (Just line) = "At line " ++ show line ++ ": " ++ errMsg

data ZXML = ZElem {tag :: String,
                   attrs :: [(String, String)],
                   children :: [ZXML],
                   maybeLine :: Maybe Line}

data XmlParsingError
  = EmptyCommand (Maybe Line)
  | EmptyDocument
  | MissingAttribute String String (Maybe Line)
  | NoBuilder (Maybe Line)
  | NoRootElement
  | ShellAndProperty (Maybe Line)
  | SetPropertyFromCmdWithoutProperty (Maybe Line)
  | UnexpectedTag [String] String (Maybe Line) -- ^ The expected tags, the actual tag
  | UnexpectedText (Maybe Line)
  | UnexpectedCRef
  deriving (Eq, Ord)

instance Show XmlParsingError where
  show (EmptyCommand line) = giveLineInMsg "Commands cannot be empty" line
  show EmptyDocument = "The document is empty"
  show (NoBuilder line) =
    giveLineInMsg "Configurations must have at least one builder" line
  show NoRootElement = "Expected exactly one top-level element but found zero"
  show (MissingAttribute elem attr line) =
    giveLineInMsg
      ("Missing attribute in element " ++ elem ++ ": " ++ attr)
      line
  show (ShellAndProperty line) = giveLineInMsg (printf "<%s> should not feature the \"%s\" attribute" tShell aProperty) line
  show (SetPropertyFromCmdWithoutProperty line) = giveLineInMsg (printf "<%s> must feature the \"%s\" attribute" tSetPropertyFromCommand aProperty) line
  show (UnexpectedTag expected actual line) =
    giveLineInMsg
      ("Expected one of " ++ intercalate ", " expected ++ ", got " ++ actual)
      line
  show (UnexpectedText cdLine) = giveLineInMsg "Unexpected Text in XML" cdLine
  show UnexpectedCRef = "Unexpected CRef in XML"

type XmlParsingErrors = Set.Set XmlParsingError
type XmlValidation = Validation XmlParsingErrors

failWith :: XmlParsingError -> XmlValidation a
failWith error = Failure (Set.singleton error)

parseAttrValue
  :: ZXML -- ^ The element in which the attribute is being searched
  -> String -- ^ The name of the attribute to look up
  -> (String -> XmlValidation a) -- ^ A parser of values
  -> XmlValidation a -- ^ An error message or the attribute's value
parseAttrValue zxml@ZElem {tag, attrs, maybeLine} attr parse =
  case lookup attr attrs of
    Just value -> parse value
    Nothing -> failWith (MissingAttribute tag attr maybeLine)

getAttrValue
  :: ZXML -- ^ The element in which the attribute is being searched
  -> String -- ^ The name of the attribute to look up
  -> XmlValidation String -- ^ An error message or the attribute's value
getAttrValue zxml attr = parseAttrValue zxml attr pure

lookupAttrValue :: ZXML -> String -> Maybe String
lookupAttrValue zxml@ZElem {attrs} attr = lookup attr attrs

zxmlToSetPropertyFromValue :: ZXML -> XmlValidation Step
zxmlToSetPropertyFromValue zxml = do
  propArg <- getAttrValue zxml aProperty
  valueArg <- getAttrValue zxml aValue
  return $ SetPropertyFromValue propArg valueArg

data ShellOrSetPropFromCmd = Shell | SetPropertyFromCommand

validateShellOrSetPropFromCmd :: ShellOrSetPropFromCmd -- ^ Whether the XML is <shell> or <setPropertyFromCommand>
                              -> Maybe String                  -- ^ The (optional) value of the "property" attribute
                              -> Maybe Line                    -- ^ The (optional) line of the XML considered
                              -> XmlValidation ()
validateShellOrSetPropFromCmd Shell Nothing _ = Success ()
validateShellOrSetPropFromCmd SetPropertyFromCommand (Just _) _ = Success ()
validateShellOrSetPropFromCmd Shell (Just _) maybeLine = failWith $ ShellAndProperty maybeLine
validateShellOrSetPropFromCmd SetPropertyFromCommand Nothing maybeLine = failWith $ SetPropertyFromCmdWithoutProperty maybeLine

zxmlToShellCmd :: ZXML -> ShellOrSetPropFromCmd -> XmlValidation Step
zxmlToShellCmd zxml@ZElem {maybeLine} shellOrSetPropertyFromCmd = do
  let workdir = lookupAttrValue zxml "workdir"
      mprop = lookupAttrValue zxml "property"
  validateShellOrSetPropFromCmd shellOrSetPropertyFromCmd mprop maybeLine
  cmd <- parseAttrValue zxml "command" (parseCommand . words)
  return (ShellCmd workdir cmd mprop)
 where
  parseCommand [] = failWith (EmptyCommand maybeLine)
  parseCommand (filepath:args) = pure (Command filepath args)

parseStep :: ZXML -> XmlValidation Step
parseStep zxml@ZElem {tag, maybeLine}
  | tag == tSetProperty = zxmlToSetPropertyFromValue zxml
  | tag == tSetPropertyFromCommand = zxmlToShellCmd zxml SetPropertyFromCommand
  | tag == tShell                  = zxmlToShellCmd zxml Shell
  | otherwise = failWith (UnexpectedTag [tSetProperty, tSetPropertyFromCommand, tShell] tag maybeLine)

zXMLToBuilder :: ZXML -> XmlValidation Builder
zXMLToBuilder zxml@ZElem {children} = do
  let workdir = lookupAttrValue zxml "workdir"
  name <- getAttrValue zxml "name"
  steps <- traverse parseStep children
  return $ Builder workdir name steps

zXMLsToBuilders :: Maybe Line -> [ZXML] -> XmlValidation (NonEmpty Builder)
zXMLsToBuilders maybeLine zxmls =
  case NE.nonEmpty zxmls of
    Just nonEmptyXmls -> traverse zXMLToBuilder nonEmptyXmls
    Nothing -> failWith (NoBuilder maybeLine)

parseEntry :: ZXML -> XmlValidation (String, String)
parseEntry zxml@ZElem {tag, maybeLine}
  | tag == tEntry = zXMLToEntry zxml
  | otherwise = failWith (UnexpectedTag [tEntry] tag maybeLine)

zXMLToEntry :: ZXML -> XmlValidation (String, String)
zXMLToEntry zxml = do
  name <- getAttrValue zxml "name"
  value <- getAttrValue zxml "value"
  return (name, value)

zXMLToSubst :: ZXML -> XmlValidation Subst
zXMLToSubst zxml@ZElem {children, maybeLine} = traverse zXMLToEntry children

data ConfigChildren = ConfigChildren
  { substs :: [ZXML]
  , builders :: [ZXML]
  , unknowns :: [ZXML]
  }

instance Semigroup ConfigChildren where
  (ConfigChildren s1 b1 u1) <> (ConfigChildren s2 b2 u2) =
    ConfigChildren (s1 <> s2) (b1 <> b2) (u1 <> u2)

instance Monoid ConfigChildren where
  mempty = ConfigChildren [] [] []

sortConfigChildren :: [ZXML] -> ConfigChildren
sortConfigChildren = foldMap inject
 where
  inject zxml@ZElem{tag}
    | tag == tBuilder = ConfigChildren [] [zxml] []
    | tag == tSubstitution = ConfigChildren [zxml] [] []
    | otherwise = ConfigChildren [] [] [zxml]

zXMLToConfig :: ZXML -> XmlValidation Config
zXMLToConfig zxml@ZElem {tag, children, maybeLine}
  | tag == tConfig = do
      traverse_ unknownTag otherXmls
      builders <- zXMLsToBuilders maybeLine builderXmls
      substs <- traverse zXMLToSubst substXmls
      return $ Config builders (concat substs)
  | otherwise = failWith $ UnexpectedTag [tConfig] tag maybeLine
 where
  ConfigChildren substXmls builderXmls otherXmls = sortConfigChildren children
  unknownTag ZElem{tag, maybeLine} =
    failWith (UnexpectedTag [tBuilder, tSubstitution] tag maybeLine)

fromJustZXml :: Maybe ZXML -> XmlValidation ZXML
fromJustZXml = maybe (failWith EmptyDocument) pure

textXMLToZXML :: Content -> XmlValidation (Maybe ZXML)
textXMLToZXML (Text CData {cdData, cdLine})
  | all isSpace cdData = pure Nothing
  | otherwise = failWith (UnexpectedText cdLine)
textXMLToZXML (CRef _) = failWith UnexpectedCRef
textXMLToZXML (Elem Element {elName, elAttribs, elContent, elLine}) = do
  children :: [ZXML] <- catMaybes <$> traverse textXMLToZXML elContent
  return $ Just (ZElem tag attrs children elLine)
 where
  tag = qName elName
  attrs = [(qName attrKey, attrVal) | Attr {attrKey, attrVal} <- elAttribs]

parseXmlString
  :: String               -- ^ The XML Content
  -> XmlValidation Config -- ^ Either an error message, or the builders decoded from XML
parseXmlString str =
  case XmlInput.parseXMLDoc str of
    Nothing -> failWith NoRootElement
    Just elem -> transform $ Elem elem
  where
  transform :: Content -> XmlValidation Config
  transform xml =
    textXMLToZXML xml
      `bindValidation` fromJustZXml
      `bindValidation` zXMLToConfig

parseXmlFile
  :: String                    -- ^ A filename
  -> IO (XmlValidation Config) -- ^ The builders
parseXmlFile filepath = do
    handle :: Handle <- openFile filepath ReadMode
    contents :: String <- hGetContents handle
    return $ parseXmlString contents
