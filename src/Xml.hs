{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Xml (
  aCommand
  , aHaltOnFailure
  , aProperty
  , aValue
  , failWith
  , parseXmlFile
  , parseXmlString
  , renderAsXml
  , tBuilder
  , tSetProperty
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
import Data.Void
import System.IO
import Text.Printf
import Text.Read
import Text.XML.Light
import Text.XML.Light.Types

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.XML.Light.Input as XmlInput
import qualified Text.XML.Light.Output as XmlOutput

import Config

---------------------------------------------------
-- Parsing of XML to create instances of Builder --
---------------------------------------------------

-- Attributes
aCommand = "command"
aHaltOnFailure = "haltOnFailure"
aName = "name"
aProperty = "property"
aValue = "value"
aWorkdir = "workdir"

-- Tags
tBuilder, tSetProperty, tShell, tConfig :: String
tBuilder = "builder"
tEntry = "entry"
tForeach = "foreach"
tSetProperty = "setProperty"
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
  | MissingAttribute String String (Maybe Line) -- ^ The tag, the missing attribute, the line
  | NoBuilder (Maybe Line)
  | NotABoolean String String (Maybe Line) -- ^ The attribute that cannot be parsed as a Boolean, the attribute's value, the attribute's line.
  | NoRootElement
  | ShellAndProperty (Maybe Line)
  | SetPropertyFromCmdWithoutProperty (Maybe Line)
  | UnexpectedTag [String] String (Maybe Line) -- ^ The expected tags, the actual tag
  | UnexpectedText (Maybe Line)
  | UnexpectedCRef
  | UnrecognizedStep String (Maybe Line) -- ^ The tag, the line
  | TagRequiresExactlyOneOf String String String (Maybe Line) -- ^ Tag, attr1, attr2, line.
  deriving (Eq, Ord)

instance Show XmlParsingError where
  show (EmptyCommand line) = giveLineInMsg "Commands cannot be empty" line
  show EmptyDocument = "The document is empty"
  show (NoBuilder line) =
    giveLineInMsg "Configurations must have at least one builder" line
  show (NotABoolean attr value line) = giveLineInMsg ("Value of attribute " ++ attr ++ " cannot be parsed as a Boolean: \"" ++ value ++ "\". Expected one of \"True\" \"False\".") line
  show NoRootElement = "Expected exactly one top-level element but found zero"
  show (MissingAttribute elem attr line) =
    giveLineInMsg
      ("Missing attribute in element " ++ elem ++ ": " ++ attr)
      line
  show (ShellAndProperty line) = giveLineInMsg (printf "<%s> should not feature the \"%s\" attribute" tShell aProperty) line
  show (SetPropertyFromCmdWithoutProperty line) = giveLineInMsg (printf "<%s> must feature the \"%s\" attribute" tSetProperty aProperty) line
  show (UnexpectedTag expected actual line) =
    giveLineInMsg
      ("Expected one of " ++ intercalate ", " expected ++ ", got " ++ actual)
      line
  show (UnexpectedText cdLine) = giveLineInMsg "Unexpected Text in XML" cdLine
  show UnexpectedCRef = "Unexpected CRef in XML"
  show (UnrecognizedStep tag line) =
    let recognizedTags = [tSetProperty, tShell]
        msg = if tag `elem` recognizedTags then printf "Invalid attributes in step <%s>" tag
              else printf
                     "Unexpected tag for step: \"%s\". Possible tags of steps are: [%s]"
                     tag $ intercalate ", " (map (\s -> "\"" ++ s ++ "\"") recognizedTags)
    in giveLineInMsg msg line
  show (TagRequiresExactlyOneOf tag attr1 attr2 line) =
    giveLineInMsg
      (printf "<%s> requires either attribute \"%s\" or attribute \"%s\""
       tag
       attr1
       attr2)
      line
    

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

parseBool :: String -- ^ An attribute
          -> String -- ^ The attribute's value
          -> Maybe Line -- ^ The line from which the attribute comes
          -> XmlValidation Bool
parseBool attr value maybeLine = 
  let mvalue :: Maybe Bool = readMaybe value in
  case mvalue of
    Nothing -> failWith $ NotABoolean attr value maybeLine
    Just b -> Success b

parseOptionalBoolAttrValue :: ZXML   -- ^ The element in which the Boolean attribute is being searched
                           -> String -- ^ The name of the attribute to look up
                           -> XmlValidation (Maybe Bool) -- ^ An error message or the attribute's value
parseOptionalBoolAttrValue zxml@ZElem {tag, attrs, maybeLine} attr =
  -- @smelc -> @polux: There's conditional reasoning here, I cannot use applicative do, right?
  case lookupAttrValue zxml attr of
    Nothing -> Success Nothing
    Just value -> Just <$> parseBool attr value maybeLine

getAttrValue
  :: ZXML -- ^ The element in which the attribute is being searched
  -> String -- ^ The name of the attribute to look up
  -> XmlValidation String -- ^ An error message or the attribute's value
getAttrValue zxml attr = parseAttrValue zxml attr pure

lookupAttrValue :: ZXML -> String -> Maybe String
lookupAttrValue zxml@ZElem {attrs} attr = lookup attr attrs

zxmlToSetPropertyFromValue :: ZXML -> XmlValidation (Step Parsed)
zxmlToSetPropertyFromValue zxml = do
  propArg <- getAttrValue zxml aProperty
  valueArg <- getAttrValue zxml aValue
  return $ SetPropertyFromValue propArg valueArg

data ShellOrSetPropFromCmd = Shell | SetPropertyFromCommand

validateShellOrSetPropFromCmd :: ShellOrSetPropFromCmd -- ^ Whether the XML is <shell> or <setProperty>
                              -> Maybe String -- ^ The (optional) value of the "property" attribute
                              -> Maybe Line -- ^ The (optional) line of the XML considered
                              -> XmlValidation ()
validateShellOrSetPropFromCmd Shell Nothing _ = Success ()
validateShellOrSetPropFromCmd SetPropertyFromCommand (Just _) _ = Success ()
validateShellOrSetPropFromCmd Shell (Just _) maybeLine = failWith $ ShellAndProperty maybeLine
validateShellOrSetPropFromCmd SetPropertyFromCommand Nothing maybeLine = failWith $ SetPropertyFromCmdWithoutProperty maybeLine

zxmlToShellCmd :: ZXML -> ShellOrSetPropFromCmd -> XmlValidation (Step Parsed)
zxmlToShellCmd zxml@ZElem {maybeLine} shellOrSetPropertyFromCmd = do
  let workdir = lookupAttrValue zxml aWorkdir
      mprop = lookupAttrValue zxml aProperty
  validateShellOrSetPropFromCmd shellOrSetPropertyFromCmd mprop maybeLine
  cmd <- parseAttrValue zxml aCommand parseCommand
  haltOnFailure <- parseOptionalBoolAttrValue zxml aHaltOnFailure
  return (ShellCmd workdir cmd mprop haltOnFailure)
 where
  parseCommand cmdString
    | all isSpace cmdString = failWith (EmptyCommand maybeLine)
    | otherwise = pure $ Command cmdString

parseStep :: ZXML -> XmlValidation (Step Parsed)
parseStep zxml@ZElem {tag, maybeLine}
  | tag == tSetProperty && isNothing (lookupAttrValue zxml aProperty) =
    failWith (MissingAttribute tag aProperty maybeLine)
  | tag == tSetProperty &&
    all isNothing (map (lookupAttrValue zxml) [aCommand, aValue]) =
    failWith (TagRequiresExactlyOneOf tSetProperty aCommand aValue maybeLine)
  | tag == tSetProperty && isJust (lookupAttrValue zxml aValue) =
    zxmlToSetPropertyFromValue zxml
  | tag == tSetProperty = zxmlToShellCmd zxml SetPropertyFromCommand
  | tag == tShell = zxmlToShellCmd zxml Shell
  | otherwise = failWith (UnrecognizedStep tag maybeLine)

zXMLToBuilder :: ZXML -> XmlValidation (Builder Parsed)
zXMLToBuilder zxml@ZElem {children} = do
  let workdir = lookupAttrValue zxml aWorkdir
  name <- getAttrValue zxml aName
  steps <- traverse parseStep children
  return $ Builder workdir name steps

zXMLsToBuilders :: Maybe Line -> [ZXML] -> XmlValidation (NonEmpty (Builder Parsed))
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
  name <- getAttrValue zxml aName
  value <- getAttrValue zxml aValue
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

zXMLToConfig :: ZXML -> XmlValidation (Config Parsed)
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
  -> XmlValidation (Config Parsed) -- ^ Either an error message, or the builders decoded from XML
parseXmlString str =
  case XmlInput.parseXMLDoc str of
    Nothing -> failWith NoRootElement
    Just elem -> transform $ Elem elem
  where
  transform :: Content -> XmlValidation (Config Parsed)
  transform xml =
    textXMLToZXML xml
      `bindValidation` fromJustZXml
      `bindValidation` zXMLToConfig

parseXmlFile
  :: String                    -- ^ A filename
  -> IO (XmlValidation (Config Parsed)) -- ^ The builders
parseXmlFile filepath = do
    handle :: Handle <- openFile filepath ReadMode
    contents :: String <- hGetContents handle
    return $ parseXmlString contents

----------------------------------------
-- Pretty-printing of Builders as XML --
----------------------------------------

(=:) :: String -> String -> [Attr]
attr =: value = [Attr (unqual attr) value]

(=?) :: String -> Maybe String -> [Attr]
attr =? Nothing = []
attr =? Just value = attr =: value

element :: String -> [Attr] -> [Content] -> Element
element tag attrs children =
  Element
    (unqual tag)
    attrs
    children
    Nothing

stepToXml :: Step Substituted -> Element
stepToXml (SetPropertyFromValue prop value) =
  element
    tSetProperty
    (aProperty =: prop <> aValue =: value)
    []
stepToXml (ShellCmd workdir cmd mprop haltOnFailure) =
  element
    tag
    (aCommand =: show cmd
      <> aWorkdir =: workdir
      <> aProperty =? mprop
      <> aHaltOnFailure =: show haltOnFailure)
    []
 where
  tag | isJust mprop = tSetProperty
      | otherwise = tShell
stepToXml (Ext ext) = absurd ext

substToXml :: Subst -> Element
substToXml subst =
  element tSubstitution [] (map (Elem . entryToXml) subst)

entryToXml :: (String, String) -> Element
entryToXml (name, value) =
  element tEntry (aName =: name <> aValue =: value) []

builderToXml :: Builder Substituted -> Element
builderToXml (Builder () name steps) =
  element tBuilder [] (map (Elem . stepToXml) steps)

configToXml :: Config Substituted -> Element
configToXml (Config builders ()) =
  element tConfig [] (toList (fmap (Elem . builderToXml) builders))

renderAsXml :: Config Substituted -> String
renderAsXml x = XmlOutput.ppElement (configToXml x)
