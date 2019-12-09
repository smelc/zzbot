{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}

module XmlParse (
  failWith
  , parseXmlFile
  , parseXmlString
  , XmlParsingError(..)
  , XmlValidation
) where

import Data.Char
import Data.Either
import Data.Either.Combinators
import Data.Functor.Alt
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Validation
import System.IO
import Text.XML.Light.Types

import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.XML.Light.Input as XmlInput

import Config

---------------------------------------------------
-- Parsing of XML to create instances of Builder --
---------------------------------------------------

tBuilder, tSetProperty, tShell, tConfig :: String
tBuilder = "builder"
tSetProperty = "setProperty"
tShell = "shell"
tConfig = "config"

giveLineInMsg :: String -> Maybe Line -> String
giveLineInMsg errMsg Nothing = errMsg
giveLineInMsg errMsg (Just line) = "At line " ++ show line ++ ": " ++ errMsg

data ZXML = ZElem {tag :: String, attrs :: [(String, String)], children :: [ZXML], maybeLine :: Maybe Line}

data XmlParsingError
  = EmptyCommand (Maybe Line)
  | EmptyDocument
  | MissingAttribute String String (Maybe Line)
  | NoBuilder (Maybe Line)
  | NoRootElement
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
  propArg <- getAttrValue zxml "property"
  valueArg <- getAttrValue zxml "value"
  return $ SetPropertyFromValue propArg valueArg

zxmlToShellCmd :: ZXML -> XmlValidation Step
zxmlToShellCmd zxml@ZElem {maybeLine} = do
  let workdir = lookupAttrValue zxml "workdir"
  cmd <- parseAttrValue zxml "command" (parseCommand . words)
  return (ShellCmd workdir cmd)
 where
  parseCommand [] = failWith (EmptyCommand maybeLine)
  parseCommand (filepath:args) = pure (Command filepath args)

parseStep :: ZXML -> XmlValidation Step
parseStep zxml@ZElem {tag, maybeLine}
  | tag == tSetProperty = zxmlToSetPropertyFromValue zxml
  | tag == tShell = zxmlToShellCmd zxml
  | otherwise = failWith (UnexpectedTag [tSetProperty, tShell] tag maybeLine)

zXMLToBuilder :: ZXML -> XmlValidation Builder
zXMLToBuilder zxml@ZElem {children} = do
  let workdir = lookupAttrValue zxml "workdir"
  name <- getAttrValue zxml "name"
  steps <- traverse parseStep children
  return $ Builder workdir name steps

parseBuilder :: ZXML -> XmlValidation Builder
parseBuilder zxml@ZElem {tag, maybeLine}
  | tag == tBuilder = zXMLToBuilder zxml
  | otherwise = failWith (UnexpectedTag [tBuilder] tag maybeLine)

zXMLToBuilders :: ZXML -> XmlValidation (NonEmpty Builder)
zXMLToBuilders zxml@ZElem {tag, children, maybeLine}
  | tag == tConfig =
      case NE.nonEmpty children of
        Just builders -> traverse parseBuilder builders
        Nothing -> failWith (NoBuilder maybeLine)
  | otherwise = failWith $ UnexpectedTag [tConfig] tag maybeLine

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
  :: String                           -- ^ The XML Content
  -> XmlValidation (NonEmpty Builder) -- ^ Either an error message, or the builders decoded from XML
parseXmlString str =
  case XmlInput.parseXMLDoc str of
    Nothing -> failWith NoRootElement
    Just elem -> transform $ Elem elem
  where
  transform :: Content -> XmlValidation (NonEmpty Builder)
  transform xml =
    textXMLToZXML xml
      `bindValidation` fromJustZXml
      `bindValidation` zXMLToBuilders

parseXmlFile
  :: String                                -- ^ A filename
  -> IO (XmlValidation (NonEmpty Builder)) -- ^ The builders
parseXmlFile filepath = do
    handle :: Handle <- openFile filepath ReadMode
    contents :: String <- hGetContents handle
    return $ parseXmlString contents
