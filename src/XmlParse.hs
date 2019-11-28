{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}

module XmlParse where

import Data.Char
import Data.Either
import Data.Either.Combinators
import Data.Functor.Alt
import Data.List
import Data.Maybe
import Data.Validation
import System.IO
import Text.XML.Light.Types

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Text.XML.Light.Input as XmlInput

import Config

---------------------------------------------------
-- Parsing of XML to create instances of Builder --
---------------------------------------------------

tBuilder, tSetProperty, tShell :: String
tBuilder = "builder"
tSetProperty = "setProperty"
tShell = "shell"

giveLineInMsg :: String -> Maybe Line -> String
giveLineInMsg errMsg Nothing = errMsg
giveLineInMsg errMsg (Just line) = "At line " ++ show line ++ ": " ++ errMsg

data ZXML = ZElem {tag :: String, attrs :: [(String, String)], children :: [ZXML], maybeLine :: Maybe Line}

data XmlParsingError
  = EmptyDocument
  | MissingAttribute String String
  | NotExactlyOneRoot Int -- ^ The number of root elements found
  | UnexpectedTag [String] String (Maybe Line) -- ^ The expected tags, the actual tag
  | UnexpectedText (Maybe Line)
  | UnexpectedCRef
  deriving (Eq, Ord)

instance Show XmlParsingError where
  show EmptyDocument = "The document is empty"
  show (NotExactlyOneRoot n) = "Expected exactly one top-level element but found " ++ show n
  show (MissingAttribute elem attr) =
    "Missing attribute in element " ++ elem ++ ": " ++ attr
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

getAttrValue :: String -- ^ The element in which the attribute is being searched (for error messages)
             -> String -- ^ The attribute's name
             -> [(String, String)] -- ^ The actual list of attributes, with the value
             -> XmlValidation String -- ^ An error message or the attribute's value
getAttrValue elem attr attrs =
  case lookup attr attrs of
    Just value -> Success value
    Nothing -> failWith (MissingAttribute elem attr)

zxmlToSetPropertyFromValue :: ZXML -> XmlValidation Step
zxmlToSetPropertyFromValue zxml@ZElem {attrs} = do
  propArg <- getAttrValue tSetProperty "property" attrs
  valueArg <- getAttrValue tSetProperty "value" attrs
  return $ SetPropertyFromValue propArg valueArg

zxmlToShellCmd :: ZXML -> XmlValidation Step
zxmlToShellCmd zxml@ZElem {attrs} = do
  cmdArg <- words <$> getAttrValue tShell "command" attrs
  return $ ShellCmd cmdArg

parseStep :: ZXML -> XmlValidation Step
parseStep zxml@ZElem {tag, maybeLine}
  | tag == tSetProperty = zxmlToSetPropertyFromValue zxml
  | tag == tShell = zxmlToShellCmd zxml
  | otherwise = failWith (UnexpectedTag [tSetProperty, tShell] tag maybeLine)

zXMLToBuilder :: ZXML -> XmlValidation Builder
zXMLToBuilder zxml@ZElem {attrs, children} = do
  name <- getAttrValue tBuilder "name" attrs
  steps <- traverse parseStep children
  return $ Builder name steps

parseBuilder :: ZXML -> XmlValidation Builder
parseBuilder zxml@ZElem {tag, maybeLine}
  | tag == tBuilder = zXMLToBuilder zxml
  | otherwise = failWith (UnexpectedTag [tBuilder] tag maybeLine)

zXMLToBuilders :: ZXML -> XmlValidation [Builder]
zXMLToBuilders zxml@ZElem {children, maybeLine} =
  if actualTag == expectedTag
  then traverse parseBuilder children
  else failWith $ UnexpectedTag [expectedTag] actualTag maybeLine
  where actualTag = tag zxml
        expectedTag = "config"

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

parseXmlString :: String                  -- ^ The XML Content
               -> XmlValidation [Builder] -- ^ Either an error message, or the builders decoded from XML
parseXmlString str =
  if lzxml == 1
  then transform $ head zxml
  else failWith $ NotExactlyOneRoot lzxml
  where
  zxml :: [Content] = XmlInput.parseXML str
  lzxml = length zxml
  transform :: Content -> XmlValidation [Builder]
  transform xml =
    textXMLToZXML xml
      `bindValidation` fromJustZXml
      `bindValidation` zXMLToBuilders

parseXmlFile :: String                       -- ^ A filename
             -> IO (XmlValidation [Builder]) -- ^ The builders
parseXmlFile filepath = do
    handle :: Handle <- openFile filepath ReadMode
    contents :: String <- hGetContents handle
    return $ parseXmlString contents
