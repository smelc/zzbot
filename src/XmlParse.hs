{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}

module XmlParse where

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

giveLineInMsg :: String -> Maybe Line -> String
giveLineInMsg errMsg Nothing = errMsg
giveLineInMsg errMsg (Just line) = "At line " ++ show line ++ ": " ++ errMsg

data ZXML = ZElem {tag :: String, attrs :: [(String, String)], children :: [ZXML], maybeLine :: Maybe Line}

data XmlParsingError
  = MissingAttribute String String
  | UnexpectedTag String String (Maybe Line) -- ^ The expected tag, the actual tag
  | UnexpectedText (Maybe Line)
  | UnexpectedCRef
  | UnrecognizedStep (Maybe Line)
  deriving (Eq, Ord)

instance Show XmlParsingError where
  show (MissingAttribute elem attr) = "Missing attribute in element " ++ elem ++ ": " ++ attr
  show (UnexpectedTag expected actual line) = giveLineInMsg ("Expected " ++ expected ++ ", got " ++ actual) line
  show (UnexpectedText cdLine) = giveLineInMsg "Unexpected Text in XML" cdLine
  show UnexpectedCRef = "Unexpected CRef in XML"
  show (UnrecognizedStep cdLine) = giveLineInMsg "Unrecognized step" cdLine

type XmlParsingErrors = Set.Set XmlParsingError
type XmlValidation = Validation XmlParsingErrors

failWith :: XmlParsingError -> XmlValidation a
failWith error = Failure (Set.singleton error)

checkTag :: String -- ^ The expected tag
         -> ZXML   -- ^ The element to check
         -> XmlValidation ()
-- ^ Checks that the tag is the first argument.
checkTag tag ZElem {tag=actual, maybeLine} =
  if actual == tag
    then Success ()
    else failWith (UnexpectedTag tag actual maybeLine)

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
  checkTag tag zxml
  propArg <- getAttrValue tag "property" attrs
  valueArg <- getAttrValue tag "value" attrs
  return $ SetPropertyFromValue propArg valueArg
  where tag = "setProperty"

zxmlToShellCmd :: ZXML -> XmlValidation Step
zxmlToShellCmd zxml@ZElem {attrs} = do
  checkTag tag zxml
  cmdArg <- words <$> getAttrValue tag "command" attrs
  return $ ShellCmd cmdArg
  where tag = "shell"

zXMLToStep :: ZXML -> XmlValidation Step
zXMLToStep zxml =
  setPropertyFromValue <!> shellCmd <!> failWith (UnrecognizedStep (maybeLine zxml))
  where setPropertyFromValue = zxmlToSetPropertyFromValue zxml
        shellCmd = zxmlToShellCmd zxml

zXMLToBuilder :: ZXML -> XmlValidation Builder
zXMLToBuilder zxml@ZElem {attrs, children} = do
  checkTag tag zxml
  name <- getAttrValue tag "name" attrs
  steps <- traverse zXMLToStep children
  return $ Builder name steps
 where
  tag = "builder"

maybezXMLToBuilder :: XmlValidation ZXML -> XmlValidation Builder
maybezXMLToBuilder vzxml = bindValidation vzxml zXMLToBuilder

textXMLToZXML :: Content -> XmlValidation ZXML
textXMLToZXML (Text CData {cdLine}) = failWith (UnexpectedText cdLine)
textXMLToZXML (CRef _)              = failWith UnexpectedCRef
textXMLToZXML (Elem Element {elName, elAttribs, elContent, elLine}) = do
  children :: [ZXML] <- traverse textXMLToZXML elContent
  return $ ZElem tag attrs children elLine
 where
  tag = qName elName
  attrs = [(qName attrKey, attrVal) | Attr {attrKey, attrVal} <- elAttribs]

-- @polux: Once again I feel there must be a function to do that, I mean
-- I'm just accumulating errors which is what Validation is about
split :: [XmlValidation a] -> (XmlParsingErrors, [a])
split (Failure err:tl) = (Set.union err (fst rest), snd rest) where rest = split tl
split (Success e:tl) = (fst rest, e:snd rest)                 where rest = split tl
split [] = (Set.empty, [])

parseXmlString :: String                  -- ^ The XML Content
               -> XmlValidation [Builder] -- ^ Either an error message, or the builders decoded from XML
parseXmlString xml =
  if null errs then Success bs
  else Failure errs
  where contents :: [Content] = XmlInput.parseXML xml
        builders :: [XmlValidation Builder] = map (maybezXMLToBuilder . textXMLToZXML) contents
        (errs, bs) = split builders

parseXmlFile :: String                       -- ^ A filename
             -> IO (XmlValidation [Builder]) -- ^ The builders
parseXmlFile filepath = do
    handle :: Handle <- openFile filepath ReadMode
    contents :: String <- hGetContents handle
    return $ parseXmlString contents
