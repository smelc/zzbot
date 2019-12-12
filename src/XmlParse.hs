{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}

module XmlParse (
  duplicates
  , failWith
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
tEntry = "entry"
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
  = DuplicateSubstEntries [String] (Maybe Line)
  | EmptyCommand (Maybe Line)
  | EmptyDocument
  | MissingAttribute String String (Maybe Line)
  | NoBuilder (Maybe Line)
  | NoRootElement
  | UnexpectedTag [String] String (Maybe Line) -- ^ The expected tags, the actual tag
  | UnexpectedText (Maybe Line)
  | UnexpectedCRef
  deriving (Eq, Ord)

instance Show XmlParsingError where
  show (DuplicateSubstEntries entries line) = giveLineInMsg ("Some substitutions members are mapped more thance once: " ++ intercalate "," entries) line
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
  | tag == tShell       = zxmlToShellCmd zxml
  | otherwise = failWith (UnexpectedTag [tSetProperty, tShell] tag maybeLine)

zXMLToBuilder :: ZXML -> XmlValidation Builder
zXMLToBuilder zxml@ZElem {children} = do
  let workdir = lookupAttrValue zxml "workdir"
  name <- getAttrValue zxml "name"
  steps <- traverse parseStep children
  return $ Builder workdir name steps

duplicates
  :: (Eq a, Ord a)
  => [a] -- ^ The list to look for duplicates in
  -> [a]
duplicates elems = Map.keys (Map.filter (>1) counts)
 where
  counts = Map.unionsWith (+) (map singleton elems)
  singleton elem = Map.singleton elem 1

zXMLToSubst :: ZXML -> XmlValidation Subst
zXMLToSubst zxml@ZElem {children, maybeLine} =
  let ventries :: XmlValidation [(String, String)] = traverse zXMLToEntry children in
      case ventries of
        Failure err -> Failure err
        Success entries ->
          let ds = duplicates $ map fst entries in
          case ds of
            []    -> Success entries
            probs -> failWith (DuplicateSubstEntries probs maybeLine)
        where zXMLToEntry :: ZXML -> XmlValidation (String, String)
              zXMLToEntry zxml = do
                name <- getAttrValue zxml "name"
                value <- getAttrValue zxml "value"
                return (name, value)

parseLevel1 :: ZXML -> XmlValidation (Either Subst Builder)
-- ^ Parse the content below <config> i.e. right below the top level element
parseLevel1 zxml@ZElem {tag, maybeLine}
  | tag == tBuilder      = Right <$> zXMLToBuilder zxml
  | tag == tSubstitution = Left <$> zXMLToSubst zxml
  | otherwise = failWith (UnexpectedTag [tBuilder, tSubstitution] tag maybeLine)

zXMLToConfig :: ZXML -> XmlValidation Config
zXMLToConfig zxml@ZElem {tag, children, maybeLine}
  | tag == tConfig =
      case NE.nonEmpty children of
        Just level1 -> traverse parseLevel1 level1 `bindValidation` (helper . NE.toList)
        Nothing -> failWith (NoBuilder maybeLine)
  | otherwise = failWith $ UnexpectedTag [tConfig] tag maybeLine
  where helper :: [Either Subst Builder] -> XmlValidation Config
        helper es =
          let (substs, builders) = partitionEithers es
          in case builders of
               [] -> failWith (NoBuilder maybeLine)
               (hd:tl) -> Success $ Config (hd NE.:| tl) (concat substs)

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
