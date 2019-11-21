{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module XmlParse where

import Data.Either
import Data.Either.Combinators
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
  | UnexpectedText (Maybe Line)
  | UnexpectedCRef
  deriving (Eq, Ord)

instance Show XmlParsingError where
  show (MissingAttribute elem attr) =  "Missing attribute in element " ++ elem ++ ": " ++ attr
  show (UnexpectedText cdLine) = giveLineInMsg "Unexpected Text in XML" cdLine
  show UnexpectedCRef = "Unexpected CRef in XML"

type XmlParsingErrors = Set.Set XmlParsingError

getAttrValue :: String -- ^ The element in which the attribute is being searched (for error messages)
             -> String -- ^ The attribute's name
             -> [(String, String)] -- ^ The actual list of attributes, with the value
             -> Validation XmlParsingErrors String -- ^ An error message or the attribute's value
getAttrValue elem attr attrs =
  validate (Set.singleton $ MissingAttribute elem attr) (lookup attr) attrs

zXMLToStep :: ZXML -> Validation XmlParsingErrors Step
zXMLToStep zxml = undefined

zXMLToBuilder :: ZXML -> Validation XmlParsingErrors Builder
zXMLToBuilder zxml = Builder <$> name <*> steps
 where
  name = getAttrValue "builder" "name" (attrs zxml)
  steps = traverse zXMLToStep (children zxml)

textXMLToZXML :: Content -> Validation XmlParsingErrors ZXML
textXMLToZXML (Text CData {cdLine}) = Failure (Set.singleton $ UnexpectedText cdLine)
textXMLToZXML (CRef _)              = Failure (Set.singleton UnexpectedCRef)
textXMLToZXML (Elem Element {elName, elAttribs, elContent, elLine}) =
  ZElem tag attrs <$> children <*> pure elLine
 where
  tag = qName elName
  attrs = [(qName attrKey, attrVal) | Attr {attrKey, attrVal} <- elAttribs]
  children = traverse textXMLToZXML elContent

parseXmlString :: String                    -- ^ The XML Content
               -> Either [String] [Builder] -- ^ Either an error message, or the builders decoded from XML
parseXmlString xml =
    undefined
    where contents :: [Content] = XmlInput.parseXML xml
          zxmls = map textXMLToZXML contents

parseXmlFile :: String                         -- ^ A filename
             -> IO (Either [String] [Builder]) -- ^ The builders
parseXmlFile filepath = do
    handle :: Handle <- openFile filepath ReadMode
    contents :: String <- hGetContents handle
    return $ parseXmlString contents