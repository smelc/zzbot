{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

module XmlParse where

import Data.Either
import Data.List
import Data.Maybe
import System.IO
import Text.XML.Light.Types

import qualified Data.Map.Strict as Map
import qualified Text.XML.Light.Input as XmlInput

import Config

---------------------------------------------------
-- Parsing of XML to create instances of Builder --
---------------------------------------------------

giveLineInMsg :: String -> Maybe Line -> String
giveLineInMsg errMsg Nothing = errMsg
giveLineInMsg errMsg (Just line) = "At line " ++ show line ++ ": " ++ errMsg

data ZXML = ZElem {tag :: String, attrs :: [(String, String)], maybeLine :: Maybe Line}

getAttrValue :: String -- ^ The element in which the attribute is being searched (for error messages)
             -> String -- ^ The attribute's name
             -> [(String, String)] -- ^ The actual list of attributes, with the value
             -> Either String String -- ^ An error message or the attribute's value
getAttrValue elem attr attrs =
    case lookup attr attrs of
        Nothing    -> Left ("Missing attribute in element " ++ elem ++ ": " ++ attr)
        Just value -> Right value

zXMLToBuilder :: ZXML -> Either String Builder
zXMLToBuilder zxml = do
    name <- getAttrValue elem "name" zattrs
    undefined
    where elem = "builder"
          zattrs = attrs zxml

textXMLToZXML :: Content -> Either String ZXML
textXMLToZXML (Text CData {cdLine}) = Left $ giveLineInMsg "Unexpected Text in XML" cdLine
textXMLToZXML (CRef _)              = Left "Unexpected CRef in XML"
textXMLToZXML (Elem Element {elName, elAttribs, elLine}) =
    return $ ZElem tag attrs elLine
    where tag :: String = qName elName
          attrs = map (\elAttr -> (qName $ attrKey elAttr, attrVal elAttr)) elAttribs

parseXmlString :: String                  -- ^ The XML Content
               -> Either String [Builder] -- ^ Either an error message, or the builders decoded from XML
parseXmlString xml =
    undefined
    where contents :: [Content] = XmlInput.parseXML xml
          zxmls = map textXMLToZXML contents

parseXmlFile :: String                       -- ^ A filename
             -> IO (Either String [Builder]) -- ^ The builders
parseXmlFile filepath = do
    handle :: Handle <- openFile filepath ReadMode  
    contents :: String <- hGetContents handle 
    return $ parseXmlString contents