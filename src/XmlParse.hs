{-# LANGUAGE ScopedTypeVariables #-}

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

-- Check that Content is an Element with the given name
checkElementAndName :: String -> Content -> Either String Element
checkElementAndName expected (Elem res@Element {elName=elName, elLine=elLine}) =
    case elName of
        QName {qName=qName} ->
            if qName == expected then Right res
            else Left $ giveLineInMsg errMsg elLine
            where errMsg = "Element has name " ++ qName ++ " while " ++ expected ++ " was expected"
checkElementAndName expected _ = Left $ "Element named " ++ expected ++ " not found"

type ZZAttr = (String, String) -- ^ An attribute name, and its value

-- | Translate xml's Attr to ZZAttr. The translation ignores qURI and qPrefix
zzAttrs :: [Attr] -> [ZZAttr]
zzAttrs = map zzAttr
    where zzAttr Attr {attrKey=attrKey, attrVal=val} = (extractName attrKey, val)
          extractName QName {qName=qName} = qName

parseAttribute :: String     -- ^ The element being inspected
               -> String     -- ^ A required attribute
               -> [ZZAttr]   -- ^ The actual attributes
               -> Maybe Line -- ^ The line of the element being parsed
               -> Either String (String, String) -- ^ Either an error message
                                                 -- or the attribute and its value
parseAttribute element attr actuals maybeLine = 
    case find (\pair -> fst pair == attr) actuals of
        Nothing -> Left $ giveLineInMsg ("Attribute " ++ attr ++ " not found. Arguments found: " ++ show(map fst actuals)) maybeLine
        Just (_, v) -> Right (attr, v)


parseAttributes :: String     -- ^ The element being inspected
                -> [String]   -- ^ The required attributes
                -> [ZZAttr]   -- ^ The actual attributes
                -> Maybe Line -- ^ The line of the element being parsed
                -> Either String (Map.Map String String) -- ^ Either an error message
                -- or a mapping from the required attributes to their values
parseAttributes element required actuals maybeLine = 
    if not $ null errors
    then Left $ unlines errors
    else Right $ Map.fromList mappings
    where eithers :: [Either String (String, String)]
          eithers = map (\attr -> parseAttribute element attr actuals maybeLine) required
          (errors, mappings) = partitionEithers eithers

parseSetProperty :: [ZZAttr] -> Maybe Line -> Either String Step
parseSetProperty actuals mLine = do
    attrs <- parseAttributes "setProperty" [propertyAttr, valueAttr] actuals mLine
    return SetPropertyFromValue{prop = fromJust $ Map.lookup propertyAttr attrs, 
                                value = fromJust $ Map.lookup valueAttr attrs}
    where propertyAttr = "property"
          valueAttr = "value"

parseShell :: [ZZAttr] -> Maybe Line -> Either String Step
parseShell actuals mLine = do
    attrs <- parseAttributes "shell" [commandAttr] actuals mLine
    return ShellCmd{cmd = words (fromJust $ Map.lookup commandAttr attrs)} 
    where commandAttr = "command"

parseStep :: Content -> Either String Step
parseStep (Elem Element {elName=elName, elAttribs=elAttribs, elLine=elLine}) =
    case elName of
        QName {qName=qName}
            | qName == "setProperty" -> parseSetProperty (zzAttrs elAttribs) elLine
            | qName == "shell"       -> parseShell (zzAttrs elAttribs) elLine
            | otherwise              -> Left $ giveLineInMsg ("Unexpected element: " ++ qName ++ ". Expected one of: 'setProperty', 'shell'") elLine
parseStep content = Left $ "Cannot parse Step from " ++ show content

-- Parses a builder from a Content
parseBuilderContent :: Content -> Either String Builder
parseBuilderContent content = do
    builder <- checkElementAndName "builder" content
    undefined

-- Parses builders from a String
parseXmlString :: String -> IO Builder
parseXmlString xml =
    undefined
    where contents :: [Content] = XmlInput.parseXML xml

parseXmlFile :: String     -- ^ A filename
             -> IO Builder -- ^ The builder
parseXmlFile filepath = do
    handle :: Handle <- openFile filepath ReadMode  
    contents :: String <- hGetContents handle 
    parseXmlString contents