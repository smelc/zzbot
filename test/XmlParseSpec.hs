module XmlParseSpec (spec) where

import Config
import Data.Validation
import XmlParse
import System.Exit
import Test.Hspec

isFailure :: Validation err a -> Bool
isFailure (Failure _)  = True
isFailure (Success _)  = False

isSuccess :: Validation err a -> Bool
isSuccess = not . isFailure

badXml1 = "<foobar></foobar>"

spec :: SpecWith ()
spec =
  describe "testBadTopLevelElement" $
    it "" $ do
        print failure1
        isFailure failure1 `shouldBe` True
  where failure1 = parseXmlString badXml1