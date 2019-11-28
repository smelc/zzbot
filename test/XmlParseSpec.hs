module XmlParseSpec (spec) where

import Config
import Control.Lens.Extras
import Data.Validation
import XmlParse
import System.Exit
import Test.Hspec

badXml1 = "<foobar></foobar>"

validXml =
  "<builder name=\"ls builder\">\
  \  <shell command=\"ls /\"/>\
  \  <setProperty property=\"prop\" value=\"foobar\"/>\
  \</builder>"

expectedBuilder =
  Builder
    { name = "ls builder"
    , steps =
      [ ShellCmd { cmd = ["ls", "/"] }
      , SetPropertyFromValue { prop = "prop", value = "foobar" }
      ]
    }

spec :: SpecWith ()
spec =
  describe "parseXmlString" $ do
    it "should succeed on valid XLM" $
      parseXmlString validXml `shouldBe` Success [expectedBuilder]
    it "should fail on bad toplevel element" $
      parseXmlString badXml1 `shouldSatisfy` is _Failure
