import Test.Hspec

import ConfigSpec
import ExecSpec
import XmlSpec
import ZZBotSpec

main = hspec $ do
    describe "Config" ConfigSpec.spec
    describe "Exec" ExecSpec.spec
    describe "Xml" XmlSpec.spec
    describe "ZZBot" ZZBotSpec.spec
