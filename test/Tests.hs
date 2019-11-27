import Test.Hspec

import ConfigSpec
import ExecSpec
import XmlParseSpec

main = hspec $ do
    describe "Config" ConfigSpec.spec
    describe "Exec" ExecSpec.spec
    describe "XmlParse" XmlParseSpec.spec