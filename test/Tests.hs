import Test.Hspec

import ConfigSpec
import ExecSpec
import XmlSpec

main = hspec $ do
    describe "Config" ConfigSpec.spec
    describe "Exec" ExecSpec.spec
    describe "Xml" XmlSpec.spec
