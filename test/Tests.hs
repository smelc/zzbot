import Test.Hspec

import ConfigSpec
import ExecSpec

main = hspec $ do
    describe "Config" ConfigSpec.spec
    describe "Exec" ExecSpec.spec