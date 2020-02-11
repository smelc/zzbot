{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module XmlSpec (spec) where

import Config
import Data.List.NonEmpty (NonEmpty(..))
import Data.Validation
import Data.Void
import Debug.Trace
import Xml
import RandomConfigs
import System.Exit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Failure, Success)
import Text.Printf

import qualified Data.Set as Set


badXml1 = "<config><builder name=\"foo\"/><foobar></foobar></config>"
expectedResultForBadXml1 = failWith (UnexpectedTag [tBuilder, "substitution"] "foobar" (Just 1))

badXml2 = "<config><builder></builder></config>"
expectedResultForBadXml2 = failWith (MissingAttribute tBuilder "name" (Just 1))

badXml3 = "<config><builder name=\"foo\"><foobar/></builder></config>"
expectedResultForBadXml3 = failWith (UnrecognizedStep "foobar" (Just 1))

badXml4 = "<config><builder name=\"foo\"><shell/></builder></config>"
expectedResultForBadXml4 = failWith (MissingAttribute tShell "command" (Just 1))

badXml5 = "<config><builder name=\"foo\"><setProperty value=\"bar\"/></builder></config>"
expectedResultForBadXml5 =failWith (MissingAttribute tSetProperty aProperty (Just 1))

badXml6 = "<config><builder name=\"foo\"><setProperty property=\"foo\"/></builder></config>"
expectedResultForBadXml6 = failWith (TagRequiresExactlyOneOf tSetProperty aCommand aValue (Just 1))

badXml7 = "<config>\n<builder>\n<shell/>\n<setProperty/>\n<unknown/>\n</builder>\n</config>"
expectedResultForBadXml7 =
  Failure $
    Set.fromList
      [ UnrecognizedStep "unknown" (Just 5)
      , MissingAttribute tBuilder "name" (Just 2)
      , MissingAttribute tShell "command" (Just 3)
      , MissingAttribute tSetProperty aProperty (Just 4)
      ]

badXml8 = ""
expectedResultForBadXml8 = failWith NoRootElement

badXml9 = "<config>aa<builder name=\"foo\"/></config>"
expectedResultForBadXml9 = failWith (UnexpectedText (Just 1))

badXml10 = "<config></config>"
expectedResultForBadXml10 = failWith (NoBuilder (Just 1))

badXml11 =
  "<config>\n\
  \  <builder name=\"foo\">\n\
  \    <shell command=\"\"/>\n\
  \    <shell command=\" \"/>\n\
  \    <shell command=\"  \"/>\n\
  \  </builder>\n\
  \</config>"
expectedResultForBadXml11 =
  Failure $
    Set.fromList
      [ EmptyCommand (Just 3)
      , EmptyCommand (Just 4)
      , EmptyCommand (Just 5)
      ]

badXml12 = "<config><substitution><entry/></substitution></config>"
expectedResultForBadXml12 =
  Failure $
    Set.fromList
      [ NoBuilder (Just 1)
      , MissingAttribute "entry" "name" (Just 1)
      , MissingAttribute "entry" aValue (Just 1)
      ]

badXml13 =
  "<config>\
  \  <builder name=\"foo\"/>\
  \  <substitution>\
  \    <entry name=\"aa\"/>\
  \  </substitution>\
  \</config>"
expectedResultForBadXml13 =
  failWith (MissingAttribute "entry" aValue (Just 1))

badXml14 =
  "<config>\
  \  <builder name=\"foo\"/>\
  \  <substitution>\
  \    <entry value=\"11\"/>\
  \  </substitution>\
  \</config>"
expectedResultForBadXml14 =
  failWith (MissingAttribute "entry" "name" (Just 1))

badXml15 =
  "<config>\n\
  \  <builder name=\"foo\">\n\
  \    <shell command=\"ls\" property=\"prop\"/>\n\
  \  </builder>\n\
  \</config>"
expectedResultForBadXml15 = Failure $ Set.fromList [ ShellAndProperty (Just 3) ]

badXml16 =
  "<config>\n\
  \  <builder name=\"foo\">\n\
  \    <shell command=\"ls\" haltOnFailure=\"bad\"/>\n\
  \  </builder>\n\
  \</config>"
expectedResultForBadXml16 = Failure $ Set.fromList [ NotABoolean aHaltOnFailure "bad" (Just 3) ]

badXml17 =
  "<config>\n\
  \  <builder name=\"foo\">\n\
  \    <setProperty command=\"ls\" property=\"whatever\" haltOnFailure=\"foo\"/>\n\
  \  </builder>\n\
  \</config>"
expectedResultForBadXml17 = Failure $ Set.fromList [ NotABoolean aHaltOnFailure "foo" (Just 3) ]

badXml18 =
  "<config>\n\
  \  <builder name=\"foo\">\n\
  \    <shell command=\"ls\" ignoreFailure=\"very bad\"/>\n\
  \  </builder>\n\
  \</config>"
expectedResultForBadXml18 = Failure $ Set.fromList [ NotABoolean aIgnoreFailure "very bad" (Just 3) ]

badXml19 =
  "<config>\n\
  \  <builder name=\"foo\">\n\
  \    <setProperty command=\"ls\" property=\"whatever\" ignoreFailure=\"wrong\"/>\n\
  \  </builder>\n\
  \</config>"
expectedResultForBadXml19 = Failure $ Set.fromList [ NotABoolean aIgnoreFailure "wrong" (Just 3) ]

validXml =
  "<config>\
  \  <substitution>\
  \    <entry name=\"aa\" value=\"11\"/>\
  \    <entry name=\"aa\" value=\"11\"/>\
  \    <entry name=\"bb\" value=\"22\"/>\
  \  </substitution>\
  \  <builder workdir=\"dir1\" name=\"ls builder\">\
  \    <shell workdir=\"dir2\" command=\"ls /\" haltOnFailure=\"True\" ignoreFailure=\"True\"/>\
  \    <setProperty property=\"prop\" value=\"foobar\"/>\
  \  </builder>\
  \</config>"
expectedResultForValidXml = Success config
 where
  builder =
    Builder
      { workdir = Just "dir1"
      , name = "ls builder"
      , steps =
        [ ShellCmd
          {
              workdir = Just "dir2"
            , cmd = Command "ls /"
            , mprop = Nothing
            , haltOnFailure=Just True
            , ignoreFailure=Just True
          }
        , SetPropertyFromValue { prop = "prop", value = "foobar" }
        ]
      }
  subst = [("aa", "11"), ("aa", "11"), ("bb", "22")]
  config = Config (builder :| []) subst


spec :: SpecWith ()
spec =
  describe "parseXmlString" $ do
    -- configs of size 40 are already very large
    modifyMaxSize (const 40) $
      it "should be the inverse of renderXml" $
        property $ \config ->
          case normalizeTestConfig config of
            Failure _ ->
              -- we discard this config as it does not normalize
              discard
            Success substitutedConfig ->
              normalizeTestConfig <$> parseXmlString (renderAsXml substitutedConfig)
              `shouldBe`
              Success (Success substitutedConfig)
    it "should succeed on valid XML" $
      parseXmlString validXml `shouldBe` expectedResultForValidXml
    it "should fail on bad toplevel element" $
      parseXmlString badXml1 `shouldBe` expectedResultForBadXml1
    it "should fail on missing name in builder" $
      parseXmlString badXml2 `shouldBe` expectedResultForBadXml2
    it "should fail on bad step element" $
      parseXmlString badXml3 `shouldBe` expectedResultForBadXml3
    it "should fail on missing command in shell" $
      parseXmlString badXml4 `shouldBe` expectedResultForBadXml4
    it "should fail on missing property in setProperty" $
      parseXmlString badXml5 `shouldBe` expectedResultForBadXml5
    it "should fail on missing value in setProperty" $
      parseXmlString badXml6 `shouldBe` expectedResultForBadXml6
    it "should collect many errors" $
      parseXmlString badXml7 `shouldBe` expectedResultForBadXml7
    it "should fail on empty document" $
      parseXmlString badXml8 `shouldBe` expectedResultForBadXml8
    it "should fail on text leaf" $
      parseXmlString badXml9 `shouldBe` expectedResultForBadXml9
    it "should fail on configs with no builder" $
      parseXmlString badXml10 `shouldBe` expectedResultForBadXml10
    it "should fail on empty shell commands" $
      parseXmlString badXml11 `shouldBe` expectedResultForBadXml11
    it "should collect errors of substitutions even if there are no builders" $
      parseXmlString badXml12 `shouldBe` expectedResultForBadXml12
    it "should fail on missing value in entry" $
      parseXmlString badXml13 `shouldBe` expectedResultForBadXml13
    it "should fail on missing name in entry" $
      parseXmlString badXml14 `shouldBe` expectedResultForBadXml14
    it "should fail on invalid '<shell> + property attribute' combination" $
      parseXmlString badXml15 `shouldBe` expectedResultForBadXml15
    it (printf "should fail on wrong Boolean attribute haltOnFailure in <%s>" tShell) $
      parseXmlString badXml16 `shouldBe` expectedResultForBadXml16
    it (printf "should fail on wrong Boolean attribute haltOnFailure in <%s>" tSetProperty) $
      parseXmlString badXml17 `shouldBe` expectedResultForBadXml17
    it (printf "should fail on wrong Boolean attribute ignoreFailure in <%s>" tShell) $
      parseXmlString badXml18 `shouldBe` expectedResultForBadXml18
    it (printf "should fail on wrong Boolean attribute ignoreFailure in <%s>" tSetProperty) $
      parseXmlString badXml19 `shouldBe` expectedResultForBadXml19
 where
  normalizeTestConfig :: Config Parsed -> ConfigValidation (Config Substituted)
  normalizeTestConfig = substAll [] . normalize ""

