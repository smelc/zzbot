{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ConfigSpec (spec) where

import Data.List
import Data.Maybe
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Modifiers

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Validation as V

import Config hiding (prop)

testParseVars =
  describe "parseVars" $ do
    it "1" $
      parseVars ("$(", ")") "foo$(b)ar"  `shouldBe` [Left "foo", Right "b", Left "ar"]
    it "no var" $
      parseVars ("$(", ")") "foobar"  `shouldBe` [Left "foobar"]
    it "2" $
      parseVars ("$(", ")") "foo$(b)a$(r)"  `shouldBe` [Left "foo", Right "b", Left "a", Right "r"]
    prop "pretty . parse . pretty = pretty" $
      \(NonEmpty open) (NonEmpty close) chunks ->
          let toStr = pretty open close
              toAst = parseVars (open, close)
          in (toStr. toAst . toStr) chunks === toStr chunks
 where
  pretty open close = concatMap (prettyChunk open close)
  prettyChunk open close (Left str) = str
  prettyChunk open close (Right str) = open ++ str ++ close

testSplitAround :: SpecWith ()
testSplitAround =
  describe "splitAround" $ do
    it "prefix" $
      splitAround "f" "foobar"  `shouldBe` Just ("", "oobar")
    it "middle" $
      splitAround "b" "foobar"  `shouldBe` Just ("foo", "ar")
    it "end" $
      splitAround "r" "foobar"  `shouldBe` Just ("fooba", "")
    it "nothing" $
      splitAround "c" "foobar"  `shouldBe` Nothing

testSplitDelimiters :: SpecWith ()
testSplitDelimiters =
  describe "splitDelimiters" $ do
    it "Nothing" $
      splitDelimiters ("(", ")") "foobar" `shouldBe` Nothing
    it "middle" $
      splitDelimiters ("(", ")") "foo(var)bar" `shouldBe` Just("foo", "var", "bar")
    it "start" $
      splitDelimiters ("(", ")") "(var)bar" `shouldBe` Just("", "var", "bar")
    it "end" $
      splitDelimiters ("(", ")") "foo(var)" `shouldBe` Just("foo", "var", "")
    it "middle2" $
      splitDelimiters ("$(", ")") "foo$(var)" `shouldBe` Just("foo", "var", "")

testSubstitute :: SpecWith ()
testSubstitute = do
  describe "duplicates" $ do
    it "duplicates . duplicates = [] (duplicates doesn't returns duplicates)" $
      property $
        \(x :: [Int]) -> duplicates (duplicates x) `shouldBe` []
    it "duplicates [0, 0] should be [0]" $
      duplicates [0, 0] `shouldBe` [0]
    it "duplicates [0, 0, 0] should be [0]" $
      duplicates [0, 0] `shouldBe` [0]
    it "duplicates [0, 1] should be []" $
      duplicates [0, 1] `shouldBe` []
  describe "subsitute" $ do
    it "should succeed when all variables are known" $
      substitute @(Builder Normalized) @(Builder Substituted) ("(", ")") goodSubst builder `shouldBe` V.Success expectedSuccess
    it "should fail when some variables are unknown" $
      substitute @(Builder Normalized) @(Builder Substituted) ("(", ")") badSubst builder `shouldBe` V.Failure expectedErrors
 where
  builder =
    Builder () "builder"
      [ SetPropertyFromValue "prop" "foo(a)bar(b)baz"
      , ShellCmd "(b)" (Command "ls" ["(a)", "(b)"]) Nothing True
      ]
  goodSubst = [("a", "xx"), ("b", "yy")]
  expectedSuccess =
    Builder () "builder"
      [ SetPropertyFromValue "prop" "fooxxbaryybaz"
      , ShellCmd "yy" (Command "ls" ["xx", "yy"]) Nothing True
      ]
  badSubst = [("c", "xx")]
  expectedErrors = Set.fromList
    [ KeyNotFound badSubst "a"
    , KeyNotFound badSubst "b"
    ]

testNormalize :: SpecWith ()
testNormalize =
  describe "normalize" $ do
    it "no workdir in builder, no workdir in step" $
      normalizesTo "/home/user" Nothing Nothing "/home/user"
    it "no workdir in builder, relative workdir in step" $
      normalizesTo "/home/user" Nothing (Just "rel/dir") "/home/user/rel/dir"
    it "no workdir in builder, absolute workdir in step" $
      normalizesTo "/home/user" Nothing (Just "/abs/dir") "/abs/dir"
    it "relative workdir in builder, no workdir in step" $
      normalizesTo "/home/user" (Just "rel/dir") Nothing "/home/user/rel/dir"
    it "relative workdir in builder, relative workdir in step" $
      normalizesTo "/home/user" (Just "rel/dir1") (Just "rel/dir2") "/home/user/rel/dir1/rel/dir2"
    it "relative workdir in builder, absolute workdir in step" $
      normalizesTo "/home/user" (Just "rel/dir") (Just "/abs/dir") "/abs/dir"
    it "absolute workdir in builder, no workdir in step" $
      normalizesTo "/home/user" (Just "/abs/dir") Nothing "/abs/dir"
    it "absolute workdir in builder, relative workdir in step" $
      normalizesTo "/home/user" (Just "/abs/dir") (Just "rel/dir") "/abs/dir/rel/dir"
    it "absolute workdir in builder, absolute workdir in step" $
      normalizesTo "/home/user" (Just "/abs/dir1") (Just "/abs/dir2") "/abs/dir2"
 where
  normalizesTo :: FilePath -> Maybe FilePath -> Maybe FilePath -> FilePath -> Expectation
  normalizesTo userWorkdir builderWorkdir stepWorkdir exepectedStepWorkdir =
    normalize userWorkdir originalConfig `shouldBe` expectedNormalizedConfig
   where
    originalConfig :: Config Parsed
    originalConfig = Config
      { builders =
          Builder
            { name = "test"
            , workdir = builderWorkdir
            , steps =
                [ ShellCmd
                    { workdir = stepWorkdir
                    , cmd = Command "" []
                    , mprop = Nothing
                    , haltOnFailure = Nothing
                    }
                ]
            }
            NE.:| []
      , subst = []
      }
    expectedNormalizedConfig :: Config Normalized
    expectedNormalizedConfig = Config
      { builders =
          Builder
            { name = "test"
            , workdir = ()
            , steps =
                [ ShellCmd
                    { workdir = exepectedStepWorkdir
                    , cmd = Command "" []
                    , mprop = Nothing
                    , haltOnFailure = True
                    }
                ]
            }
            NE.:| []
      , subst = []
      }



spec = do
  testParseVars
  testSplitAround
  testSplitDelimiters
  testSubstitute
  testNormalize
