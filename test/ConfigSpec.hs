module Main where

import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Modifiers

import Config hiding (prop)

testParseVars =
  describe "parseVars" $ do
    it "1" $
      parseVars ("$(", ")") "foo$(b)ar"  `shouldBe` [Left "foo", Right "b", Left "ar"]
    it "no var" $
      parseVars ("$(", ")") "foobar"  `shouldBe` [Left "foobar"]
    it "2" $
      parseVars ("$(", ")") "foo$(b)a$(r)"  `shouldBe` [Left "foo", Right "b", Left "a", Right "r"]
    modifyMaxSuccess (const 10000) $
      prop "parse . pretty . parse . pretty = id" $
        \(NonEmpty open) (NonEmpty close) chunks ->
            let toAst = pretty open close
                toStr = parseVars (open, close)
            in (toAst . toStr . toAst . toStr) chunks === chunks
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

main :: IO ()
main = hspec $ do
  testParseVars
  testSplitAround
  testSplitDelimiters
