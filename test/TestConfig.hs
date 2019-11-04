module Main where

import Data.List
import Test.Hspec

import Config

main :: IO ()
main = hspec $ do
  describe "splitAround" $ do
    it "splitAround prefix" $
      splitAround "f" "foobar"  `shouldBe` Just ("", "oobar")
    it "splitAround middle" $
      splitAround "b" "foobar"  `shouldBe` Just ("foo", "ar")
    it "splitAround end" $
      splitAround "r" "foobar"  `shouldBe` Just ("fooba", "")
    it "splitAround nothing" $
      splitAround "c" "foobar"  `shouldBe` Nothing

    it "splitDelimiters Nothing" $
      splitDelimiters "(" ")" "foobar" `shouldBe` Nothing
    it "splitDelimiters 1" $
      splitDelimiters "(" ")" "foo(var)bar" `shouldBe` Just("foo", "var", "bar")
    it "splitDelimiters 2" $
      splitDelimiters "(" ")" "(var)bar" `shouldBe` Just("", "var", "bar")
    it "splitDelimiters 3" $
      splitDelimiters "(" ")" "foo(var)" `shouldBe` Just("foo", "var", "")
    it "splitDelimiters 4" $
      splitDelimiters "$(" ")" "foo$(var)" `shouldBe` Just("foo", "var", "")