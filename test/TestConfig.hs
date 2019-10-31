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