{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module RandomConfigs where

import Config
import Data.Char
import Generic.Random
import Test.QuickCheck

import qualified Data.List.NonEmpty as NE

instance Arbitrary a => Arbitrary (NE.NonEmpty a) where
  arbitrary = NE.fromList . getNonEmpty <$> arbitrary
  shrink = genericShrink

isValidCommand Command{cmdString} = not (all isSpace cmdString)

instance Arbitrary Command where
  arbitrary = genericArbitrary uniform `suchThat` isValidCommand
  shrink = filter isValidCommand . genericShrink

instance Arbitrary (ForeachExtension Parsed) where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance Arbitrary (Step Parsed) where
  -- We do not generate foreach tags for now
  arbitrary = genericArbitrary (1 % 1 % 0 % ())
  shrink = genericShrink

instance Arbitrary (Builder Parsed) where
  arbitrary = Builder Nothing <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (Config Parsed) where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink
