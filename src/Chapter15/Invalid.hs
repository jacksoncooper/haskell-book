-- Exercises, Page 604

module Chapter15.Invalid where

import Test.Hspec
import Test.QuickCheck

import Chapter15.Properties

data Bull = Fools | Twoo
  deriving (Eq, Show)

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

instance Arbitrary Bull where
  arbitrary = elements [Fools, Twoo]

test_invalid_monoid :: IO ()
test_invalid_monoid = hspec $ do
  describe "semigroupAssociativeProperty" $ do
    it "Testing semigroupAssociativeProperty :: Bull -> Bull -> Bull -> Bool." $ do
      property (semigroupAssociativeProperty :: Bull -> Bull -> Bull -> Bool)

  describe "monoidLeftIdentityProperty" $ do
    it "Testing monoidLeftIdentityProperty :: Bull -> Bool." $ do
      property (monoidLeftIdentityProperty :: Bull -> Bool)

  describe "monoidRightIdentityProperty" $ do
    it "Testing monoidRightIdentityProperty :: Bull -> Bool." $ do
      property (monoidRightIdentityProperty :: Bull -> Bool)