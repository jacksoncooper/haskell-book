-- Exercises, Page 604

module Chapter15.Invalid where

import Test.Hspec
import Test.QuickCheck

data Bull = Fools | Twoo
  deriving (Eq, Show)

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

instance Arbitrary Bull where
  arbitrary = elements [Fools, Twoo]

monoidAssociativeProperty :: (Eq a, Monoid a) => a -> a -> a -> Bool
monoidAssociativeProperty x y z = (x <> y) <> z == x <> (y <> z)

monoidLeftIdentityProperty :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentityProperty x = mempty <> x == x

monoidRightIdentityProperty :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentityProperty x = x <> mempty == x

main :: IO ()
main = hspec $ do
  describe "monoidAssociativeProperty" $ do
    it "Testing monoidAssociativeProperty :: Bull -> Bull -> Bull -> Bool." $ do
      property (monoidAssociativeProperty :: Bull -> Bull -> Bull -> Bool)

  describe "monoidLeftIdentityProperty" $ do
    it "Testing monoidLeftIdentityProperty :: Bull -> Bool." $ do
      property (monoidLeftIdentityProperty :: Bull -> Bool)

  describe "monoidRightIdentityProperty" $ do
    it "Testing monoidRightIdentityProperty :: Bull -> Bool." $ do
      property (monoidRightIdentityProperty :: Bull -> Bool)