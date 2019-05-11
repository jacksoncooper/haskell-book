-- Page 717

module Chapter17.ZipMonoid where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Unfortunate orphan instances follow, beware!

instance Monoid a => Semigroup (ZipList a) where
  (<>) = liftA2 mappend

instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

-- Testing.

myZipList :: ZipList (Sum Integer)
myZipList = undefined

testZipListMonoid :: IO ()
testZipListMonoid = quickBatch $ monoid myZipList