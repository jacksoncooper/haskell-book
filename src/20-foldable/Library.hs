-- Page 818

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- 2.

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- 3.

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (Any . (a ==))

-- 4.

-- I need to come back to this one, because this solution is nasty. I
-- effectively implemented a new Maybe datatype, with an Ord instance such that
-- (Just _) < Nothing. But it's a Monoid for use with foldMap. I'm sure I'm
-- overlooking something really obvious, but I can't figure it out.

data Minimum a = Minimum a | Floor
  deriving (Eq, Show)

instance Ord a => Semigroup (Minimum a) where
  (<>) (Minimum a) (Minimum a') = Minimum $ min a a'
  (<>) minimumOfA Floor = minimumOfA
  (<>) Floor minimumOfA = minimumOfA

instance Ord a => Monoid (Minimum a) where
  mempty = Floor

instance Arbitrary a => Arbitrary (Minimum a) where
  arbitrary = do
    a <- arbitrary
    elements [Minimum a, Floor]

instance Eq a => EqProp (Minimum a) where
  (=-=) = eq

minimumToMaybe :: Minimum a -> Maybe a
minimumToMaybe (Minimum a) = Just a
minimumToMaybe Floor = Nothing

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = minimumToMaybe . foldMap Minimum

-- 5.

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr (max . Just) Nothing

-- 6.

null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

-- 7.

length' :: Foldable t => t a -> Int
length' = getSum . foldMap (const $ Sum 1)

-- 8.

toList' :: Foldable t => t a -> [a]
toList' = foldMap (: [])

-- 9.

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

-- 10.

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' aToMonoid = foldr ((<>) . aToMonoid) mempty

-- Testing.

myMinimum :: Minimum (Integer, Integer, Integer)
myMinimum = undefined

testMinimumMonoid :: IO ()
testMinimumMonoid = quickBatch $ monoid myMinimum