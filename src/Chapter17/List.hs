-- Page 719

module Chapter17.List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)
  fmap _ Nil = Nil 

listToVerboseList :: [a] -> List a
listToVerboseList = foldr Cons Nil

-- Attempt one:

instance Semigroup (List a) where
  (<>) = append

-- instance Applicative List where
--   pure a = Cons a Nil
--   (<*>) fs ys = fmap (uncurry ($)) $ cartesian fs ys

append :: List a -> List a -> List a
append (Cons x xs) ys = Cons x $ append xs ys
append Nil ys = ys

cartesian :: List a -> List b -> List (a, b)
cartesian (Cons x xs) ys = ((,) x <$> ys) <> cartesian xs ys
cartesian _ _ = Nil

-- Attempt two. With book guidance, because the above is hideous:

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons x xs) = f x $ fold f b xs

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure a = Cons a Nil
  (<*>) fs xs = flatMap (flip fmap xs) fs

  -- ^ The cases involving the application of Nil work because of fmap's
  --   implementation. So that's neat.

-- Testing.

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    listOfA <- arbitrary
    return $ listToVerboseList listOfA

instance Eq a => EqProp (List a) where
  (=-=) = eq

myList :: List (Integer, Integer, Integer)
myList = undefined

testListApplicative :: IO ()
testListApplicative = quickBatch $ applicative myList