-- Page 770

module Chapter18.Instances where

import Control.Applicative (liftA)
import Control.Monad (ap)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.

data Nope a = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap = liftA

instance Applicative Nope where
  pure = return
  (<*>) = ap

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq


-- 2.

data BahEither b a =
    PLeft a
  | PRight b
  deriving (Eq, Show)

instance Functor (BahEither b) where
  fmap = liftA

instance Applicative (BahEither b) where
  pure = return
  (<*>) = ap

instance Monad (BahEither b) where
  return = PLeft
  (>>=) (PLeft a) f = f a
  (>>=) (PRight b) _ = PRight b

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [PLeft a, PRight b]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

-- 3.

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap = liftA

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Monad Identity where
  return = Identity
  (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 4.

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap = liftA

instance Applicative List where
  pure = return
  (<*>) = ap

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    as <- arbitrary
    frequency [(3, return $ Cons a as), (1, return Nil)]

instance Monad List where
  return = flip Cons Nil
  (>>=) (Cons a as) f = append (f a) (as >>= f)
  (>>=) Nil _ = Nil

instance Eq a => EqProp (List a) where
  (=-=) = eq

append :: List a -> List a -> List a
append (Cons x xs) ys = Cons x $ append xs ys
append Nil ys = ys

-- Testing.

myNope :: Nope (Integer, Integer, Integer)
myNope = undefined

myBahEither :: BahEither String (Integer, Integer, Integer)
myBahEither = undefined

myIdentity :: Identity (Integer, Integer, Integer)
myIdentity = undefined

myList :: List (Integer, Integer, Integer)
myList = undefined

-- testAll :: I'm gonna let type inference take this one.
testAll signature = do
  quickBatch $ functor signature
  quickBatch $ applicative signature
  quickBatch $ monad signature

testMonadInstances :: IO ()
testMonadInstances = do
  testAll myNope
  testAll myBahEither
  testAll myIdentity
  testAll myList