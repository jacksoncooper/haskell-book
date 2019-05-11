-- Page 726

module Chapter17.Instances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Note to future self: Didn't import 'Chapter16.Instances' to avoid orphan
-- instances, not sure of an alternative.

-- 1.

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2.

data Two a b = Two a b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 3.

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4.

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' a f f') (Three' a' b b') = Three' (a <> a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 5.

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (<*>) (Four a b c f) (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6.

data Four' a b = Four' a a a b
  deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  (<*>) (Four' a1 a3 a5 f) (Four' a2 a4 a6 b) = Four' (a1 <> a2) (a3 <> a4) (a5 <> a6) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Testing.

myPair :: Pair (Integer, Integer, Integer)
myPair = undefined

myTwo :: Two String (Integer, Integer, Integer)
myTwo = undefined

myThree :: Three String String (Integer, Integer, Integer)
myThree = undefined

myThree' :: Three' String (Integer, Integer, Integer)
myThree' = undefined

myFour :: Four String String String (Integer, Integer, Integer)
myFour = undefined

myFour' :: Four' String (Integer, Integer, Integer)
myFour' = undefined

testApplicativeInstances :: IO ()
testApplicativeInstances = do
  quickBatch $ applicative myPair
  quickBatch $ applicative myTwo
  quickBatch $ applicative myThree
  quickBatch $ applicative myThree'
  quickBatch $ applicative myFour
  quickBatch $ applicative myFour'