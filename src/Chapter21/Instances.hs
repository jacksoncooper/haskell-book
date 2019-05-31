-- Page 839

{-# LANGUAGE FlexibleContexts #-}

module Chapter21.Instances where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- 1.

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 2.

newtype Constant a b = Constant { getConstant :: a}
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure . Constant $ a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

-- 3.

data Optional a = Nada | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nada), (3, return $ Yep a)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-- 4.

data List a = Nil | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    as <- arbitrary
    frequency [(1, return Nil), (3, return $ Cons a as)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- 5.

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 6.

data Pair a b = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- 7.

data Big a b = Big a b b
  deriving (Show, Eq)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- 8

data Bigger a b = Bigger a b b b
  deriving (Show, Eq)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- 9.

data S n a = S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a') = S (f <$> na) (f a')

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

-- 10.

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node treeOfA treeOfA') = Node (f <$> treeOfA) (f <$> treeOfA')

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node treeOfA treeOfA') = foldMap f treeOfA <> foldMap f treeOfA'

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node treeOfA treeOfA') =
    Node <$> traverse f treeOfA <*> traverse f treeOfA'

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    treeOfA <- arbitrary
    treeOfA' <- arbitrary

    frequency
      [ (1, return Empty)
      , (2, return $ Leaf a)
      , (3, return $ Node treeOfA treeOfA')
      ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

-- Testing.

-- The 'traversable' function does not check the Traversable laws, but instead
-- checks the laws of Traversable's superclasses, delegating to 'fmapDefault'
-- and 'foldMapDefault'.

-- traversable ::
--   (Traversable f, Monoid m, Show (f a), Arbitrary (f a), Arbitrary b,
--    Arbitrary m, CoArbitrary a, EqProp (f b), EqProp m) =>
--   f (a, b, m) -> TestBatch

-- fmapDefault :: Traversable t => (a -> b) -> t a -> t b

-- foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m

myIdentity :: Identity (String, Integer, String)
myIdentity = undefined

myConstant :: Identity (String, Integer, String)
myConstant = undefined

myOptional :: Identity (String, Integer, String)
myOptional = undefined

myList :: List (String, Integer, String)
myList = undefined

myThree :: Three String String (String, Integer, String)
myThree = undefined

myPair :: Pair String (String, Integer, String)
myPair = undefined

myBig :: Big String (String, Integer, String)
myBig = undefined

myBigger :: Big String (String, Integer, String)
myBigger = undefined

myS :: S [] (String, Integer, String)
myS = undefined

myTree :: Tree (String, Integer, String)
myTree = undefined

testTraversableInstances :: IO ()
testTraversableInstances = do
  quickBatch $ traversable myIdentity
  quickBatch $ traversable myConstant
  quickBatch $ traversable myOptional
  quickBatch $ traversable myList
  quickBatch $ traversable myThree
  quickBatch $ traversable myPair
  quickBatch $ traversable myBig
  quickBatch $ traversable myBigger
  quickBatch $ traversable myS
  quickBatch $ traversable myTree