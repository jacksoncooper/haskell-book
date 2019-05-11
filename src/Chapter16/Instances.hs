-- Page 650

module Chapter16.Excercises where

import Test.Hspec
import Test.QuickCheck

import Chapter16.Properties

-- 1.

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = (Identity $ f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- 2.

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

-- 3.

data Two a b = Two a b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- 4.

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- 5.

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

-- 6.

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

-- 7.

data Four' a b = Four' a a a b
  deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

-- 8.

-- There is no way to implement a Functor instance for 'data Trivial = Trivial'
-- because the type constructor Trivial has kind '*'. I.e., the type is entirely
-- structure. There is no content over which to map a function.

-- Testing.

testFunctors :: IO ()
testFunctors = hspec $ do
  describe "functorIdentityProperty" $ do
    -- 1.

    it "Testing functorIdentityProperty :: Identity Integer -> Bool." $ do
      property (functorIdentityProperty :: Identity Integer -> Bool)

    -- 2.

    it "Testing functorIdentityProperty :: Pair Integer -> Bool." $ do
      property (functorIdentityProperty :: Pair Integer -> Bool)

    -- 3.

    it "Testing functorIdentityProperty :: Two String Integer -> Bool." $ do
      property (functorIdentityProperty :: Two String Integer -> Bool)

    -- 4.

    it "Testing functorIdentityProperty :: Three Bool String Integer -> Bool." $ do
      property (functorIdentityProperty :: Three Bool String Integer -> Bool)

    -- 5.

    it "Testing functorIdentityProperty :: Three' String Integer -> Bool." $ do
      property (functorIdentityProperty :: Three' String Integer -> Bool)

    -- 6.

    it "Testing functorIdentityProperty :: Four Bool String Char Integer -> Bool." $ do
      property (functorIdentityProperty :: Four Bool String Char Integer -> Bool)

    -- 7.

    it "Testing functorIdentityProperty :: Four' String Integer -> Bool." $ do
      property (functorIdentityProperty :: Four' String Integer -> Bool)

  describe "functorComposeProperty" $ do
    -- 1.

    it "Testing functorComposeProperty :: Identity Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Identity Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 2.

    it "Testing functorComposeProperty :: Pair Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Pair Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 3.

    it "Testing functorComposeProperty :: Two String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Two String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 4.

    it "Testing functorComposeProperty :: Three Bool String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Three Bool String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 5.

    it "Testing functorComposeProperty :: Three' String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Three' String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 6.

    it "Testing functorComposeProperty :: Four Bool String Char Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Four Bool String Char Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 7.

    it "Testing functorComposeProperty :: Four' String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Four' String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)