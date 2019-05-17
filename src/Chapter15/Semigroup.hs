-- Chapter Exercises, Page 612

module Chapter15.Semigroup where

import Data.Monoid (All, Any, Sum)
import Test.Hspec
import Test.QuickCheck

import Chapter15.Properties

-- 1.

data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) Trivial Trivial = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- 2.

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (<>) (Identity x) (Identity x') = Identity $ x <> x'

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- 3.

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two x y) (Two x' y') = Two (x <> x') (y <> y')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- 4.

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three x y z) (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance Monoid BoolConj where
  mempty = BoolConj True

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- 5.

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four w x y z) (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

-- 6.

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) (BoolConj bool) (BoolConj bool') = BoolConj $ bool && bool'

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

-- 7.

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) (BoolDisj bool) (BoolDisj bool') = BoolDisj $ bool || bool'

instance Monoid (Compose a) where
  mempty = Compose id

instance Arbitrary BoolDisj where
  arbitrary = elements [BoolDisj True, BoolDisj False]

-- 8.

data Or a b = Fst a | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (<>) (Fst _) (Fst y) = Fst y
  (<>) (Fst _) (Snd y) = Snd y
  (<>) (Snd x) (Fst _) = Snd x
  (<>) (Snd _) (Snd y) = Snd y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

-- 9.

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine $ \a -> f a <> g a

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

-- 10.

newtype Compose a = Compose { unCompose :: (a -> a) }

instance Semigroup (Compose a) where
  (<>) (Compose f) (Compose g) = Compose $ f . g

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Compose a) where
  arbitrary = do
    f <- arbitrary
    return $ Compose f

-- 11.

data Validation a b = Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) (Success' s) (Success' _)  = Success' s
  (<>) (Success' s) (Failure' _)  = Success' s
  (<>) (Failure' _) (Success' s)  = Success' s
  (<>) (Failure' f) (Failure' f') = Failure' $ f <> f'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Failure' a, Success' b]

-- Properties.

-- The following properties only generate one 'a' to approximate function
-- equality. I'm not sure how to hold the generated functions constant and test
-- against multiple values of 'a'. Maybe come back to this after Monads?

combineAssociativeProperty :: (Eq b, Semigroup b) => Blind (Combine a b) -> Blind (Combine a b) -> Blind (Combine a b) -> a -> Bool
combineAssociativeProperty (Blind c) (Blind c') (Blind c'') x =
  semigroupAssociativeProperty' c c' c'' unCombine x

composeAssociativeProperty :: Eq a => Blind (Compose a) -> Blind (Compose a) -> Blind (Compose a) -> a -> Bool
composeAssociativeProperty (Blind c) (Blind c') (Blind c'') x =
  semigroupAssociativeProperty' c c' c'' unCompose x

-- Testing.

testSemigroups :: IO ()
testSemigroups = hspec $ do
  describe "semigroupAssociativeProperty" $ do
    -- 1.

    it "Testing semigroupAssociativeProperty :: Trivial -> Trivial -> Trivial -> Bool." $ do
      property (semigroupAssociativeProperty :: Trivial -> Trivial -> Trivial -> Bool)

    -- 2.

    it "Testing semigroupAssociativeProperty :: (Identity String) -> ... -> ... -> Bool." $ do
      property (semigroupAssociativeProperty :: (Identity String) -> (Identity String) -> (Identity String) -> Bool)

    -- 3.

    it "Testing semigroupAssociativeProperty :: (Two String Any) -> ... -> ... -> Bool." $ do
      property (semigroupAssociativeProperty :: (Two String Any) -> (Two String Any) -> (Two String Any) -> Bool)

    -- 4.

    it "Testing semigroupAssociativeProperty :: (Three String Any All) -> ... -> ... -> Bool." $ do
      property (semigroupAssociativeProperty :: (Three String Any All) -> (Three String Any All) -> (Three String Any All) -> Bool)

    -- 5.

    it "Testing semigroupAssociativeProperty :: (Four String Any All (Sum Integer)) -> ... -> ... -> Bool." $ do
      property (semigroupAssociativeProperty :: (Four String Any All (Sum Integer)) -> (Four String Any All (Sum Integer)) -> (Four String Any All (Sum Integer)) -> Bool)

    -- 6.

    it "Testing semigroupAssociativeProperty :: BoolConj -> ... -> ... -> Bool." $ do
      property (semigroupAssociativeProperty :: BoolConj -> BoolConj -> BoolConj -> Bool)

    -- 7.

    it "Testing semigroupAssociativeProperty :: BoolDisj -> ... -> ... -> Bool." $ do
      property (semigroupAssociativeProperty :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)

    -- 8.

    it "Testing semigroupAssociativeProperty :: Or Integer String -> ... -> ... -> Bool." $ do
      property (semigroupAssociativeProperty :: Or Integer String -> Or Integer String -> Or Integer String -> Bool)

    -- 9.

    it "Testing combineAssociativeProperty :: Blind (Combine Integer (Sum Integer)) -> ... -> ... -> Integer -> Bool." $ do
      property (combineAssociativeProperty :: Blind (Combine Integer (Sum Integer)) -> Blind (Combine Integer (Sum Integer)) -> Blind (Combine Integer (Sum Integer)) -> Integer -> Bool)

    -- 10.

    it "Testing composeAssociativeProperty :: Blind (Combine (Sum Integer)) -> ... -> ... -> (Sum Integer) -> Bool." $ do
      property (composeAssociativeProperty :: Blind (Compose (Sum Integer)) -> Blind (Compose (Sum Integer)) -> Blind (Compose (Sum Integer)) -> (Sum Integer) -> Bool)

    -- 11.

    it "Testing semigroupAssociativeProperty :: Validation String Int -> ... -> ... -> Bool." $ do
      property (semigroupAssociativeProperty :: Validation String Int -> Validation String Int -> Validation String Int -> Bool)