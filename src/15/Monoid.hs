-- Chapter Exercises, Page 609

import Data.Monoid (Any, Sum)
import Test.Hspec
import Test.QuickCheck

-- import Chapter15.Properties
-- import Chapter15.Semigroup

-- 8.

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  (<>) m m' = Mem $ chainMems m m'

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)

instance (Arbitrary a, Monoid a, Arbitrary s, CoArbitrary s) => Arbitrary (Mem s a) where
  arbitrary = do
    f <- arbitrary
    frequency [(1, return mempty), (3, return $ Mem f)]

chainMems :: Semigroup a => Mem s a -> Mem s a -> s -> (a, s)
chainMems m m' s'' = (a <> a', s)
  where
    (a', s') = runMem m' s''
    (a, s)   = runMem m s'

-- Properties.

combineLeftIdentityProperty :: (Eq b, Monoid b) => Blind (Combine a b) -> a -> Bool
combineLeftIdentityProperty (Blind c) x = monoidLeftIdentityProperty' c unCombine x

combineRightIdentityProperty :: (Eq b, Monoid b) => Blind (Combine a b) -> a -> Bool
combineRightIdentityProperty (Blind c) x = monoidRightIdentityProperty' c unCombine x

composeLeftIdentityProperty :: Eq a => Blind (Compose a) -> a -> Bool
composeLeftIdentityProperty (Blind c) x = monoidLeftIdentityProperty' c unCompose x

composeRightIdentityProperty :: Eq a => Blind (Compose a) -> a -> Bool
composeRightIdentityProperty (Blind c) x = monoidRightIdentityProperty' c unCompose x

memAssociativeProperty :: (Eq a, Semigroup a, Eq s) => Blind (Mem s a) -> Blind (Mem s a) -> Blind (Mem s a) -> s -> Bool
memAssociativeProperty (Blind m) (Blind m') (Blind m'') x = semigroupAssociativeProperty' m m' m'' runMem x

memLeftIdentityProperty :: (Eq a, Monoid a, Eq s) => Blind (Mem s a) -> s -> Bool
memLeftIdentityProperty (Blind m) x = monoidLeftIdentityProperty' m runMem x

memRightIdentityProperty :: (Eq a, Monoid a, Eq s) => Blind (Mem s a) -> s -> Bool
memRightIdentityProperty (Blind m) x = monoidRightIdentityProperty' m runMem x

-- Mem monoid sanity check.

mem :: Mem Int String
mem = Mem $ \s -> ("hi", s + 1)

memSanity :: IO ()
memSanity = do
  let rmzero  = runMem mempty 0
      rmleft  = runMem (mem <> mempty) 0
      rmright = runMem (mempty <> mem) 0

  print $ rmleft                    -- ("hi",1)
  print $ rmright                   -- ("hi",1)
  print $ (rmzero :: (String, Int)) -- ("",0)
  print $ rmleft == runMem mem 0    -- True
  print $ rmright == runMem mem 0   -- True

-- Testing.

testMonoids :: IO ()
testMonoids = hspec $ do
  describe "semigroupAssociativeProperty" $ do
    -- 8.

    it "Testing memAssociativeProperty :: Blind (Mem Integer String) -> ... -> ... -> Integer -> Bool." $ do
      property (memAssociativeProperty :: Blind (Mem Integer String) -> Blind (Mem Integer String) -> Blind (Mem Integer String) -> Integer -> Bool)

  describe "monoidLeftIdentityProperty" $ do
    -- 1.

    it "Testing monoidLeftIdentityProperty :: Trivial -> Bool." $ do
      property (monoidLeftIdentityProperty :: Trivial -> Bool)

    -- 2.

    it "Testing monoidLeftIdentityProperty :: Identity String -> Bool." $ do
      property (monoidLeftIdentityProperty :: Identity String -> Bool)

    -- 3.

    it "Testing monoidLeftIdentityProperty :: Two String Any -> Bool." $ do
      property (monoidLeftIdentityProperty :: Two String Any -> Bool)

    -- 4.

    it "Testing monoidLeftIdentityProperty :: BoolConj -> Bool." $ do
      property (monoidLeftIdentityProperty :: BoolConj -> Bool)

    -- 5.

    it "Testing monoidLeftIdentityProperty :: BoolDisj -> Bool." $ do
      property (monoidLeftIdentityProperty :: BoolDisj -> Bool)

    -- 6.

    it "Testing combineLeftIdentityProperty :: Blind (Combine Integer (Sum Integer)) -> Integer -> Bool." $ do
      property (combineLeftIdentityProperty :: Blind (Combine Integer (Sum Integer)) -> Integer -> Bool)

    -- 7.

    it "Testing composeLeftIdentityProperty :: Blind (Compose (Sum Integer)) -> (Sum Integer) -> Bool." $ do
      property (composeLeftIdentityProperty :: Blind (Compose (Sum Integer)) -> (Sum Integer) -> Bool)

    -- 8.

    it "Testing memLeftIdentityProperty :: Blind (Mem Integer String) -> Integer -> Bool." $ do
      property (memLeftIdentityProperty :: Blind (Mem Integer String) -> Integer -> Bool)
    
  describe "monoidRightIdentityProperty" $ do
    -- 1.

    it "Testing monoidRightIdentityProperty :: Trivial -> Bool." $ do
      property (monoidRightIdentityProperty :: Trivial -> Bool)

    -- 2.

    it "Testing monoidRightIdentityProperty :: Identity String -> Bool." $ do
      property (monoidRightIdentityProperty :: Identity String -> Bool)

    -- 3.

    it "Testing monoidRightIdentityProperty :: Two String Any -> Bool." $ do
      property (monoidRightIdentityProperty :: Two String Any -> Bool)

    -- 4.

    it "Testing monoidRightIdentityProperty :: BoolConj -> Bool." $ do
      property (monoidRightIdentityProperty :: BoolConj -> Bool)

    -- 5.

    it "Testing monoidRightIdentityProperty :: BoolDisj -> Bool." $ do
      property (monoidRightIdentityProperty :: BoolDisj -> Bool)

    -- 6.

    it "Testing combineRightIdentityProperty :: Blind (Combine Integer (Sum Integer)) -> Integer -> Bool." $ do
      property (combineRightIdentityProperty :: Blind (Combine Integer (Sum Integer)) -> Integer -> Bool)

    -- 7.

    it "Testing composeRightIdentityProperty :: Blind (Compose (Sum Integer)) -> (Sum Integer) -> Bool." $ do
      property (composeRightIdentityProperty :: Blind (Compose (Sum Integer)) -> (Sum Integer) -> Bool)

    -- 8.

    it "Testing memRightIdentityProperty :: Blind (Mem Integer String) -> Integer -> Bool." $ do
      property (memRightIdentityProperty :: Blind (Mem Integer String) -> Integer -> Bool)