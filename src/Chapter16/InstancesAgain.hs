-- Page 669

{-# LANGUAGE FlexibleInstances #-}

module Chapter16.InstancesAgain where

import Test.Hspec
import Test.QuickCheck

import Chapter16.Properties

-- 1.

data Quant a b = Finance | Desk a | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Finance, Desk a, Bloor b]

-- 2.

-- data K a b = K a
--   deriving (Show, Eq)

instance Functor (K a) where
  fmap _ (K a) = K a

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return $ K a

-- 3.

data Flip f a b = Flip (f b a)
  deriving (Eq, Show)

newtype K a b = K a
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K $ f b

instance Arbitrary b => Arbitrary (Flip K a b) where
  arbitrary = do
    b <- arbitrary
    return $ Flip $ K b

-- 4.

data EvilGoateeConst a b = GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    return $ GoatyConst b

-- 5.

data LiftItOut f a = LiftItOut (f a)
  deriving (Show, Eq)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

-- instance Arbitrary (LiftItOut f a) where
  -- Not sure how to generate Functors of kind '* -> *'. I.e., something of
  -- type 'Functor f => Gen (LiftItOut f a)'.
  
instance Arbitrary a => Arbitrary (LiftItOut Maybe a) where
  arbitrary = do
    maybeOfA <- arbitrary
    return $ LiftItOut maybeOfA

-- 6.

data Parappa f g a = DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

instance Arbitrary a => Arbitrary (Parappa Maybe [] a) where
  arbitrary = do
    maybeOfA <- arbitrary
    listOfA <- arbitrary
    return $ DaWrappa maybeOfA listOfA

-- 7.

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
  deriving (Show, Eq)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne Maybe [] a b) where
  arbitrary = do
    maybeOfA <- arbitrary
    listOfB <- arbitrary
    return $ IgnoringSomething maybeOfA listOfB

-- 8.

data Notorious g o a t = Notorious (g o) (g a) (g t)
  deriving (Show, Eq)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance (Arbitrary a, Arbitrary o, Arbitrary t) => Arbitrary (Notorious Maybe o a t) where
  arbitrary = do
    maybeOfO <- arbitrary
    maybeOfA <- arbitrary
    maybeOfT <- arbitrary
    return $ Notorious maybeOfO maybeOfA maybeOfT

-- 9.

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)
  fmap _ Nil = Nil 

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    listOfA <- arbitrary
    return $ listToVerboseList listOfA

listToVerboseList :: [a] -> List a
listToVerboseList = foldr Cons Nil

-- 10.

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Show, Eq)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats g g' g'') = MoreGoats (fmap f g) (fmap f g') (fmap f g'')

instance Arbitrary a => Arbitrary (GoatLord a) where
  arbitrary = do
    a <- arbitrary
    goatLord <- arbitrary
    goatLord' <- arbitrary
    goatLord'' <- arbitrary

    -- There's a possibility this will produce an interminable GoatLord.
    frequency
      [
        (1, return $ NoGoat)
      , (3, return $ OneGoat a)
      , (2, return $ MoreGoats goatLord goatLord' goatLord'')
      ]

-- 11.

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read sToA) = Read $ fmap f sToA

instance Arbitrary a => Arbitrary (TalkToMe a) where
  arbitrary = do
    s <- arbitrary
    a <- arbitrary
    sToA <- arbitrary
    elements [Halt, Print s a, Read sToA]

talkToMeEquality :: Eq a => TalkToMe a -> TalkToMe a -> String -> Bool
talkToMeEquality Halt Halt _ = True
talkToMeEquality (Print s a) (Print s' a') _ = s == s' && a == a'
talkToMeEquality (Read sToA) (Read sToA') s = functionEqualityProperty sToA sToA' s
talkToMeEquality _ _ _ = False

-- Properties.

talkToMeIdentityProperty :: Eq a => Blind (TalkToMe a) -> String -> Bool
talkToMeIdentityProperty (Blind t) s = functorIdentityProperty' t (\t' t'' -> talkToMeEquality t' t'' s)

talkToMeComposeProperty :: Eq a => Blind (TalkToMe a) -> Fun a a -> Fun a a -> String -> Bool
talkToMeComposeProperty (Blind t) f g s = functorComposeProperty' t f g (\t' t'' -> talkToMeEquality t' t'' s)

-- Testing.

test_functors :: IO ()
test_functors = hspec $ do
  describe "functorIdentityProperty" $ do
    -- 1.

    it "Testing functorIdentityProperty :: Quant String Integer -> Bool." $ do
      property (functorIdentityProperty :: Quant String Integer -> Bool)

    -- 2.

    it "Testing functorIdentityProperty :: K String Integer -> Bool." $ do
      property (functorIdentityProperty :: K String Integer -> Bool)

    -- 3.

    it "Testing functorIdentityProperty :: Flip K String Integer -> Bool." $ do
      property (functorIdentityProperty :: Flip K String Integer -> Bool)

    -- 4.

    it "Testing functorIdentityProperty :: EvilGoateeConst String Integer -> Bool." $ do
      property (functorIdentityProperty :: EvilGoateeConst String Integer -> Bool)

    -- 5.

    it "Testing functorIdentityProperty :: LiftItOut Maybe Integer -> Bool." $ do
      property (functorIdentityProperty :: LiftItOut Maybe Integer -> Bool)

    -- 6.

    it "Testing functorIdentityProperty :: Parappa Maybe [] Integer -> Bool." $ do
      property (functorIdentityProperty :: Parappa Maybe [] Integer -> Bool)

    -- 7.

    it "Testing functorIdentityProperty :: IgnoreOne Maybe [] String Integer -> Bool." $ do
      property (functorIdentityProperty :: IgnoreOne Maybe [] String Integer -> Bool)

    -- 9.

    it "Testing functorIdentityProperty :: List Integer -> Bool." $ do
      property (functorIdentityProperty :: List Integer -> Bool)

    -- 10.

    it "Testing functorIdentityProperty :: GoatLord Integer -> Bool." $ do
      property (functorIdentityProperty :: GoatLord Integer -> Bool)

    -- 11.

    it "Testing talkToMeIdentityProperty :: Blind (TalkToMe Integer) -> String -> Bool." $ do
      property (talkToMeIdentityProperty :: Blind (TalkToMe Integer) -> String -> Bool)

  describe "functorComposeProperty" $ do
    -- 1.

    it "Testing functorComposeProperty :: Quant String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Quant String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 2.

    it "Testing functorComposeProperty :: K String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: K String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 3.

    it "Testing functorComposeProperty :: Flip K String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Flip K String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 4.

    it "Testing functorComposeProperty :: EvilGoateeConst String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: EvilGoateeConst String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 5.

    it "Testing functorComposeProperty :: LiftItOut Maybe Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: LiftItOut Maybe Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 6.

    it "Testing functorComposeProperty :: Parappa Maybe [] Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Parappa Maybe [] Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 7.

    it "Testing functorComposeProperty :: IgnoreOne Maybe [] String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: IgnoreOne Maybe [] String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 8.

    it "Testing functorComposeProperty :: Notorious Maybe Bool String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: Notorious Maybe Bool String Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 9.

    it "Testing functorComposeProperty :: List Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: List Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 10.

    it "Testing functorComposeProperty :: GoatLord Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool." $ do
      property (functorComposeProperty :: GoatLord Integer -> Fun Integer Integer -> Fun Integer Integer -> Bool)

    -- 11.

    it "Testing talkToMeComposeProperty :: Blind (TalkToMe Integer) -> Fun Integer Integer -> Fun Integer Integer -> String -> Bool." $ do
      property (talkToMeComposeProperty :: Blind (TalkToMe Integer) -> Fun Integer Integer -> Fun Integer Integer -> String -> Bool)