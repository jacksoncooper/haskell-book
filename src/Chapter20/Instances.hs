-- Page 819

module Chapter20.Instances where

-- 1.

data Constant a b = Constant b
  deriving Show

instance Foldable (Constant a) where
  foldMap bToMonoid (Constant b) = bToMonoid b

-- 2.

data Two a b = Two a b
  deriving Show

instance Foldable (Two a) where
  foldMap bToMonoid (Two _ b) = bToMonoid b

-- 3.

data Three a b c = Three a b c
  deriving Show

instance Foldable (Three a b) where
  foldMap cToMonoid (Three _ _ c) = cToMonoid c

-- 4.

data Three' a b = Three' a b b
  deriving Show

instance Foldable (Three' a) where
  foldMap bToMonoid (Three' _ b b') = bToMonoid b <> bToMonoid b'

-- 5.

data Four' a b = Four' a b b b
  deriving Show

instance Foldable (Four' a) where
  foldMap bToMonoid (Four' _ b b' b'') = mconcat $ map bToMonoid [b, b', b'']