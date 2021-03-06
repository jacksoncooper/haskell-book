-- Exercises, Page 592

import Test.QuickCheck

data Optional a = Nada | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only monoid) (Only monoid') =
    Only (monoid <> monoid')
  (<>) optional Nada = optional
  (<>) Nada optional = optional

instance Semigroup a => Monoid (Optional a) where
  mempty = Nada

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nada), (3, return $ Only a)]