module Chapter15.Properties where

-- Semigroup.

semigroupAssociativeProperty :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssociativeProperty s s' s'' = (s <> s') <> s'' == s <> (s' <> s'')

semigroupAssociativeProperty' :: (Semigroup a, Eq c) => a -> a -> a -> (a -> b -> c) -> b -> Bool
semigroupAssociativeProperty' s s' s'' f x =
  functionEqualityProperty (f $ (s <> s') <> s'') (f $ s <> (s' <> s'')) x

-- Monoid.

monoidLeftIdentityProperty :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentityProperty m = mempty <> m == m

monoidRightIdentityProperty :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentityProperty m = m <> mempty == m

monoidLeftIdentityProperty' :: (Eq c, Monoid a) => a -> (a -> b -> c) -> b -> Bool
monoidLeftIdentityProperty' m f x = functionEqualityProperty (f $ mempty <> m) (f m) x

monoidRightIdentityProperty' :: (Eq c, Monoid a) => a -> (a -> b -> c) -> b -> Bool
monoidRightIdentityProperty' m f x = functionEqualityProperty (f $ m <> mempty) (f m) x

-- Functions.

functionEqualityProperty :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
functionEqualityProperty f g x = f x == g x