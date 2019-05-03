module Chapter16.Properties where

import Test.QuickCheck

-- Functor.

functorIdentityProperty :: (Eq (f a), Functor f) => f a -> Bool
functorIdentityProperty f = functorIdentityProperty' f (==)

functorIdentityProperty' :: Functor f => f a -> (f a -> f a -> Bool) -> Bool
functorIdentityProperty' f equalityFunction =
  f `equalityFunction` fmap id f

functorComposeProperty :: (Eq (f c), Functor f) => f a -> Fun b c -> Fun a b -> Bool
functorComposeProperty f g h = functorComposeProperty' f g h (==)

functorComposeProperty' :: Functor f => f a -> Fun b c -> Fun a b -> (f c -> f c -> Bool) -> Bool
functorComposeProperty' f (Fn g) (Fn h) equalityFunction =
  fmap (g . h) f `equalityFunction` (fmap g $ fmap h f)
functorComposeProperty' _ _ _ _ =
  error "This should be unreachable. GHC issue with pattern synonyms."

-- Functions.

functionEqualityProperty :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
functionEqualityProperty f g x = f x == g x