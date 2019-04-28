module Chapter16.Properties where

import Test.QuickCheck

functorIdentityProperty :: (Eq (f a), Functor f) => f a -> Bool
functorIdentityProperty f =
  f == fmap id f

functorComposeProperty :: (Eq (f c), Functor f) => f a -> Fun b c -> Fun a b -> Bool
functorComposeProperty f (Fn g) (Fn h) =
  fmap (g . h) f == (fmap g $ fmap h f)
functorComposeProperty _ _ _ =
  error "This should be unreachable. GHC issue with pattern synonyms."