-- Page 725

module Chapter17.Validation where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) = applyValidation

applyValidation :: Monoid e => Validation e (a -> b) -> Validation e a -> Validation e b
applyValidation (Success' f) (Success' a) = Success' $ f a
applyValidation (Success' _) (Failure' e) = Failure' e
applyValidation (Failure' e) (Success' _) = Failure' e
applyValidation (Failure' e) (Failure' e') = Failure' $ e <> e'

-- Testing.

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    elements [Failure' e, Success' a]

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (=-=) = eq

myValidation :: Validation String (Integer, Integer, Integer)
myValidation = undefined

testValidationApplicative :: IO ()
testValidationApplicative = quickBatch $ applicative myValidation