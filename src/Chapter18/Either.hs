-- Page 758

module Chapter18.Either where

import Control.Applicative (liftA)
import Control.Monad (ap)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap = liftA

instance Applicative (Sum a) where
  pure = return
  (<*>) = ap

instance Monad (Sum a) where
  return = Second
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b