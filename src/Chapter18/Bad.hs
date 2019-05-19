-- Page 761

module Chapter18.Bad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a = CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) = CountMe (i + 1) (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe i f <*> CountMe i' a = CountMe (i + i') (f a)

instance Monad CountMe where
  return = pure
  CountMe i a >>= f =
    let CountMe _ b = f a -- <- Pattern matching. Remember that 'f a' produces a new CountMe.
    in CountMe (i + 1) b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary = CountMe <$> arbitrary <*> arbitrary

  -- ^ CountMe :: Integer -> a -> CountMe a
  --   CountMe <$> arbitrary :: Gen (CountMe Integer)
  --   Gen (CountMe _) <*> arbitrary :: Gen (CountMe Integer a)

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

one :: String -> CountMe Int
one = return . length

two :: Int -> CountMe String
two = return . flip replicate '*'

testBadMonad :: IO ()
testBadMonad = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined

  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger

-- Right Identity:

-- m >>= return == m
-- (CountMe 5 "Hello." >>= return) == CountMe 5 "Hello."
-- CountMe 6 "Hello." == CountMe 5 "Hello."
-- Nope.

-- Left Identity:

-- return x >>= f == f x
-- (return "Hello." >>= one) == one "Hello."
-- CountMe 1 6 == CountMe 0 6
-- Nope.

-- Associativity:

-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
-- (CountMe 5 "Hello." >>= one >>= two) == (CountMe 5 "Hello." >>= \x -> one x >>= two)
-- CountMe 7 '******' == CountMe 6 "Hello." >>= ("Hello." -> one "Hello." >>= two)
-- CountMe 7 '******' == CountMe 6 "Hello." >>= ("Hello." -> CountMe 0 6 >>= two)
-- CountMe 7 '******' == CountMe 6 "Hello." >>= ("Hello." -> CountMe 1 '******')
-- CountMe 7 '******' == CountMe 6 "******"
-- Nope.