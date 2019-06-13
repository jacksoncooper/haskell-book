-- Page 867

module Chapter22.Stretch where

import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)

x :: [Integer]
x = [1, 2, 3]

y :: [Integer]
y = [4, 5, 6]

z :: [Integer]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = liftA2 (,) z' z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (> 3) (< 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Integer
s' = summed <$> x1

-- 1.

one :: Integral a => a -> Bool
one = foldr (&&) False . sequA

-- 2.

two :: Maybe [Bool]
two = sequA <$> s'

-- 3.

three :: Maybe Bool
three = bolt <$> ys

main :: IO ()
main = do
  putStr "sequenceA [Just 3, Just 2, Just 1]: "
  print $ sequenceA [Just 3, Just 2, Just 1]

  putStr "sequenceA [[1, 2, 3], [4, 5, 6]]: "
  print $ sequenceA [x, y]

  putStr "summed <$> ((,) <$> xs <*> ys): "
  print $ summed <$> x1

  putStr "fmap summed ((,) <$> xs <*> zs): "
  print $ fmap summed ((,) <$> xs <*> zs)

  putStr "bolt 7: "
  print $ bolt 7

  putStr "fmap bolt z: "
  print $ fmap bolt z

  putStr "sequenceA [(> 3), (< 8), even] 7: "
  print $ sequenceA [(> 3), (< 8), even] 7