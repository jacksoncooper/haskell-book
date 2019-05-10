-- Page 689

module Chapter17.Lookups where

import Data.List (elemIndex)

-- 1.

added :: Maybe Integer
added = (+ 3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

-- y = Just 6

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

-- z = Just 5

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- (,) <$> (Just 6) = (Just (6, ))
-- (Just (6, )) <*> (Just 5) = (Just (6, 5))

-- 3.

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

-- x = Just 2

y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

-- y = Just 3

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

-- 4.

xs = [1, 2, 3]
ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

-- x' = Just 6

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

-- y'' = Just 5 

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x' <*> y''