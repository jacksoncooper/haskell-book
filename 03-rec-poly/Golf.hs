-- https://www.seas.upenn.edu/~cis194/spring13/lectures/03-rec-poly.html

{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Exercise 1
-- TODO: Not sure if improvement.

skips :: [a] -> [[a]]
skips xs = [takeEvery n xs | n <- [1..length xs]]

takeEvery :: Int -> [a] -> [a]
takeEvery n = map snd . filter fst . zip (cycle $ boolList n)

boolList :: Int -> [Bool]
boolList 1 = [True]
boolList n = [False] ++ (boolList $ n - 1)

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [y | (x, y, z) <- (split xs), y > x && y > z]

split :: [a] -> [(a, a, a)]
split (x:y:z:xs) = (x,y,z) : (split $ y:z:xs)
split _ = []
