-- https://www.seas.upenn.edu/~cis194/spring13/lectures/03-rec-poly.html

{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Exercise 1

skips :: [a] -> [[a]]
skips xs = [takeEvery n xs | n <- [1..length xs]]

takeEvery :: Int -> [a] -> [a]
takeEvery n = map snd . filter fst . zip (cycle $ boolList n)

boolList :: Int -> [Bool]
boolList n = replicate (n-1) False ++ [True]

-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [y | (x, y, z) <- (split xs), y > x && y > z]

split :: [a] -> [(a, a, a)]
split (x:y:z:zs) = (x,y,z) : (split $ y:z:zs)
split _ = []

-- Exercise 3

histogram :: [Integer] -> String
histogram xs = (unlines . reverse . body $ count xs) ++ "==========\n0123456789\n"

body :: [Int] -> [String]
body xs
    | filter (> 0) xs == []  = []
    | otherwise              = toRow xs : body (map pred xs)

toRow :: [Int] -> String
toRow = map (\x -> if x > 0 then '*' else ' ')

count :: [Integer] -> [Int]
count xs = map (\x -> length $ filter (== x) xs) [0..9]
