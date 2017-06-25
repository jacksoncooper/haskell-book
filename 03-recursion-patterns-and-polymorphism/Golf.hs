-- https://www.seas.upenn.edu/~cis194/spring13/lectures/03-rec-poly.html

{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Exercise 1
-- TODO: Smaller!

skips :: [a] -> [[a]]
skips xs = [f n xs | n <- [1..length xs]]

f :: Int -> [a] -> [a]
f n l = case drop (n - 1) l of
    (x : xs) -> x : f n xs
    []       -> []
