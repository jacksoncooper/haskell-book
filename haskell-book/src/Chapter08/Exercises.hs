module Chapter08.Exercises where

import Data.List (intersperse)

-- Intermission: Exercise p.280

-- applyTimes 5 (+ 1) 5
-- (+ 1) (applyTimes 4 (+ 1) 5)
-- (+ 1) (+ 1) (applyTimes 3 (+ 1) 5)
-- (+ 1) (+ 1) (+ 1) (applyTimes 2 (+ 1) 5)
-- (+ 1) (+ 1) (+ 1) (+ 1) (applyTimes 1 (+ 1) 5)
-- (+ 1) (+ 1) (+ 1) (+ 1) (+ 1) (applyTimes 0 (+ 1) 5)
-- (+ 1) (+ 1) (+ 1) (+ 1) (+ 1) (5)
-- (+ 1) (+ 1) (+ 1) (+ 1) (6)
-- (+ 1) (+ 1) (+ 1) (7)
-- ...
-- 10


-- Chapter Exercises p.294

-- Review of types

-- 1. D
-- 2. B
-- 3. D
-- 4. B

-- Review currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny -- \y x -> x ++ " mrow " ++ y

appedCatty :: String -> String
appedCatty = cattyConny "whoops" -- \y -> "whoops mrow " ++ y

frappe :: String -> String
frappe = flippy "haha" -- \x -> x ++ " mrow haha"

-- 1. appedCatty "woohoo!" = "whoops mrow woohoo!"
-- 2. frappe "1" = "1 mrow haha"
-- 3. frappe (appedCatty "2")
--      = frappe "2 mrow haha"
--      = "whoops mrow 2 mrow haha"
-- 4. appedCatty (frappe "blue")
--      = appedCatty "blue mrow haha"
--      = "whoops mrow blue mrow haha"
-- 5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
--      = cattyConny "pink mrow haha" (cattyConny "green" "whoops mrow blue")
--      = cattyConny "pink mrow haha" "green mrow whoops mrow blue"
--      = "pink mrow haha mrow green mrow whoops mrow blue"

-- This is surreal.

-- 6. cattyConny (flippy "Pugs" "are") "awesome"
--      = cattyConny ("are mrow Pugs") "awesome"
--      = "are mrow Pugs mrow awesome"

-- Recursion

-- 1.

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

-- dividedBy 15 2
--   = go 13 2 1
--   = go 11 2 2
--   = go 9 2 3
--   = go 7 2 4
--   = go 5 2 5
--   = go 3 2 6
--   = go 1 2 7
--   = (7, 1)

-- 2.

sumFromOneTo :: (Eq a, Num a) => a -> a
sumFromOneTo 1 = 1
sumFromOneTo n = n + sumFromOneTo (n - 1)

-- 3.

multiply :: (Integral a) => a -> a -> a
multiply _ 0 = 0
multiply x y = x + multiply x (y - 1)

-- Fixing dividedBy

-- Question for #haskell: Constraints on data constructors?
-- E.g. data DividedResult = Result (Integral a => a) | DividedByZero

data DividedResult = Result Integer | DividedByZero
  deriving Show

safeDividedBy :: Integer -> Integer -> DividedResult
safeDividedBy numerator denominator = go (abs numerator) (abs denominator) 0
  where go num denom times
          | denom == 0  = DividedByZero
          | num < denom = Result $ sign * times
          | otherwise = go (num - denom) denom (times + 1)
          where sign = signum $ numerator * denominator

-- McCarthy 91 function

carthyNinetyOne :: Integral a => a -> a
carthyNinetyOne n
  | n > 100 = n - 10
  | n <= 100 = carthyNinetyOne . carthyNinetyOne $ n + 11

-- Numbers into words

digitToWord :: Int -> String
digitToWord =  (!!) ["zero", "one", "two", "three", "four", "five",
  "six", "seven", "eight", "nine"]

digits :: Int -> [Int]
digits n
  | n >= 10 = digits (n `div` 10) ++ [n `mod` 10]
  | otherwise = [n]

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits