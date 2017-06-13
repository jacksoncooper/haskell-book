-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

-- Exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n < 1     = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Exercise 2
-- TODO: Come back to this one. This solution is kind of unwieldy.

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherRev (reverse xs))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:y:zs) = x : (2 * y) : doubleEveryOtherRev zs

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- Exercise 4

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
