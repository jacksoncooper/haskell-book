-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

-- Exercise 1
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n < 1     = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

-- Exercise 2
-- TODO: Pattern matching to start from right?
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = (2 * x) : y : doubleEveryOther zs
