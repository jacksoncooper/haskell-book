-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

-- Exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0     = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2
-- TODO: Come back to this one. This solution is kind of unwieldy. [1]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

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
validate n
    | n < 0     = False
    | otherwise = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi numberOfDisks startPeg endPeg sparePeg = hanoi (numberOfDisks - 1) startPeg sparePeg endPeg ++ [(startPeg, endPeg)] ++ hanoi (numberOfDisks - 1) sparePeg endPeg startPeg

-- [1] William's Solution

-- data Parity = Even | Odd
--
-- mapAlternating :: (a -> a) -> [a] -> ([a], Parity)
-- mapAlternating _ [] = ([], Even)
-- mapAlternating f (x : xs) =
--     let (xs', parity) = mapAlternating f xs
--     in case parity of
--         Even -> (x : xs', Odd)
--         Odd -> (f x : xs', Even)
--
-- doubleEveryOther :: [Integer] -> [Integer]
-- doubleEveryOther = fst . mapAlternating (* 2)
