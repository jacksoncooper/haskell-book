import Data.Bool (bool)

-- myHead p.304

myHead :: [a] -> Maybe a
myHead []      = Nothing
myHead (x:_) = Just x


-- Exercise: EnumFromTo p.307

-- enumFromThenBool :: Bool -> Bool -> [Bool]
-- enumFromThenBool False True = [False, True]
-- enumFromThenBool True False = []
-- enumFromThenBool x _        = [x]

myEnumFromThen :: (Enum a, Ord a) => a -> a -> [a]
myEnumFromThen x y
  | x > y     = []
  | x == y    = [x]
  | otherwise = x : myEnumFromThen (succ x) y

enumFromThenBool :: Bool -> Bool -> [Bool]
enumFromThenBool = myEnumFromThen

enumFromThenOrd :: Ordering -> Ordering -> [Ordering]
enumFromThenOrd = myEnumFromThen

enumFromThenInt :: Int -> Int -> [Int]
enumFromThenInt = myEnumFromThen

enumFromThenChar :: Char -> Char -> [Char]
enumFromThenChar = myEnumFromThen


-- Exercises: Thy Fearful Symmetry p.311

-- 1.

-- myWords :: String -> [String]
-- myWords "" = []
-- myWords xs = firstWord : myWords restOfTheWords
--   where firstWord = takeWhile (\x -> x /= ' ') xs
--         restOfTheWords = drop (1 + length firstWord) xs

myWords :: String -> [String]
myWords = splitAtItem ' '

-- 2.

firstSentence = "Tyger Tyger, burning bright\n"
secondSentence = "In the forests of the night\n"
thirdSentence = "What immortal hand or eye\n"
fourthSentence = "Could frame thy fearful symmetry?"

sentences = firstSentence ++ secondSentence ++ thirdSentence ++ fourthSentence

-- myLines :: String -> [String]
-- myLines "" = []
-- myLines xs = firstSentence : myLines restOfTheSentences
--   where firstSentence = takeWhile (\x -> x /= '\n') xs
--         restOfTheSentences = drop (1 + length firstSentence) xs

myLines :: String -> [String]
myLines = splitAtItem '\n'

-- 3.

-- Questionable behavior: If the split token is located at the end of the list,
-- it is ignored.

splitAtItem :: Eq a => a -> [a] -> [[a]]
splitAtItem _ [] = []
splitAtItem item xs = firstSegment : splitAtItem item restOfTheSegments
  where firstSegment = takeWhile (\x -> x /= item) xs
        restOfTheSegments = drop (1 + length firstSegment) xs


-- Exercises: Comprehend Thy Lists p.315

-- mySqr = [x^2 | x <- [1..10]]
--         [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- 1. [x | x <- mySqr, rem x 2 == 0]
--    Yields the squares of the integers 1 to 10 that are even.
--    I.e., [4, 16, 25, 64, 100].

-- 2. [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
--    Yields every unique ordered pair that can be made from the set of the
--    squares of the integers 1 to 10 (mySqr) that are less than 50 and the
--    the set of those greater than 50.
--    I.e., [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81), (4, 100), ..., (49, 100)]

-- 3. take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
--    The first five elements of the set from #2.
--    I.e., [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)].


-- Exercises: Square Cubes p.317

-- mySquare = [x^2 | x <- [1..5]]
-- myCube = [y^3 | y <- [1..5]]

-- 1. [(x, y) | x <- mySquare, y <- myCube]
-- 2. [(x, y) | x <- mySquare, y <- myCube, x < 50, y < 50]
-- 3. length [(x, y) | x <- mySquare, y <- myCube, x < 50, y < 50]


-- Possible erratum p.323

-- "The following is a representation of a list that isn’t spine strict and is
-- awaiting something to force the evaluation:

--   :
--  / \
-- _   _

-- By default, it stops here and never evaluates even the first cons cell
-- unless it’s forced to, as we saw."

-- Shouldn't the diagram simply be:

-- _

-- If the non-spine-strict function never evaluates the first cons cell,
-- shouldn't the entire tree be unevaluated? The specific wording is that the
-- list is non-spine-strict, but I wasn't sure how to interpret that.

-- If I'm not mistaken, take is a non-spine-strict function.

-- > myList :: [Integer]; myList = [1..10]

-- > take 0 myList
-- []
-- > :sprint myList
-- myList = _

--          ^ Reflection of the tree structure mentioned above.


-- Exercises: Will it blow up? p.326

-- This exercise seems to assume an implicit 'print' before each expression.

-- 1. [x^y | x <- [1..5], y <- [2, undefined]]
--    The expression is not bottom. The list comprehension contains undefined
--    expressions, but the evaluation of these expressions is never forced.
--    When evaluated, such as if print is applied to the list comprehension, an
--    exception will be raised when evaluating 1^undefined.

-- 2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
--    The expression will return [1]. Although take is non-spine-strict,
--    Haskell's laziness ensures that the rest of the comprehension is never
--    evaluated.

-- 3. sum [1, undefined, 3]
--    The expression is bottom. Sum is a none-spine-strict function and forces
--    the evaluation of all of the items in the list.

-- 4. length [1, 2, undefined]
--    The expression is not bottom. Length is spine-strict.

-- 5. length $ [1, 2, 3] ++ undefined
--    The expression is bottom. Undefined is not a cons cell and an exception
--    will be raised when it is traversed.

-- 6. take 1 $ filter even [1, 2, 3, undefined]
--    The expression is not bottom. Because there is a single even value before
--    the undefined element, filter does not need to evaluate the list items
--    further because the take function requests a single item.

-- 7. take 1 $ filter even [1, 3, undefined]
--    The expression is bottom. Because there are no even items in the list,
--    filter (out of desperation) applies the predicate 'even' to all values in
--    the list until it reaches 'even undefined', which is bottom.

-- 8. take 1 $ filter odd [1, 3, undefined]
--    The expression is not bottom and returns [1].

-- 9. take 2 $ filter odd [1, 3, undefined]
--    The expression is not bottom and returns [1, 3].

-- 10. take 3 $ filter odd [1, 3, undefined]
--     The expression is bottom.


-- Intermission: is it in normal form? p.326

-- 1. Normal form -> weak head normal form.
-- 2. Weak head normal form. The expression is evaluated up to the cons cell
--    containing a 4 as the head and an unevaluated body (_).
-- 3. Neither.
-- 4. Neither.
-- 5. Neither.
-- 6. Neither.
-- 7. Weak head normal form.


-- Exercises: More Bottoms p.333

-- 1. take 1 $ map (+1) [undefined, 2, 3]
--    The expression is bottom. Because undefined is the first element in the
--    list, to be returned by the take function, '(+1) undefined' is forced
--    to be evaluated.

-- 2. take 1 $ map (+1) [1, undefined, 3]
--    The expression is not bottom, returns [2].

-- 3. take 2 $ map (+1) [1, undefined, 3]
--    The expression is bottom.

-- 4. itIsMystery xs = map (\x -> elem x "aeiou") xs
--    itIsMystery :: String -> [Bool], where the truth value of the nth
--    Bool in the resulting list represents whether the nth character in the
--    input String is a vowel.

-- 5A. map (^2) [1..10] =
--     [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
-- 5B. map minimum [[1..10], [10..20], [20..30]] =
--     [1, 10, 20]
-- 5C. map sum [[1..5], [1..5], [1..5]] =
--     [15, 15, 15]

-- 6.

negateThrees :: (Eq a, Num a) => [a] -> [a]
negateThrees = map negateThree
  where negateThree n = bool n (-n) (n == 3)


-- Exercises: Filtering p.335

-- 1. filter (\x -> x `mod` 3 == 0) [1..30]
-- 2. length $ filter (\x -> x `mod` 3 == 0) [1..30]
-- 3. filter (\x -> x `notElem` ["the", "a", "an"]) . words $ "the brown dog was a goof"


-- Zipping exercises p.338

-- 1.

myZip :: [a] -> [b] -> [(a, b)]
myZip (x:xs) (y:ys) = (x, y) : zip xs ys
myZip _ _ = []

-- 2.

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _ = []

-- 3.

myZipInTerms :: [a] -> [b] -> [(a, b)]
myZipInTerms = myZipWith (,)