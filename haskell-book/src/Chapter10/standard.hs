-- Chapter Exercises, Page 380

-- 1.

myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- 3.

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==) x) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

-- 4.

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myReverse' :: [a] -> [a]
myReverse' = foldr (\x y -> y ++ [x]) []

-- 5.

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6.

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x y -> if f x then x : y else y) []

-- 7.

squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9.

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x y -> if f x y == GT then x else y) (last xs) (init xs)

-- 11.

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldr (\x y -> if f x y == LT then x else y) (last xs) (init xs)