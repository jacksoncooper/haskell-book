-- 9.12 Chapter Exercises, Page 341

module Standard where

-- 1.

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- 2.

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

-- 3.

myElemRecursive :: Eq a => a -> [a] -> Bool
myElemRecursive _ [] = False
myElemRecursive x (y:ys) = x == y || myElemRecursive x ys

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

-- 4.

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

-- 5.

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6.

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7.

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8.

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "Cannot find the maximum element of an empty list."
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) =
  case f x maximumOfTail of
    GT -> x
    _  -> maximumOfTail
  where maximumOfTail = myMaximumBy f xs

-- 9.

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "Cannot find the minimum element of an empty list."
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:xs) =
  case f x minimumOfTail of
    LT -> x
    _  -> minimumOfTail
  where minimumOfTail = myMinimumBy f xs

-- 10.

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare