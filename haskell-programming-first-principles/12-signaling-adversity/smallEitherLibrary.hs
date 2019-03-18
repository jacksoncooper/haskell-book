-- Chapter Exercises, p.482

import Data.Maybe (catMaybes)

-- 1.

getLeft :: Either a b -> Maybe a
getLeft (Left x)  = Just x
getLeft _         = Nothing

lefts' :: [Either a b] -> [a]
lefts' = catMaybes . map getLeft

-- 2.

getRight :: Either a b -> Maybe b
getRight (Right x)  = Just x
getRight _          = Nothing

rights' :: [Either a b] -> [b]
rights' = catMaybes . map getRight

-- 3.

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- 4.

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right x) = Just $ f x

-- 5.

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x)  = f x
either' _ g (Right y) = g y

-- 6.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (Just . f)