-- Chapter Exercises, Page 485

module Chapter12.Iterate where

-- 1.

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : (myIterate f $ f x)

-- 2.

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f y =
  case f y of
    Just (x, y') -> x : myUnfoldr f y'
    Nothing      -> []

-- 3.

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\x -> Just (x, f x))