-- Chapter Exercises, p.480

-- 1.

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing  = True

-- 2.

maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' _ f (Just x) = f x
maybe' y _ Nothing  = y

-- 3.

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe y Nothing  = y

-- 4.

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing  = []

-- 5.

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr ((++) . maybeToList) []

-- 6.

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
  | all isJust xs = Just (catMaybes xs)
  | otherwise     = Nothing