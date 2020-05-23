module Chapter10.Exercises where

import Data.Time

-- Exercises: Understanding Folds p.365

-- 1. B and C

-- 2.
-- foldl (flip (*)) 1 [1, 2, 3]
-- foldl (flip (*)) ((flip (*)) 1 1) [2, 3]
-- foldl (flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) [3]
-- foldl (flip (*)) ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3) []
-- ((flip (*)) ((flip (*)) ((flip (*)) 1 1) 2) 3)
-- ((flip (*)) ((flip (*)) (1) 2) 3)
-- ((flip (*)) (2) 3)
-- 6

-- 3. C

-- 4. A

-- 5A. foldr (++) [] ["woot", "WOOT", "woot"]
-- 5B. foldr max 'a' "fear is the little death"
-- 5C. foldr (&&) True [False, True]
-- 5D. foldr (||) True [False, True]
--     Nope, because the accumulator is True.
-- 5E. foldl (flip $ (++) . show) "" [1..5]
-- 5F. foldr const 0 [1..5]
-- 5G. foldr const 'a' "tacos"
-- 5H. foldl (flip const) 'a' "burritos"


-- Exercises: Database Processing p.371

data DatabaseItem = DBString String | DBNumber Integer | DBDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DBDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DBNumber 9001
  , DBString "Hello, world!"
  , DBDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1.

filterDBDate :: [DatabaseItem] -> [UTCTime]
filterDBDate = foldr foldDatabaseItem []
  where
    foldDatabaseItem item rest =
      case item of
        (DBDate utcTime) -> utcTime : rest
        _ -> rest

-- 2.

filterDBNumber :: [DatabaseItem] -> [Integer]
filterDBNumber = foldr foldDatabaseItem []
  where
    foldDatabaseItem item rest =
      case item of
        (DBNumber number) -> number : rest
        _ -> rest

-- 3.

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = foldr max (last dbDates) (init dbDates)
  where
    dbDates = filterDBDate items

-- 4.

sumDatabase :: [DatabaseItem] -> Integer
sumDatabase = foldr (+) 0 . filterDBNumber


-- Scans Exercises p.279

fibs = 1 : scanl (+) 1 fibs

-- 1.

twentyFibs = take 20 fibs

-- 2.

lessThanOneHundredFibs = filter (\x -> x < 100) fibs

-- 3.

folder = (\accumulator previous -> accumulator * succ (accumulator `div` previous)) 
factorial = 1 : scanl folder 1 factorial