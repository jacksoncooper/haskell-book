-- Chapter Exercises, Page 572

module Chapter14.Idempotence where

import Data.List (sort)
import Test.QuickCheck

import Chapter11.AsPatterns (capitalizeWord)

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

nTimes :: Int -> (a -> a) -> a -> a
nTimes n f x = foldr ($) x (replicate n f)

-- 1.

capitalizeIdempotenceProperty :: String -> Bool
capitalizeIdempotenceProperty x =
     capitalizeWord x == twice capitalizeWord x
  && capitalizeWord x == fourTimes capitalizeWord x

capitalizeIdempotenceProperty' :: Positive Int -> String -> Bool
capitalizeIdempotenceProperty' (Positive n) x =
  capitalizeWord x == nTimes n capitalizeWord x

-- 2.

sortIdempotenceProperty :: Ord a => [a] -> Bool
sortIdempotenceProperty x =
     sort x == twice sort x
  && sort x == fourTimes sort x

sortIdempotenceProperty' :: Ord a => Positive Int -> [a] -> Bool
sortIdempotenceProperty' (Positive n) x =
  sort x == nTimes n sort x

main :: IO ()
main = do
  putStrLn "Testing capitalizeIdempotenceProperty."
  quickCheck capitalizeIdempotenceProperty

  putStrLn "Testing capitalizeIdempotenceProperty'."
  quickCheck capitalizeIdempotenceProperty'

  putStrLn "Testing sortIdempotenceProperty."
  quickCheck (sortIdempotenceProperty :: [Double] -> Bool)
  quickCheck (sortIdempotenceProperty :: [Integer] -> Bool)
  quickCheck (sortIdempotenceProperty :: [String] -> Bool)

  putStrLn "Testing sortIdempotenceProperty'."
  quickCheck (sortIdempotenceProperty' :: Positive Int -> [Double] -> Bool)
  quickCheck (sortIdempotenceProperty' :: Positive Int -> [Integer] -> Bool)
  quickCheck (sortIdempotenceProperty' :: Positive Int -> [String] -> Bool)