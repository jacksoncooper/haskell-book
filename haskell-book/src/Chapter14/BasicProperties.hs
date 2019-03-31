-- Chapter Exercises, Page 570

module Chapter14.BasicProperties where

import Data.List (sort)
import Test.QuickCheck

-- 1.

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

halfIdentityProperty :: (Eq a, Fractional a) => a -> Bool
halfIdentityProperty x = halfIdentity x == x

-- 2.

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, t) = (Just y, x >= y)

listOrderedProperty :: (Ord a) => [a] -> Bool
listOrderedProperty = listOrdered . sort

-- 3.

plusAssociativeProperty :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociativeProperty x y z = x + (y + z) == (x + y) + z

plusCommutativeProperty :: (Eq a, Num a) => a -> a -> Bool
plusCommutativeProperty x y = x + y == y + x

-- 4.

timesAssociativeProperty :: (Eq a, Num a) => a -> a -> a -> Bool
timesAssociativeProperty x y z = x * (y * z) == (x * y) + z

timesCommutativeProperty :: (Eq a, Num a) => a -> a -> Bool
timesCommutativeProperty x y = x * y == y * x

-- 5.

quotRemProperty :: Integral a => NonZero a -> NonZero a -> Bool
quotRemProperty (NonZero x) (NonZero y) = (quot x y) * y + (rem x y) == x

divModProperty :: Integral a => NonZero a -> NonZero a -> Bool
divModProperty (NonZero x) (NonZero y) = (div x y) * y + (mod x y) == x

-- 6.

exponentAssociativeProperty :: (Eq a, Num a, Integral b) => a -> b -> b -> Bool
exponentAssociativeProperty x y z = (x ^ y) ^ z == x ^ (y ^ z)

exponentCommutativeProperty :: (Eq a, Integral a) => a -> a -> Bool
exponentCommutativeProperty x y = x ^ y == y ^ x

-- 7.

reverseProperty :: Eq a => [a] -> Bool
reverseProperty xs = (reverse . reverse $ xs) == id xs

-- 8.

functionApplicationProperty :: Eq b => Fun a b -> a -> Bool
functionApplicationProperty (Fn f) x = (f $ x) == f x

functionCompositionProperty :: Eq c => Fun b c -> Fun a b -> a -> Bool
functionCompositionProperty (Fn f) (Fn g) x = (f . g $ x) == f (g x)

-- 9.

appendProperty :: Eq a => [a] -> [a] -> Bool
appendProperty xs ys = foldr (:) ys xs == xs ++ ys

concatProperty :: Eq a => [[a]] -> Bool
concatProperty xs = foldr (++) [] xs == concat xs

-- 10.

takeProperty :: Int -> [a] -> Bool
takeProperty n xs = length (take n xs) == n

-- 11.

roundTripProperty :: (Eq a, Read a, Show a) => a -> Bool
roundTripProperty x = (read $ show x) == x

-- 12.

square :: Num a => a -> a
square x = x * x

squareIdentity :: Floating a => a -> a
squareIdentity = square . sqrt

squareIdentityProperty :: (Eq a, Floating a) => a -> Bool
squareIdentityProperty x = squareIdentity x == x

main :: IO ()
main = do
  -- 1.

  putStrLn "Testing halfIdentity."
  quickCheck (halfIdentityProperty :: Float -> Bool)
  quickCheck (halfIdentityProperty :: Double -> Bool)

  -- 2.

  putStrLn "Testing listOrdered."
  quickCheck (listOrderedProperty :: [Float] -> Bool)
  quickCheck (listOrderedProperty :: [Double] -> Bool)
  quickCheck (listOrderedProperty :: [Int] -> Bool)
  quickCheck (listOrderedProperty :: [Integer] -> Bool)
  quickCheck (listOrderedProperty :: [Char] -> Bool)
  quickCheck (listOrderedProperty :: [String] -> Bool)

  -- 3.

  putStrLn "Testing plusAssociativeProperty."
  -- quickCheck (plusAssociativeProperty :: Float -> Float -> Float -> Bool)
  -- quickCheck (plusAssociativeProperty :: Double -> Double -> Double -> Bool)
  quickCheck (plusAssociativeProperty :: Int -> Int -> Int -> Bool)
  quickCheck (plusAssociativeProperty :: Integer -> Integer -> Integer -> Bool)

  putStrLn "Testing plusCommutativeProperty."
  quickCheck (plusCommutativeProperty :: Float -> Float -> Bool)
  quickCheck (plusCommutativeProperty :: Double -> Double -> Bool)
  quickCheck (plusCommutativeProperty :: Int -> Int -> Bool)
  quickCheck (plusCommutativeProperty :: Integer -> Integer -> Bool)

  -- 4.

  putStrLn "Testing timesAssociativeProperty."
  -- quickCheck (timesAssociativeProperty :: Float -> Float -> Float -> Bool)
  -- quickCheck (timesAssociativeProperty :: Double -> Double -> Double -> Bool)
  -- quickCheck (timesAssociativeProperty :: Int -> Int -> Int -> Bool)
  -- quickCheck (timesAssociativeProperty :: Integer -> Integer -> Integer -> Bool)

  putStrLn "Testing timesCommutativeProperty."
  quickCheck (timesCommutativeProperty :: Float -> Float -> Bool)
  quickCheck (timesCommutativeProperty :: Double -> Double -> Bool)
  quickCheck (timesCommutativeProperty :: Int -> Int -> Bool)
  quickCheck (timesCommutativeProperty :: Integer -> Integer -> Bool)

  -- 5.

  -- From StackOverflow answer by Yuuri: https://bit.ly/2I2Wq67.
  -- newtype NonZero a = NonZero {getNonZero :: a}
     -- Defined in ‘Test.QuickCheck.Modifiers’

  putStrLn "Testing quotRemProperty."
  quickCheck (quotRemProperty :: NonZero Int -> NonZero Int -> Bool)
  quickCheck (quotRemProperty :: NonZero Integer -> NonZero Integer -> Bool)

  putStrLn "Testing divModProperty."
  quickCheck (divModProperty :: NonZero Int -> NonZero Int -> Bool)
  quickCheck (divModProperty :: NonZero Integer -> NonZero Integer -> Bool)

  -- 6.

  putStrLn "Testing exponentAssociativeProperty."
  -- quickCheck (exponentAssociativeProperty :: Float -> Int -> Int -> Bool)
  -- quickCheck (exponentAssociativeProperty :: Float -> Integer -> Integer -> Bool)
  -- quickCheck (exponentAssociativeProperty :: Double -> Int -> Int -> Bool)
  -- quickCheck (exponentAssociativeProperty :: Double -> Integer -> Integer -> Bool)

  putStrLn "Testing exponentCommutativeProperty."
  -- quickCheck (exponentCommutativeProperty :: Int -> Int -> Bool)
  -- quickCheck (exponentCommutativeProperty :: Integer -> Integer -> Bool)

  -- 7.

  putStrLn "Testing reverseProperty."
  quickCheck (reverseProperty :: [Float] -> Bool)
  quickCheck (reverseProperty :: [Double] -> Bool)
  quickCheck (reverseProperty :: [Int] -> Bool)
  quickCheck (reverseProperty :: [Integer] -> Bool)
  quickCheck (reverseProperty :: [Char] -> Bool)
  quickCheck (reverseProperty :: [String] -> Bool)

  -- 8.

  -- From StackOverflow answer by Li-yao Xia: http://bitly.com/2HX4JjL.
  -- data Fun a b = ???
     -- Defined in ‘Test.QuickCheck.Function’

  putStrLn "Testing functionApplicationProperty."
  quickCheck (functionApplicationProperty :: Fun Integer Integer -> Integer -> Bool)
  quickCheck (functionApplicationProperty :: Fun String Integer -> String -> Bool)
  quickCheck (functionApplicationProperty :: Fun [String] String -> [String] -> Bool)

  putStrLn "Testing functionCompositionProperty."
  quickCheck (functionCompositionProperty :: Fun Integer Integer -> Fun String Integer -> String -> Bool)
  quickCheck (functionCompositionProperty :: Fun String Integer -> Fun [String] String -> [String] -> Bool)

  -- 9.

  putStrLn "Testing appendProperty."
  quickCheck (appendProperty :: [Double] -> [Double] -> Bool)  
  quickCheck (appendProperty :: [Integer] -> [Integer] -> Bool)
  quickCheck (appendProperty :: [String] -> [String] -> Bool)

  putStrLn "Testing concatProperty."
  quickCheck (concatProperty :: [[Double]] -> Bool)
  quickCheck (concatProperty :: [[Integer]] -> Bool)
  quickCheck (concatProperty :: [[String]] -> Bool)

  -- 10.

  putStrLn "Testing takeProperty."
  -- quickCheck (takeProperty :: Int -> [Double] -> Bool)
  -- quickCheck (takeProperty :: Int -> [Integer] -> Bool)
  -- quickCheck (takeProperty :: Int -> [String] -> Bool)
  
  -- 11.

  putStrLn "Testing roundTripProperty."
  quickCheck (roundTripProperty :: Double -> Bool)
  quickCheck (roundTripProperty :: Integer -> Bool)
  quickCheck (roundTripProperty :: String -> Bool)
  quickCheck (roundTripProperty :: [Integer] -> Bool)
  quickCheck (roundTripProperty :: [String] -> Bool)

  -- 12.

  putStrLn "Testing squareIdentityProperty."
  -- quickCheck (squareIdentityProperty :: Float -> Bool)
  -- quickCheck (squareIdentityProperty :: Double -> Bool)