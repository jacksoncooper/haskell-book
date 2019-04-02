-- Chapter Exercises, Page 570

module Chapter14.BasicProperties where

import Data.List (sort)
import Test.Hspec
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
main = hspec $ do
  -- 1.

  describe "halfIdentityProperty" $ do
    it "Testing halfIdentityProperty :: Float -> Bool." $ do
      property (halfIdentityProperty :: Float -> Bool)
    it "Testing halfIdentityProperty :: Double -> Bool." $ do
      property (halfIdentityProperty :: Double -> Bool)

  -- 2.

  describe "listOrderedProperty" $ do
    it "Testing listOrderedProperty :: [Float] -> Bool." $ do
      property (listOrderedProperty :: [Float] -> Bool)
    it "Testing listOrderedProperty :: [Double] -> Bool." $ do
      property (listOrderedProperty :: [Double] -> Bool)
    it "Testing listOrderedProperty :: [Int] -> Bool." $ do
      property (listOrderedProperty :: [Int] -> Bool)
    it "Testing listOrderedProperty :: [Integer] -> Bool." $ do
      property (listOrderedProperty :: [Integer] -> Bool)
    it "Testing listOrderedProperty :: [Char] -> Bool." $ do
      property (listOrderedProperty :: [Char] -> Bool)
    it "Testing listOrderedProperty :: [String] -> Bool." $ do
      property (listOrderedProperty :: [String] -> Bool)

  -- 3.

  describe "plusAssociativeProperty" $ do
    it "Testing plusAssociativeProperty :: Float -> Float -> Float -> Bool." $ do
      property (plusAssociativeProperty :: Float -> Float -> Float -> Bool)
    it "Testing plusAssociativeProperty :: Double -> Double -> Double -> Bool." $ do
      property (plusAssociativeProperty :: Double -> Double -> Double -> Bool)
    it "Testing plusAssociativeProperty :: Int -> Int -> Int -> Bool." $ do
      property (plusAssociativeProperty :: Int -> Int -> Int -> Bool)
    it "Testing plusAssociativeProperty :: Integer -> Integer -> Integer -> Bool." $ do
      property (plusAssociativeProperty :: Integer -> Integer -> Integer -> Bool)

  describe "plusCommutativeProperty" $ do
    it "Testing plusCommutativeProperty :: Float -> Float -> Bool." $ do
      property (plusCommutativeProperty :: Float -> Float -> Bool)
    it "Testing plusCommutativeProperty :: Double -> Double -> Bool." $ do
      property (plusCommutativeProperty :: Double -> Double -> Bool)
    it "Testing plusCommutativeProperty :: Int -> Int -> Bool." $ do
      property (plusCommutativeProperty :: Int -> Int -> Bool)
    it "Testing plusCommutativeProperty :: Integer -> Integer -> Bool." $ do
      property (plusCommutativeProperty :: Integer -> Integer -> Bool)

  -- 4.

  describe "timesAssociativeProperty" $ do
    it "Testing timesAssociativeProperty :: Float -> Float -> Float -> Bool." $ do
      property (timesAssociativeProperty :: Float -> Float -> Float -> Bool)
    it "Testing timesAssociativeProperty :: Double -> Double -> Double -> Bool." $ do
      property (timesAssociativeProperty :: Double -> Double -> Double -> Bool)
    it "Testing timesAssociativeProperty :: Int -> Int -> Int -> Bool." $ do
      property (timesAssociativeProperty :: Int -> Int -> Int -> Bool)
    it "Testing timesAssociativeProperty :: Integer -> Integer -> Integer -> Bool." $ do
      property (timesAssociativeProperty :: Integer -> Integer -> Integer -> Bool)

  describe "timesCommutativeProperty" $ do
    it "Testing timesCommutativeProperty :: Float -> Float -> Bool." $ do
      property (timesCommutativeProperty :: Float -> Float -> Bool)
    it "Testing timesCommutativeProperty :: Double -> Double -> Bool." $ do
      property (timesCommutativeProperty :: Double -> Double -> Bool)
    it "Testing timesCommutativeProperty :: Int -> Int -> Bool." $ do
      property (timesCommutativeProperty :: Int -> Int -> Bool)
    it "Testing timesCommutativeProperty :: Integer -> Integer -> Bool." $ do
      property (timesCommutativeProperty :: Integer -> Integer -> Bool)

  -- 5.

  -- -- From StackOverflow answer by Yuuri: https://bit.ly/2I2Wq67.
  -- -- newtype NonZero a = NonZero {getNonZero :: a}
  --    -- Defined in ‘Test.QuickCheck.Modifiers’

  describe "quotRemProperty" $ do
    it "Testing quotRemProperty :: NonZero Int -> NonZero Int -> Bool." $ do
      property (quotRemProperty :: NonZero Int -> NonZero Int -> Bool)
    it "Testing quotRemProperty :: NonZero Integer -> NonZero Integer -> Bool." $ do
      property (quotRemProperty :: NonZero Integer -> NonZero Integer -> Bool)

  describe "divModProperty" $ do
    it "Testing divModProperty :: NonZero Int -> NonZero Int -> Bool." $ do
      property (divModProperty :: NonZero Int -> NonZero Int -> Bool)
    it "Testing divModProperty :: NonZero Integer -> NonZero Integer -> Bool." $ do
      property (divModProperty :: NonZero Integer -> NonZero Integer -> Bool)

  -- 6.

  describe "exponentAssociativeProperty" $ do
    it "Testing exponentAssociativeProperty :: Float -> Int -> Int -> Bool." $ do
      property (exponentAssociativeProperty :: Float -> Int -> Int -> Bool)
    it "Testing exponentAssociativeProperty :: Float -> Integer -> Integer -> Bool." $ do
      property (exponentAssociativeProperty :: Float -> Integer -> Integer -> Bool)
    it "Testing exponentAssociativeProperty :: Double -> Int -> Int -> Bool." $ do
      property (exponentAssociativeProperty :: Double -> Int -> Int -> Bool)
    it "Testing exponentAssociativeProperty :: Double -> Integer -> Integer -> Bool." $ do
      property (exponentAssociativeProperty :: Double -> Integer -> Integer -> Bool)

  describe "exponentCommutativeProperty" $ do
    it "Testing exponentCommutativeProperty :: Int -> Int -> Bool." $ do
      property (exponentCommutativeProperty :: Int -> Int -> Bool)
    it "Testing exponentCommutativeProperty :: Integer -> Integer -> Bool." $ do
      property (exponentCommutativeProperty :: Integer -> Integer -> Bool)

  -- 7.

  describe "reverseProperty" $ do
    it "Testing reverseProperty :: [Float] -> Bool." $ do
      property (reverseProperty :: [Float] -> Bool)
    it "Testing reverseProperty :: [Double] -> Bool." $ do
      property (reverseProperty :: [Double] -> Bool)
    it "Testing reverseProperty :: [Int] -> Bool." $ do
      property (reverseProperty :: [Int] -> Bool)
    it "Testing reverseProperty :: [Integer] -> Bool." $ do
      property (reverseProperty :: [Integer] -> Bool)
    it "Testing reverseProperty :: [Char] -> Bool." $ do
      property (reverseProperty :: [Char] -> Bool)
    it "Testing reverseProperty :: [String] -> Bool." $ do
      property (reverseProperty :: [String] -> Bool)

  -- 8.

  -- From StackOverflow answer by Li-yao Xia: http://bitly.com/2HX4JjL.
  -- data Fun a b = ???
     -- Defined in ‘Test.QuickCheck.Function’

  describe "functionApplicationProperty" $ do
    it "Testing functionApplicationProperty :: Fun Integer Integer -> Integer -> Bool." $ do
      property (functionApplicationProperty :: Fun Integer Integer -> Integer -> Bool)
    it "Testing functionApplicationProperty :: Fun String Integer -> String -> Bool." $ do
      property (functionApplicationProperty :: Fun String Integer -> String -> Bool)
    it "Testing functionApplicationProperty :: Fun [String] String -> [String] -> Bool." $ do
      property (functionApplicationProperty :: Fun [String] String -> [String] -> Bool)

  describe "functionCompositionProperty" $ do
    it "Testing functionCompositionProperty :: Fun Integer Integer -> Fun String Integer -> String -> Bool." $ do
      property (functionCompositionProperty :: Fun Integer Integer -> Fun String Integer -> String -> Bool)
    it "Testing functionCompositionProperty :: Fun String Integer -> Fun [String] String -> [String] -> Bool." $ do
      property (functionCompositionProperty :: Fun String Integer -> Fun [String] String -> [String] -> Bool)

  -- 9.

  describe "appendProperty" $ do
    it "Testing appendProperty :: [Double] -> [Double] -> Bool." $ do
      property (appendProperty :: [Double] -> [Double] -> Bool)
    it "Testing appendProperty :: [Integer] -> [Integer] -> Bool." $ do
      property (appendProperty :: [Integer] -> [Integer] -> Bool)
    it "Testing appendProperty :: [String] -> [String] -> Bool." $ do
      property (appendProperty :: [String] -> [String] -> Bool)

  describe "concatProperty" $ do
    it "Testing concatProperty :: [[Double]] -> Bool." $ do
      property (concatProperty :: [[Double]] -> Bool)
    it "Testing concatProperty :: [[Integer]] -> Bool." $ do
      property (concatProperty :: [[Integer]] -> Bool)
    it "Testing concatProperty :: [[String]] -> Bool." $ do
      property (concatProperty :: [[String]] -> Bool)

  -- 10.

  describe "takeProperty" $ do
    it "Testing takeProperty :: Int -> [Double] -> Bool." $ do
      property (takeProperty :: Int -> [Double] -> Bool)
    it "Testing takeProperty :: Int -> [Integer] -> Bool." $ do
      property (takeProperty :: Int -> [Integer] -> Bool)
    it "Testing takeProperty :: Int -> [String] -> Bool." $ do
      property (takeProperty :: Int -> [String] -> Bool)

  -- 11.

  describe "roundTripProperty" $ do
    it "Testing roundTripProperty :: Double -> Bool." $ do
      property (roundTripProperty :: Double -> Bool)
    it "Testing roundTripProperty :: Integer -> Bool." $ do
      property (roundTripProperty :: Integer -> Bool)
    it "Testing roundTripProperty :: String -> Bool." $ do
      property (roundTripProperty :: String -> Bool)
    it "Testing roundTripProperty :: [Integer] -> Bool." $ do
      property (roundTripProperty :: [Integer] -> Bool)
    it "Testing roundTripProperty :: [String] -> Bool." $ do
      property (roundTripProperty :: [String] -> Bool)

  -- 12.

  describe "squareIdentityProperty" $ do
    it "Testing squareIdentityProperty :: Float -> Bool." $ do
      property (squareIdentityProperty :: Float -> Bool)
    it "Testing squareIdentityProperty :: Double -> Bool." $ do
      property (squareIdentityProperty :: Double -> Bool)