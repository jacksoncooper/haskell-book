-- Chapter Exercises, p.451

import Data.Char

data Direction = Left | Right
  deriving Eq

vignere :: Direction -> String -> String -> String -> String
vignere direction set key message = unwords $ zipWith (shiftWord shiftFunction set) (words message) (spreadKey key $ words message)
  where
    shiftFunction = if direction == Vignere.Right then shiftRightUppercase else shiftLeftUppercase

spreadKey :: String -> [String] -> [String]
spreadKey "" words = words
spreadKey key words = go (cycle key) words
  where
    go _ [] = []
    go key (x:xs) = take (length x) key : go (drop (length x) key) xs

shiftWord :: (Int -> Char -> Char) -> String -> String -> String -> String
shiftWord shiftFunction set word key = zipWith (shiftLetter shiftFunction set) word key

shiftLetter :: (Int -> Char -> Char) -> String -> Char -> Char -> Char
shiftLetter shiftFunction set character keyCharacter = shiftFunction (shiftAmount set (head set) keyCharacter) character
    
shiftAmount :: String -> Char -> Char -> Int
shiftAmount set origin = (`mod` length set) . (subtract $ ord origin) . ord

-- Adapted from previous cipher exercise:

shift :: String -> Int -> Char -> Char
shift set amount =
    chr
  . (+) setBase
  . (`mod` length set)
  . (+ amount)
  . subtract setBase
  . ord
  where
    setBase = ord $ head set

shiftRightUppercase :: Int -> Char -> Char
shiftRightUppercase = shift ['A'..'Z']

shiftLeftUppercase :: Int -> Char -> Char
shiftLeftUppercase = shift ['A'..'Z'] . negate