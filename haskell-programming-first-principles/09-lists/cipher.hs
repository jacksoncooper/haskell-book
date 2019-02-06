-- Chapter Exercises, Page 339

module Cipher where

import Data.Char

caesar :: Int -> String -> String
caesar amount = map $ shiftRight amount

unCaesar :: Int -> String -> String
unCaesar amount = map $ shiftLeft amount

shiftLeft :: Int -> Char -> Char
shiftLeft = shift . negate

shiftRight :: Int -> Char -> Char
shiftRight = shift

shift :: Int -> Char -> Char
shift amount = chr . (`mod` length unicodeSet) . (+ amount) . ord
  where unicodeSet = enumFrom $ chr 0

-- Below functions assume a Char input from the input set, otherwise they
-- produce a mapping that isn't one-to-one. Better to cover the entire
-- Unicode set and pretend this doesn't exist.

-- shift :: [Char] -> Int -> Char -> Char
-- shift set amount =
--     chr
--   . (+) setBase
--   . (`mod` length set)
--   . (+ amount)
--   . subtract setBase
--   . ord
--   where setBase = ord $ head set

-- shiftLowercase :: Int -> Char -> Char
-- shiftLowercase = shift ['a'..'z']

-- shiftUnicode :: Int -> Char -> Char
-- shiftUnicode = shift (enumFrom $ chr 0)