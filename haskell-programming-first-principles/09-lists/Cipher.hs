-- Chapter Exercises, Pages 339 and 529

module Cipher where

import Data.Char
import System.IO

type ShiftFunction = Int -> Char -> Char

-- Note: The 'shift' function can produce mappings that aren't one-to-one if a
-- character outside of the input set is used. The below function resolves this
-- by expanding the input set to all possible characters but produces
-- nontraditional characters.

-- shift :: Int -> Char -> Char
-- shift amount = chr . (`mod` length unicodeSet) . (+ amount) . ord
--   where unicodeSet = enumFrom $ chr 0

shift :: String -> ShiftFunction
shift set amount =
    chr
  . (+) origin
  . (`mod` length set)
  . (+ amount)
  . subtract origin
  . ord
  where origin = ord $ head set

shiftUppercase :: ShiftFunction
shiftUppercase = shift ['A'..'Z']

shiftLeftUppercase :: ShiftFunction
shiftLeftUppercase = shiftUppercase . negate

shiftRightUppercase :: ShiftFunction
shiftRightUppercase = shiftUppercase

caesar :: ShiftFunction -> Int -> String -> String
caesar shiftFunction amount = unwords . map (map $ shiftFunction amount) . words

encode :: Int -> String -> String
encode = caesar shiftRightUppercase

decode :: Int -> String -> String
decode = caesar shiftLeftUppercase

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStr "Would you like to encode or decode a message? [E/D]: "
  shiftDirection <- getLine

  putStr "Enter your integer key: [0 to ∞]: "
  shiftAmount <- getLine

  putStr "Enter your message: "
  message <- getLine

  putStr "Your resulting message: "
  case shiftDirection of
    "E" -> putStrLn $ encode (read shiftAmount) message
    "D" -> putStrLn $ decode (read shiftAmount) message
    _   -> error $ "The shift direction '" ++ shiftDirection ++ "' is not valid."