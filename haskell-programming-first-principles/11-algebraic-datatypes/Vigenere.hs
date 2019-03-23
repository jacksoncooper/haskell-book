-- Chapter Exercises, Page 451 and 529

module Vignere where

import Cipher (shiftLeftUppercase, shiftRightUppercase, ShiftFunction)
import Data.Char
import System.IO

spreadKey :: String -> [String] -> [String]
spreadKey "" xs = xs
spreadKey key xs = go (cycle key) xs
  where
    go _ [] = []
    go key (x:xs) = take (length x) key : go (drop (length x) key) xs

shiftWord :: ShiftFunction -> String -> String -> String -> String
shiftWord shiftFunction set key word =
  zipWith shiftLetter key word
  where
    shiftLetter keyCharacter wordCharacter =
      shiftFunction (shiftAmount (head set) keyCharacter) wordCharacter
    
shiftAmount :: Char -> Char -> Int
shiftAmount origin = (subtract $ ord origin) . ord

vignere :: ShiftFunction -> String -> String -> String -> String
vignere shiftFunction set key message =
  unwords $ zipWith (shiftWord shiftFunction set) (spreadKey key messageWords) messageWords
  where
    messageWords = words message

encode :: String -> String -> String
encode = vignere shiftRightUppercase ['A'..'Z']

decode :: String -> String -> String
decode = vignere shiftLeftUppercase ['A'..'Z']

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStr "Would you like to encode or decode a message? [E/D]: "
  shiftDirection <- getLine

  putStr "Enter your string key: "
  key <- getLine

  putStr "Enter your message: "
  message <- getLine

  putStr "Your resulting message: "
  case shiftDirection of
    "E" -> putStrLn $ encode key message
    "D" -> putStrLn $ decode key message
    _   -> error $ "The shift direction '" ++ shiftDirection ++ "' is not valid."