-- Chapter Exercises, Page 529

module Chapter13.Palindrome where

import Control.Monad
import Data.Char (isLetter, toLower)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line <- getLine
  let line' = map toLower $ filter isLetter line
  case (line' == reverse line') of
    True  -> putStrLn $ "'" ++ line ++ "' is a palindrome."
    False -> do
      putStrLn $ "'" ++ line ++ "' is not a palindrome."
      exitSuccess