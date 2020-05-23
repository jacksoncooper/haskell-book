-- Chapter Exercises, Page 338

import Data.Char

-- 1. isUpper :: Char -> Bool
--    toUpper :: Char -> Char

-- 2.

removeLowercase :: String -> String
removeLowercase = filter isUpper

-- 3.

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

-- 4.

capitalizeAll :: String -> String
capitalizeAll [] = []
capitalizeAll (x:xs) = toUpper x : capitalizeAll xs

-- 5.

capitalizeHead :: String -> Char
capitalizeHead = head . capitalize

-- 6. Oops.