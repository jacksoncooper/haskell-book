-- Chapter Exercises, Page 452

module Chapter11.AsPatterns where

import Data.Char (toUpper, isSpace)

-- 1.

-- isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
-- isSubseqOf xs ys = foldr ((&&) . (`elem` ys)) True xs

-- "Remember that the sub-sequence has to be in the original order!"
-- isSubseqOf "blah" "halbwoot" -> False.
-- isSubseqOf "blah" "wboloath" -> True.

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf (x:xs) ys = (elem x ys) && isSubsequenceOf xs (tail . dropWhile (/= x) $ ys)

-- ^ 'tail . dropWhile (/= x)' would normally be dangerous if not guaranteed to
--   never be evaluated if 'elem x ys' fails.

--   This doesn't use an as-pattern!

-- 2.

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\word -> (word, capitalizeWord word)) . words

-- 3.

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (x:xs) = toUpper x : xs

-- 4.

capitalizePaddedWord :: String -> String
capitalizePaddedWord "" = ""
capitalizePaddedWord word@(x:xs)
  | isSpace x = x : capitalizePaddedWord xs
  | otherwise = capitalizeWord word

capitalizeParagraph :: String -> String
capitalizeParagraph = joinAtCharacter '.' . map capitalizePaddedWord . splitAtCharacter '.'

splitAtCharacter :: Char -> String -> [String]
splitAtCharacter character xs = go character xs ""
  where
    go _ "" builder = [builder]
    go character (x:xs) builder
      | character == x = [builder] ++ go character xs ""
      | otherwise      = go character xs (builder ++ [x])

joinAtCharacter :: Char -> [String] -> String
joinAtCharacter character xs = foldr ((++) . (++ [character])) (last xs) (init xs)