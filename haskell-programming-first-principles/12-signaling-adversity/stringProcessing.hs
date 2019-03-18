-- Chapter Exercises, p.478

import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)

-- 1.

notThe :: String -> Maybe String
notThe text = if text == "the" then Nothing else Just text

replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

-- 2.

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go ("the" : noun : text) =
      if elem (toLower $ head noun) "aeiou"
      then 1 + go text
      else go text
    go (_ : text) = go text
    go [] = 0

-- 3.

isVowel :: Char -> Bool
isVowel = (`elem` "aeiou") . toLower

countCharacters :: (Char -> Bool) -> String -> Integer
countCharacters shouldCount = fromIntegral . length . filter shouldCount

countVowels :: String -> Integer
countVowels = countCharacters isVowel

-- 4.

isConsonant :: Char -> Bool
isConsonant x = not (isVowel x || isSpace x)

countConsonants :: String -> Integer
countConsonants = countCharacters isConsonant

newtype Word' = Word' String
  deriving (Eq, Show)

makeWord :: String -> Maybe Word'
makeWord word =
  if countConsonants word > countVowels word
  then Just $ Word' word
  else Nothing