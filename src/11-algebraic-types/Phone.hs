-- Chapter Exercises, Page 454

import Data.Char (isLetter, toLower, isUpper)
import Data.List (elemIndex)

data Phone = Phone [Button]
data Button = Button Char String

type Digit = Char
type Presses = Int

phone = Phone
  [ Button '2' "abc"
  , Button '3' "def"
  , Button '4' "ghi"
  , Button '5' "jkl"
  , Button '6' "mno"
  , Button '7' "pqrs"
  , Button '8' "tuv"
  , Button '9' "wxyz"
  , Button '*' "^"
  , Button '0' "+ "
  , Button '#' ".," ]

conversation :: [String]
conversation =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn" ]

-- Part 1

characterToTaps :: Phone -> Char -> [(Digit, Presses)]
characterToTaps phone character = go $ getButton phone (toLower character)
  where
    go (Just (Button label sequence)) =
      case shouldShift of
        True  -> characterToTaps phone '^' ++ characterToTaps phone (toLower character)
        False -> [(label, getLocationInSequence character (sequence ++ [label]))]
        where
          shouldShift = isLetter character && isUpper character
    go Nothing = error ("Unable to translate '" ++ [character] ++ "' to taps. No button contains '" ++ [character] ++ "'.")

sentenceToTaps :: Phone -> String -> [(Digit, Presses)]
sentenceToTaps phone = concat . map (characterToTaps phone)

totalTapsInTapSequence :: [(Digit, Presses)] -> Presses
totalTapsInTapSequence = sum . map snd

getButton :: Phone -> Char -> Maybe Button
getButton (Phone buttons) character =
  case matches of
    [] -> Nothing
    _  -> Just $ head matches
  where
    matches = filter characterInButton buttons
    characterInButton (Button label sequence) = elem character (sequence ++ [label])

getLocationInSequence :: Char -> String -> Int
getLocationInSequence character sequence =
  case elemIndex character sequence of
    (Just index) -> 1 + index
    Nothing      -> error ("Unable to find location of '" ++ [character] ++ "' in '" ++ sequence ++ "'.")

-- Part 2

mostFrequentLetter :: String -> Maybe [Char]
mostFrequentLetter = mostFrequent

mostFrequentWord :: String -> Maybe [String]
mostFrequentWord = mostFrequent . words

mostFrequentLetterInConversation :: [String] -> Maybe [Char]
mostFrequentLetterInConversation = mostFrequent . concat

mostFrequentWordInConversation :: [String] -> Maybe [String]
mostFrequentWordInConversation = mostFrequent . concat . map words

mostFrequent :: Eq a => [a] -> Maybe [a]
mostFrequent [] = Nothing
mostFrequent xs = Just (map fst $ filter ((== greatestFrequency) . snd) frequencies)
  where
    frequencies = frequencyList xs
    greatestFrequency = maximum $ map snd frequencies

frequencyList :: Eq a => [a] -> [(a, Int)]
frequencyList [] = []
frequencyList (x:xs) = (x, 1 + count x xs) : (frequencyList $ filter (/= x) xs)

count :: Eq a => a -> [a] -> Int
count character = length . filter (== character)