import Data.Char (isLetter, isUpper, toLower)
import Data.List (elemIndex)

-- Chapter Exercises, p.454

data Phone = Phone [Button]

data Button = Button Char String
  deriving Show

type Digit = Char
type Presses = Int

phone = Phone
  [ Button '2' "abc",
    Button '3' "def",
    Button '4' "ghi",
    Button '5' "jkl",
    Button '6' "mno",
    Button '7' "pqrs",
    Button '8' "tuv",
    Button '9' "wxyz",
    Button '*' "^",
    Button '0' "+ ",
    Button '#' ".," ]

conversation :: [String]
conversation =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn" ]

characterToTaps :: Phone -> Char -> [(Digit, Presses)]
characterToTaps phone character = go $ getButton phone character
  where
    go (Just (Button label sequence)) =
      case isLetter character && isUpper character of
        True  -> characterToTaps phone '^' ++ characterToTaps phone (toLower character)
        False -> [(label, getLocationInSequence character (sequence ++ [label]))]
    go Nothing = error ("Unable to translate '" ++ [character] ++ "' to taps. No button contains '" ++ [character] ++ "'.")

sentenceToTaps :: Phone -> String -> [(Digit, Presses)]
sentenceToTaps phone = concat . map (characterToTaps phone)

totalTapsInTapSequence :: [(Digit, Presses)] -> Presses
totalTapsInTapSequence = foldr ((+) . snd) 0

getButton :: Phone -> Char -> Maybe Button
getButton (Phone buttons) character =
  case matches of
    [] -> Nothing
    _  -> Just $ head matches
  where
    matches = filter characterInButton buttons
    characterInButton (Button label sequence) =
      elem (toLower character) (sequence ++ [label])

getLocationInSequence :: Char -> String -> Int
getLocationInSequence character sequence =
  case elemIndex (toLower character) sequence of
    (Just index) -> 1 + index
    Nothing      -> error ("Unable to find location of '" ++ [character] ++ "' in '" ++ sequence ++ "'.")