-- Page 850

import Data.Char

capitalize :: String -> String
capitalize = map toUpper

composed :: String -> String
composed = capitalize . reverse

fmapped :: String -> String
fmapped = capitalize <$> reverse

tupled :: String -> (String, String)
tupled = (,) <$> capitalize <*> reverse

tupled' :: String -> (String, String)
tupled' = do
  capitalized <- capitalize
  reversed <- reverse
  return (capitalized, reversed)

tupled'' :: String -> (String, String)
tupled'' =
  capitalize >>=
    \capitalized ->
      reverse >>=
        \reversed ->
          return (capitalized, reversed)