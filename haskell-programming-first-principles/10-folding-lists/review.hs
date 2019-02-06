-- Chapter Exercises, Page 379

-- 1A.

threePermutation :: [a] -> [b] -> [c] -> [(a, b, c)]
threePermutation xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs]

stops = "pbtdkg"
vowels = "aeiou"

stopVowelStops = threePermutation stops vowels stops

-- 1B.

stopVowelStopsWithP = filter (\(x, _, _) -> x == 'p') stopVowelStops

-- 1C.

nouns = ["clam", "donkey", "belief", "lunch", "rock", "pies", "control",
  "wool", "hospital", "turkey"]

verbs = ["crawl", "instruct", "grate", "bang", "soothe", "end", "mate",
  "extend", "educate", "ski"]

nounVerbNouns = threePermutation nouns verbs nouns

-- 2.

seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

-- seekritFunc :: String -> Int

-- seekritFunc calculates the average word length of a given string, rounded
-- down to an integer.

-- seekritFunc "hello there silly shrew"
--  = div (sum (map length (words x))) (length (words x))
--  = div (sum [5, 5, 5, 5]) (4)
--  = div (20 4)
--  = 5

-- 3.

seekritFuncPrecise x = numberOfCharacters / numberOfWords
  where numberOfCharacters = fromIntegral . sum . map length . words $ x
        numberOfWords = fromIntegral . length . words $ x