-- Page 727

import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 ( , , )

-- combos stops vowels stops
-- = liftA3 ( , , ) stops vowels stops

-- ( , , ) <$> stops
--   = [('p', , ), ('b', , ), ..., ('k', , ), ('g', , )]

-- [('p', , ), ('b', , ), ..., ('k', , ), ('g', , )] <*> vowels
--   = [('p', 'a', ), ('p', 'e', ), ..., ('p', 'o', ), ('p', 'u', ), ..., ('g', 'u', )]

-- [('p', 'a', ), ('p', 'e', ), ..., ('p', 'o', ), ('p', 'u', ), ..., ('g', 'u', )] <*> stops
--   = [('p', 'a', 'p'), ('p', 'a', 'b'), ..., ('p', 'a', 'k'), ('p', 'a', 'g'), ..., ('g', 'u', 'g')]