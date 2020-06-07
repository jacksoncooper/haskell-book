-- Page 1091

import Criterion.Main

(!?) :: [a] -> Int -> Maybe a
_      !? n | n < 0 = Nothing
[]     !? _         = Nothing
(x:_)  !? 0         = Just x
(_:xs) !? n         = xs !? (n - 1)

infixl 9 !?

myList :: [Int]
myList = [1..9999]

-- Example:

-- myList !? 2
-- (_ : [2..99]) !? 1
-- (_ : [3..99]) !? 0
-- Just 3

main :: IO ()
main = defaultMain
  [ bench "index list 9999"
    $ whnf (myList !!) 9998
  , bench "index list maybe index 9999"
    $ whnf (myList !?) 9998
  ]
