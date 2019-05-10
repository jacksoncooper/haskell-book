-- Page 707

module Chapter17.Fixer where

-- 1.

one :: Maybe String
one = const <$> Just "Hello" <*> pure "World"

--2.

two :: Maybe (Integer, Integer, String, [Integer])
two = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]