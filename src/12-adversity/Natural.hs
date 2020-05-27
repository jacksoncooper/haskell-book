-- Chapter Exercises, Page 480

data Natural = Zero | Successor Natural
  deriving (Eq, Show)

naturalToInteger :: Natural -> Integer
naturalToInteger Zero = 0
naturalToInteger (Successor natural) = 1 + naturalToInteger natural

integerToNatural :: Integer -> Maybe Natural
integerToNatural integer =
  if integer < 0
  then Nothing
  else Just $ go integer
  where
    go 0 = Zero
    go integer = Successor (go $ integer - 1)

-- Well I guess it's only life, it's only natural
-- We all spend a little while going down the rabbit hole
-- https://www.youtube.com/watch?v=fweNLKBCh5A