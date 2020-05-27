-- Page 645

-- 1. 
-- a = (+ 1)  $ read "[1]" :: [Int]

a :: [Int]
a = fmap (+ 1) $ read "[1]"

-- 2.
-- b = (++ "lol") (Just ["Hi,", "Hello"])

b :: Maybe [String]
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
-- c = (*2) (\x -> x - 2)

c :: Num a => a -> a
c = fmap (* 2) (\x -> x - 2)

-- 4.
-- d = ((return '1' ++) . show) (\x -> [x, 1..3])
--       ^
-- Not sure what the purpose of the above 'return' is.

d :: (Enum a, Num a, Show a) => a -> String
d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])

-- 5.
-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123" ++) show ioi
--     in (* 3) changed

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
    in fmap (* 3) changed