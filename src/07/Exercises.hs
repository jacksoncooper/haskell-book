-- Exercises: Grab Bag p.226

-- 1. A, B, C, and D are equivalent.

-- 2. D

-- 3A.

addOneIfOdd :: Integral a => a -> a
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

-- 3B.

addFive :: (Ord a, Num a) => a -> a -> a
addFive = \x y -> (if x > y then y else x) + 5

-- 3C.

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f x y = f y x


-- Exercises: Variety Pack p.237

-- 1A. k :: (a, b) -> a
-- 1B. k2 :: String, not the same as k1 :: Num a => a and k3 :: Num a => a.
-- 1C. k1 and k3.

-- 2.

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))


-- Exercises: Case Practice p.239

-- 1.

functionC :: Ord a => a -> a -> Bool
functionC x y =
    case x > y of
        True -> True
        False -> False

-- 2.

ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n =
    case even n of
        True -> n + 2
        False -> n

-- 3.

nums :: (Ord a, Num a) => a -> a
nums x =
    case compare x 0 of
        LT -> -1
        EQ -> 0
        GT -> 1


-- Exercises: Artful Dodgy p.247

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1        -- \y -> 1 + y * 10 <-> \y -> 1 + (y * 10)

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2 -- \x -> x + 2 * 10 <-> \x -> x + 20

-- 1. dodgy 1 0 = 1
-- 2. dodgy 1 1 = 11
-- 3. dodgy 2 2 = 22
-- 4. dodgy 1 2 = 21
-- 5. dodgy 2 1 = 12

-- 6. oneIsOne 1 = 11
-- 7. oneIsOne 2 = 21
-- 8. oneIsTwo 1 = 21
-- 9. oneIsTwo 2 = 22

-- 10. oneIsOne 3 = 31
-- 11. oneIsTwo 3 = 23


-- Exercises: Guard Duty p.253

-- 1. The expression associated with the first case will always be returned.
-- 2. The function will typecheck but not work the same depending on the
--    order of the cases.
-- 3. B
-- 4. The pal function can take any argument of type Eq a => [a].
-- 5. pal :: Eq a => [a] -> Bool
-- 6. C
-- 7. The numbers function can take any argument that has an instance of the
--    Ord and Num typeclasses.
-- 8. numbers :: (Ord a, Num a, Num b) => a -> b


-- Point Free Demonstration p.259

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne x = x + 1

addOnePF :: Int -> Int
addOnePF = (+ 1)

pointFreeStuff :: IO ()
pointFreeStuff = do
    print (0 :: Int)              -- 0
    print (add 1 0)               -- 1
    print (addOne 0)              -- 1
    print (addOnePF 0)            -- 1
    print ((addOne . addOne) 0)   -- 2
    print ((addOnePF . addOne) 0) -- 2
    print ((addOne . addOnePF) 0) -- 2
    -- ...


-- Chapter Exercises p.263

-- Multiple choice

-- 1. D
-- 2. B
-- 3. D
-- 4. B
-- 5. A

-- Let's write code

-- 1A.
-- This is ugly.

tensDigit :: Integral a => a -> a
tensDigit n = remainder
    where quotient  = fst $ n `divMod` 10
          remainder = snd $ quotient `divMod` 10

-- 1B. Yep.

-- 1C.

hundredsDigit :: Integral a => a -> a
hundredsDigit n = remainder
    where quotient  = n `div` 100
          remainder = quotient `mod` 10

-- 2.

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y z =
    case z of
        True  -> y
        False -> x

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard x y z
    | z         = y
    | otherwise = x

-- 3.

g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)

-- 4.

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read $ show a

-- 5.

roundTripPointFree :: (Show a, Read a) => a -> a
roundTripPointFree = read . show

-- 6.

roundTripAmbiguous :: (Show a, Read b) => a -> b
roundTripAmbiguous = read . show

-- print ((roundTripAmbiguous 4) :: Integer)