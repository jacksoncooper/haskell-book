-- https://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf

{-# OPTIONS_GHC -Wall #-}

-- Exercise 1
-- TODO: That 'filter even' in fun2' is really gross because it only removes the first odd element in the resulting list.

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Exercise 2
-- TODO: The populate function is a hack that requires an extra traversal of the tree. Find a way to populate height values in the 'insert' function.

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = populate . foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert value Leaf = Node 0 Leaf value Leaf
insert value (Node _ leftChild nodeValue rightChild)
    | height leftChild <= height rightChild = (Node 0 (insert value leftChild) nodeValue rightChild)
    | otherwise                             = (Node 0 leftChild nodeValue (insert value rightChild))

height :: Tree a -> Integer
height Leaf = -1
height (Node _ leftChild _ rightChild) = succ $ max (height leftChild) (height rightChild)

populate :: Tree a -> Tree a
populate Leaf = Leaf
populate self@(Node _ leftChild value rightChild) = (Node (height self) (populate leftChild) value (populate rightChild))

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldl (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\n -> 2 * n + 1) . uncurry filterList . mapTuple (\n -> [1..n], oddPrimes)

toForm :: [(Integer, Integer)] -> [Integer]
toForm xs = [i + j + 2 * i * j | (i, j) <- xs]

oddPrimes :: Integer -> [Integer]
oddPrimes n = toForm $ cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

filterList :: (Eq a) => [a] -> [a] -> [a]
filterList as bs = [a | a <- as, a `notElem` bs]

mapTuple :: (a -> b, a -> b) -> a -> (b, b)
mapTuple (f, g) x = (f x, g x)
