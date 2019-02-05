-- Implementation of foldr. Foldr is head recursive:

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs) 

-- foldr (+) 0 [1, 2, 3]
-- (+) 1 (foldr (+) 0 [2, 3])
-- (+) 1 ((+) 2 (foldr (+) 0 [3]))
-- (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [])))
-- (+) 1 ((+) 2 ((+) 3 (0)))
-- (+) 1 ((+) 2 (3))
-- (+) 1 (5)
-- 6


-- Demonstrating Haskell's non-strict evaluation with foldr:

overNineThousand :: Num b => a -> a -> b
overNineThousand _ _ = 9001

-- foldr overNineThousand 0 ([1, 2] ++ undefined)
-- overNineThousand 1 (foldr overNineThousand 0 ([2] ++ undefined))

--                    ^
-- Because overNineThousand throws away its second argument, foldr does not
-- evaluate the rest of the spine. However, it must evaluate the spine of the
-- first cons cell to determine the pattern match case.

--                  ^
-- overNineThousand also chucks its first argument.

-- overNineThousand _ _ 
-- 9001


-- Implementation of foldl. Foldl is tail recursive:

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ accumulator [] = accumulator
myFoldl f accumulator (x:xs) = myFoldl f (f accumulator x) xs

-- foldl (+) 0 [1, 2, 3]
-- foldl (+) ((+) 0 1) [2, 3]
-- foldl (+) ((+) ((+) 0 1) 2) [3]
-- foldl (+) ((+) ((+) ((+) 0 1) 2) 3) []
-- ((+) ((+) ((+) 0 1) 2) 3)
-- ((+) ((+) (1) 2) 3)
-- ((+) (3) 3)
-- 6

dropFirst :: a -> b -> b
dropFirst = flip const

-- foldl dropFirst 0 [1, 2, 3, 4, 5]
-- foldl (dropFirst 0 1) [2, 3, 4, 5]
-- foldl (dropFirst (dropFirst 0 1) 2) [3, 4, 5]
-- foldl (dropFirst (dropFirst (dropFirst 0 1) 2) 3) [4, 5]
-- foldl (dropFirst (dropFirst (dropFirst (dropFirst 0 1) 2) 3) 4) [5]
-- foldl (dropFirst (dropFirst (dropFirst (dropFirst (dropFirst 0 1) 2) 3) 4) 5) []
-- (dropFirst (dropFirst (dropFirst (dropFirst (dropFirst 0 1) 2) 3) 4) 5)
-- (dropFirst (dropFirst (dropFirst (dropFirst (1) 2) 3) 4) 5)
-- (dropFirst (dropFirst (dropFirst (2) 3) 4) 5)
-- (dropFirst (dropFirst (3) 4) 5)
-- (dropFirst (4) 5)
-- 5

-- foldl const 0 [1, 2, 3]
-- foldl const (const 0 1) [2, 3]
-- foldl const (const (const 0 1) 2) [3]
-- foldl const (const (const (const 0 1) 2) 3) []
-- (const (const (const 0 1) 2) 3)
--               ^
-- Because foldl is tail recursive, and the rest of the fold is not governed
-- by a single call to the folding function, we can't stop at this call and
-- must evaluate the whole spine. The definition says that the traversal stops
-- only if the given list is empty.


-- Implementation of scanl:

myScanl :: (a -> b -> a) -> a -> [b] -> [a]
myScanl f q ls =
  q : (case ls of
        [] -> []
        x:xs -> myScanl f (f q x) xs)


-- Implementation of scanr:

-- This one wasn't written in the book, so it likely doesn't reflect the actual
-- implementation.

myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr f q xs = 
  (case xs of
    [] -> []
    _ -> myScanr f (f (last xs) q) (init xs)) ++ [q]


-- Demonstrating scans:

-- scanl (+) 0 [1, 2, 3]
-- 0 : scanl (+) ((+) 0 1) [2, 3]
-- 0 : 1 : scanl (+) ((+) 1 2) [3]
-- 0 : 1 : 3 : scanl (+) ((+) 3 3) []
-- 0 : 1 : 3 : 6 : []
-- [0, 1, 3, 6]


-- Fibonacci generation with scanl:

-- fibs = 1 : scanl (+) 1 fibs
--      = 1 : 1 : scanl (+) 2 (tail $ 1 : ...)
--       (x : ...)
--      = 1 : 1 : 2 : scanl (+) 3 (tail $ 1 : ...)
--           (x : ...)
--      = 1 : 1 : 2 : 3 : scanl (+) 5 (tail $ 2 : ...)
--               (x : ...)
--      = 1 : 1 : 2 : 3 : 5 : scanl (+) 8 (tail $ 3 : ...)
--                   (x : ...)
--      = 1 : 1 : 2 : 3 : 5 : 8 : scanl (+) 13 (tail $ 5 : ...)
--                       (x : ...)

-- The key here is that scanl is traversing the list while it's being
-- generated.

-- Alternatively, nest the starting expression indefinitely. Or any of the
-- expressions, as they're all equivalent:

-- fibs = 1 : scanl (+) 1 fibs
--      = 1 : scanl (+) 1 (1 : scanl (+) 1 (1 : scanl (+) 1 (1 : ...)))
--      = 1 : scanl (+) 1 (1 : scanl (+) 1 (1 : 1 : scanl (+) 2 (...)))
--      = 1 : scanl (+) 1 (1 : 1 : scanl (+) 2 (1 : scanl (+) 2 (...)))
--      = 1 : scanl (+) 1 (1 : 1 : 2 : scanl (+) 3 (scanl (+) 2 (...)))
--      = 1 : scanl (+) 1 (1 : 1 : 2 : scanl (+) 3 (2 : scanl (+) (2 + _) (...)))
--      = 1 : scanl (+) 1 (1 : 1 : 2 : 3 : scanl (+) 5 (scanl (+) (2 + _) (...)))
--      = 1 : scanl (+) 1 (1 : 1 : 2 : 3 : 5 : scanl (+) (5 + _) (scanl (+) (2 + _) (...)))
--      = 1 : 1 : scanl (+) 2 (1 : 2 : 3 : 5 : scanl (+) (5 + _) (scanl (+) (2 + _) (...)))
--      = 1 : 1 : 2 : scanl (+) 3 (2 : 3 : 5 : scanl (+) (5 + _) (scanl (+) (2 + _) (...)))
--      = 1 : 1 : 2 : 3 : scanl (+) 5 (3 : 5 : scanl (+) (5 + _) (scanl (+) (2 + _) (...)))
--      = 1 : 1 : 2 : 3 : 5 : scanl (+) 8 (5 : scanl (+) (5 + _) (scanl (+) (2 + _) (...)))
--      = 1 : 1 : 2 : 3 : 5 : 8 : scanl (+) 13 (scanl (+) (5 + _) (scanl (+) (2 + _) (...)))
--      = 1 : 1 : 2 : 3 : 5 : 8 : 13 : scanl (+) (13 + _) (scanl (+) (5 + _) (scanl (+) (2 + _) (...)))
--      = ...