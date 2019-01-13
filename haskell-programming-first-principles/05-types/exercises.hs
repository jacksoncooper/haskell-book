{-# LANGUAGE NoMonomorphismRestriction #-}


-- Exercises: Type Matching p.127

-- 1A -> 2C
-- 1B -> 2D
-- 1C -> 2B
-- 1D -> 2A
-- 1E -> 2E


-- Exercises: Type Arguments p.136

-- 1. The answer is A. The signature f :: a -> a -> a -> a is equivalent to
--    f :: a -> (a -> (a -> a)). If x :: Char, then f x returns a function of
--    type Char -> (Char -> Char) because the type variable a is constrained to
--    a Char.
-- 2. The answer is C. If g :: a -> (b -> (c -> b)), then (((g 0) 'c') "woot")
--    returns a Char because the type of the second argument is the same as the
--    type of the return value.
-- 3. The answer is D. If h :: (Num a, Num b) => a -> (b -> b), then (h 1.0) 2
--    returns an expression of type Num b => b. The first argument of h
--    constrains the type of a to a Double. The second argument, 2, could
--    represent any numeric type, so the type of b is Num => b. h returns an
--    expression of type b.
-- 4. The answer is C. If h :: (Num a, Num b) => a -> (b -> b), then h 1
--    returns an expression of type b -> b. (h 1) (5.5 :: Double) returns an
--    expression of type Double.
-- 5. The answer is A. If jackal :: (Ord a, Eq b) => a -> (b -> a), then the
--    first argument to jackal constrains the type variable a and determines
--    the value of the expression. The first argument of jackal "keyboard"
--    "has the word ... " has the type String, so jackal returns an expression
--    of type String.
-- 6. The answer is E. If jackal :: (Ord a, Eq b) => a -> (b -> a), and
--    jackal is partially applied with the argument "keyboard", then the
--    type variable a is constrained to a String and the type variable b
--    remains of type Eq b => b. So jackal returns an expression of type
--    Eq b => b -> String.
-- 7. The answer is D. If kessel :: (Ord a, Num b) => a -> b -> a, then the
--    type of the first argument determines the value of the expression. 
--    kessel 1 constrains the type variable a to anything that is orderable and
--    numeric, returning an expression of type (Ord a, Num a, Num b) => b -> a.
--    (kessel 1) 2 then returns an expression of type (Ord a, Num a) => a. The
--    type variable b can be effectively discarded.
-- 8. The answer is A for the same reason as in #7.
-- 9. The answer is C. If kessel :: (Ord a, Num b) => a -> b -> a, and
--    kessel (1 :: Integer) 2, then the type variable a is constrained to an
--    Integer, so the expression returns an Integer. Returning
--    Ord Integer => Integer would be redundant because Integer has an instance
--    of the Ord typeclass.


-- Exercises: Parametricity p.141

-- 1.

identity :: a -> a
identity x = x

-- There is no operation that can be applied to all types, so any
-- parametrically polymorphic function with type signature a -> a must be the
-- identity function.

-- 2.

anyArgument :: a -> a -> a
anyArgument x _ = x

anyArgument' :: a -> a -> a
anyArgument' _ y = y

-- 3.

secondArgument :: a -> b -> b
secondArgument _ y = y


-- Exercises: Apply Yourself p.146

-- 1. The type of myConcat is String -> String, i.e. [Char] -> [Char]. By 
--    partially applying the (++) function with a String, the type variable
--    a in [a] -> [a] -> [a] is constrained to a String because all elements
--    in a list must be the same type.

-- 2. The type of myMult is Fractional a => a -> a. The type of the (/)
--    operator is Fractional a => a -> a -> a, so the variable x in the
--    expression (x / 3) * 5 must have an instance of Fractional. The
--    polymorphic constant 5 is forced to become of type Fractional a => a
--    because the type signature of the (*) function declares that the type of
--    the second argument is the same as the first. The return type is also
--    constrained to Fractional a => a because of the (*) function.

-- 3. The type of take is Int -> String.

-- 4. The type of myCom is Int -> Bool. 

-- 5. The type of myAlph is Char -> Bool.


-- Chapter Exercises p.149

-- Multiple choice

-- 1. C
-- 2. A
-- 3. B
-- 4. C

-- Determine the type

-- For the following problems, a :: b denotes, "The most polymorphic type of a
-- is b."

-- 1A. (* 9) 6 :: Num a => a.
-- 1B. head [(0, "doge"), (1, "kitteh")] :: Num a => (a, String)
-- 1C. head [(0 :: Integer, "doge"), (1, "kitteh")] :: (Integer, String)
-- 1D. if False then True else False :: Bool
-- 1E. length [1, 2, 3, 4, 5] :: Int
-- 1F. (length [1, 2, 3, 4]) > (length "TACOCAT") :: Bool

-- 2. Num a => a
-- 3. Num a => a -> a
-- 4. Fractional a => a
-- 5. String

-- Does it compile

-- 1. Does not compile. (^) 5 $ 10 <-> (^) 5 10  <-> 5 ^ 10 <-> 9765625.
--    Attempting to apply the ($) function with the arguments 9765625 and 10
--    doesn't make sense.

-- 2. Compiles.

-- 3. Does not compile. 10 cannot be partially applied to 5, because
--    5 :: Num a => a.

-- a = (+)
-- b = 5
-- c = a b 10
-- d = a c 200

-- 4. Does not compile. The variable c is not in scope.

-- Type variable or specific type constructor

-- 1. Fully polymorphic, concrete, concrete.
-- 2. Constrained polymorphic, fully polymorphic, fully polymorphic, concrete.
-- 3. Fully polymorphic, fully polymorphic, concrete.

-- Write a type signature

functionH ::[a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function

-- 1.

i :: a -> a
i x = x

-- 2.

c :: a -> b -> a
c x _ = x

-- 3.

c'' :: b -> a -> b
c'' x _ = x

-- 4.

c' :: a -> b -> b
c' _ y = y

-- 5.

r :: [a] -> [a]
r xs = reverse xs

-- 6.

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC $ aToB a

-- 7.

a :: (a -> c) -> a -> a
a _ a = a

-- 8.

a' :: (a -> b) -> a -> b
a' aToB a = aToB a

-- Fix it

-- 1.

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain."

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow."

sing = if (x > y) then fstString x else sndString y
    where x = "Singing"
          y = "Somewhere"

-- 2. Minor change: (x < y) on line 214.

-- 3.

arith3broken :: IO ()
arith3broken = do
    print $ 1 + 2
    print 10
    print $ negate (-1)
    print $ 0 + blah
        where blah = negate 1

-- Type-Kwon-Do

-- 1.

f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = g $ f x  

-- 2.

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = w $ q x

-- 3.

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- 4.

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ x = fst $ yToWZ $ xToY x