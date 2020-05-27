-- Exercises: Mood Swing p.90

data Mood = Blah | Woot deriving Show

-- 1. The type constructor is Mood.
-- 2. The possible values of Mood are Blah and Woot.
-- 3. The problem with the signature 'changeMood :: Mood -> Woot' is that the
--    data constructor Woot constructs a value of type Mood, namely Woot. Woot
--    is not a type, so it does not make sense for the function to return
--    something of type Woot.

-- 4.
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah


-- Exercises: Find the Mistakes p.103

-- 1. not (True && True)
-- 2. not (x == 6)
--    This question feels kind of contrived. It is unclear whether 'x' is
--    intended to be in scope.
-- 3. Correct as is.
-- 4. ["Merry"] > ["Happy"]
-- 5. ['1', '2', '3'] ++ "look at me!"


-- Chapter Exercises p.111

-- Untitled

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- 1. length :: [a] -> Integer

-- 2A. 5
-- 2B. 3
-- 2C. 2
-- 2D. 5

-- 3. The expression '6 / length [1, 2, 3]' fails because the 'length' function
--    returns an Int (length :: Foldable t => t a -> Int). The (/) operator
--    takes two arguments whose types must have an instance of the Fractional
--    type class ((/) :: Fractional a => a -> a -> a). There is no
--    implementation of Int in the Fractional type class. I still need
--    clarification about Haskell's type resolution. In the expression '6 / 3',
--    how are the types resolved to something that has an instance of the
--    Fractional type class?
--    
--    From #haskell: (05:40:20 PM) ski: numeric literals like `6',`3' are
--    polymorphic, overloaded over all numeric types. so `6' can be an `Int',
--    but can also be a `Float'

-- 4. 6 `div` length [1, 2, 3]

-- 5. The type of the expression '2 + 3 == 5' is a Bool, and evaluates to True.

-- 6. The type of the expression is a Bool, and evaluates to False.

-- 7. 'length allAwesome == 2' will work, because the 'length' function returns
--    an Int, and Int has an instance of the Eq class. 'length
--    [1, 'a', 3, 'b']' will not work, because 'length' requires that all items
--    in its Foldable argument have the same type. 'length allAwesome + length
--    awesome' will work, because the lists 'awesome' and 'also' each have
--    elements of the same type. '(8 == 8) && ('b' < 'a')' will work, because
--    there is an instance of Eq Int and Ord Char. '(8 == 8) && 9' will not
--    work, because (&&) has the type 'Bool -> Bool -> Bool' and 9 is not a
--    Bool.

-- 8.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 9.
myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else -x

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))

-- Correcting syntax

-- 1.
-- x = (+)
-- f xs = w `x` 1
--     where w = length xs

-- 2.
-- I have no idea what the original syntax intended: \X = x.
-- f x = x

-- 3.
-- f (a, b) = a

-- Matching the function names to their types

-- 1. C
-- 2. B
-- 3. A
-- 4. D