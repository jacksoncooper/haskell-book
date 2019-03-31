module Chapter03.Exercises where

-- Exercises: Scope p.74

-- 1. The variable 'y' is in scope for the variable 'z' because it is defined
--    before it is used in the variable 'z'. This is only relevant in the REPL.

-- 2. The variable 'h' is not in scope for the variable 'g' because 'h' is
--    not defined. This is relevant in both the REPL and any Haskell module.

-- 3. No. The variable 'd' is not in scope. The parameter 'd' in the function
--    'area' is unnecessary, because whatever it is bound to is not used in the
--    body of the function. When the variable 'r' is defined, it refers to a
--    variable 'd' that was never defined.

-- 4. Yes. By making the variable 'r' local to the body of the function 'area'
--    with parameter 'd', 'r' can reference 'd' in its definition.


-- Exercises: Syntax Errors p.77

-- 1. Will not compile, when an infix operator is used in postfix form, it must
--    be surrounded by parentheses.
-- 2. Will not compile. The use of single quotes indicate that the enclosed
--    expression is of type Char. 
-- 3. Will compile, the function 'concat' is partially applied with the
--    argument ["<3", " Haskell"].
--    Incorrect: Compiles, but for a different reason. The 'concat' function is
--    not an alias for the ++ operator.


-- Chapter Exercises p.82

-- Reading Syntax

-- 1A. Correct syntax.
-- 1B. Incorrect syntax, for the same reason as #1 in "Exercises: Syntax
--     Errors". Correct syntax is: (++) [1, 2, 3] [4, 5, 6].
-- 1C. Correct syntax.
-- 1D. Incorrect syntax. Correct syntax is: ["hello" ++ " world"].
-- 1E. Incorrect syntax. The first argument to (!!) expects a String, not an
--     Int. Correct syntax is: "hello" !! 4.
-- 1F. Correct syntax.
-- 1G. Incorrect syntax. The 'take' function expects an Int and a String.
--     Correct syntax is: take 4 "lovely".
-- 1H. Correct syntax.

-- 2.
-- A -> D
-- B -> C
-- C -> E
-- D -> A
-- E -> B

-- Building Functions

-- 1A. "Curry is awesome" ++ "!"
-- 1B. "Curry is awesome!" !! 4
-- 1C. drop 9 "Curry is awesome!"

-- 2A.
exclaim :: String -> String
exclaim words = words ++ "!"

-- 2B.
fourthIndex :: [a] -> a
fourthIndex xs = xs !! 4

-- 2C.
dropNine :: [a] -> [a]
dropNine xs = drop 9 xs

-- 3.
thirdLetter :: String -> Char
thirdLetter xs = xs !! 2

-- 4.
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! (x - 1)

-- 5.
-- Note: Only intended to be used with the argument "Curry is awesome".
rvrs :: String -> String
rvrs xs = drop 9 xs ++ (drop 5 $ take 9 xs) ++ take 5 xs

-- 6.
-- Skipped for now.