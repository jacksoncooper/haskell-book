module Chapter02.Exercises where

-- Exercises: Comprehension Check p.34

-- 1. Using GHCI version 8.4.4, function declarations do not need to be
--    preceded with 'let'.

-- 2.
naiveMultiplyByPi x = 3.14 * x

-- 3.
multiplyByPi x = pi * x


-- Exercises: Parentheses and Association p.39

-- 1. The expression 8 + 7 * 9 evaluates to 71 because the multiplication
--    operator has a precedence of 7 which is greater than the addition
--    operator's precedence of 6. The expression (8 + 7) * 9 overrides the
--    multiplication operator's precedence using parentheses and evaluates to
--    135.
-- 2. The expressions are equivalent because parentheses in (b) are implied
--    because the precedence of the multiplication operator is higher than that
--    of the addition operator.
-- 3. The expressions are not equivalent for the same reason as in 1.


-- Exercises: Heal the Sick p.45

-- 1.
area x = 3.14 * (x * x)

-- 2.
double b = b * 2

-- 3.
x = 7
y = 10
f = x + y


-- Exercises: A Head Code p.59

-- 1.
multiplyAndAdd = x * 3 + y
    where x = 3
          y = 1000

-- 2.
multiplyByFive = x * 5
    where x = 10 * 5 + y
          y = 10

-- 3.
divideAndAdd = z / x + y
    where x = 7
          y = negate x
          z = y * 10


-- Chapter Exercises p.60

-- Parenthesization

-- 1. 2 + 2 * 3 - 1 is equivalent to 2 + (2 * 3) - 1.
-- 2. (^) 10 $ 1 + 1 is equivalent to (^) 10 (1 + 1).
-- 3. 2 ^ 2 * 4 ^ 5 + 1 is equivalent to (2 ^ 2) * (4 ^ 5) + 1.

-- Equivalent expressions

-- 1. 1 + 1 and 2 will return the same result when evaluated.
-- 2. 10 ^ 2 and 10 + 9 * 10 will return the same result when evaluated.
-- 3. 400 - 37 and (-) 37 400 will not return the same result when evaluated.
--    (-) 37 400 is equivalent to 37 - 400.
-- 4. 100 `div` 3 and 100 / 3 will not return the same result when evaluated.
--    The div function performs integer division (rounding down).
-- 5. 2 * 5 + 18 and 2 * (5 + 18) will not return the same result when
--    evaluated. Parentheses override the multiplication function's precedence.

-- More fun with functions

-- 1. Prelude> z = 7
--    Prelude> y = z + 8
--    Prelude> x = y ^ 2
--    Prelude> waxOn = x * 5

--    10 + waxOn is 1135.
--    (+ 10) waxOn is 1135.
--    (-) 15 waxOn is -1120.
--    (-) waxOn 15 is 1120.

-- 3. Triple waxOn is 3375.

-- 4.
waxOn = x * 5
    where z = 7
          y = z + 8
          x = y ^ 2

-- 5.
triple x = x * 3

-- 6.
waxOff x = triple x