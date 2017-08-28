-- https://www.seas.upenn.edu/~cis194/spring13/hw/05-type-classes.pdf

{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser

-- Exercise 1

eval :: ExprT -> Integer
eval exprT = case exprT of
    Lit n  -> n
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr string =
    let parsedString = parseExp Lit Add Mul string
    in case parsedString of
        Nothing -> Nothing
        Just exprT -> Just (eval exprT)

-- Exercise 3

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add a b = Add a b
    mul a b = Mul a b

-- Resolves type ambiguity when constructing ExprT instances from Expr typeclass. Eg. reify $ mul (add (lit 2) (lit 3)) (lit 4)
reify :: ExprT -> ExprT
reify = id

-- Exercise 4

instance Expr Integer where
    lit = id
    add a b = a + b
    mul a b = a * b

instance Expr Bool where
    lit n = if n <= 0 then False else True
    add a b = a || b
    mul a b = a && b

instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 (a + b)
    mul (Mod7 a) (Mod7 b) = Mod7 (a * b)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer -- Expect: Just -7
testBool = testExp :: Maybe Bool       -- Expect: Just True
testMM = testExp :: Maybe MinMax       -- Expect: Just (MinMax 5)
testSat = testExp :: Maybe Mod7        -- Expect: Just (Mod7 14)
