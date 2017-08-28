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
    lit n = Lit n
    add a b = Add a b
    mul a b = Mul a b
