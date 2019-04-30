-- Page 667

{-# LANGUAGE FlexibleInstances, StandaloneDeriving #-}

module Chapter16.Valid where

import GHC.Arr

-- 1.

-- data Bool = True | False

-- There is no valid instance of Functor for Bool. Bool has kind '*'. There is
-- no content on which to apply 'a -> b'. The data constructors True and False
-- are the only structures present at the term level.

-- 2.

data BoolAndSomethingElse a = False' a | True' a
  deriving (Eq, Show)

-- There is a valid Functor instance for BoolAndSomethingElse.
-- BoolAndSomethingElse has kind '* -> *'.

instance Functor BoolAndSomethingElse where
  fmap f (True' a) = True' $ f a
  fmap f (False' a) = False' $ f a

-- 3. 

data BoolAndMaybeSomethingElse a = Falsish | Truish a
  deriving (Eq, Show)

-- There is a valid Functor instance for BoolAndMaybeSomethingElse.
-- BoolAndMaybeSomethingElse has kind '* -> *'.

instance Functor BoolAndMaybeSomethingElse where
  fmap f (Truish a) = Truish $ f a
  fmap _ Falsish = Falsish

-- 4.

newtype Mu f = InF { outF :: f (Mu f) }

-- There is no valid Functor instance for Mu. Mu has kind '(* -> *) -> *'. It
-- takes a type constructor of kind '* -> *' to construct a complete type. There
-- is no content on which to apply 'a -> b'; Mu's are arbitrarily nested data
-- constructors. The only thing 'f' can contain is itself.

-- instance Show (Mu []) where
--   show (InF mu) = "InF " ++ show mu

deriving instance Show (Mu [])

-- 5.

data D = D (Array Word Word) Int Int

-- There is no valid Functor instance for D. D has kind '*'. Every object of
-- type D will always be the product of an Array and two Int's. Because there
-- is no polymorphic component to D, there is no function that could map its
-- contents from 'a' to 'b'.