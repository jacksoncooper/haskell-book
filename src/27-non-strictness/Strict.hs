-- Page 1083

{-# LANGUAGE BangPatterns #-}

data List a =
    Nil
  | Cons !a (List a)
  deriving Show

sTake :: Int -> List a -> List a
sTake n _
  | n <= 0 = Nil
sTake _ Nil = Nil
sTake n (Cons x !xs) =
  Cons x (sTake (n - 1) xs)

twoEls :: List Int
twoEls = Cons 1 (Cons undefined Nil)

oneEl :: List Int
oneEl = sTake 1 twoEls

threeElements :: List Int
threeElements = Cons 2 twoEls

oneElT :: List Int
oneElT = sTake 1 threeElements

