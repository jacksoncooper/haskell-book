-- Page 820

filterFoldable :: (Applicative f, Foldable t, Monoid (f a)) =>
                  (a -> Bool)
               -> t a
               -> f a
filterFoldable aToBool foldableOfA = foldMap aToMonoid foldableOfA
    where aToMonoid a = if aToBool a then pure a else mempty

-- E.g.

-- filterFoldable (<= 2) [1..4] :: [Integer]

--                              ^^
-- Must coerce the type here, otherwise Haskell doesn't know what Applicative
-- instance to dispatch for 'f', and what Monoid instance to dispatch for 'f a'.

-- foldMap aToMonoid [1..4] =
-- foldr (mappend . aToMonoid) mempty [1..4]

-- (mappend . aToMonoid) 4 mempty =
-- mappend (aToMonoid 4) mempty

-- aToMonoid 4 = if (4 <= 2) then pure 4 else mempty
--             = if False then [4] else []
--             = []

-- mappend [] [] = []

-- ...