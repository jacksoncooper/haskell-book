-- Page 771

module Chapter18.Function where

-- "Write the following functions using the methods provided by Monad and
-- Functor. Using stuff like identity and composition is fine, but it has to
-- typecheck with types provided."

-- 1.

j :: Monad m => m (m a) -> m a
j = (>>= (>>= return))

-- 2.

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- 3.

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 aToBToC monadOfA monadOfB = aToBToC <$> monadOfA >>= (<$> monadOfB)

-- 4.

a :: Monad m => m a -> m (a -> b) -> m b
a monadOfA monadOfAToB = monadOfAToB >>= (<$> monadOfA)

-- 5.

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (a : as) aToMonadOfB = aToMonadOfB a >>= (\b -> (b :) <$> meh as aToMonadOfB)
meh [] _ = return []

-- 6.

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id