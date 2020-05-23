-- Page 725

-- Applicative class methods:

-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

-- 1. []

pureList :: a -> [a]
pureList = pure

applyList :: [a -> b] -> [a] -> [b]
applyList = (<*>)

-- 2. IO

pureIO :: a -> IO a
pureIO = pure

applyIO :: IO (a -> b) -> IO a -> IO b
applyIO = (<*>)

-- 3. (,) a

pureTuple :: Monoid a => b -> (a, b)
pureTuple = pure

applyTuple :: Monoid a => (a, b -> c) -> (a, b) -> (a, c)
applyTuple = (<*>)

-- 4. (->) e

pureFunction :: b -> (a -> b)
pureFunction = pure

applyFunction :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
applyFunction = (<*>)