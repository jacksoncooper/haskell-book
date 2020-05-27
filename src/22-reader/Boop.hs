-- Page 845

import Control.Applicative

boop :: Num a => a -> a
boop = (* 2)

doop :: Num a => a -> a
doop = (+ 10)

bip :: Num a => a -> a
bip = boop . doop

bloop :: Num a => a -> a
bloop = fmap boop doop

bbop :: Num a => a -> a
bbop = (+) <$> boop <*> doop

duwop :: Num a => a -> a
duwop = liftA2 (+) boop doop

boopDoop :: Num a => a -> a
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

boopDoop' :: Num a => a -> a
boopDoop' =
  boop >>=
    \a ->
      doop >>=
        \b ->
          return $ a + b


-- Playing around with the above:

-- From GHC Base 4.12.0.0:

-- instance Applicative ((->) a) where
--     pure = const
--     (<*>) f g x = f x (g x)
--     liftA2 q f g x = q (f x) (g x)

-- instance Monad ((->) r) where
--     f >>= k = \r -> k (f r) r

--   (+) <$> boop
-- = (+) . (* 2)

--   (+) . (* 2) <*> doop
-- = (+) . (* 2) <*> (+ 10)

-- (<*>)
--   :: f (a -> b) -> f a -> f b

-- (+) . (* 2)
--   :: Num a => a -> a -> a
--   :: Num a => (-> a (-> a a))

-- (<*>)
--   :: (-> a) (-> a a) -> (-> a) a -> (-> a) a
--         ^       ^ ^        ^   ^       ^   ^
--         f       a b        f   a       f   b

-- (>>=)
--   :: Monad m => m a -> (a -> m b) -> m b

-- (>>=)
--  :: Num a => (-> a) a -> (-> a (-> a) a) -> (-> a) a

-- boop >>= (\a -> ...) =
--   \r -> (\a -> ...) (boop r) r

-- doop >>= (\b -> return $ a + b) =
--   \r' -> (\b -> return $ a + b) (doop r') r'

-- boop >>= (\a -> ...) =
--   \r -> (\a -> (\r' -> (\b -> return $ a + b) (doop r') r')) (boop r) r

-- 1. Bind.
-- 2. Evaluate.
-- 3. Reduce.

-- \r -> (\a -> (\r' -> (\b -> return $ a + b) (doop r') r')) (boop r) r $ 5

-- 1. \5 -> (\a -> (\r' -> (\b -> return $ a + b) (doop r') r')) (boop 5) 5
-- 2. \5 -> (\a -> (\r' -> (\b -> return $ a + b) (doop r') r')) 10 5
-- 3. \a -> (\r' -> (\b -> return $ a + b) (doop r') r') 10 5

-- 1. \10 -> (\r' -> (\b -> return $ 10 + b) (doop r') r') 5
-- 3. (\r' -> (\b -> return $ 10 + b) (doop r') r') 5

-- 1. (\5 -> (\b -> return $ 10 + b) (doop 5) 5)
-- 2. (\5 -> (\b -> return $ 10 + b) 15 5)
-- 3. (\b -> return $ 10 + b) 15 5

-- 1. (\15 -> return $ 10 + 15) 5
-- 2. (\15 -> const 15) 5
-- 3. const 15 5

-- 15

-- "The Applicative and Monad chain the argument forward in addition to the
--  composition."