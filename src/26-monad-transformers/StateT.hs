-- Page 997

{-# LANGUAGE InstanceSigs #-}

import Control.Monad.Trans.Class

newtype StateT s m a =
    StateT { runStateT :: s -> m (a, s) }

-- Apparently this is the strict variant of the StateT monad.

-- StateT s m a :: s -> m (a, s)
--              :: (->) s (m (a, s))

-- 1.

instance Functor m => Functor (StateT s m) where
    fmap :: (a -> b) -> (StateT s m) a -> (StateT s m) b
    fmap f (StateT g) =
        let first = \(a, s) -> (f a, s)
        in StateT $ (first <$>) <$> g

-- 2.

-- This constraint is 'Monad m', not 'Applicative m'. Without the 'Monad m'
-- constraint, you can't seed the second computation with the one that results
-- from the first computation that carries a -> b. You get 'm (m (b, s))'.
--       v
instance Monad m => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure a = StateT $ \s -> pure (a, s)

    -- (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    -- (StateT f) <*> (StateT g) =
    --     StateT $ \s ->
    --         let what = ((<*>) . swap) <$> f s
    --                      ^
    --         in swap <$> (what <*> (swap <$> g s))
    --                      |    ^
    -- Could not deduce (Monoid s) arising from a use of ‘<*>’.

    -- I don't know what I expected. Also, this version gives the initial
    -- state to both 'f' and 'g', which isn't good.

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (StateT f) <*> (StateT g) =
        StateT $ \s ->
            f s >>= \(aToB, s') ->
                (\(a, s'') -> (aToB a, s'')) <$> g s'

    -- Why is this applicative instance so nuts?

    -- StateT s m (a -> b) :: s -> m (a -> b, s)
    -- StateT s m a :: s -> m (a, s)
    -- StateT s m b :: s -> m (b, s)

-- 3.

instance Monad m => Monad (StateT s m) where
    return :: a -> StateT s m a
    return = pure

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (StateT f) >>= g =
        StateT $ \s ->
            f s >>= \(a, s') ->
                runStateT (g a) s'

-- Page 1016

-- 2.

-- lift :: Monad m => m a -> t m a
--      :: Monad m => m a -> StateT s m a
--      :: Monad m => m a -> (s -> m (a, s))

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> (\a -> (a, s)) <$> m
