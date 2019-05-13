-- Page 725

module Chapter18.Bind where

import Control.Monad (join)

-- The following is the bind function (>>=) flipped, implemented using 'fmap'
-- and 'join'.

-- fmap :: Functor f => (a -> b) -> f a -> f b
-- join :: Monad m => m (m a) -> m a
-- inject monadOfA :: m (m b)

bind :: Monad m => (a -> m b) -> m a -> m b
bind inject monadOfA = join $ fmap inject monadOfA