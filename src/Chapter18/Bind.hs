-- Page 725

module Chapter18.Bind where

import Control.Monad (join)

-- The following is the bind function (>>=) flipped, implemented using 'fmap'
-- and 'join':

bind :: Monad m => (a -> m b) -> m a -> m b
bind inject monadOfA = join $ fmap inject monadOfA