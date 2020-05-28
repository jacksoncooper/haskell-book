-- Page 1007

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Class
import Data.Monoid (mconcat)

import Web.Scotty

main :: IO ()
main = scotty 3000 $
    get "/:word" $
        param "word" >>= \beam ->
            let hello = putStrLn "hello"
            in (lift :: IO a -> ActionM a) hello >>
                (html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"])

-- lift :: (Monad m, MonadTrans t) => m a -> t m a
-- lift :: MonadTrans t => IO a -> t IO a
-- lift :: IO a -> ActionM a
-- lift :: IO () -> ActionM ()

-- We bubble the IO structure of
--     putStrLn "hello" :: IO ()
-- outwards to produce a function
--     \beam -> ActionM a
-- that we can bind with
--     param "word"
-- to enable Scotty to work its state magic. Note:
--     type ActionM =
--       Web.Scotty.Internal.Types.ActionT Data.Text.Internal.Lazy.Text IO
--       :: * -> *
-- So ActionM is a specified version of ActionT wrapped in IO, which is why
-- we need to bubble IO to the outside of our ActionM structure.

-- Page 1015

-- Control.Monad.Trans.Class:

-- class MonadTrans (t :: (* -> *) -> * -> *) where
--   lift :: Monad m => m a -> t m a
--   {-# MINIMAL lift #-}

-- instance MonadTrans (ReaderT r) where
--     lift = ReaderT . const

-- lift :: Monad m => m a -> t m a
--      :: Monad m => m a -> ReaderT r m a
--      :: Monad m => m a -> (r -> m a)

-- v :: Monad m => m a

-- const :: a -> b -> a
--       :: Monad m => m a -> b -> m a

-- const v :: b -> m a
