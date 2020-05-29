-- Page 994

{-# LANGUAGE InstanceSigs #-}

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

-- 1.

instance Functor m => Functor (EitherT e m) where
    fmap :: (a -> b) -> (EitherT e m) a -> (EitherT e m) b
    fmap ab (EitherT mea) = EitherT $ (ab <$>) <$> mea

-- 2.

instance Applicative m => Applicative (EitherT e m) where
    pure :: a -> EitherT e m a
    pure a = EitherT $ pure (pure a)

    (<*>) :: EitherT e m (a -> b) -> (EitherT e m) a -> (EitherT e m) b
    (EitherT meab) <*> (EitherT mea) =
        EitherT $ (<*>) <$> meab <*> mea

-- 3.

instance Monad m => Monad (EitherT e m) where
    return :: a -> EitherT e m a
    return = pure

    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT mea) >>= ameb =
        EitherT $ mea >>= \ea ->
            case ea of
                Right a -> runEitherT $ ameb a
                Left e  -> return $ Left e

    -- EitherT e m b :: m (Either e b)

    -- Any reasonable naming conventions for these variables?

-- 4.

swapEither :: Either e a -> Either a e
swapEither (Right e) = Left e
swapEither (Left a)  = Right a

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT = EitherT . (swapEither <$>) . runEitherT

-- 5.

-- either :: (a -> c) -> (b -> c) -> Either a b -> c

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT f g t =
    (runEitherT t) >>= \e ->
        case e of
            Left a  -> f a
            Right b -> g b

-- Page 1016

-- 1.

-- lift :: Monad m => m a -> t m a
--      :: Monad m => m a -> EitherT e m a
--      :: Monad m => m a -> m (Either e a)

instance MonadTrans (EitherT e) where
    lift m = EitherT $ Right <$> m

-- Page 1012

instance MonadIO m => MonadIO (EitherT e m) where
    liftIO = lift . liftIO
