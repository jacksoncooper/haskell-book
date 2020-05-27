-- Page 989

{-# LANGUAGE InstanceSigs #-}

newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) =
        MaybeT $ (f <$>) <$> ma

instance Applicative m => Applicative (MaybeT m) where
    pure a =
        MaybeT (pure (pure a))

    (MaybeT fab) <*> (MaybeT mma) =
        MaybeT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a
          -> (a -> MaybeT m b)
          -> MaybeT m b

    -- Fom the text:

    -- (MaybeT ma) >>= f =
    --     MaybeT $ do
    --         v <- ma
    --         case v of
    --             Nothing -> return Nothing
    --             Just y -> runMaybeT (f y)

    -- ma :: m (Maybe a)
    -- f :: a -> MaybeT m b
    -- ((f <$>) <$>) f ma :: m (Maybe (MaybeT m b))
    -- ((runMaybeT . f <$>) <$>) f ma :: m (Maybe (m b))
    --                                      ^ This is the troublemaker.

    -- The inner bind has the following type:

    -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
    --       :: Monad m => m (Maybe a) -> (Maybe a -> m (Maybe b)) -> m (Maybe b)

    -- Alternatively, without 'do' notation:

    (MaybeT ma) >>= f =
        MaybeT $ ma >>= \v ->
            case v of
                Nothing -> return Nothing
                Just y -> runMaybeT (f y)
