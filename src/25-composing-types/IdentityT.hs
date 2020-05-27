-- Page 974

newtype Identity a =
    Identity { runIdentity :: a }
    deriving (Eq, Show)

newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Functor m => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

instance Applicative m => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)
    (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

-- instance Monad m => Monad (IdentityT m) where
--     return = pure
--     (IdentityT ma) >>= f =
--         IdentityT $ ma >>= runIdentityT . f

-- The inner bind has the following type:

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

-- f :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
-- (\a -> runIdentityT (f a)) :: a -> m b

-- Alternatively:

instance Monad m => Monad (IdentityT m) where
    return = pure

    (IdentityT ma) >>= f =
        IdentityT $ ma >>= (\a -> runIdentityT (f a))
