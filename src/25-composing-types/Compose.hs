{-# LANGUAGE InstanceSigs #-}

-- fmap :: (a -> b) -> f a -> f b
-- (.) :: (b -> c) -> (a -> b) -> a -> c
--
-- Specifying (.) for (fmap . fmap).
--
-- ((f a -> f b)) -> (f (f a) -> f (f b)))
-- -> ((a -> b) -> (f a -> f b)))
-- -> (a -> b) -> (f (f:t fma a) -> f (f b))
--
-- (fmap . fmap) :: (a -> b) -> f (f a) -> f (f b)

newtype Compose f g a =
    Compose { getCompose :: f (g a)}
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b)
         -> Compose f g a
         -> Compose f g b
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- Page 969

-- 1.

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a
         -> Compose f g a
    pure = Compose . pure . pure

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

-- Page 971

-- 1.

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: Monoid m
            => (a -> m)
            -> Compose f g a
            -> m
    foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- 2.

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative h
             => (a -> h b)
             -> Compose f g a
             -> h (Compose f g b)
    traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

-- Page 971

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    -- The definition of this type class is recursive. Either 'bimap' must be
    -- defined by an instance, or both 'first' and 'second'. See the above
    -- MINIMAL pragma.

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second f = bimap id f

-- 1.

data Deux a b = Deux a b
    deriving Show

instance Bifunctor Deux where
    first :: (a -> b) -> Deux a c -> Deux b c
    first aToB (Deux a c) = Deux (aToB a) c

    second :: (b -> c) -> Deux a b -> Deux a c
    second bToC (Deux a b) = Deux a (bToC b)

-- 2.

data Const a b = Const a
    deriving Show

instance Bifunctor Const where
    first :: (a -> b) -> Const a c -> Const b c
    first aToB (Const a) = Const (aToB a)

    second :: (b -> c) -> Const a b -> Const a c
    second _ (Const a) = (Const a)

    -- Note to self. You must reconstruct the Const data in 'second' or Haskell
    -- will get upset that you're trying to return the 'Const a b' argument,
    -- but it will happily reconstruct one of type 'Const a c'.

-- 3.

data Drei a b c = Drei a b c
    deriving Show

instance Bifunctor (Drei a) where
    first :: (b -> c) -> (Drei a) b d -> (Drei a) c d
    first bToC (Drei a b d) = Drei a (bToC b) d

    second :: (c -> d) -> (Drei a) b c -> (Drei a) b d
    second cToD (Drei a b c) = Drei a b (cToD c)

-- 4.

data SuperDrei a b c = SuperDrei a b
    deriving Show

instance Bifunctor (SuperDrei a) where
    bimap :: (b -> c) -> (d -> e) -> (SuperDrei a) b d -> (SuperDrei a) c e
    bimap bToC _ (SuperDrei a b) = SuperDrei a (bToC b)

-- 5.

data SemiDrei a b c = SemiDrei a
    deriving Show

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

-- 6.

data Quadriceps a b c d = Quadzzz a b c d
    deriving Show

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- 7.

instance Bifunctor Either where
    first :: (a -> b) -> Either a c -> Either b c
    first f (Left a) = Left $ f a
    first _ (Right c) = Right c

    second :: (b -> c) -> Either a b -> Either a c
    second f (Right b)  = Right $ f b
    second _ (Left a) = Left a
