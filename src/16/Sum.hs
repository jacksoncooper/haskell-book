-- Page 656

-- 1.

data Sum a b = First a | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second $ f b

-- 2. Why is a Functor instance that applies the function only to First,
--    Either’s Left, impossible?

--    Functor instances must be a higher-kinded type '* -> *' because if they
--    were simply '*', fmap could not enforce how the last type contained within
--    the Functor instance(?) changes through function application. The type of
--    fmap would be something like:

--    fmap :: (a -> b) -> f -> f

--    The only thing we could do is return the input Functor. The definition of
--    fmap essentially forces all of the type variables except the last in the
--    Functor instance to be absorbed into the structure of the Functor.

--    fmap :: (a -> b) -> f a -> f b
--    fmap :: (a -> b) -> (Sum a') a -> (Sum a') b

--    In the above substitution, we cannot manipulate the type contained in
--    the data constructor First because fmap declares the types in the Functor
--    instance immutable; the a' that goes in is the same that goes out.

--    (?) I'm not sure if this is the correct wording. The type that changes
--        isn't actually part of the Functor instance. It's referred to in the
--        pattern matching that goes on in the instance declaration, but it's
--        not a part of the instance itself.