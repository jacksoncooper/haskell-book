module Chapter16.Notes where

-- Constants:

replaceWithP :: a -> Char
replaceWithP = const 'p'

ave :: Maybe String
ave = Just "Ave"

n :: Maybe String
n = Nothing

w :: Maybe String
w = Just "woohoo"

lms :: [Maybe String]
lms = [ave, n, w]

-- lms = [Just "Ave", Nothing, Just "woohoo"]


-- Form of compose filled in with 'fmap . fmap':

-- (.) :: (b -> c) -> (a -> b) -> a -> c
--          fmap        fmap

-- fmap :: Functor f => (m -> n) -> f m -> f n
-- fmap :: Functor g => (x -> y) -> g x -> g y

-- Equivalent:

-- fmap :: Functor f => (m -> n) -> (f m -> f n)
-- fmap :: Functor g => (x -> y) -> (g x -> g y)

-- (.) :: ((m -> n) -> (f m -> f n))
--        -> ((x -> y) -> (g x -> g y))
--        -> (x -> y)
--        -> (f m -> f n)

-- Then:

-- m :: g x
-- n :: g y

-- (.) :: ((g x -> g y) -> (f (g x) -> f (g y)))
--        -> ((x -> y) -> (g x -> g y))
--        -> (x -> y)
--        -> (f (g x) -> f (g y))


-- Form of compose filled in with 'map . map':

-- (.) :: (b -> c) -> (a -> b) -> a -> c
--          map         map

-- map :: (m -> n) -> [m] -> [n]
-- map :: (x -> y) -> [x] -> [y]

-- Equivalent:

-- map :: (m -> n) -> ([m] -> [n])
-- map :: (x -> y) -> ([x] -> [y])

-- (.) :: ((m -> n) -> ([m] -> [n]))
--        -> ((x -> y) -> ([x] -> [y]))
--        -> (x -> y)
--        -> ([m] -> [n])

-- Then:

-- m :: [x]
-- n :: [y]

-- (.) :: (([x] -> [y]) -> ([[x]] -> [[y]]))
--        -> ((x -> y) -> ([x] -> [y]))
--        -> (x -> y)
--        -> ([[x]] -> [[y]])


-- From GHC.Base 4.12.0.0:

-- (.)    :: (b -> c) -> (a -> b) -> a -> c
-- (.) f g = \x -> f (g x)

-- instance  Functor Maybe  where
--     fmap _ Nothing       = Nothing
--     fmap f (Just a)      = Just (f a)

-- instance Functor [] where
--     fmap = map


-- When applying:

-- (fmap . fmap) replaceWithP
--   f      g          x

-- (.) :: ((g x -> g Char) -> (f (g x) -> f (g Char)))
--        -> ((x -> Char) -> (g x -> g Char))
--        -> (x -> Char)
--        -> (f (g x) -> f (g Char))


-- When applying:

-- (fmap . fmap)* replaceWithP lms

-- * I believe Haskell's type system is the only thing that allows this to
--   compose properly. I.e., the implementation details of the composed function
--   are not known until its final argument is applied. When 'replaceWithP' is
--   applied to '(fmap . fmap)', Haskell knows the general structure of the
--   return type to be 'f (g Char)', but only knows which instance of Functor
--   to use as it peels apart each data constructor in the 'lms'.

--   No, that doesn't make sense because because the function lifted over the
--   outermost Functor must be composed to lift itself over the rest. Meaning
--   Haskell must compose this function from the inside out, because it needs
--   to know which instance to dispatch for the innermost Functor before it can
--   can compose a function to lift the input function over the Functor above
--   the innermost one, and so on.

-- In the innermost fmap:

-- fmap replaceWithP (Just a) = Just (replaceWithP a)
--   OR
-- fmap replaceWithP Nothing = Nothing

-- In the outermost fmap:

-- fmap (Just (replaceWithP a)) lms = map (Just (replaceWithP a)) lms
--   OR
-- fmap Nothing lms = map Nothing lms


-- Using the constants above:

-- replaceWithP lms :: Char
-- fmap replaceWithP lms :: [Char]
-- (fmap . fmap) replaceWithP lms :: [Maybe Char]
-- (fmap . fmap . fmap) replaceWithP lms :: [Maybe String]


-- More funky composition, from the book:

-- Prelude> fc = fmap (const 3)
-- Prelude> fc' = fmap (const 5)
-- Prelude> separate = fc . fc'
-- Prelude> c = const 3
-- Prelude> c' = const 5
-- Prelude> fused = fmap (c . c')
-- Prelude> cw = Constant "WOOHOO"
-- Prelude> getConstant $ separate $ cw
-- "WOOHOO"
-- Prelude> cdr = Constant "Dogs rule"
-- Prelude> getConstant $ fused $ cdr
-- "Dogs rule"


-- Form of compose filled in with 'fmap (const 3) . fmap (const 5)':

-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- fc = fmap (const 3) :: (Functor f, Num b) => f a -> f b
-- fc'= fmap (const 5) :: (Functor f', Num b') => f' a' -> f' b'

-- (.) :: (Num b, Num b', Functor f') => (f' b' -> f' b) -> (f' a' -> f' b') -> f' a' -> f' b

-- separate = fc . fc' :: (Functor f', Num b) => f' a' -> f' b


-- Functors in datatypes:

data Wrap f a = Wrap (f a)
  deriving (Eq, Show)

-- instance Functor (Wrap f) where
--   fmap f (Wrap fa) = Wrap (f fa)

-- fmap :: (a -> b) -> f a -> f b
-- fmap :: (a -> b) -> (Wrap f) a -> (Wrap f) b

-- This doesn't work because we need to return a Wrap that contains something of
-- type 'b'. We're applying 'f :: a -> b' to a type constructor of kind '* -> *'
-- that wraps the 'a' value that we need to transform.

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

-- This works. The only way we can access the 'a' within the wrapper is to
-- constrain the wrapper to a Functor, or some other typeclass that lets us
-- access the thing inside the wrapper.