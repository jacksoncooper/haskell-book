-- Page 886

import Control.Monad.Trans.State
import qualified Data.DList as DL


-- A sane implementation of FizzBuzz:

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod`  5 == 0 = "Buzz"
  | n `mod`  3 == 0 = "Fizz"
  | otherwise       = show n

-- main :: IO ()
-- main =
--   mapM_ (putStrLn . fizzBuzz) [1..100]


-- The curious mapM_:

-- mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
-- mapM :: (Traversable [], Monad IO) => (Integer -> IO ()) -> [Integer] -> IO [()]

-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
-- mapM_ :: (Foldable [], Monad IO) => (Integer -> IO ()) -> [Integer] -> IO ()


-- FizzBuzz using the State monad:

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put $ DL.snoc xs result

main :: IO ()
main =
  mapM_ putStrLn $ fizzBuzzList [1..100]

-- From GHC Base 4.12.0.0:

-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
-- mapM_ f = foldr ((>>) . f) (return ())

-- (>>) :: forall a b. m a -> m b -> m b
-- m >> k = m >>= \_ -> k

-- mapM_ addResult [1..5]
-- foldr ((>>) . f) (return ()) [1..5]

-- addResult 5
-- \xs -> (fizzBuzz 5, fizzBuzz 5 : xs)

-- return ()
-- \xs -> ((), xs)

-- \xs -> (fizzBuzz 5, fizzBuzz 5 : xs) >> \xs -> ((), xs)
-- \xs -> (fizzBuzz 5, fizzBuzz 5 : xs) >>= \_ -> (\xs -> ((), xs))

-- ^ Creates a new function that accepts some state :: [String], passes that
--   to the function on the left side of the bind to get an 'a' value and a new
--   state, discards the 'a' value by passing it to the right of the bind, and
--   passes the new state to the resulting function.

-- And so on, through the fold.

-- We then use 'execState' to get the final state.


-- Naive implementation using the State monad:

fizzBuzzList' :: [Integer] -> [String]
fizzBuzzList' list =
  execState (mapM_ addResult' list) []

addResult' :: Integer -> State [String] ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put $ result : xs

-- main :: IO ()
-- main =
--   mapM_ putStrLn $
--     reverse $ fizzBuzzList [1..100]

--     ^ To flip the final state around because we're cons-ing each application
--       of 'fizzBuzz n' to the state list before it is passed down the line,
--       starting with 'fizzBuzz 1' because that is part of the last state
--       function wrapped by repeated applications of (>>).


fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to = execState (mapM_ addResult' [to, to - 1 .. from]) []
