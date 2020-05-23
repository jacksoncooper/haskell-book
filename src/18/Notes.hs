import Control.Monad (join)

-- Note to future self: Lots of name shadowing going on here, but all is well.

-- Sugar. Oh, honey honey. You are my candy girl...

awkwardExchange :: IO ()
awkwardExchange = do
  putStrLn "Basil: Why, hallo dear friend, er..."
  putStr "You: "
  name <- getLine

  putStrLn "Basil: Say, old pal, remind me how old you're turning?"
  putStr "You: "
  age <- getLine

  putStrLn $ "Basil: Ah, yes, my dear " ++ name ++ ". " ++ age ++
             ". Those are golden years."

-- ¬ (Sugar. Oh, honey honey. You are my candy girl...)

awkwardExchange' :: IO ()
awkwardExchange' =
  putStrLn "Basil: Why, hallo dear friend, er..." >>
  putStr "You: " >>
  getLine >>=

    \name ->
      putStrLn "Basil: Say, old pal, remind me how old you're turning?" >>
      putStr "You: " >>
      getLine >>=

        \age ->
          putStrLn $ "Basil: Ah, yes, my dear " ++ name ++ ". " ++ age ++
                     ". Those are golden years."

-- Sugared:

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

-- Desugared:

twiceWhenEven':: [Integer] -> [Integer]
twiceWhenEven' xs =
  xs >>= \x ->
  if even x
    then [x * x, x * x]
    else [x * x]

-- Can be rewritten using Applicative methods:

doSomething1 :: Monad m => m a -> m b -> m c -> m (a, b, c)
doSomething1 f g h = do
  a <- f
  b <- g
  c <- h
  pure (a, b, c)

doSomething2 :: Monad m => m a -> m b -> m c -> m (a, b, c)
doSomething2 f g h =
  f >>=
    \a ->
      g >>=
        \b ->
          h >>=
            \c ->
              return (a, b, c)

doSomething3 :: Applicative f => f a -> f b -> f c -> f (a, b, c)
doSomething3 f g h = ( , , ) <$> f <*> g <*> h

-- Cannot be rewritten using Applicative methods:

doSomething4 :: Monad m => (t -> m a) -> (a -> m b) -> (b -> m c) -> t -> m (a, b, c)
doSomething4 f g h n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

doSomething5 :: Monad m => (t -> m a) -> (a -> m b) -> (b -> m c) -> t -> m (a, b, c)
doSomething5 f g h n =
  f n >>=
    \a ->
      g a >>=
        \b ->
          h b >>=
            \c ->
              return (a, b, c)

  -- f n >>=
  -- \a ->
  -- g a >>=
  -- \b ->
  -- h b >>=
  -- \c ->
  -- return (a, b, c)

-- ^ So, this compiles. I have no idea why. Is not '\a -> ...' part of the
--   expression 'f n >>= ...'?

-- "If you don’t believe us, try translating 'doSomething4' to Applicative: so
--  no resorting to >>= or join."

-- I did, had to use 'join' and add the Monad constraint:

doSomething6 :: Monad f => (t -> f a) -> (a -> f b) -> (b -> f c) -> t -> f (a, b, c)
doSomething6 f g h n = join $ (\a -> join $ (\b -> (\c -> (a, b, c)) <$> h b) <$> g a) <$> f n

-- This implementation will not compile:

-- doSomething6 f g h n = (\a -> (\b -> ((\c -> (a, b, c)) <$> h b) <$> g a)) <$> f n

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
  then Just $ i + 1
  else Nothing

h :: Integer -> Maybe String
h i = Just $ "10191" ++ show i

-- doSomething4 f g h 2 = Just (2, 3, "101913")