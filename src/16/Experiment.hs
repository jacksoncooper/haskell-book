-- Page 642

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f, Functor g) => f (g a) -> f (g Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f, Functor g, Functor h) => f (g (h a)) -> f (g (h Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe String] -> [Maybe String]
thriceLifted' = thriceLifted

main :: IO ()
main = do
  putStr "replaceWithP lms: "
  print $ replaceWithP lms

  putStr "replaceWithP' lms: "
  print $ replaceWithP' lms

  putStr "liftedReplace lms: "
  print $ liftedReplace lms

  putStr "liftedReplace' lms: "
  print $ liftedReplace' lms

  putStr "twiceLifted lms: "
  print $ twiceLifted lms

  putStr "twiceLifted' lms: "
  print $ twiceLifted' lms

  putStr "thriceLifted lms: "
  print $ thriceLifted lms

  putStr "thriceLifted' lms: "
  print $ thriceLifted' lms