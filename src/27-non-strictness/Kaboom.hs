-- Page 1042

possiblyKaboom :: Num e
               => (((a, b) -> a) -> ((c, d) -> d) -> ((e, f) -> f))
               -> f
possiblyKaboom =
    \f -> f fst snd (0, undefined)

-- 'true' takes two arguments and returns the first one.

true :: a -> a -> a
true = \a -> (\_ -> a)

-- 'false' takes two arguments and returns the second one.

false :: a -> a -> a
false = \_ -> (\b -> b)

possiblyKaboom' :: Num a => Bool -> a
possiblyKaboom' b =
    case b of
        True -> fst tup
        False -> snd tup
    where tup = (0, undefined)

-- Page 1047

wc :: a -> b -> a
wc x z =
    let y = undefined `seq` 'y'
    in x

doSequence :: IO ()
doSequence =
    putStrLn $ "foldr wc 'z' ['a'..'e']: " ++ [foldr wc 'z' ['a'..'e']]

