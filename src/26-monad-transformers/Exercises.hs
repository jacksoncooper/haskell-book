-- Page 1032

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-- Write the Code

-- 1.

rDec :: Num a => Reader a a -- Num a => (a -> a)
rDec = reader $ subtract 1

-- 2. Done.

-- 3.

rShow :: Show a => ReaderT a Identity String -- Show a => (a -> Identity String)
rShow = reader show

-- 4. Done.

-- 5.

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a ->
    putStrLn ("Hello. The input is " <> show a <> ".")
        >> return (a + 1)

-- traverse (runReaderT rPrintAndInc) [1..10]
-- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
--          :: Num a => (a -> IO a) -> [a] -> IO [a] 

-- Oh my gosh we're actually doing IO! And only 26 chapters in!

-- 6.

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \a ->
    putStrLn ("Hello. The input is " <> show a <> ".")
        >> return (show a, a + 1)

-- Fix the Code

-- 1.

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $
    getLine >>= \exclamation ->
        -- This case expression is kind of gross. I butchered whatever
        -- abstraction the authors had going.
        case isValid exclamation of
            True  -> return (Just exclamation)
            False -> return Nothing

doExcite :: IO ()
doExcite = do
    putStrLn "Say something excite!"
    excite <- runMaybeT maybeExcite

    case excite of
      Nothing -> putStrLn "Moar excite!"
      Just e -> putStrLn $ "Good, was very excite: " <> e

