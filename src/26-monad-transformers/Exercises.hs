-- Page 1032

import Data.Functor.Identity
import Control.Monad.Trans.Reader

-- 1.

rDec :: Num a => Reader a a -- Num a => (a -> a)
rDec = reader $ subtract 1

-- 2. Done.

-- 3.

rShow :: Show a => ReaderT a Identity String -- Show a => (a -> Identity String)
rShow = reader show

-- 4. Done.

-- 5.
