-- Page 1003

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-- MaybeT :: m (Maybe a) -> MaybeT m a
-- ExceptT :: m (Either e a) -> ExceptT e m a
-- ReaderT :: (r -> m a) -> ReaderT r m a

-- readerUnwrap () = Right (Just 1)

-- Page 1105

filling :: a -> Either b (Maybe Int)
filling = const (Right (Just 1))

readerWrap :: ReaderT () IO (Either String (Maybe Int))
readerWrap = ReaderT $ pure <$> filling

eitherWrap :: ExceptT String (ReaderT () IO) (Maybe Int)
eitherWrap = ExceptT readerWrap

maybeWrap :: MaybeT (ExceptT String (ReaderT () IO)) Int
maybeWrap = MaybeT eitherWrap

-- Playing around.

unwrap :: MaybeT (ExceptT String (ReaderT () IO)) Int
       -> (() -> IO (Either String (Maybe Int)))
unwrap = runReaderT . runExceptT . runMaybeT

peek :: MaybeT (ExceptT String (ReaderT () IO)) Int
     -> IO ()
peek bundle = (unwrap bundle) () >>= putStrLn . show

bigLift :: MaybeT (ExceptT String (ReaderT () IO)) Int
        -> MaybeT (ExceptT String (ReaderT () IO)) Int
bigLift bundle = bundle >>= \a -> return $ a + 1

-- peek . bigLift $ embedded
