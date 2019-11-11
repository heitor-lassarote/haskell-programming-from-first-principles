import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Class

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
    pure = ReaderT . pure . pure

    ReaderT fmab <*> ReaderT rma = ReaderT $ (<*>) <$> fmab <*> rma

instance Monad m => Monad (ReaderT r m) where
    return = pure

    ReaderT rma >>= f = ReaderT $ \r -> do
        a <- rma r
        runReaderT (f a) r

instance MonadTrans (ReaderT r) where
    lift = ReaderT . const

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

