import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor f => Functor (MaybeT f) where
    fmap f (MaybeT mMa) = MaybeT $ (fmap . fmap) f mMa

instance Applicative f => Applicative (MaybeT f) where
    pure = MaybeT . pure . pure

    MaybeT mMf <*> MaybeT mMa = MaybeT $ (<*>) <$> mMf <*> mMa

instance Monad m => Monad (MaybeT m) where
    return = pure

    MaybeT ma >>= f = MaybeT $ do
        v <- ma
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)

instance MonadTrans MaybeT where
    lift = MaybeT . fmap Just

instance MonadIO m => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

