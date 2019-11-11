{-# LANGUAGE TupleSections #-}

module StateT where

import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Class

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f (StateT st) = StateT $ \s -> (\t -> (f $ fst t, s)) <$> st s

instance Monad m => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)

    StateT f <*> StateT st = StateT $ \s -> do
        (f', s')  <- f s
        (a', s'') <- st s'
        return (f' a', s'')

instance Monad m => Monad (StateT s m) where
    return = pure

    StateT st >>= f = StateT $ \s -> do
        (a, s') <- st s
        (runStateT $ f a) s'

-- Lift More:
instance MonadTrans (StateT s) where
    lift mas = StateT $ \s -> (, s) <$> mas

-- Some Instances:
instance MonadIO m => MonadIO (StateT r m) where
    liftIO = lift . liftIO

