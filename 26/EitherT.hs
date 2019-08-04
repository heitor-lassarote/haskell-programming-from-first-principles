{-# LANGUAGE InstanceSigs #-}

module EitherT where

import Control.Monad.Trans

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- 1)
instance Functor m => Functor (EitherT e m) where
    fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT e) = EitherT $ (fmap . fmap) f e

-- 2)
instance Applicative m => Applicative (EitherT e m) where
    pure :: a -> EitherT e m a
    pure = EitherT . pure . pure

    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    EitherT f <*> EitherT a = EitherT $ (<*>) <$> f <*> a

-- 3)
instance Monad m => Monad (EitherT e m) where
    return :: a -> EitherT e m a
    return = pure

    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    EitherT v >>= f =
        EitherT $ do
            e <- v
            case e of
                Left  l -> return $ Left l
                Right r -> runEitherT $ f r

-- 4)
swapEither :: Either e a -> Either a e
swapEither (Left  l) = Right l
swapEither (Right r) = Left  r

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT e) = EitherT $ fmap swapEither e

-- 5)
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT amb) = do
    eab <- amb
    case eab of
        Left  a -> f a
        Right b -> g b

-- Lift More:
instance MonadTrans (EitherT e) where
    lift = EitherT . fmap Right

