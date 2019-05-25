module EitherMonad where

data Sum a b
    = First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First  a) = First a
    fmap f (Second a) = Second $ f a

instance Applicative (Sum a) where
    pure = Second

    First  f <*> _        = First f
    _        <*> First  b = First b
    Second f <*> Second b = Second $ f b

instance Monad (Sum a) where
    return = pure

    First  a >>= _ = First a
    Second a >>= f = f a

