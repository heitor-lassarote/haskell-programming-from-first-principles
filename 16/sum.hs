module Sum where

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b)  = Second $ f b

-- | 2: It's impossible because a Functor requires kind * -> *, and so in the
-- | definition we must build Sum with a so it's not a * -> * -> * anymore, but
-- | rather a * -> *. Because of this, we can't interact with the first type,
-- | but only the second one, since it's the one being recognized by the
-- | Functor.

