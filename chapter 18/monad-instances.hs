module MonadInstances where

import Prelude
    ( Functor (fmap)
    , Applicative (pure, (<*>))
    , Monad (return, (>>=))
    , ($)
    , Eq
    , Ord
    , Show
    )

-- | 1)
data Nope a = NopeDotJpg

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return _ = NopeDotJpg
    _ >>= _ = NopeDotJpg

-- | 2)
data PhhhbbtttEither b a
    = Left a
    | Right b

instance Functor (PhhhbbtttEither b) where
    fmap f (Left a)  = Left $ f a
    fmap _ (Right b) = Right b

instance Applicative (PhhhbbtttEither b) where
    pure = Left

    Left  f <*> Left  a = Left $ f a
    Right f <*> _       = Right f
    _       <*> Right a = Right a

instance Monad (PhhhbbtttEither b) where
    return = pure

    Left  a >>= f = f a
    Right a >>= _ = Right a

-- | 3)
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    pure = Identity

    Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
    return = pure

    Identity a >>= f = f a

-- | 4)
data List a
    = Nil
    | Cons a (List a)
    deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil         ls = ls
append (Cons x xs) ls = Cons x (append xs ls)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil         = b
fold f b (Cons x xs) = f x (fold f b xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    pure a = Cons a Nil

    Nil <*> _   = Nil
    _   <*> Nil = Nil
    fs  <*> xs  = flatMap (\f -> fmap f xs) fs

instance Monad List where
    return = pure

    Nil       >>= _ = Nil
    Cons x xs >>= f = append (f x) (xs >>= f)

