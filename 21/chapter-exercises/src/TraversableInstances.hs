{-# LANGUAGE FlexibleContexts #-}

module TraversableInstances where

import Data.Monoid ((<>))

-- | Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
    foldr f b (Identity a) = f a b

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

-- | Constant
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldr _ b _ = b

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure $ Constant a

-- | Maybe
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada    = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
    foldr _ b Nada    = b
    foldr f b (Yep a) = f a b

instance Traversable Optional where
    traverse _ Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a

-- | List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
    foldr _ b Nil         = b
    foldr f b (Cons x xs) = f x (foldr f b xs)

instance Traversable List where
    traverse _ Nil         = pure Nil
    traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

-- | Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldr f b (Three _ _ c) = f c b

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

-- | Pair
data Pair a b = Pair a b deriving (Eq, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldr f b' (Pair _ b) = f b b'

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b

-- | Big
data Big a b = Big a b b deriving (Eq, Show)

instance Functor (Big a) where
    fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
    foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
    traverse f (Big a b b') = Big a <$> f b <*> f b'

-- | Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show)

instance Functor (Bigger a) where
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
    foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
    traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

-- | S
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S n a) = S (f <$> n) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
    traverse f (S n a) = S <$> traverse f n <*> f a

-- | Tree
data Tree a =
      Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty         = Empty
    fmap f (Leaf a)      = Leaf $ f a
    fmap f (Node t a t') = Node (f <$> t) (f a) (f <$> t')

instance Foldable Tree where
    foldMap _ Empty         = mempty
    foldMap f (Leaf a)      = f a
    foldMap f (Node t a t') = foldMap f t <> f a <> foldMap f t'

instance Traversable Tree where
    traverse _ Empty         = pure Empty
    traverse f (Leaf a)      = Leaf <$> f a
    traverse f (Node t a t') = Node <$> traverse f t <*> f a <*> traverse f t'

