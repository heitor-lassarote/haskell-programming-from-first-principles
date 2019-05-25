module List where

import Test.QuickCheck.Checkers

data List a = Nil | Cons a (List a) deriving (Eq, Show)

append :: List a -> List a -> List a
append Nil         ls = ls
append (Cons x xs) ls = Cons x (append xs ls)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ acc Nil         = acc
fold f acc (Cons x xs) = f x (fold f acc xs)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Monoid (List a) where
    mempty = Nil

    mappend Nil         ys = ys
    mappend (Cons x xs) ys = Cons x (xs `mappend` ys)

instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
    pure x = Cons x Nil

    Nil <*> _   = Nil
    _   <*> Nil = Nil
    fs  <*> xs  = flatMap (\f -> fmap f xs) fs

newtype ZipList a = ZipList (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Eq a => EqProp (ZipList a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList l) = xs
                    in take' 3000 l
              ys' = let (ZipList l) = ys
                    in take' 3000 l

instance Monoid (ZipList a) where
    mempty = ZipList Nil

    mappend (ZipList xs) (ZipList ys) = ZipList $ xs `mappend` ys

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList $ fmap f xs

instance Applicative ZipList where
    pure x = ZipList $ infinite x
        where infinite x = Cons x (infinite x)

    (ZipList fs) <*> (ZipList xs) = ZipList $ apply fs xs
        where apply Nil         _           = Nil
              apply _           Nil         = Nil
              apply (Cons f fs) (Cons x xs) = Cons (f x) (apply fs xs)

