module LibraryFunctions where

import Data.Maybe
import Data.Monoid

-- | 1
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

-- | 2
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

-- | 3)
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr ((||) . (== x)) False

-- | 4)
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr min' Nothing
    where min' a (Just b) = Just $ min a b
          min' a Nothing  = Just a

-- | 5)
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr max' Nothing
    where max' a (Just b) = Just $ max a b
          max' a Nothing  = Just a

-- | 6)
null' :: Foldable t => t a -> Bool
null' = isNothing . foldr (const . Just) Nothing

-- | 7)
length' :: Foldable t => t a -> Int
length' = foldr (\_ b -> b + 1) 0

-- | 8)
toList' :: Foldable t => t a -> [a]
toList' = foldr (:) []

-- | 9)
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (mappend mempty)

-- | 10)
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (const . f) mempty

