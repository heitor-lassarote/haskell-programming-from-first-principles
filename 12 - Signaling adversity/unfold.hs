module Unfold where

-- | Exercise 1
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

-- | Exercise 2
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b
  = case f b of
        Just (a, b') -> a : myUnfoldr f b'
        Nothing      -> []

-- | Exercise 3
betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\a -> Just (a, f a))
