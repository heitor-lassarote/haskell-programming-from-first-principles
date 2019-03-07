module MyFolds where

import Data.Bool

-- | Exercise 1
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- | Exercise 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- | Exercise 3
-- | With fold:
myElem :: Eq a => a -> [a] -> Bool
myElem e = foldr ((||) . (== e)) False

-- | With any:
myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (== e)

-- | Exercise 4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- | Exercise 5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- | Exercise 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\a b -> if p a then a : b else b) []

-- | Exercise 7
squish :: [[a]] -> [a]
squish = foldr (++) []

-- | Exercise 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- | Exercise 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- | Exercise 10
myCompareBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myCompareBy _ _ [] = error "empty list"
myCompareBy o f xs = foldr (\a b -> if f a b == o then a else b) (last xs) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myCompareBy GT

-- | Exercise 11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myCompareBy LT
