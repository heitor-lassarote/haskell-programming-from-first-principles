module MyBool where

-- | Exercise 1
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- | Exercise 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

-- | Exercise 3
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = e == x || myElem e xs

-- | Using myAny
myElem' :: Eq a => a -> [a] -> Bool
myElem' e = myAny (== e)

-- | Exercise 4
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs
  = myReverseImpl xs []
    where myReverseImpl [] ys = ys
          myReverseImpl (x:xs) ys = myReverseImpl xs (x:ys)

-- | Exercise 5
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- | Exercise 6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- | Exercise 7
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- | Exercise 8
myCompareBy :: Ordering -> (a -> a -> Ordering) -> [a] -> a
myCompareBy _ _ [] = error "empty list"
myCompareBy _ _ (x:[]) = x
myCompareBy o f (x:xs)
  = if f x xs' == o then x else xs'
    where xs' = myCompareBy o f xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = myCompareBy GT

-- | Exercise 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myCompareBy LT

-- | Exercise 10
myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare
