module Scans where

-- | Exercise 1
fibs :: [Integer]
fibs = take 20 fibsImpl
       where fibsImpl = 1 : scanl (+) 1 fibsImpl

-- | Exercise 2
fibs' :: [Integer]
fibs' = takeWhile (< 100) fibsImpl
        where fibsImpl = 1 : scanl (+) 1 fibsImpl

-- | Exercise 3
factorial = scanl (*) 1 (enumFrom 1)
