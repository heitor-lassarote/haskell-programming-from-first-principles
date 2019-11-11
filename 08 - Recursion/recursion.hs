module Recursion where

-- | Exercise 2
sumFrom :: (Eq a, Num a) => a -> a
sumFrom 1 = 1
sumFrom n = n + sumFrom (n - 1)

-- | Exercise 3
mult :: Integral a => a -> a -> a
mult a b
    | a < 0     = -(mult (-a)   b)
    | b < 0     = -(mult   a  (-b))
    | a < b     = multImpl a b
    | otherwise = multImpl b a
    where multImpl a b | a == 0 || b == 0 = 0
                       | otherwise        = b + multImpl (a - 1) b

-- | Fixing dividedBy
data DividedResult a
    = Result a
    | DividedByZero
    deriving Show

dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy num denom
    | denom == 0                 = DividedByZero
    | signum num /= signum denom = Result (-(go (abs num) (abs denom) 0))
    | otherwise                  = Result   (go (abs num) (abs denom) 0)
    where go n d count | n < d     = count
                       | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 function
mc91 :: Integral a => a -> a
mc91 n | n > 100   = n - 10
       | otherwise = mc91 . mc91 $ n + 11
