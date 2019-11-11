module Arithmetic where

-- | 1
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Fractional a => a -> a
halfIdentity = (*2) . half

-- | 2
listOrdered :: Ord a => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

-- | 3
plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

-- | 4
multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

-- | 5
quotRemId :: (Eq a, Integral a) => a -> a -> Bool
quotRemId x y = (quot x y) * y + (rem x y) == x

divModId :: (Eq a, Integral a) => a -> a -> Bool
divModId x y = (div x y) * y + (mod x y) == x

-- | 6
powAssossicative :: Integral a => a -> a -> a -> Bool
powAssossicative x y z = x ^ (y ^ z) == (x ^ y) ^ z

powCommutative :: Integral a => a -> a -> Bool
powCommutative x y = (x ^ y) == (y ^ x)

-- | 7
reverseTwiceIsId :: Eq a => [a] -> Bool
reverseTwiceIsId xs = (reverse $ reverse xs) == (id xs)

