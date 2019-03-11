{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module LogicGoats where

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

-- | Exercise 1
instance TooMany (Int, String) where
    tooMany (i, s) = tooMany i || tooMany (length s)

-- | Exercise 2
instance TooMany (Int, Int) where
    tooMany (i, i') = tooMany (i + i')

-- | Exercise 3
instance (Num a, TooMany a) => TooMany (a, a) where
    tooMany (x, y) = tooMany (x + y)
