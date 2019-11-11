module Chapter2 where

-- | Exercise 4
waxOn       = x * 5
    where z = 7
          y = z + 8
          x = y ^ 2
          
-- | Exercise 5
triple x = x * 3

-- | Exercise 6
waxOff x = triple x
