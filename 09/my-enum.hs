module MyEnum where

eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True  = [False, True]
eftBool True  False = []
eftBool True  True  = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b | a <= b    = a : eftOrd (succ a) b
           | otherwise = []

eftInt :: Int -> Int -> [Int]
eftInt a b | a <= b    = a : eftInt (succ a) b
           | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar a b | a <= b    = a : eftChar (succ a) b
            | otherwise = []
