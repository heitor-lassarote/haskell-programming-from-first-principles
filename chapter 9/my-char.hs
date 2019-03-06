module MyChar where

import Data.Char

-- | Exercise 2
filterUpper :: String -> String
filterUpper = filter isUpper

-- | Exercise 3
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- | Exercise 4
yell :: String -> String
yell [] = []
yell (x:xs) = toUpper x : yell xs

-- | Exercises 5 and 6
headUpper = toUpper . head
