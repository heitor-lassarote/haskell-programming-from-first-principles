module Phone where

import Data.Char (isUpper, toLower)
import Data.Function (on)
import Data.List (group, intersperse, maximumBy, sort)

-- | Exercise 1
type Digit  = Char
type Digits = [Digit]
    
type Phone = [(Digit, Digits)]
    
type Presses = Int

type Key = (Digit, Presses)

-- | Exercise 2
convo :: [String]
convo =
    [ "Wanna play 20 questions"
    , "Ya"
    , "U 1st haha"
    , "Lol ok. Have u ever tasted alcohol"
    , "Lol ya"
    , "Wow ur cool haha. Ur turn"
    , "Ok. Do u think I am pretty Lol"
    , "Lol ya"
    , "Just making sure rofl ur turn"
    ]

makePhone :: [Digits] -> Phone
makePhone ds = map (\d -> (head d, d)) ds

phone :: Phone
phone = makePhone [ "1"    , "2abc", "3def" 
                  , "4ghi" , "5jkl", "6mno" 
                  , "7pqrs", "8tuv", "9wxyz"
                  , "*^"   , "0+ _", "#.,"  
                  ]

digitPresses :: Digit -> Digits -> Presses
digitPresses _ [] = error "empty list"
digitPresses d ds
  = digitPresses' d ds 0
    where
        digitPresses' _ [] _ = 0
        digitPresses' d (d':ds) c
            = if d == d' then (c + 1) else digitPresses' d ds (c + 1)

findKey :: Digit -> Phone -> Key
findKey _ [] = error "not found"
findKey d' ((d, ks):p)
   = if elem d' ks then
        (d, digitPresses d' ks)
    else
        findKey d' p

reverseTaps :: Phone -> Char -> [Key]
reverseTaps [] _ = []
reverseTaps p c
  = if isUpper c then
        [findKey '*' p, findKey (toLower c) p]
    else
        [findKey c p]

cellPhonesDead :: Phone -> String -> [Key]
cellPhonesDead p = concatMap (reverseTaps p)

-- | Exercise 3
fingerTaps :: [Key] -> Presses
fingerTaps [] = 0
fingerTaps ((_, p):ks) = p + fingerTaps ks

-- | Exercise 4
mode :: Ord a => [a] -> a
mode = head . maximumBy (compare `on` length) . group . sort

mostPopularLetter :: String -> Char
mostPopularLetter [] = error "empty string"
mostPopularLetter cs = fst $ mode $ cellPhonesDead phone cs

-- | Exercise 5
coolestLtr :: [String] -> Char
coolestLtr = mode . map mostPopularLetter

coolestWord :: [String] -> String
coolestWord = mode . concatMap words
