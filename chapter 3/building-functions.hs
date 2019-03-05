module SomeStrings where

-- | Exercise 2
exclamate :: String -> String
exclamate s = s ++ "!"

at4 :: String -> Char
at4 s = s !! 4

lastWord :: String -> String
lastWord s = last $ words s

-- | Exercise 3
thirdLetter :: String -> Char
thirdLetter s = s !! 2

-- | Exercise 4
letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

-- | Exercise 5
rvrs :: String
rvrs = let str = "Curry is awesome"
           fst = take 5 str
           snd = take 4 $ drop 5 str
           trd = take 7 $ drop 9 str
       in concat [trd, snd, fst]
