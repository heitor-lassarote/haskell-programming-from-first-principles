module StringProcessing where

import Data.List (elem)

-- | Exercise 1
notThe :: String -> Maybe String
notThe s = if s == "the" then Nothing else Just s

replaceThe :: String -> String
replaceThe
  = unwords . replaceThe' . words
    where
        replaceThe' [] = []
        replaceThe' (w : ws)
          = case notThe w of
                Nothing -> "a" : replaceThe' ws
                Just w' -> w'  : replaceThe' ws

-- | Exercise 2
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel
  = countTheBeforeVowel' False . words
    where
        -- | Assume all text is in lowercase for now.
        vowels = "aeiou"
        countTheBeforeVowel' _ [] = 0
        countTheBeforeVowel' b (w:ws)
            = case notThe w of
                Nothing -> countTheBeforeVowel' True ws
                Just w' ->
                    if elem (head w') vowels then
                        1 + countTheBeforeVowel' False ws
                    else
                        countTheBeforeVowel' False ws

-- | Exercise 3
countVowels :: String -> Integer
countVowels [] = 0
countVowels (c:cs)
  = if elem c vowels then
        1 + vowelCount
    else
        vowelCount
    where
        vowels = "aeiou"
        vowelCount = countVowels cs
