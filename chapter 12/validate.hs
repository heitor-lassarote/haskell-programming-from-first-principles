module Validate where

newtype Word'
  = Word' String
    deriving (Eq, Show)

vowels = "AEIOUaeiou"
consonants = filter (not . (flip elem) vowels) ['a'..'z'] ++ ['A'..'Z']

mkWord :: String -> Maybe Word'
mkWord cs =
    let (v, c) = mkWord' cs 0 0
    in if v > c then Nothing else Just $ Word' cs
    where
        mkWord' [] v c = (v, c)
        mkWord' (x : xs) v c | elem x vowels     = mkWord' xs (v + 1) c
                             | elem x consonants = mkWord' xs v (c + 1)
                             | otherwise         = mkWord' xs v c
