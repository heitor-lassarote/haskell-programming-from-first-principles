module Language where

import Data.Char

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (s:ss) = toUpper s : ss

-- | Notice: List is by no means exhaustive.
terminators :: String
terminators = ":;-!?,.\n"

isTerminator :: Char -> Bool
isTerminator c = any (== c) terminators

capitalizeParagraph :: String -> String
capitalizeParagraph ss
  = capitalizeParagraph' True ss
    where
        capitalizeParagraph' _ [] = []
        capitalizeParagraph' b ss'@(s:ss)
            | isTerminator s = s : capitalizeParagraph' True ss
            | isAlpha s && b = toUpper s : capitalizeParagraph' False ss
            | otherwise      = s : capitalizeParagraph' b ss
