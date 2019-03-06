module Ciphers where

import Data.Char

caesar :: Int -> String -> String
caesar _ [] = []
caesar s xs = map (chr . (+ aOrd) . (`mod` aOrd) . ((-) s . (-) aOrd) . ord) $ xs
              where aOrd = ord 'a'
              
unCaesar :: Int -> String -> String
unCaesar _ [] = []
unCaesar s xs = caesar (-s) xs
