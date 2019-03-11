module Chiphers where

import Data.Char

type Keyword = String

vegenère :: String -> Keyword -> String
vegenère str []  = str
vegenère []  cod = []
vegenère ss cs
  = vegenère' ss (cycle cs)
    where
        vegenère' ss [] = ss
        vegenère' (s:ss) (c:cs)
          = if isAlpha s then
                chr ((ord s - a + c') `mod` 26 + a) : vegenère ss cs
            else
                s : vegenère ss (c:cs)
            where (a, c') = if isUpper s then
                                (ord 'A', ord (toUpper c) - ord 'A')
                            else
                                (ord 'a', ord (toLower c) - ord 'a')
