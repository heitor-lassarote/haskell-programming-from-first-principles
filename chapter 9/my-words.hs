module MyWords where

myWords :: String -> [String]
myWords s = let r = dropWhile (== ' ') s
            in if r == ""
               then []
               else takeWhile (/= ' ') r : (myWords (dropWhile (/= ' ') r))
