module SplitBy where

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy b s  = let r = dropWhile (== b) s
               in if r == []
                  then []
                  else takeWhile (/= b) r : (splitBy b (dropWhile (/= b) r))

myWords :: String -> [String]
myWords = splitBy ' '

myLines :: String -> [String]
myLines = splitBy '\n'
