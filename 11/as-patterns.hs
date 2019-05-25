module AsPatterns where

import Data.Char

-- | Exercise 1
isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xs'@(x:xs) (y:ys)
  = if x == y then
        isSubseqOf xs ys
    else
        isSubseqOf xs' ys
        
-- | Exercise 2
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (s:ss) = toUpper s : ss

capitalizeWords :: String -> [(String, String)]
capitalizeWords ss
  = capitalizeWords' (dropWhile (== ' ') ss)
    where
        capitalizeWords' [] = []
        capitalizeWords' ss'@(_:ss)
          = (capitalizeWord thisWord, thisWord) : capitalizeWords word
            where word     = dropWhile (/= ' ') ss
                  thisWord = takeWhile (/= ' ') ss'
    
main :: IO ()
main = do print $ isSubseqOf "blah" "blahwoot"
          print $ isSubseqOf "blah" "wootblah"
          print $ isSubseqOf "blah" "wboloath"
          print $ isSubseqOf "blah" "wootbla"
          print $ isSubseqOf "blah" "halbwoot"
          print $ isSubseqOf "blah" "blawhoot"
          print $ capitalizeWords "hello world"
