module Palindrome where

import Control.Monad
import Data.Char

isPalindrome :: String -> Bool
isPalindrome cs =
    cs' == reverse cs'
    where cs' = map toLower $ filter isAlphaNum cs

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case isPalindrome line1 of
        True  -> putStrLn "It's a palindrome!"
        False -> putStrLn "Nope!"

