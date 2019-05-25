module Reverse where

rvrs :: String -> String
rvrs x = unwords $ reverse $ words x

main :: IO ()
main = print (rvrs "Curry is awesome")
