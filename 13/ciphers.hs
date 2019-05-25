module Ciphers where

import Data.Char
import System.IO

caesar :: Int -> String -> String
caesar _ [] = []
caesar s xs = map (chr . (+ aOrd) . (`mod` aOrd) . ((-) s . (-) aOrd) . ord) $ xs
              where aOrd = ord 'a'
              
unCaesar :: Int -> String -> String
unCaesar _ [] = []
unCaesar s xs = caesar (-s) xs

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

handleCaesar :: IO ()
handleCaesar = do
    putStr "Input: "
    input <- getLine
    putStr "Offset: "
    offset' <- getLine
    let offset = read offset'
    let cipher = caesar offset input
    putStrLn $ "The encoded message is: " ++ cipher

handleVegenère :: IO ()
handleVegenère = do
    putStr "Input: "
    input <- getLine
    putStr "Keyword: "
    keyword <- getLine
    let cipher = vegenère input keyword
    putStrLn $ "The encoded message is: " ++ cipher

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Caesar or Vegenère ciphering?"
    putStr "Your input [c/v]: "
    input <- getLine
    case input of
        "c" -> handleCaesar
        "C" -> handleCaesar
        "v" -> handleVegenère
        "V" -> handleVegenère
        _   -> putStrLn "Invalid option."

