module Main where

import           Control.Monad
import           Data.Char
import qualified System.Environment as SE
import           System.Exit
import qualified System.IO          as SIO

type Keyword = String

vigenère :: String -> Keyword -> String
vigenère ss [] = ss
vigenère [] _  = []
vigenère ss cs = vigenère' ss (cycle cs)
  where
    vigenère' [] _  = []
    vigenère' ss [] = ss
    vigenère' (s:ss) (c:cs)
      = if isAlpha s then
            chr ((ord s - a + c') `mod` 26 + a) : vigenère ss cs
        else
            s : vigenère' ss (c:cs)
      where
        (a, c')
          = if isUpper s then
                (ord 'A', ord (toUpper c) - a)
            else
                (ord 'a', ord (toLower c) - a)

unVigenère :: Keyword -> String -> String
unVigenère _  [] = []
unVigenère [] ss = ss
unVigenère cs ss = unVigenère' ss (cycle cs)
  where
    unVigenère' [] _  = []
    unVigenère' ss [] = ss
    unVigenère' (s:ss) (c:cs)
      = if isAlpha s then
            chr ((ord s - c') `mod` 26 + a) : unVigenère' ss cs
        else
            s : unVigenère' ss (c:cs)
      where
        (a, c')
          = if isUpper s then
                (ord 'A', ord (toUpper c))
            else
                (ord 'a', ord (toLower c))

main :: IO ()
main = do
    args <- SE.getArgs
    when (length args < 3) $ putStrLn "Usage: -d or -e followed by key then text." >> exitFailure
    let (op : key : text) = args
        text' = unwords text
    putStrLn $
        if op == "-d" then
            unVigenère key text'
        else
            vigenère text' key

