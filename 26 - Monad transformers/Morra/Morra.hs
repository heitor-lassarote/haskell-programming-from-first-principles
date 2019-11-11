module Morra where

import Control.Monad (forever)
import Control.Monad.Trans.State
import Data.Char (toUpper)
import Data.Ix (inRange)
import System.Exit (exitSuccess)
import System.Random (getStdRandom, randomR)
import Text.Read (readMaybe)

data MorraState = MorraState
    { player1      :: Int
    , player2      :: Int
    , player2IsBot :: Bool
    , trigram      :: [Int] }

chooseTrigram :: [Int] -> IO Int
chooseTrigram t =
    let s1 = length $ filter (== 1) t
        s2 = length $ filter (== 2) t
        b = compare s1 s2
     in case b of
            GT -> return 1
            LT -> return 2
            EQ -> getStdRandom $ randomR (1, 2)

computer :: [Int] -> IO Int
computer t = do
    r <- chooseTrigram t
    putStrLn $ "C: " ++ show r
    return r

inputNumber :: String -> IO Int
inputNumber msg = do
    putStr msg
    i <- readMaybe <$> getLine
    case i of
        Just i' ->
            if inRange (1, 2) i' then
                return i'
            else
                putStrLn "Value must be 1 or 2. Try again." >>
                inputNumber msg
        Nothing ->
            putStrLn "Invalid input! Must be a natural number. Try again." >>
            inputNumber msg

gameLoop :: StateT MorraState IO Int
gameLoop = forever $ StateT $ \s -> do
    let isBot = player2IsBot s
    p1 <- inputNumber "P: "
    p2 <- if isBot then computer $ trigram s else inputNumber "C: "
    let sum = p1 + p2
        cWon = odd sum
        (s1, s2) = (player1 s, player2 s)
        (s1', s2') = if cWon then (s1, s2 + 1) else (s1 + 1, s2)
        t = p1 : trigram s
        s' = MorraState s1' s2' isBot t
    if cWon then
        putStrLn "- C wins"
    else
        putStrLn "- P wins"

    if s1' == 3 || s2' == 3 then do
        putStrLn $ "C has " ++ show s1' ++ " points and P has " ++ show s2' ++ " points."
        exitSuccess
    else
        return (sum, s')

chooseHumanOrBot :: IO Bool
chooseHumanOrBot = do
    putStr "Play against [C]omputer or [H]uman?: "
    op <- getLine
    let o = toUpper <$> op
     in if o == "H" || o == "HUMAN" then
            return False
        else if o == "C" || o == "COMPUTER" then
            return True
        else
            putStrLn "Invalid choice! Try again." >> chooseHumanOrBot

main :: IO ()
main = do
    op <- chooseHumanOrBot
    putStrLn "-- P is Player"
    if op then
        putStrLn "-- C is Computer"
    else
        putStrLn "-- C is Player"

    putStrLn "-- P is odds, C is evens."

    runStateT gameLoop (MorraState 0 0 op [])
    return ()

