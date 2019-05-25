module Main where

import Control.Monad (forever)
import Data.Char (isAlpha, toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

newtype WordList = WordList [String] deriving (Eq, Show)

data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
    show (Puzzle _ discovered guessed remainingGuesses) =
        (intersperse ' '
         $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed ++ " | "
        ++ show remainingGuesses ++ " " ++ try remainingGuesses ++ " remaining."
        where try r = if r == 1 then "try" else "tries"

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

numberOfTries :: Int
numberOfTries = 7

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ WordList $ lines dict

gameWords :: IO WordList
gameWords = do
    (WordList aw) <- allWords
    return $ WordList $ filter gameLength aw
    where
        gameLength w =
            let l = length (w :: String)
            in    l >= minWordLength
               && l <  maxWordLength

randomWord :: WordList -> IO String
randomWord wl'@(WordList wl) = do
    randomIndex <- randomRIO (0, length wl - 1)
    let word = wl !! randomIndex
    if all isAlpha word then
        return word
    else
        randomWord wl'

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle cs = Puzzle cs (map (\_ -> Nothing) cs) [] numberOfTries

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle cs _ _ _) c = elem c cs

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ gs _) c = elem c gs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s remainingGuesses) c =
    Puzzle word newFilledInSoFar (c : s) remainingGuesses
    where
        zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
        newFilledInSoFar =
            zipWith (zipper c) word filledInSoFar

decrementRemaining :: Puzzle -> Puzzle
decrementRemaining (Puzzle word filledInSoFar s remainingGuesses) =
    Puzzle word filledInSoFar s (remainingGuesses - 1)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word, filling in the word\
                     \ accordingly."
            return $ fillInCharacter puzzle guess
        (False, _) -> do
            putStrLn "This character wasn't in the word, try again."
            return $ decrementRemaining $ fillInCharacter puzzle guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ remainingGuesses) =
    if remainingGuesses == 0 then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ wordToGuess
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
    if all isJust filledInSoFar then
        do putStrLn "You win!"
           exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _   -> putStrLn "Your guess must be a single character."

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

