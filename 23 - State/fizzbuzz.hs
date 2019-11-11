import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo a b | a > b     = []
                   | otherwise = fizzBuzz a : fizzBuzzFromTo (a + 1) b

main :: IO ()
main = mapM_ putStrLn $ fizzBuzzFromTo 1 100

