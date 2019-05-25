-- | Exercise: Fix the Print3Broken code.
module Print3Broken where

main :: IO ()
main = do putStrLn greeting
          printSecond
          where greeting    = "Yarrrrr"
                printSecond = do putStrLn greeting
                