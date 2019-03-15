module Person where

import System.IO

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
      NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person

mkPerson name age
    | name /= "" && age > 0 =
          Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise =
        Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++
                " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    hSetBuffering stdout NoBuffering
    putStr "Input the name: "
    name <- getLine
    putStr "Input the age: "
    age' <- getLine
    let age = read age'
    let person = mkPerson name age
    case person of
        Left (NameEmpty) -> putStrLn "Name cannot be empty."
        Left (AgeTooLow) -> putStrLn "Age must be non-negative."
        Left (PersonInvalidUnknown str) -> putStrLn $ "Invalid name and age: "
                                                      ++ str
        Right (Person n a) -> putStrLn $ "Yay! Successfully got a person: "
                                         ++ (show $ Person n a)

