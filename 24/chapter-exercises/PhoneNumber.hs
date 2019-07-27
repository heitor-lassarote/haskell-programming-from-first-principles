module PhoneNumber where

import Control.Applicative
import Text.Trifecta

-- | Using Brazilian phone numbers:
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

ex1, ex2, ex3, ex4 :: String
ex1 = "12 3456 7890"
ex2 = "1234567890"
ex3 = "(12) 3456-7890"
ex4 = "12 9 3456 7890"

notDigit :: Parser Char
notDigit = noneOf ['0'..'9']

skipNotDigit :: Parser ()
skipNotDigit = skipMany notDigit

area :: Parser NumberingPlanArea
area = do
    n1 <- digit
    n2 <- digit
    return $ read [n1, n2]

read4 :: Parser String
read4 = count 4 digit

exchange :: Parser Exchange
exchange = read <$> (readMobile <|> readTelephone)
    where
        readMobile = do
            nine <- char '9'
            skipNotDigit
            digits <- read4
            return $ nine : digits
        readTelephone = read4

lineNumber :: Parser Exchange
lineNumber = read <$> read4

parsePhone :: Parser PhoneNumber
parsePhone = do
    skipNotDigit
    a <- area
    skipNotDigit
    e <- exchange
    skipNotDigit
    n <- lineNumber

    return $ PhoneNumber a e n

