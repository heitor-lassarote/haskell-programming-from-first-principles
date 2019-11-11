import Control.Applicative
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = read <$> (withSignal <|> withoutSignal)
    where withSignal = (:) <$> char '-' <*> some parseDigit
          withoutSignal = char '+' *> some parseDigit <|> some parseDigit

