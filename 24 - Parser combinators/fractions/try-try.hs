import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return $ numerator % denominator

parseDecimal :: Parser Double
parseDecimal = do
    int <- realToFrac <$> decimal
    char '.'
    frac <- realToFrac <$> decimal
    return $ int + frac / (10 ** size frac)
    where size x = realToFrac $ floor $ logBase 10 x + 1

fractionOrDecimal :: Parser (Either Rational Double)
fractionOrDecimal =
        (Left  <$> try parseFraction <?> "Fraction")
    <|> (Right <$> try parseDecimal  <?> "Decimal")

