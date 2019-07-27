import Text.Trifecta

-- | Alternatively:
-- parseInteger = integer >>= \int -> eof >> return int
parseInteger :: Parser Integer
parseInteger = do
    int <- integer
    eof
    return int

