import Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = do
    c <- cap
    r <- rev
    return (c, r)

tupled' :: [Char] -> ([Char], [Char])
tupled' =
    rev >>=
    \r ->
        cap >>= return . ((,) r)

