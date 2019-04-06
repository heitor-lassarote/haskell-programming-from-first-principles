module Optional where

data Optional a
    = Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend Nada Nada = Nada
    mappend Nada b = b
    mappend a Nada = a
    mappend (Only a) (Only b) = Only $ mappend a b

