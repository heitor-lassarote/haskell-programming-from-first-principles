{-# LANGUAGE InstanceSigs #-}

-- | 1)
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ \r -> f $ ra r

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ \_ -> a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    Reader rab <*> Reader ra = Reader $ \r -> rab r $ ra r

instance Monad (Reader r) where
    return :: a -> Reader r a
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    Reader ra >>= arb = Reader $ \r -> (runReader $ arb $ ra r) r

-- | 2)
newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person =
    Person
    { humanName :: HumanName
    , dogName   :: DogName
    , address   :: Address
    } deriving (Eq, Show)

data Dog =
    Dog
    { dogsName    :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = do
    name <- Reader dogName
    addy <- Reader address
    return $ Dog name addy

