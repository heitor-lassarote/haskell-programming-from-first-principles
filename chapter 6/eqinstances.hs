module EqInstances where

-- | Exercise 1
data TisAnInteger = TisAnInteger
instance Eq TisAnInteger where
    (==) TisAnInteger TisAnInteger = True
    
-- | Exercise 2
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'
    
-- | Exercise 3
data StringOrInt
    = TisAnInt   Int
    | TisAString String
instance Eq StringOrInt where
    (==) (TisAnInt i)   (TisAnInt i')   = i == i'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) _              _               = False
    
-- | Exercise 4
data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') = a == a' && b == b'
    
-- | Exercise 5
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

-- | Exercise 6
data Which a
    = ThisOne a
    | ThatOne a
instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) (ThisOne a) (ThatOne a') = a == a'
    (==) (ThatOne a) (ThisOne a') = a == a'
    
-- | Exercise 7
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a)   (Hello a')   = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'
    (==) _           _            = False
