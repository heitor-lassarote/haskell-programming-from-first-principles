module WriteInstances where

-- | 1)
data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
    pure x = Pair x x

    (Pair f g) <*> (Pair a b) = Pair (f a) (g b)

-- | 2)
data Two a b = Two a b deriving Show

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    (Two f g) <*> (Two a b) = Two (mappend f a) (g b)

-- | 3)
data Three a b c = Three a b c deriving Show

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    (Three f g h) <*> (Three a b c) = Three (mappend f a) (mappend g b) (h c)

-- | 4)
data Three' a b = Three' a b b deriving Show

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
    pure x = Three' mempty x x
    (Three' f g h) <*> (Three' a b b') = Three' (mappend f a) (g b) (h b')

-- | 5)
data Four a b c d = Four a b c d deriving Show

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    (Four f g h i) <*> (Four a b c d)
      = Four (mappend f a) (mappend g b) (mappend h c) (i d)

