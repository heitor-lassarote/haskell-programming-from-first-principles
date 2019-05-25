module InstancesOfFunc where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f)
               => (a -> b)
               -> (b -> c)
               -> f a
               -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Functor f, Eq (f c))
                => Fun a b
                -> Fun b c
                -> f a
                -> Bool
functorCompose' (Fun _ f) (Fun _ g) x = functorCompose f g x

main :: IO ()
main = do
    quickCheck (functorIdentity :: Identity Int -> Bool)
    quickCheck (functorCompose' :: Fun Int Int -> Fun Int Int -> Identity Int -> Bool)
    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck (functorCompose' :: Fun Int Int -> Fun Int Int -> Pair Int -> Bool)
    quickCheck (functorIdentity :: Two Int String -> Bool)
    quickCheck (functorCompose' :: Fun String Int -> Fun Int Int -> Two Int String -> Bool)
    quickCheck (functorIdentity :: Three Int String Bool -> Bool)
    quickCheck (functorCompose' :: Fun Bool Int -> Fun Int Int -> Three Int String Bool -> Bool)
    quickCheck (functorIdentity :: Three' Int Bool -> Bool)
    quickCheck (functorCompose' :: Fun Bool Int -> Fun Int Int -> Three' Int Bool -> Bool)
    quickCheck (functorIdentity :: Four Int String Bool Double -> Bool)
    quickCheck (functorCompose' :: Fun Double Int -> Fun Int Int -> Four Int String Bool Double -> Bool)
    quickCheck (functorIdentity :: Four' Int String -> Bool)
    quickCheck (functorCompose' :: Fun String Int -> Fun Int Int -> Four' Int String -> Bool)

-- | 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

-- | 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        return $ Pair a a'

-- | 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

-- | 4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

-- | 5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c

-- | 6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
        => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

-- | 7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        a'' <- arbitrary
        b <- arbitrary
        return $ Four' a a' a'' b

-- | 8
-- Nope. You can't implement it, because Functor instances require kind * -> *,
-- but Trivial has kind *.

