module MaybeAnotherMonoid where

import Data.Monoid
import Test.QuickCheck

import Optional

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a
    = First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        frequency [(1, return $ First' Nada), (1, return $ First' $ Only a)]

instance Monoid (First' a) where
    mempty = First' Nada
    mappend a@(First' (Only _)) _ = a
    mappend _ b@(First' (Only _)) = b
    mappend _ _ = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FirstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FirstId)
    quickCheck (monoidRightIdentity :: FirstId)

