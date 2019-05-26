module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import TraversableInstances

-- | Identity
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

-- | Constant
instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = do
        a <- arbitrary
        return $ Constant a

instance Eq a => EqProp (Constant a b) where
    Constant a =-= Constant a' = a `eq` a'

-- | Maybe
instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        elements [Yep a, Nada]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

-- | List
instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        as <- arbitrary
        elements [Nil, Cons a as]

instance Eq a => EqProp (List a) where
    (=-=) = eq

-- | Three
instance (Arbitrary a, Arbitrary b, Arbitrary c)
        => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-- | Pair
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq

-- | Big
instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return $ Big a b b'

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq

-- | Bigger
instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        b'' <- arbitrary
        return $ Bigger a b b' b''

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq

-- | S
instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = do
        n <- arbitrary
        a <- arbitrary
        return $ S n a

instance (Eq (n a), Eq a) => EqProp (S n a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = do
        a <- arbitrary
        t <- arbitrary
        t' <- arbitrary
        elements [Empty, Leaf a, Node t a t']

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

type T1 = (Int, Int, [Int])
type T2 = (String, String, String)
type T3 = (Maybe Int, Int, [Int])

main :: IO ()
main = do
    putStr "\nIdentity"
    quickBatch $ traversable (undefined :: Identity T1)

    putStr "\nConstant"
    quickBatch $ traversable (undefined :: Constant T1 T2)

    putStr "\nOptional"
    quickBatch $ traversable (undefined :: Optional T1)

    putStr "\nList"
    quickBatch $ traversable (undefined :: List T1)

    putStr "\nThree"
    quickBatch $ traversable (undefined :: Three T1 T2 T3)

    putStr "\nS"
    quickBatch $ traversable (undefined :: S Optional T1)

    putStr "\nTree"
    quickBatch $ traversable (undefined :: Tree T1)

