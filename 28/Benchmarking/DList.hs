module DList where

import Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a] }

-- 1
empty :: DList a
empty = DL id
{-# INLINE empty #-}

-- 2
singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

-- 3
toList :: DList a -> [a]
toList (DL f) = f []

-- 4
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)
{-# INLINE cons #-}

-- 5
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL (unDL xs . (x :))
{-# INLINE snoc #-}

-- 6
append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) ([n] ++ xs)

constructDList :: Int -> [Int]
constructDList i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (singleton n `append` xs)

main :: IO ()
main = defaultMain
    [ bench "concat list"  $ whnf schlemiel 123456
    , bench "concat dlist" $ whnf constructDList 123456
    ]

