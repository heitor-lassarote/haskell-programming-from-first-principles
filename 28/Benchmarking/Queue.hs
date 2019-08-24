module Queue where

import Criterion.Main
import Data.Maybe (fromJust)
import qualified Data.Sequence as S

data Queue a = Queue
    { enqueue :: [a]
    , dequeue :: [a]
    } deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push x xs = Queue (x : enqueue xs) (dequeue xs)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue es []) = Just (head res, Queue [] (tail res))
  where res = reverse es
pop (Queue es ds) = Just (head ds, Queue es (tail ds))

data Queue' a = Queue' { queue' :: [a] } deriving (Eq, Show)

empty' :: Queue' a
empty' = Queue' []

push' :: a -> Queue' a -> Queue' a
push' x xs = Queue' (x : queue' xs)

pop' :: Queue' a -> Maybe (a, Queue' a)
pop' (Queue' []) = Nothing
pop' (Queue' xs) = Just (head rxs, Queue' $ reverse $ tail rxs)
  where rxs = reverse xs

benchQueue :: Int -> Queue ()
benchQueue i = go i (push () $ push () empty)
  where
    go 0 q = q
    go n q = go (n - 1) (push () $ snd $ fromJust $ pop q)

benchQueue' :: Int -> Queue' ()
benchQueue' i = go i (push' () $ push' () empty')
  where
    go 0 q = q
    go n q = go (n - 1) (push' () $ snd $ fromJust $ pop' q)

benchSeq :: Int -> S.Seq ()
benchSeq i = go i (S.fromList [(), ()])
  where
    go 0 s = s
    go n s = go (n - 1) (S.insertAt 0 () $ S.deleteAt (S.length s - 1) s)

main :: IO ()
main = defaultMain
    [ bench "queue"  $ whnf benchQueue  1234567
    , bench "queue'" $ whnf benchQueue' 1234567
    , bench "seq"    $ whnf benchSeq    1234567
    ]

