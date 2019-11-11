module ArithmeticTests where

import Data.List (sort)

import Test.QuickCheck

import Arithmetic

-- | 1
prop_twiceHalfIsId :: Double -> Bool
prop_twiceHalfIsId x = halfIdentity x == x

-- | 2
prop_sortSortsList :: [Int] -> Bool
prop_sortSortsList xs = listOrdered $ sort xs

-- | 3
prop_plusAssociative :: Int -> Int -> Int -> Bool
prop_plusAssociative x y z = plusAssociative x y z

prop_plusCommutative :: Int -> Int -> Bool
prop_plusCommutative x y = plusCommutative x y

-- | For real, CBA to do these tests...

main :: IO ()
main = do quickCheck prop_twiceHalfIsId
          quickCheck prop_sortSortsList
          quickCheck prop_plusAssociative
          quickCheck prop_plusCommutative

