module BinaryTree where

data BinaryTree a
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- | Exercise 1
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a
  = case f a of
        Just (a', b, a'') -> Node (unfold f a') b (unfold f a'')
        Nothing           -> Leaf

-- | Exercise 2
treeBuild :: Integer -> BinaryTree Integer
treeBuild n
   = unfold (\a -> if a == n then Nothing else Just (a + 1, a, a + 1)) 0
