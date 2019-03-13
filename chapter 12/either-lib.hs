module EitherLib where

-- | Exercise 1
isLeft' :: Either a b -> Bool
isLeft' (Left a) = True
isLeft' _        = False

lefts' :: [Either a b] -> [a]
lefts' xs
  = foldr ((:) . left') [] $ filter isLeft' $ xs
    where
        left' (Left  x) = x
        left' (Right _) = undefined
-- lefts' [] = []
-- lefts' (Left  x : xs) = x : lefts' xs
-- lefts' (Right _ : xs) =     lefts' xs

-- | Exercise 2
isRight' :: Either a b -> Bool
isRight' = not . isLeft'

rights' :: [Either a b] -> [b]
rights' xs
  = foldr ((:) . right') [] $ filter isRight' $ xs
    where
        right' (Left  _) = undefined
        right' (Right x) = x

-- | Exercise 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

-- | Exercise 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left  _) = Nothing
eitherMaybe' f (Right b) = Just $ f b

-- | Exercise 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  a) = f a
either' _ f (Right b) = f b

-- | Exercise 6
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\b -> Just $ f b)
