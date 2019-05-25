module MaybeLib where

-- | Exercise 1
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- | Exercise 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _    Nothing  = b
mayybee _ aToB (Just a) = aToB a

-- | Exercise 3
fromMaybe :: a -> Maybe a -> a
fromMaybe a m = mayybee a id m

-- | Exercise 4
listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (a : _) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]

-- | Exercise 5
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x  : xs) = x : catMaybes xs
catMaybes (Nothing : xs) =     catMaybes xs

-- | Exercise 6
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
  = flipMaybe' [] xs
    where
        flipMaybe' acc [] = Just $ reverse acc
        flipMaybe' acc (Just x  : xs) = flipMaybe' (x : acc) xs
        flipMaybe' _   (Nothing : _ ) = Nothing
