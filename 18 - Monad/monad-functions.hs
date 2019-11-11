module MonadFunctions where

-- | 1)
j :: Monad m => m (m a) -> m a
j a = a >>= id

-- | 2)
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

-- | 3)
-- | According to BoeingX, another possible solution: f <$> a <*> b
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = a >>= (\a' -> b >>= (return . f a'))

-- | 4)
a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- | 5)
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = (:) <$> (f x) <*> (meh xs f)

-- | 6)
flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id

