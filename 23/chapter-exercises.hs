{-# LANGUAGE InstanceSigs #-}

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State sas) = State $ \s -> let (a, s') = sas s in (f a, s')

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State $ \s -> (a, s)

    (<*>) :: State s (a -> b) -> State s a -> State s b
    State f <*> State sas = State $ \s ->
        let (f', s') = f s
            (a, s'') = sas s'
        in (f' a, s'')

instance Monad (State s) where
    return :: a -> State s a
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    State f >>= g = State $ \s -> let (a, s') = f s in (runState $ g a) s'

-- | 1)
get :: State s s
get = State $ \s -> (s, s)

-- | 2)
put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- | 3)
exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s


-- | 4)
eval :: State s a -> s -> a
eval (State sa) = fst . sa

-- | 5)
modify :: (s -> s) -> State s ()
modify ss = State $ \s -> ((), ss s)

