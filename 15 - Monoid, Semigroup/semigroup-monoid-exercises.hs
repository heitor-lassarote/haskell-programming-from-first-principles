module SemigroupMonoidExercises where

import Data.Monoid (Monoid, mappend, mempty)
import Data.Semigroup (Semigroup, (<>))
import Test.QuickCheck (Arbitrary, CoArbitrary, oneof, arbitrary, quickCheck)

-- | Exercise 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- | Exercise 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity $ a <> b

instance Monoid a => Monoid (Identity a) where
    mempty = Identity $ mempty
    mappend (Identity a) (Identity b) = Identity $ a `mappend` b

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

type IdTest = Identity String
type IdAssoc = IdTest -> IdTest -> IdTest -> Bool

-- | Exercise 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty
    mappend (Two a b) (Two a' b') = Two (a `mappend` a') (b `mappend` b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

type TwoTest = Two String [Int]
type TwoAssoc = TwoTest -> TwoTest -> TwoTest -> Bool

-- | Exercise 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
        Semigroup (Three a b c) where
    Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')


instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty
    mappend (Three a b c) (Three a' b' c')
      = Three (a `mappend` a') (b `mappend` b') (c `mappend` c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
        Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

type ThreeTest = Three String [Int] (Either Double Integer)
type ThreeAssoc = ThreeTest -> ThreeTest -> ThreeTest -> Bool

-- | Exercise 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
        Semigroup (Four a b c d) where
    Four a b c d <> Four a' b' c' d'
      = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) =>
        Monoid (Four a b c d) where
    mempty = Four mempty mempty mempty mempty
    mappend (Four a b c d) (Four a' b' c' d')
      = Four (a `mappend` a') (b `mappend` b') (c `mappend` c') (d `mappend` d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
        Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d

type FourTest = Four [Int] String (Maybe [Integer]) (Either Int String)
type FourAssoc = FourTest -> FourTest -> FourTest -> Bool

-- | Exercise 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj $ a && b

instance Monoid BoolConj where
    mempty = BoolConj True
    mappend = (<>)

instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- | Exercise 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    BoolDisj a <> BoolDisj b = BoolDisj $ a || b

instance Monoid BoolDisj where
    mempty = BoolDisj False
    mappend = (<>)

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- arbitrary
        return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- | Exercise 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    Snd a <> _ = Snd a
    _ <> Snd b = Snd b
    _ <> Fst b = Fst b

{- instance Monoid a => Monoid (Or a b) where
    mempty = Fst mempty
    mappend = (<>) -}

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Fst a, return $ Snd b]

type OrTest = Or Int String
type OrAssoc = OrTest -> OrTest -> OrTest -> Bool

-- | Exercise 9
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Show (Combine a b) where
    show _ = "Combine"

instance Semigroup b => Semigroup (Combine a b) where
    Combine f <> Combine f' = Combine (\x -> (f x) <> (f' x))

instance Monoid b => Monoid (Combine a b) where
    mempty = Combine $ \_ -> mempty
    mappend (Combine f) (Combine f') = Combine $ \x -> f x `mappend` f' x

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = do
        f <- arbitrary
        return $ Combine f

type CombineTest = Combine String [Int]
type CombineAssoc = String -> CombineTest -> CombineTest -> CombineTest -> Bool

combineAssoc :: CombineAssoc
combineAssoc s a b c = unCombine (a <> (b <> c)) s == unCombine ((a <> b) <> c) s

combineLeftIdentity :: String -> CombineTest -> Bool
combineLeftIdentity s c = ((unCombine (mempty `mappend` c)) s) == ((unCombine c) s)

combineRightIdentity :: String -> CombineTest -> Bool
combineRightIdentity s c = ((unCombine (c `mappend` mempty)) s) == ((unCombine c) s)

-- | Exercise 10
newtype Comp a = Comp { unComp :: (a -> a) }

instance Show (Comp a) where
    show _ = "Comp"

instance Semigroup (Comp a) where
    Comp f <> Comp f' = Comp $ f . f'

instance Monoid (Comp a) where
    mempty = Comp id
    mappend (Comp f) (Comp f') = Comp $ f . f'

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = do
        f <- arbitrary
        return $ Comp f

type CompTest = Comp String
type CompAssoc = String -> CompTest -> CompTest -> CompTest -> Bool

compAssoc :: CompAssoc
compAssoc s a b c = unComp (a <> (b <> c)) s == unComp ((a <> b) <> c) s

compLeftIdentity :: String -> CompTest -> Bool
compLeftIdentity s c = ((unComp (mempty `mappend` c)) s) == ((unComp c) s)

compRightIdentity :: String -> CompTest -> Bool
compRightIdentity s c = ((unComp (c `mappend` mempty)) s) == ((unComp c) s)

-- | Exercise 11
data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    Success a <> _ = Success a
    _ <> Success b = Success b
    Failure a <> Failure b = Failure $ a <> b

instance Monoid a => Monoid (Validation a b) where
    mempty = Failure mempty
    mappend (Success a) _ = Success a
    mappend _ (Success b) = Success b
    mappend (Failure a) (Failure b) = Failure $ a `mappend` b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ Failure a, return $ Success b]

type ValidTest = Validation String Int
type ValidAssoc = ValidTest -> ValidTest -> ValidTest -> Bool

-- | Exercise 8 from Monoid
newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend (Mem f) (Mem f') = Mem $ \s ->
        let (a', s') = f s
            (a'', s'') = f' s'
        in (a' `mappend` a'', s'')

f' = Mem $ \s -> ("hi", s + 1)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

main :: IO ()
main = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
    quickCheck combineAssoc
    quickCheck compAssoc
    quickCheck (semigroupAssoc :: ValidAssoc)

    let failure :: String -> Validation String Int
        failure = Failure
        success :: Int -> Validation String Int
        success = Success
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2

    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    quickCheck (monoidLeftIdentity :: Two [Int] String -> Bool)
    quickCheck (monoidRightIdentity :: Two [Int] String -> Bool)
    quickCheck (monoidLeftIdentity :: Three [Int] String BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: Three [Int] String BoolConj -> Bool)
    quickCheck (monoidLeftIdentity :: Four [Int] String BoolConj BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: Four [Int] String BoolConj BoolDisj -> Bool)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
    -- quickCheck (monoidLeftIdentity :: Or String Int -> Bool)
    -- quickCheck (monoidRightIdentity :: Or String Int -> Bool)
    quickCheck combineLeftIdentity
    quickCheck combineRightIdentity
    quickCheck compLeftIdentity
    quickCheck compRightIdentity
    quickCheck (monoidLeftIdentity :: Validation String [Int] -> Bool)
    quickCheck (monoidRightIdentity :: Validation String [Int] -> Bool)

    -- | Exercise 8 from Monoid
    let rmzero = runMem mempty 0
        rmleft = runMem (f' `mappend` mempty) 0
        rmright = runMem (mempty `mappend` f') 0
    print $ (rmleft :: (String, Int))
    print $ (rmright :: (String, Int))
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0

