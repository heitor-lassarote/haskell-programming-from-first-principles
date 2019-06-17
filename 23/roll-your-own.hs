import System.Random

data Die
    = DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
        1 -> DieOne
        2 -> DieTwo
        3 -> DieThree
        4 -> DieFour
        5 -> DieFive
        6 -> DieSix
        x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 n g
    where
        go :: Int -> Int -> Int -> StdGen -> Int
        go sum count n gen
            | sum >= n  = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) n nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 n [] g
    where
        go :: Int -> Int -> Int -> [Die] -> StdGen -> (Int, [Die])
        go sum count n dice gen
            | sum >= n  = (count, dice)
            | otherwise =
                let (dieNumber, nextGen) = randomR (1, 6) gen
                    die = intToDie dieNumber
                in go (sum + dieNumber) (count + 1) n (die : dice) nextGen

