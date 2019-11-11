import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt (i, v) = (i + 1, v + 1)
boopIt (i, v) = (i * 2, v * 2)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

m' :: M.Map Int Int
m' = M.fromList $ take 10000 stream
  where stream = iterate boopIt (1, 1)

membersMap :: Int -> Bool
membersMap i = M.member i m

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

s' :: S.Set Int
s' = S.fromList $ take 10000 stream
  where stream = iterate (*2) 1

membersSet :: Int -> Bool
membersSet i = S.member i s

main :: IO ()
main = defaultMain
    [ bench "member check map" $ whnf membersMap 9999
    , bench "union map" $ whnf (M.union m) m'
    , bench "member check set" $ whnf membersSet 9999
    , bench "union set" $ whnf (S.union s) s'
    ]

