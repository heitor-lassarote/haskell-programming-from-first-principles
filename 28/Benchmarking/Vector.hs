import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

v :: V.Vector Int
v = V.replicate 65536 0

uv :: UV.Vector Int
uv = UV.replicate 65536 0

main :: IO ()
main = defaultMain
    [ bench "map over boxed vector"   $ whnf (V.map  (+ 1)) v
    , bench "map over unboxed vector" $ whnf (UV.map (+ 1)) uv
    ]

