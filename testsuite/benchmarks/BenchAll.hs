import Criterion.Main
import qualified Data.Vector.Unboxed as V

import Pressure

{--
g++ -O3 20M 650 ms
ghc -O2:
130624: 1M 234 ms (1M 4690 ms - pipes)
130626: 1M 95 ms Array
130626: 1M 58 ms No refs in inner loop
130627: 1M 40 ms Oddly optimal
--}

benchEncode :: Int -> Int
benchEncode n =
  snd $ (rangeCoder $ take n $ cycle [SymbolFreq 1 2 4, SymbolFreq 3 6 10])
  --snd $ rangeCoder $ V.generate n (\n2 -> if even n2 then (1, 2, 4) else (3, 6, 10))

main :: IO ()
main = defaultMain [
  bgroup "encode" [ bench "0.1M" $ whnf benchEncode 100000
                  , bench "1M" $ whnf benchEncode 1000000
               ]
  ]