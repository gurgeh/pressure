import Criterion.Main
import Control.Monad.State

import Pressure

{--
g++ -O3 20M 650 ms
ghc -O2:
130614: 1M 234 ms
--}

--benchEncode :: Int -> [Word8]
benchEncode n =
  length $ concat $ evalState (encode $ take n $ cycle [SymbolFreq 1 2 4, SymbolFreq 3 6 10]) startRange

main :: IO ()
main = defaultMain [
  bgroup "encode" [ bench "0.1M" $ whnf benchEncode 100000
                  , bench "1M" $ whnf benchEncode 1000000
               ]
  ]