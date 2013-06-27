import Criterion.Main
import Data.Array.Unboxed ((!))

import Pressure2
--import Pressure

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
  fromIntegral $ (rangeCoder $ take n $ cycle [SymbolFreq 1 2 4, SymbolFreq 3 6 10]) ! 5
  --length $ concat $ evalState (encode $ take n $ cycle [SymbolFreq 1 2 4, SymbolFreq 3 6 10]) startRange
  --getSum $ snd $ evalState (runProxy $ W.runWriterK $ (fromListS $ take n $ cycle [SymbolFreq 1 2 4, SymbolFreq 3 6 10]) >-> PPressure.encode1 >-> lengthD) startRange
  

main :: IO ()
main = defaultMain [
  bgroup "encode" [ bench "0.1M" $ whnf benchEncode 100000
                  , bench "1M" $ whnf benchEncode 1000000
               ]
  ]