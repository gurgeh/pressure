import Criterion.Main
import Control.Monad.State
import Control.Proxy

import qualified Control.Proxy.Trans.Writer as W

import PPressure

{--
g++ -O3 20M 650 ms
ghc -O2:
130614: 1M 234 ms (1M 4690 ms - pipes)
--}

benchEncode :: Int -> Int
benchEncode n =
  --length $ concat $ evalState (encode $ take n $ cycle [SymbolFreq 1 2 4, SymbolFreq 3 6 10]) startRange
  getSum $ snd $ evalState (runProxy $ W.runWriterK $ (fromListS $ take n $ cycle [SymbolFreq 1 2 4, SymbolFreq 3 6 10]) >-> PPressure.encode1 >-> lengthD) startRange
  

main :: IO ()
main = defaultMain [
  bgroup "encode" [ bench "0.1M" $ whnf benchEncode 100000
                  , bench "1M" $ whnf benchEncode 1000000
               ]
  ]