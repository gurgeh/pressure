{-- Range encoding implemented with Pipes --}
module PPressure where


import Data.Word
import Data.Bits
import Control.Monad
import qualified Control.Monad.State as S
import Control.Proxy

import BitPrec

type Precision = Word

kSpaceForByte :: Int
kSpaceForByte = fromIntegral $ (bitPrec::Precision) - 8
kHalfPrec :: Int
kHalfPrec = fromIntegral $ (bitPrec::Precision) `div` 2

kTop :: Precision
kTop = 1 `shift` kSpaceForByte

kBot :: Precision
kBot = 1 `shift` kHalfPrec

data SymbolFreq = SymbolFreq !Precision !Precision !Precision

data Range = Range !Precision !Precision

startRange :: Range
startRange = Range 0 (-1::Precision)

encode1 :: Proxy p => () -> Pipe p SymbolFreq Word8 (S.State Range) r
encode1 () =
  let loop = do
        (Range low range) <- lift S.get
        let lx = low `xor` (low + range) < kTop
        let r2 = if lx then range else (-low) .&. (kBot - 1)
        when (lx || range < kBot) $ do
          lift $ S.put $ Range (low `shift` 8) (r2 `shift` 8)
          respond $ fromIntegral (low `shiftR` kSpaceForByte)
          loop
  in
   runIdentityP $ forever $ do
     (SymbolFreq cf f tf) <- request ()
     (Range low range) <- lift S.get
     lift $ S.put $ Range (low + cf * (range `div` tf)) ((range `div` tf) * f)
     loop

  -- ++ [map (fromIntegral . (`shiftR` kSpaceForByte)) $ take (kHalfPrec `div` 4) $ iterate (`shift` 8) low]

