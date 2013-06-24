module Pressure where
import Data.Word
import Data.Bits
import Control.Monad.State

import BitPrec

-- import Data.ByteString.Lazy.Builder
--import qualified Data.ByteString.Lazy as LB

{--
Todo:
profiling
can I make code faster?
  bs builder?
  CPS in?


decode

unit tests
--}

type Precision = Word

kSpaceForByte :: Int
kSpaceForByte = fromIntegral $ (bitPrec::Precision) - 8
kHalfPrec :: Int
kHalfPrec = fromIntegral $ (bitPrec::Precision) `div` 2

kTop :: Precision
kTop = 1 `shift` kSpaceForByte

kBot :: Precision
kBot = 1 `shift` kHalfPrec

data SymbolFreq = SymbolFreq { cumFreq :: Precision,
                               freq :: Precision,
                               totFreq :: Precision }

data Range = Range { low :: Precision,
                     range :: Precision
                   }

startRange :: Range
startRange = Range 0 (-1::Precision)

encode :: [SymbolFreq] -> State Range [[Word8]]
encode sf = do
  e <- mapM encode1 sf
  r <- get
  return $ e ++ [map (fromIntegral . (`shiftR` kSpaceForByte)) $ take (kHalfPrec `div` 4) $ iterate (`shift` 8) (low r)]

encode1 :: SymbolFreq -> State Range [Word8]
encode1 (SymbolFreq cf f tf) =
  let loop = do
        r <- get
        let lx = low r `xor` (low r + range r) < kTop
        let r2 = if lx then range r else (-low r) .&. (kBot - 1)
        if lx || range r < kBot then (do
          put $ Range (low r `shift` 8) (r2 `shift` 8)
          rest <- loop
          return $ fromIntegral (low r `shiftR` kSpaceForByte):rest)
        else return []
  in do
    r <- get
    put $ Range (low r + cf * (range r `div` tf)) ((range r `div` tf) * f)
    loop
      
{--

//Likewise, this primes the class for decoding and should be followed by the first call to *GetFreq*.
void RangeCoder::StartDecode(){
  _passed = _low = _code = 0;
  _range = (precision)-1;
  
  for(int i = 0; i < sizeof(precision); i++)
    _code = _code << (sizeof(uc) * 8) | InByte();
}

//Decode the next symbol. Get the result with *GetFreq*. The arguments are the values for the last symbol
//received through GetFreq.
void RangeCoder::Decode(precision cumFreq, precision freq, precision totFreq){
  assert(cumFreq + freq <= totFreq && freq && totFreq <= kBot);
  _low += cumFreq * _range;
  _range *= freq;
  while((_low ^ _low + _range) < kTop or 
        _range < kBot and ((_range = -_low & kBot - 1), 1)){
    _code = _code << (sizeof(uc) * 8) | InByte();
    _range <<= sizeof(uc) * 8;
    _low <<= sizeof(uc) * 8;
  }
}

--}