{-# LANGUAGE BangPatterns #-}
module Pressure where
import Data.Word
import Data.Bits
import Control.Monad.State

import BitPrec

-- import Data.ByteString.Lazy.Builder
--import qualified Data.ByteString.Lazy as LB

{--
Todo:
fix space leak

try FFI to C function?

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

data SymbolFreq = SymbolFreq !Precision !Precision !Precision

data Range = Range !Precision !Precision

startRange :: Range
startRange = Range 0 (-1::Precision)

encode :: [SymbolFreq] -> State Range [[Word8]]
encode sf = do
  e <- mapM encode1 sf
  Range low _ <- get
  return $ e -- ++ [map (fromIntegral . (`shiftR` kSpaceForByte)) $ take (kHalfPrec `div` 4) $ iterate (`shift` 8) low]

encode1 :: SymbolFreq -> State Range [Word8]
encode1 (SymbolFreq cf f tf) =
  let loop = do
        (Range low range) <- get
        let lx = low `xor` (low + range) < kTop
        let r2 = if lx then range else (-low) .&. (kBot - 1)
        if lx || range < kBot then (do
          put $ Range (low `shift` 8) (r2 `shift` 8)
          rest <- loop
          return $! fromIntegral (low `shiftR` kSpaceForByte):rest)
        else return []
  in do
    (Range low range) <- get
    put $ Range (low + cf * (range `div` tf)) ((range `div` tf) * f)
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