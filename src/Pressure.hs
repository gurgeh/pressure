{-# LANGUAGE BangPatterns #-}
module Pressure where
import Data.Word
import Data.Bits
--import Control.Monad.State
import Control.Monad
import Data.Monoid

import Control.Monad.ST
import Data.STRef

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Word

import BitPrec

import qualified Data.ByteString as B

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

data SymbolFreq = SymbolFreq Precision Precision Precision

data Range = Range !Precision !Precision

startRange :: Range
startRange = Range 0 (-1::Precision)

encode :: [SymbolFreq] -> B.ByteString
encode sf =
  toByteString $ fromWriteList id $ runST $ do
    low' <- newSTRef (0::Precision)
    range' <- newSTRef (-1::Precision)
    let encode1 (SymbolFreq cf f tf) =
          let loop = do
                low <- readSTRef low'
                range <- readSTRef range'
                let !lx = low `xor` (low + range) < kTop
                let !r2 = if lx then range else (-low) .&. (kBot - 1)
                if lx || range < kBot then
                  do
                    let l8 = (low `shift` 8)
                    let r8 = (r2 `shift` 8)
                    writeSTRef low' l8
                    writeSTRef range' r8
                    let w = writeWord8 (fromIntegral (low `shiftR` kSpaceForByte))
                    l <- loop
                    return $ w `mappend` l
                else return mempty
          in do
           range <- readSTRef range'
           low <- readSTRef low'
           let ll = low + cf * (range `div` tf)
           let rl = (range `div` tf) * f
           writeSTRef low' ll
           writeSTRef range' rl
           loop
    mapM encode1 sf
    
 




{--
encode :: [SymbolFreq] -> Write Word8
encode sf = 
  runST $ do
    low' <- newSTRef (0::Precision)
    range' <- newSTRef (-1::Precision)
    let encode1 (SymbolFreq cf f tf) =
          let loop = do
                low <- readSTRef low'
                range <- readSTRef range'
                let lx = low `xor` (low + range) < kTop
                let r2 = if lx then range else (-low) .&. (kBot - 1)
                if lx || range < kBot then (
                  do
                    modifySTRef low' (`shift` 8)
                    writeSTRef range' (r2 `shift` 8)
                    --rest <- loop
                    --let rest = []
                    return $! [(fromIntegral $! (low `seq` low `shiftR` kSpaceForByte))])
                else return []
          in do
           range <- readSTRef range'
           modifySTRef low' (\x -> x + cf * (range `div` tf))
           writeSTRef range' ((range `div` tf) * f)
           loop
    e <- forM sf encode1
    return $ e -- ++ [map (fromIntegral . (`shiftR` kSpaceForByte)) $ take (kHalfPrec `div` 4) $ iterate (`shift` 8) low]


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
   
--}

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