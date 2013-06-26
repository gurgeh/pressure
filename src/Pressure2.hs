module Pressure2 where
--import Control.Monad.Trans.State.Strict
import Data.Array.ST (runSTUArray, newArray, writeArray)
import Data.Array.Unboxed (UArray)
--import Control.Monad.ST
import Control.Monad (when)
import Data.STRef
import Data.Bits (xor, shiftL, shiftR, (.&.))
import Foreign.Storable (sizeOf)
import Data.Word (Word8, Word64)

type Precision = Word64
type UC        = Word8

ucBits :: Int
ucBits = sizeOf (undefined :: UC) * 8

precisionBits :: Int
precisionBits = sizeOf (undefined :: Precision) * 8

kSpaceForByte :: Int
kSpaceForByte = precisionBits - 8

kHalfPrec :: Int
kHalfPrec = precisionBits `div` 2

kTop :: Precision
kTop = 1 `shiftL` kSpaceForByte

kBot :: Precision
kBot = 1 `shiftL` kHalfPrec

data SymbolFreq = SymbolFreq 
    { _cumFreq :: {-# UNPACK #-} !Precision, 
      _freq    :: {-# UNPACK #-} !Precision, 
      _totFreq :: {-# UNPACK #-} !Precision }

data Status = Status
    { _low   :: {-# UNPACK #-} !Precision
    , _range :: {-# UNPACK #-} !Precision }

init :: Status
init = Status { _low = 0, _range = -1 }

type CumFreq = Precision
type Freq    = Precision
type TotFreq = Precision

--try STURef
--try fun instead of STRef

rangeCoder :: [SymbolFreq] -> UArray Int Word8
rangeCoder freqs = runSTUArray $ do
  -- assert (cumFreq + freq <= totFreq && freq /= 0 && totFreq <= kBot)
  a <- newArray (0,1000000::Int) (0::Word8)
  n' <- newSTRef (0::Int)
  low' <- newSTRef (0::Precision)
  range' <- newSTRef (-1::Precision)
  let encode (SymbolFreq cf f tf) = 
        let loop =
              do
              low <- readSTRef low'
              range <- readSTRef range'
              let lx = low `xor` (low + range) < kTop
              when (lx || range < kBot) $ do
                  let r2 = if lx then range else (-low) .&. (kBot - 1)
                  n <- readSTRef n'
                  writeArray a n (fromIntegral (low `shiftR` kSpaceForByte))
                  modifySTRef n' (+1)
                  let l8 = (low `shiftL` 8)
                  let r8 = (r2 `shiftL` 8)
                  writeSTRef low' l8
                  writeSTRef range' r8
                  loop
        in do
          range <- readSTRef range'
          low <- readSTRef low'
          let ll = low + cf * (range `div` tf)
          let rl = (range `div` tf) * f
          writeSTRef low' ll
          writeSTRef range' rl
          loop 
  mapM_ encode freqs
  return a

--loop :: Precision -> Precision -> STUArray Word8
