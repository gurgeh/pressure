module Pressure2 where
--import Control.Monad.Trans.State.Strict
import Data.Array.ST (runSTUArray, newArray, writeArray)
import Data.Array.Unboxed (UArray)
--import Control.Monad.ST
import Control.Monad (when, foldM)
import Data.STRef
import Data.Bits (xor, shiftL, shiftR, (.&.))
import Foreign.Storable (sizeOf)
import Data.Word (Word8, Word32)

type Precision = Word32
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
    , _range :: {-# UNPACK #-} !Precision
    , _n     :: {-# UNPACK #-} !Int }

type CumFreq = Precision
type Freq    = Precision
type TotFreq = Precision

--finalize data
--return n, too
--decode

rangeCoder :: [SymbolFreq] -> UArray Int Word8
rangeCoder freqs = runSTUArray $ do
  a <- newArray (0,1000000::Int) (0::Word8)
  s' <- newSTRef $ Status 0 (-1) 0
  let 
    encode (SymbolFreq cf f tf) = 
        let loop low range n =
              do
              let lx = low `xor` (low + range) < kTop
              if (lx || range < kBot) then do
                  let r2 = if lx then range else (-low) .&. (kBot - 1)
                  writeArray a n (fromIntegral (low `shiftR` kSpaceForByte))
                  let l8 = (low `shiftL` 8)
                  let r8 = (r2 `shiftL` 8)
                  loop l8 r8 (n+1)
              else return $ Status low range n
                  
        in do
          Status l0 r0 n0 <- readSTRef s'
          let ll = l0 + cf * (r0 `div` tf)
          let rl = (r0 `div` tf) * f
          s <- loop ll rl n0
          writeSTRef s' s
          
          
  mapM_ encode freqs
  return a

