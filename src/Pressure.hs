module Pressure (rangeCoder) where

import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Bits (xor, shiftL, shiftR, (.&.))
import Foreign.Storable (sizeOf)
import Data.Word (Word8, Word32)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

type Precision = Word32

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

type SymbolFreq = (Precision, Precision, Precision)

--    { _cumFreq :: {-# UNPACK #-} !Precision, 
--      _freq    :: {-# UNPACK #-} !Precision, 
--      _totFreq :: {-# UNPACK #-} !Precision }

data Status = Status
    { _low   :: {-# UNPACK #-} !Precision
    , _range :: {-# UNPACK #-} !Precision
    , _n     :: {-# UNPACK #-} !Int }

{--
Change list to vector in
finalize

decode
unit testing
--}

rangeCoder :: V.Vector SymbolFreq -> (V.Vector Word8, Int)
rangeCoder freqs = runST $ do
    v  <- VM.replicate 1000000 (0 :: Word8)
    s' <- newSTRef $ Status 0 (-1) 0
    let encode (cf, f, tf) = do
            let loop low range n = do
                    let lx = low `xor` (low + range) < kTop
                    if lx || range < kBot
                        then do
                            let r2 = if lx then range else (-low) .&. (kBot - 1)
                            VM.unsafeWrite v n (fromIntegral (low `shiftR` kSpaceForByte))
                            let l8 = low `shiftL` 8
                                r8 = r2 `shiftL` 8
                            loop l8 r8 (n+1)
                        else writeSTRef s' (Status low range n)
            Status l0 r0 n0 <- readSTRef s'
            let ll = l0 + cf * (r0 `quot` tf)
                rl = (r0 `quot` tf) * f
            loop ll rl n0
    V.mapM_ encode freqs
    vf <- V.freeze v
    s <- readSTRef s'
    return (vf, _n s)

{--
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

  Status l0 _ n0 <- readSTRef s' --finalize
  foldM_(\n low -> do
      writeArray a n (fromIntegral (low `shiftR` kSpaceForByte))
      return (n + 1)) 
        n0 $ take (kHalfPrec `div` 4) $ iterate (`shiftL` 8) l0
    
  return a
--}
