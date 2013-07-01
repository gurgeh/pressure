module Pressure where

import Control.Monad (foldM)
import Control.Monad.ST (runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Bits (xor, shiftL, shiftR, (.&.), (.|.))
import Foreign.Storable (sizeOf)
import Data.Word (Word8, Word32)

import Data.Vector.Generic.Base
import Data.Vector.Generic.Mutable
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

data SymbolFreq = SymbolFreq 
                  { _cumFreq :: {-# UNPACK #-} !Precision, 
                    _freq    :: {-# UNPACK #-} !Precision, 
                    _totFreq :: {-# UNPACK #-} !Precision }
                  
data EncStatus = EncStatus
    { _low   :: {-# UNPACK #-} !Precision
    , _range :: {-# UNPACK #-} !Precision
    , _n     :: {-# UNPACK #-} !Int }

data DecStatus = DecStatus
    { _low2   :: {-# UNPACK #-} !Precision
    , _range2 :: {-# UNPACK #-} !Precision
    , _code   :: {-# UNPACK #-} !Precision
    , _n2     :: {-# UNPACK #-} !Int }

{--
decode

unit testing

fix dynamic buffer size
--}

rangeCoder :: [SymbolFreq] -> (V.Vector Word8, Int)
rangeCoder freqs = runST $ do
    v  <- VM.replicate 1000000 (0 :: Word8)
    s' <- newSTRef $ EncStatus 0 (-1) 0
    let encode (SymbolFreq cf f tf) = do
            let loop low range n = do
                    let lx = low `xor` (low + range) < kTop
                    if lx || range < kBot
                        then do
                            let r2 = if lx then range else (-low) .&. (kBot - 1)
                            VM.unsafeWrite v n (fromIntegral (low `shiftR` kSpaceForByte))
                            let l8 = low `shiftL` 8
                                r8 = r2 `shiftL` 8
                            loop l8 r8 (n+1)
                        else writeSTRef s' (EncStatus low range n)
            EncStatus l0 r0 n0 <- readSTRef s'
            let ll = l0 + cf * (r0 `quot` tf)
                rl = (r0 `quot` tf) * f
            loop ll rl n0
    mapM_ encode freqs
    
    EncStatus l0 _ n0 <- readSTRef s' --finalize
    n1 <- foldM (\n low -> do
                    VM.unsafeWrite v n (fromIntegral (low `shiftR` kSpaceForByte))
                    return (n + 1))
          n0 $ Prelude.take (precisionBits `div` 8) $ iterate (`shiftL` 8) l0
    
    vf <- V.freeze v
    return (vf, n1)

rangeDecoder :: V.Vector Word8 -> Int -> [SymbolFreq]
rangeDecoder vec n =
  runST $ do
    s' <- newSTRef DecStatus 0 (-1) 0 0
    precode <- foldM ((.|.) ib . `shiftL` 8)

    let decode (SymbolFreq cf f tf) = do
            let loop low range n = do
                    let lx = low `xor` (low + range) < kTop
                    if lx || range < kBot
                        then do
                            let r2 = if lx then range else (-low) .&. (kBot - 1)
                            
                            let l8 = low `shiftL` 8
                                r8 = r2 `shiftL` 8
                                c8 = code `shiftL` 8 (.|.) ib
                            loop l8 r8 (n+1)
                        else writeSTRef s' (EncStatus low range n)
            EncStatus l0 r0 n0 <- readSTRef s'
            let ll = l0 + cf * r0
                rl = r0 * f
            loop ll rl n0
    mapM_ encode freqs




