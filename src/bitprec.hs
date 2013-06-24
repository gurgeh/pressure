{-# LANGUAGE ScopedTypeVariables #-}
module BitPrec where
import Data.Bits

bitPrec :: forall a . (Bounded a, Integral a) => a
bitPrec =
  fromIntegral $ calcBits $ toInteger (maxBound::a)
  where
    calcBits = length . takeWhile (> 0) . iterate (`shiftR` 1)