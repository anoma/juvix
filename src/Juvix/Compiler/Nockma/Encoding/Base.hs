module Juvix.Compiler.Nockma.Encoding.Base where

import Data.Bit as Bit
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Juvix.Prelude.Base
import VectorBuilder.Builder as Builder
import VectorBuilder.Vector

integerToVectorBits :: Integer -> Bit.Vector Bit
integerToVectorBits = build . integerToBuilder

integerToBuilder :: (Integral a) => a -> Builder Bit
integerToBuilder x
  | x < 0 = error "integerToVectorBits: negative integers are not supported in this implementation"
  | otherwise = unfoldBits (fromIntegral x)
  where
    unfoldBits :: Integer -> Builder Bit
    unfoldBits n
      | n == 0 = Builder.empty
      | otherwise = Builder.singleton (Bit (testBit n 0)) <> unfoldBits (n `shiftR` 1)

bitLength :: forall a. (Integral a) => a -> Int
bitLength = \case
  0 -> 0
  n -> go (fromIntegral n) 0
    where
      go :: Integer -> Int -> Int
      go 0 acc = acc
      go x acc = go (x `shiftR` 1) (acc + 1)

vectorBitsToInteger :: Bit.Vector Bit -> Integer
vectorBitsToInteger = U.ifoldl' go 0
  where
    go :: Integer -> Int -> Bit -> Integer
    go acc idx (Bit b)
      | b = setBit acc idx
      | otherwise = acc

safeNaturalToInt :: Natural -> Maybe Int
safeNaturalToInt n
  | n > fromIntegral (maxBound :: Int) = Nothing
  | otherwise = Just (fromIntegral n)
