module Juvix.Compiler.Nockma.Encoding.Base where

import Data.Bit as Bit
import Data.Bits
import Data.Vector.Unboxed qualified as U
import Juvix.Compiler.Nockma.Encoding.Effect.BitReader
import Juvix.Compiler.Nockma.Encoding.Effect.BitWriter
import Juvix.Prelude.Base

-- | Binary encode an integer to a vector of bits, ordered from least to most significant bits.
-- NB: 0 is encoded as the empty bit vector is specified by the Hoon serialization spec
writeIntegral :: forall a r. (Integral a, Member BitWriter r) => a -> Sem r ()
writeIntegral x
  | x < 0 = error "integerToVectorBits: negative integers are not supported in this implementation"
  | otherwise = unfoldBits (fromIntegral x)
  where
    unfoldBits :: Integer -> Sem r ()
    unfoldBits n
      | n == 0 = return ()
      | otherwise = writeBit (Bit (testBit n 0)) <> unfoldBits (n `shiftR` 1)

integerToVectorBits :: (Integral a) => a -> Bit.Vector Bit
integerToVectorBits = run . execBitWriter . writeIntegral

-- | Computes the number of bits required to store the argument in binary
-- NB: 0 is encoded to the empty bit vector (as specified by the Hoon serialization spec), so 0 has bit length 0.
bitLength :: forall a. (Integral a) => a -> Int
bitLength =
  length
    . takeWhile (/= 0)
    . iterate (`shiftR` 1)
    . toInteger

-- | Decode a vector of bits (ordered from least to most significant bits) to an integer
vectorBitsToInteger :: Bit.Vector Bit -> Integer
vectorBitsToInteger = U.ifoldl' go 0
  where
    go :: Integer -> Int -> Bit -> Integer
    go acc idx (Bit b)
      | b = setBit acc idx
      | otherwise = acc

-- | Transform a Natural to an Int, computes Nothing if the Natural does not fit in an Int
safeNaturalToInt :: Natural -> Maybe Int
safeNaturalToInt n
  | n > fromIntegral (maxBound :: Int) = Nothing
  | otherwise = Just (fromIntegral n)

-- | Write the binary encoding of argument interpreted as a length to the output
writeLength :: forall r. (Member BitWriter r) => Int -> Sem r ()
writeLength len = do
  let lenOfLen = finiteBitSize len - countLeadingZeros len
  replicateM_ lenOfLen writeZero
  writeOne
  unless (lenOfLen == 0) (go len)
  where
    go :: Int -> Sem r ()
    -- Exclude the most significant bit of the length
    go l = unless (l == 1) $ do
      writeBit (Bit (testBit l 0))
      go (l `shiftR` 1)

-- | Consume the encoded length from the input bits
consumeLength :: forall r. (Members '[BitReader, Error BitReadError] r) => Sem r Int
consumeLength = do
  lenOfLen <- countBitsUntilOne
  if
    | lenOfLen == 0 -> return 0
    | otherwise -> do
        -- The most significant bit of the length is omitted
        let lenBits = lenOfLen - 1
        foldlM go (bit lenBits) [0 .. lenBits - 1]
  where
    go :: Int -> Int -> Sem r Int
    go acc n = do
      Bit b <- nextBit
      return
        $ if
          | b -> setBit acc n
          | otherwise -> acc
