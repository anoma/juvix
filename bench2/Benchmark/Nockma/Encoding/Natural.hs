module Benchmark.Nockma.Encoding.Natural where

import Data.Serialize qualified as Serial
import Juvix.Prelude
import Juvix.Prelude.Bytes
import System.Random
import Test.Tasty.Bench

randomNatural :: IO Natural
randomNatural = do
  sg <- getStdGen
  let numDigits :: Natural = 1000000
  return (fst (uniformR (10 ^ numDigits, 10 ^ numDigits + 10000) sg))

randomNaturalBS :: IO ByteString
randomNaturalBS = naturalToByteString <$> randomNatural

bm :: Benchmark
bm = bgroup "Natural Encoding" [byteStringToNatBm, natToByteStringBm]
  where
    byteStringToNatBm =
      let old = byteStringToNaturalOld
          new = byteStringToNatural
       in bgroup
            "ByteString -> Natural"
            [ env
                randomNaturalBS
                (\nat -> bench "Old" (nf old nat)),
              env
                randomNaturalBS
                (\nat -> bench "New" (nf new nat))
            ]

    natToByteStringBm =
      let shiftBased = padByteStringWord . naturalToByteStringOld
          wordBased = padByteStringWord . naturalToByteString
          byteBased = padByteStringWord . naturalToByteStringHelper True
       in bgroup
            "Natural -> ByteString"
            [ env
                randomNatural
                (\nat -> bench "Shift based" (nf shiftBased nat)),
              env
                randomNatural
                (\nat -> bench "Word64 based" (nf wordBased nat)),
              env
                randomNatural
                (\nat -> bench "Word8 based" (nf byteBased nat)),
              env
                randomNatural
                (\nat -> bench "Cereal library" (nf Serial.encode nat))
            ]
