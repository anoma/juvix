module Benchmark.Nockma.Encoding.Natural where

import Juvix.Prelude
import Juvix.Prelude.Bytes
import System.Random
import Test.Tasty.Bench

randomNatural :: IO Natural
randomNatural = do
  sg <- getStdGen
  let numDigits :: Natural = 1000000
  return (fst (uniformR (10 ^ numDigits, 10 ^ numDigits + 100) sg))

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
                (\nat -> bench "New" (nf new nat)),
              env
                randomNaturalBS
                ( \nat ->
                    bench
                      "TEST"
                      ( nf
                          ( \i ->
                              if new i == old i
                                then True
                                else error "wrong"
                          )
                          nat
                      )
                )
            ]

    natToByteStringBm =
      let old = padByteStringWord . naturalToByteStringOld
          new = padByteStringWord . naturalToByteString
          new2 = padByteStringWord . naturalToByteStringTest False
       in bgroup
            "Natural -> ByteString"
            [ env
                randomNatural
                (\nat -> bench "Old" (nf old nat)),
              env
                randomNatural
                (\nat -> bench "New" (nf new nat)),
              env
                randomNatural
                (\nat -> bench "New2" (nf new2 nat)),
              env
                randomNatural
                ( \nat ->
                    bench
                      "TEST"
                      ( nf
                          ( \i ->
                              if new i == new2 i
                                then True
                                else error "wrong"
                          )
                          nat
                      )
                )
            ]
