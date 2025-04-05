module Benchmark.Nockma.Encoding.Natural where

import Juvix.Prelude
import System.Random
import Test.Tasty.Bench

randomNatural :: IO Natural
randomNatural = do
  sg <- getStdGen
  let numDigitsMin = 100000
      numDigitsMax = 1000000
  return (fst (uniformR (10 ^ numDigitsMin, 10 ^ numDigitsMax) sg))

bm :: Benchmark
bm =
  let old = padByteString8 . naturalToByteStringOld
      new = padByteString8 . naturalToByteString
   in bgroup
        "Natural -> ByteString"
        [ env
            (randomNatural)
            (\nat -> bench "Old" (nf old nat)),
          env
            (randomNatural)
            (\nat -> bench "New" (nf new nat))
          -- env
          --   (randomNatural)
          --   ( \nat ->
          --       bench
          --         "TEST"
          --         ( nf
          --             ( \i ->
          --                 if old i == new i
          --                   then True
          --                   else error "wrong"
          --             )
          --             nat
          --         )
          --   )
        ]
