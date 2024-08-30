module Benchmark.Nockma.Encoding.ByteString where

import Juvix.Compiler.Nockma.Encoding.ByteString qualified as Encoding
import Juvix.Prelude.Base
import System.Random
import Test.Tasty.Bench

randomBytes :: Int -> IO ByteString
randomBytes numBytes = do
  sg <- getStdGen
  return (fst (genByteString numBytes sg))

testBytesSize :: Int
testBytesSize = 250000

bm :: Benchmark
bm =
  bgroup
    "ByteString Encoding to/from integer"
    [ env
        (randomBytes testBytesSize)
        (\bs -> bench "encode bytes to integer" (nf Encoding.encodeByteString bs)),
      env
        (Encoding.encodeByteString <$> randomBytes testBytesSize)
        (\i -> bench "decode bytes from integer" (nf (Encoding.decodeByteStringWithDefault (error "failed to decode")) i))
    ]
