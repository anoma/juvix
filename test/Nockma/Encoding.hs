module Nockma.Encoding where

import Base
import Data.ByteString qualified as BS
import Hedgehog as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Juvix.Compiler.Nockma.Encoding.ByteString qualified as Encoding
import Test.Tasty.Hedgehog

propEncodingRoundtrip :: Property
propEncodingRoundtrip = property $ do
  -- The range must be greater than the chunkSize in `byteStringToIntegerLEChunked`
  bs <- forAll (Gen.bytes (Range.linear 0 3000))
  Encoding.decodeByteStringWithDefault (error "failed to decode") (Encoding.encodeByteString bs) === bs

propSha256Length :: Property
propSha256Length = property $ do
  n <- forAll (Gen.integral (Range.linear 0 1000000))
  BS.length (Encoding.sha256Natural n) === 32

propSha256HandlesLargeIntegers :: Property
propSha256HandlesLargeIntegers = property $ do
  n <- forAll (Gen.integral (Range.linear 0 1000000))
  let extendedInteger = n + (2 ^ (25 :: Integer))
  let hashOriginal = Encoding.sha256Natural n
  let hashExtended = Encoding.sha256Natural extendedInteger
  hashOriginal /== hashExtended

allTests :: TestTree
allTests =
  testGroup
    "Nockma encoding"
    [ testProperty "Roundtrip ByteArray to/from integer encoding" propEncodingRoundtrip,
      testGroup
        "Sha256"
        [ testProperty "hashInteger length" propSha256Length,
          testProperty "hashInteger handles large integers" propSha256HandlesLargeIntegers
        ]
    ]
