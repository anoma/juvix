module Nockma.Encoding where

import Base
import Data.ByteString qualified as BS
import Hedgehog as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Juvix.Compiler.Nockma.Encoding.ByteString qualified as Encoding
import Juvix.Data.SHA256 qualified as SHA256
import Test.Tasty.Hedgehog

propEncodingRoundtrip :: Property
propEncodingRoundtrip = property $ do
  -- The range must be greater than the chunkSize in `byteStringToIntegerLEChunked`
  bs <- forAll (Gen.bytes (Range.linear 0 3000))
  Encoding.decodeByteStringWithDefault (error "failed to decode") (Encoding.encodeByteString bs) === bs

propSha256Length :: Property
propSha256Length = property $ do
  n <- forAll (Gen.integral (Range.linear (-1000000) 1000000))
  BS.length (SHA256.hashInteger n) === 64

propSha256IsSignSensitive :: Property
propSha256IsSignSensitive = property $ do
  n <- forAll (Gen.integral (Range.linear 1 1000000))
  let hashPos = SHA256.hashInteger n
  let hashNeg = SHA256.hashInteger (-n)
  hashPos /== hashNeg

propSha256HandlesLargeIntegers :: Property
propSha256HandlesLargeIntegers = property $ do
  n <- forAll (Gen.integral (Range.linear 1 1000000))
  let extendedInteger = n + (2 ^ (25 :: Integer))
  let hashOriginal = SHA256.hashInteger n
  let hashExtended = SHA256.hashInteger extendedInteger
  hashOriginal /== hashExtended

allTests :: TestTree
allTests =
  testGroup
    "Nockma encoding"
    [ testProperty "Roundtrip ByteArray to/from integer encoding" propEncodingRoundtrip,
      testGroup
        "Sha256"
        [ testProperty "hashInteger length" propSha256Length,
          testProperty "hashInteger is sign insensitive" propSha256IsSignSensitive,
          testProperty "hashInteger handles large integers" propSha256HandlesLargeIntegers
        ]
    ]
