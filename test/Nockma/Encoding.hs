module Nockma.Encoding where

import Base
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Juvix.Compiler.Nockma.Encoding.ByteString qualified as Encoding
import Test.Tasty.Hedgehog

propEncodingRoundtrip :: Property
propEncodingRoundtrip = property $ do
  -- The range must be greater than the chunkSize in `byteStringToIntegerLEChunked`
  bs <- forAll (Gen.bytes (Range.linear 0 3000))
  Encoding.decodeByteStringWithDefault (error "failed to decode") (Encoding.encodeByteString bs) === bs

allTests :: TestTree
allTests = testGroup "Nockma encoding" [testProperty "Roundtrip ByteArray to/from integer encoding" propEncodingRoundtrip]
