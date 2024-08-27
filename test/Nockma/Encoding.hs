module Nockma.Encoding where

import Base
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Juvix.Compiler.Nockma.Encoding.ByteString qualified as Encoding
import Test.Tasty.Hedgehog

propEncodingRoundtrip :: Property
propEncodingRoundtrip = property $ do
  bs <- forAll (Gen.bytes (Range.linear 0 1000))
  Encoding.decodeByteString (Encoding.encodeByteString bs) === bs

allTests :: TestTree
allTests = testGroup "Nockma encoding" [testProperty "Roundtrip ByteArray to/from integer encoding" propEncodingRoundtrip]
