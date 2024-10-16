module Juvix.Data.SHA256 where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Bits
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BL
import Data.ByteString.Lazy qualified as BL (toStrict)
import Juvix.Prelude

digestText :: Text -> Text
digestText =
  decodeUtf8Lenient
    . Base16.encode
    . SHA256.hash
    . encodeUtf8

-- | Create a HEX encoded, SHA256 digest of the contents of a file.
digestFile :: (Member Files r) => Path Abs File -> Sem r Text
digestFile = fmap (decodeUtf8Lenient . Base16.encode . SHA256.hash) . readFileBS'

integerToByteString :: Integer -> ByteString
integerToByteString = BL.toStrict . BL.toLazyByteString . go
  where
    go :: Integer -> BL.Builder
    go n = case compare n 0 of
      EQ -> BL.word64BE 0
      -- prefix a sign byte so that positive and negative integers with the same
      -- absolute value have different ByteString representations
      GT -> BL.word8 0 <> buildChunks n
      LT -> BL.word8 1 <> buildChunks (abs n)

    buildChunks :: Integer -> BL.Builder
    buildChunks n
      | n == 0 = mempty
      | otherwise = buildChunks (n `shiftR` 64) <> BL.word64BE (fromIntegral (n .&. maxWord64))

    maxWord64 :: Integer
    maxWord64 = fromIntegral (maxBound @Word64)

hashInteger :: Integer -> ByteString
hashInteger =
  Base16.encode
    . SHA256.hash
    . integerToByteString
