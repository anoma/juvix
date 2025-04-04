module Juvix.Compiler.Nockma.Encoding.ByteString where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Bit (Bit)
import Data.Bit qualified as Bit
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Builder qualified as BS
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Encoding.Effect.BitReader
import Juvix.Compiler.Nockma.Encoding.Effect.BitWriter
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base

-- | Encode an atom to little-endian bytes
atomToByteString :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r ByteString
atomToByteString = fmap naturalToByteString . nockNatural

-- | Encode an atom to little-endian bytes, padded with zeros up to a specified length
atomToByteStringLen :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => Int -> Atom a -> Sem r ByteString
atomToByteStringLen len = fmap (padByteString len) . atomToByteString

sha256Atom :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r ByteString
sha256Atom = fmap sha256Natural . nockNatural

byteStringToAtom :: forall a r. (NockNatural a, Member (Error (ErrNockNatural a)) r) => ByteString -> Sem r (Atom a)
byteStringToAtom = fmap mkEmptyAtom . fromNatural . byteStringToNatural

byteStringToNatural :: ByteString -> Natural
byteStringToNatural = fromInteger . byteStringToIntegerLE

naturalToByteString :: Natural -> ByteString
naturalToByteString = naturalToByteStringLE

naturalToBase64 :: Natural -> Text
naturalToBase64 = decodeUtf8 . Base64.encode . naturalToByteString

byteStringToIntegerLE :: ByteString -> Integer
byteStringToIntegerLE = byteStringToIntegerLEChunked

byteStringToIntegerLEChunked :: ByteString -> Integer
byteStringToIntegerLEChunked = foldr' go 0 . map (first byteStringChunkToInteger) . chunkByteString
  where
    chunkSize :: Int
    chunkSize = 1024

    go :: (Integer, Int) -> Integer -> Integer
    go (i, size) acc = acc `shiftL` (8 * size) .|. i

    chunkByteString :: ByteString -> [(ByteString, Int)]
    chunkByteString bs
      | BS.null bs = []
      | otherwise =
          let (chunk, rest) = BS.splitAt chunkSize bs
           in (chunk, BS.length chunk) : chunkByteString rest

    byteStringChunkToInteger :: ByteString -> Integer
    byteStringChunkToInteger = BS.foldr' (\b acc -> acc `shiftL` 8 .|. fromIntegral b) 0

-- | TODO: this is quadratic (`shiftR` is O(n))
naturalToByteStringLE :: Natural -> ByteString
naturalToByteStringLE = BS.toStrict . BS.toLazyByteString . go
  where
    go :: Natural -> BS.Builder
    go = \case
      0 -> mempty
      n -> BS.word8 (fromIntegral n) <> go (n `shiftR` 8)

naturalToByteStringLELen :: Int -> Natural -> ByteString
naturalToByteStringLELen len = padByteString len . naturalToByteStringLE

textToNatural :: Text -> Natural
textToNatural = byteStringToNatural . encodeUtf8

atomToText :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r Text
atomToText = fmap decodeUtf8Lenient . atomToByteString

-- | Construct an atom formed by concatenating the bits of two atoms, where each atom represents a sequence of bytes
atomConcatenateBytes :: forall a r. (NockNatural a, Member (Error (ErrNockNatural a)) r) => Atom a -> Atom a -> Sem r (Atom a)
atomConcatenateBytes l r = do
  lBs <- atomToByteString l
  rBs <- atomToByteString r
  byteStringToAtom (lBs <> rBs)

mkEmptyAtom :: a -> Atom a
mkEmptyAtom x =
  Atom
    { _atomInfo = emptyAtomInfo,
      _atom = x
    }

-- | Pad a ByteString with zeros up to a specified length
padByteString :: Int -> ByteString -> ByteString
padByteString n bs
  | BS.length bs >= n = bs
  | otherwise = BS.append bs (BS.replicate (n - BS.length bs) 0)

vectorBitsToInteger :: Bit.Vector Bit -> Integer
vectorBitsToInteger = byteStringToIntegerLEChunked . vectorBitsToByteString

-- | encode a ByteString to an Integer with its length as part of the encoding.
encodeByteString :: ByteString -> Integer
encodeByteString = vectorBitsToInteger . run . execBitWriter . go
  where
    go :: ByteString -> Sem (BitWriter ': r) ()
    go bs = do
      let len = BS.length bs
      writeLength len
      writeByteString bs

-- | decode a ByteString that was encoded using `encodeByteString`
decodeByteString :: forall r. (Member (Error BitReadError) r) => Integer -> Sem r ByteString
decodeByteString i = evalBitReader (integerToVectorBits i) go
  where
    go :: Sem (BitReader ': r) ByteString
    go = do
      len <- consumeLength
      v <- consumeRemaining
      return (padByteString len (Bit.cloneToByteString v))

-- | decode a ByteString that was encoded using `encodeByteString` with a default that's used if decoding fails.
decodeByteStringWithDefault :: ByteString -> Integer -> ByteString
decodeByteStringWithDefault d = fromRight d . run . runErrorNoCallStack @BitReadError . decodeByteString

sha256Natural :: Natural -> ByteString
sha256Natural = SHA256.hash . naturalToByteStringLE
