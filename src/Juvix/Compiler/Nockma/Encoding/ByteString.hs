module Juvix.Compiler.Nockma.Encoding.ByteString where

import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Builder qualified as BS
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base

-- | Encode an atom to little-endian bytes
atomToByteString :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r ByteString
atomToByteString = fmap naturalToByteString . nockNatural

-- | Encode an atom to little-endian bytes, padded with zeros up to a specified length
atomToByteStringLen :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => Int -> Atom a -> Sem r ByteString
atomToByteStringLen len = fmap (padByteString len) . atomToByteString

byteStringToAtom :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => ByteString -> Sem r (Atom a)
byteStringToAtom = fmap mkEmptyAtom . fromNatural . byteStringToNatural

byteStringToNatural :: ByteString -> Natural
byteStringToNatural = fromInteger . byteStringToIntegerLE

naturalToByteString :: Natural -> ByteString
naturalToByteString = integerToByteStringLE . toInteger

byteStringToIntegerLE :: ByteString -> Integer
byteStringToIntegerLE = BS.foldr (\b acc -> acc `shiftL` 8 .|. fromIntegral b) 0

integerToByteStringLE :: Integer -> ByteString
integerToByteStringLE = BS.toStrict . BS.toLazyByteString . go
  where
    go :: Integer -> BS.Builder
    go = \case
      0 -> mempty
      n -> BS.word8 (fromIntegral n) <> go (n `shiftR` 8)

integerToByteStringLELen :: Int -> Integer -> ByteString
integerToByteStringLELen len = padByteString len . integerToByteStringLE

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

-- | Encode an Int with a variable-length encoding
--
-- The input Int is encoded in 7 bit chunks in LSB ordering. The most significant
-- bit of each chunk is used to indicate when there are more bytes to read,
-- 1 meaning more bytes, 0 meaning no more bytes.
--
-- For example, the binary representation of 263202 is divided into 3 7-bit chunks:
--
-- 263202 =  10000   0001000  0100010
--           chunk1  chunk2   chunk3
--
-- The chunks are then combined using 3 bytes in LSB ordering, with a 1 in the MSB of the first
-- two bytes (indicating that another byte follows). The final byte has a 0 in the MSB bit.
--
--   chunk3    chunk2    chunk1
-- 1_0100010 1_0001000 0_0010000
encodeVarInt :: Int -> ByteString
encodeVarInt = \case
  0 -> BS.singleton 0
  n -> BS.toStrict (BB.toLazyByteString (buildVarInt n))
  where
    buildVarInt :: Int -> BB.Builder
    buildVarInt = \case
      0 -> mempty
      i ->
        let byteChunk = fromIntegral (i .&. 0x7F) -- Extract a 7-bit chunk
            next = i `shiftR` 7 -- Shift to the next 7-bit chunk
            currentByte =
              if
                  | next == 0 -> byteChunk -- No more bytes, so most significant bit for this chunk is 0
                  | otherwise -> byteChunk .|. 0x80 -- More bytes, so most significant bit for this chunk is 1
         in BB.word8 currentByte <> buildVarInt next

byteStringToIntegerBE :: ByteString -> Integer
byteStringToIntegerBE = foldl' go 0 . map (first byteStringChunkToInteger) . chunkByteString
  where
    chunkSize :: Int
    chunkSize = 1024

    go :: Integer -> (Integer, Int) -> Integer
    go acc (i, size) = acc `shiftL` (8 * size) .|. i

    -- Split the ByteString into chunks and store their sizes
    chunkByteString :: ByteString -> [(ByteString, Int)]
    chunkByteString bs
      | BS.null bs = []
      | otherwise =
          let (chunk, rest) = BS.splitAt chunkSize bs
           in (chunk, BS.length chunk) : chunkByteString rest

    byteStringChunkToInteger :: ByteString -> Integer
    byteStringChunkToInteger = BS.foldl' (\acc b -> acc `shiftL` 8 .|. fromIntegral b) 0

-- | encode a ByteString to an Integer (in MSB ordering) with its length as part of the encoding.
encodeByteString :: ByteString -> Integer
encodeByteString bs = byteStringToIntegerBE (encodedLength <> bs)
  where
    encodedLength :: ByteString
    encodedLength = encodeVarInt (BS.length bs)

-- | decode a ByteString that was encoded using `encodeByteString`
decodeByteString :: Integer -> ByteString
decodeByteString n = padByteString len bytes
  where
    (len, bytes) = decodeVarInt (integerToBytes n)

-- | Decode an integer in MSB ordering to a bytestring.
integerToBytes :: Integer -> ByteString
integerToBytes 0 = BS.singleton 0
integerToBytes n = BS.reverse $ BS.unfoldr go n
  where
    go :: Integer -> Maybe (Word8, Integer)
    go = \case
      0 -> Nothing
      i -> Just (fromIntegral (i .&. 0xff), i `shiftR` 8)

-- | Decode a variable-length encoded Int (using `encodeVarInt`) from the start of a ByteString.
--
-- An Int is accumulated from the least significant 7-bits chunk of each byte. The
-- most significant bit of each byte indicates if more bytes of the input should
-- be read. If the most significant bit is one, then there are more bytes, if it
-- is 0 then there are no more bytes.
--
-- For example:
--
-- byte1     byte2     byte3      remainder
-- 1_0100010 1_0001000 0_0010000  ...
--
-- The first byte has most significant bit = 1 so we accumulate the least significant 7 bits and continue.
--
-- acc: 100010
--
-- The second byte has most significant bit = 1 so we accumulate and continue. The bytes are
-- encoded using LSB ordering so we must shift this chunk left by 7:
--
-- acc: 0001000 0100010
--
-- The third byte has most significant bit = 0 so this is the last byte. We must shift this chunk by 2 * 7 = 14
--
-- result : 10000 0001000 0100010 = 263202
decodeVarInt :: ByteString -> (Int, ByteString)
decodeVarInt bs = go 0 0 bs
  where
    go :: Int -> Int -> ByteString -> (Int, ByteString)
    go acc toShift s = case BS.uncons s of
      Nothing -> (acc, BS.empty)
      Just (x, xs) ->
        if
            | x .&. 0x80 == 0 -> (acc .|. (fromIntegral x `shiftL` toShift), xs) -- The most significant bit is 0, no more bytes
            | otherwise ->
                let chunk = x .&. 0x7F -- Extract the next 7-bit chunk
                 in go (acc .|. (fromIntegral chunk `shiftL` toShift)) (toShift + 7) xs
