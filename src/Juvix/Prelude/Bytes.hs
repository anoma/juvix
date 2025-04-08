{-# LANGUAGE MagicHash #-}

module Juvix.Prelude.Bytes where

import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Builder qualified as BS
import Data.Primitive.ByteArray qualified as GHCByteArray
import Data.Primitive.Types
import GHC.Exts (Word (W#))
import GHC.Natural
import Juvix.Prelude.Base.Foundation

byteArrayToList :: (Prim a) => GHCByteArray -> [a]
byteArrayToList = GHCByteArray.foldrByteArray (:) mempty

byteArrayToBytes :: GHCByteArray -> [Word8]
byteArrayToBytes = byteArrayToList

-- | It assumes bytesPerWord == 8
naturalToWord64 :: Natural -> [Word64]
naturalToWord64 = \case
  NS w -> [fromIntegral (W# w)]
  NB b -> byteArrayToList (GHCByteArray.ByteArray b)

naturalToBytes :: Natural -> [Word8]
naturalToBytes = \case
  NS w -> wordToBytes (W# w)
  NB b -> byteArrayToBytes (GHCByteArray.ByteArray b)

-- | Pad a ByteString with zeros up to the smallest length such that is
-- divisible by `align`
padByteStringMod :: Int -> ByteString -> ByteString
padByteStringMod align bs =
  let (d, m) = divMod (BS.length bs) align
   in if
          | m == 0 -> bs
          | otherwise -> padByteString ((d + 1) * align) bs

padByteStringWord :: ByteString -> ByteString
padByteStringWord = padByteStringMod bytesPerWord

-- | Pad a ByteString with zeros up to a specified length
padByteString :: Int -> ByteString -> ByteString
padByteString n bs
  | BS.length bs >= n = bs
  | otherwise = BS.append bs (BS.replicate (n - BS.length bs) 0)

naturalToByteStringLELen :: Int -> Natural -> ByteString
naturalToByteStringLELen len = padByteString len . naturalToByteStringLE

-- | This is quadratic (`shiftR` is O(n))
naturalToByteStringOld :: Natural -> ByteString
naturalToByteStringOld = do
  BS.toStrict . BS.toLazyByteString . go
  where
    go :: Natural -> BS.Builder
    go = \case
      0 -> mempty
      n -> BS.word8 (fromIntegral n) <> go (n `shiftR` 8)

naturalToByteStringLE :: Natural -> ByteString
naturalToByteStringLE = naturalToByteString

-- | Little endian
naturalToByteStringHelper :: Bool -> Natural -> ByteString
naturalToByteStringHelper forceWord8 n
  -- most common case
  | (not forceWord8) && 8 == bytesPerWord =
      let w :: [Word64] = naturalToWord64 n
       in build (mconcat (map BS.word64LE w))
  | otherwise = BS.pack (naturalToBytes n)
  where
    build :: BS.Builder -> ByteString
    build = BS.toStrict . BS.toLazyByteString

naturalToByteString :: Natural -> ByteString
naturalToByteString = naturalToByteStringHelper False

-- | Little endian
wordToBytes :: Word -> [Word8]
wordToBytes w = [fromIntegral (w `shiftR` (i * 8)) | i <- [0 .. bytesPerWord - 1]]

-- | Platform dependent
bytesPerWord :: Int
bytesPerWord = sizeOf (impossible :: Word)

byteStringToNaturalOld :: ByteString -> Natural
byteStringToNaturalOld = fromInteger . byteStringToIntegerLE

byteStringToNatural :: ByteString -> Natural
byteStringToNatural = mkNatural . byteStringToWords

byteStringToWords :: ByteString -> [Word]
byteStringToWords = go []
  where
    go :: [Word] -> ByteString -> [Word]
    go acc b
      | BS.null b = acc
      | otherwise =
          let (w, ws') = BS.splitAt bytesPerWord b
              new :: Word =
                BS.foldr' (\wi wacc -> wacc `shiftL` bytesPerWord .|. fromIntegral wi) 0 w
           in go (new : acc) ws'

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
