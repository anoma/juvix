{-# LANGUAGE MagicHash #-}

module Juvix.Prelude.Bytes where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS
import Data.Primitive.ByteArray qualified as GHCByteArray
import Data.Primitive.Types
import GHC.Exts (Word (W#))
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
-- divisible by the given arg
padByteStringMod :: Int -> ByteString -> ByteString
padByteStringMod align bs =
  let (d, m) = divMod (BS.length bs) align
   in if
          | m == 0 -> bs
          | otherwise -> padByteString ((d + 1) * align) bs

padByteString8 :: ByteString -> ByteString
padByteString8 = padByteStringMod 8

-- | Pad a ByteString with zeros up to a specified length
padByteString :: Int -> ByteString -> ByteString
padByteString n bs
  | BS.length bs >= n = bs
  | otherwise = BS.append bs (BS.replicate (n - BS.length bs) 0)

naturalToByteStringLELen :: Int -> Natural -> ByteString
naturalToByteStringLELen len = padByteString len . naturalToByteStringLE

naturalToByteStringOld :: Natural -> ByteString
naturalToByteStringOld = naturalToByteStringLE

-- | TODO: this is quadratic (`shiftR` is O(n))
naturalToByteStringLE :: Natural -> ByteString
naturalToByteStringLE = BS.toStrict . BS.toLazyByteString . go
  where
    go :: Natural -> BS.Builder
    go = \case
      0 -> mempty
      n -> BS.word8 (fromIntegral n) <> go (n `shiftR` 8)

-- | Little endian
naturalToByteStringTest :: Bool -> Natural -> ByteString
naturalToByteStringTest word64 n
  -- most common case
  | word64 && 8 == bytesPerWord =
      let w :: [Word64] = naturalToWord64 n
       in build (mconcat (map BS.word64LE w))
  | otherwise = BS.pack (naturalToBytes n)
  where
    build :: BS.Builder -> ByteString
    build = BS.toStrict . BS.toLazyByteString

naturalToByteString :: Natural -> ByteString
naturalToByteString = naturalToByteStringTest True

-- | Little endian
wordToBytes :: Word -> [Word8]
wordToBytes w = [fromIntegral (w `shiftR` (i * 8)) | i <- [0 .. bytesPerWord - 1]]

-- | Platform dependent
bytesPerWord :: Int
bytesPerWord = sizeOf (impossible :: Word)
