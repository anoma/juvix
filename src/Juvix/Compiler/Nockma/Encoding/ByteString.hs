module Juvix.Compiler.Nockma.Encoding.ByteString where

import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BS
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base

atomToByteString :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r ByteString
atomToByteString = fmap naturalToByteString . nockNatural

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
