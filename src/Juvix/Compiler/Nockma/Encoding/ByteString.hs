{-# LANGUAGE MagicHash #-}

module Juvix.Compiler.Nockma.Encoding.ByteString where

import Crypto.Hash.SHA256 qualified as SHA256
import Data.Bit (Bit)
import Data.Bit qualified as Bit
import Data.ByteString qualified as BS
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Encoding.Effect.BitReader
import Juvix.Compiler.Nockma.Encoding.Effect.BitWriter
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base
import Juvix.Prelude.Bytes

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
