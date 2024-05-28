module Juvix.Compiler.Nockma.Encoding.ByteString where

import Data.Bit
import Juvix.Compiler.Nockma.Encoding.Base
import Juvix.Compiler.Nockma.Language
import Juvix.Prelude.Base

atomToByteString :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r ByteString
atomToByteString = fmap naturalToByteString . nockNatural

byteStringToAtom :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => ByteString -> Sem r (Atom a)
byteStringToAtom = fmap mkEmptyAtom . fromNatural . byteStringToNatural

byteStringToNatural :: ByteString -> Natural
byteStringToNatural = bitsToNatural . cloneFromByteString

naturalToByteString :: Natural -> ByteString
naturalToByteString = cloneToByteString . naturalToBits

textToNatural :: Text -> Natural
textToNatural = byteStringToNatural . encodeUtf8

bitsToNatural :: Vector Bit -> Natural
bitsToNatural = fromInteger . vectorBitsToInteger

naturalToBits :: Natural -> Vector Bit
naturalToBits = integerToVectorBits . toInteger

atomToText :: (NockNatural a, Member (Error (ErrNockNatural a)) r) => Atom a -> Sem r Text
atomToText = fmap decodeUtf8Lenient . atomToByteString

-- | Construct an atom formed by concatenating the bits of two atoms, where each atom represents a sequence of bytes
atomConcatenateBytes :: forall a r. (NockNatural a, Member (Error (ErrNockNatural a)) r) => Atom a -> Atom a -> Sem r (Atom a)
atomConcatenateBytes l r = do
  -- cloneToByteString ensures that the bytestring is zero-padded up to the byte boundary
  lBs <- cloneToByteString <$> atomToBits l
  rBs <- cloneToByteString <$> atomToBits r
  byteStringToAtom (lBs <> rBs)
  where
    atomToBits :: Atom a -> Sem r (Vector Bit)
    atomToBits = fmap naturalToBits . nockNatural

mkEmptyAtom :: a -> Atom a
mkEmptyAtom x =
  Atom
    { _atomInfo = emptyAtomInfo,
      _atom = x
    }
